//! LLVM IR code generation from Typed Intermediate Representation (TIR)

mod context;
mod expr;
mod runtime;
mod stmt;
mod types;

use std::collections::HashMap;
use std::path::Path;

use inkwell::OptimizationLevel;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::BasicTypeEnum;
use inkwell::values::{FunctionValue, PointerValue};

use crate::analysis::{SymbolId, TypeArena, TypeId, Universe};
use crate::tir::{TFunction, TGlobal, TMethod, TProgram};

/// Holds all LLVM context and state needed for IR generation
pub struct Codegen<'ctx> {
    // LLVM core
    pub(crate) context: &'ctx Context,
    pub(crate) module: Module<'ctx>,
    pub(crate) builder: Builder<'ctx>,

    // Type information from analysis
    pub(crate) types: TypeArena,
    pub(crate) universe: Universe,

    // Type cache: Go TypeId -> LLVM type
    pub(crate) llvm_types: HashMap<TypeId, BasicTypeEnum<'ctx>>,

    // Symbol mappings
    pub(crate) globals: HashMap<SymbolId, PointerValue<'ctx>>,
    pub(crate) locals: HashMap<SymbolId, (PointerValue<'ctx>, TypeId)>,
    pub(crate) functions: HashMap<SymbolId, FunctionValue<'ctx>>,
    pub(crate) methods: HashMap<String, FunctionValue<'ctx>>,

    // String literal deduplication
    pub(crate) string_intern: HashMap<String, PointerValue<'ctx>>,

    // Current compilation state
    pub(crate) current_fn: Option<FunctionValue<'ctx>>,

    // Control flow stacks
    pub(crate) break_stack: Vec<BasicBlock<'ctx>>,
    pub(crate) continue_stack: Vec<BasicBlock<'ctx>>,
}

impl<'ctx> Codegen<'ctx> {
    pub fn new(
        context: &'ctx Context,
        types: TypeArena,
        universe: Universe,
        module_name: &str,
    ) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        let mut codegen = Self {
            context,
            module,
            builder,
            types,
            universe,
            llvm_types: HashMap::new(),
            globals: HashMap::new(),
            locals: HashMap::new(),
            functions: HashMap::new(),
            methods: HashMap::new(),
            string_intern: HashMap::new(),
            current_fn: None,
            break_stack: Vec::new(),
            continue_stack: Vec::new(),
        };

        codegen.init_type_cache();
        codegen.declare_runtime_functions();

        codegen
    }

    pub fn compile(&mut self, program: &TProgram) {
        // Pass 1: Forward declare all procedures
        self.register_functions(&program.functions);
        self.register_methods(&program.methods);

        // Pass 2: Compile global variables
        self.compile_globals(&program.globals);

        // Pass 3: Compile procedure bodies
        self.compile_function_bodies(&program.functions);
        self.compile_method_bodies(&program.methods);
    }

    fn register_functions(&mut self, functions: &[TFunction]) {
        for func in functions {
            let fn_type = self.build_function_type(&func.params, func.result);

            // Runtime wraps go_main with the real main
            let (func_name, linkage) = if func.name == "main" {
                ("go_main", Some(Linkage::External))
            } else {
                (func.name.as_str(), None)
            };

            let fn_val = self.module.add_function(func_name, fn_type, linkage);
            self.functions.insert(func.symbol_id, fn_val);
        }
    }

    fn register_methods(&mut self, methods: &[TMethod]) {
        for method in methods {
            let fn_type =
                self.build_method_type(method.receiver_type, &method.params, method.result);

            let fn_val = self
                .module
                .add_function(&method.mangled_name, fn_type, None);
            self.methods.insert(method.mangled_name.clone(), fn_val);
        }
    }

    fn compile_globals(&mut self, globals: &[TGlobal]) {
        for global in globals {
            let llvm_type = self.llvm_type(global.type_id);
            let gv = self.module.add_global(llvm_type, None, &global.name);
            gv.set_linkage(Linkage::Internal);

            // Initialize with value or zero
            let init_val = if let Some(ref init) = global.init {
                self.gen_const_init(init)
            } else {
                self.gen_zero(global.type_id)
            };
            gv.set_initializer(&init_val);

            self.globals.insert(global.symbol_id, gv.as_pointer_value());
        }
    }

    fn compile_function_bodies(&mut self, functions: &[TFunction]) {
        for func in functions {
            self.compile_function(func);
        }
    }

    fn compile_function(&mut self, func: &TFunction) {
        let fn_val = *self
            .functions
            .get(&func.symbol_id)
            .expect("codegen bug: function not registered");

        // Entry block
        let entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry);
        self.current_fn = Some(fn_val);
        self.locals.clear();

        // Parameters
        for (i, (sym_id, type_id)) in func.params.iter().enumerate() {
            let param_val = fn_val
                .get_nth_param(i as u32)
                .expect("codegen bug: param count mismatch");
            let alloca = self.create_entry_block_alloca("param", *type_id);
            self.builder.build_store(alloca, param_val).unwrap();
            self.locals.insert(*sym_id, (alloca, *type_id));
        }

        // Body
        for stmt in &func.body {
            self.gen_stmt(stmt);
            if self.current_block_terminated() {
                break;
            }
        }

        // Implicit return if needed
        if !self.current_block_terminated() {
            if !func.result.is_some() {
                self.builder.build_return(None).unwrap();
            } else {
                panic!("codegen bug: unterminated function");
            }
        }

        self.current_fn = None;
    }

    fn compile_method_bodies(&mut self, methods: &[TMethod]) {
        for method in methods {
            self.compile_method(method);
        }
    }

    fn compile_method(&mut self, method: &TMethod) {
        let fn_val = *self
            .methods
            .get(&method.mangled_name)
            .expect("codegen bug: method not registered");

        // Entry block
        let entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry);
        self.current_fn = Some(fn_val);
        self.locals.clear();

        // Receiver parameter
        let recv_ptr = fn_val
            .get_nth_param(0)
            .expect("codegen bug: no receiver")
            .into_pointer_value();

        let recv_base_type = if method.receiver_is_pointer {
            self.types
                .is_pointer(method.receiver_type)
                .unwrap_or(method.receiver_type)
        } else {
            method.receiver_type
        };

        if method.receiver_is_pointer {
            // Pointer receiver
            let ptr_type = self.types.pointer(recv_base_type);
            let alloca = self.create_entry_block_alloca("recv", ptr_type);
            self.builder.build_store(alloca, recv_ptr).unwrap();
            self.locals
                .insert(method.receiver_sym, (alloca, method.receiver_type));
        } else {
            // Value receiver
            self.locals
                .insert(method.receiver_sym, (recv_ptr, recv_base_type));
        }

        // Other parameters
        for (i, (sym_id, type_id)) in method.params.iter().enumerate() {
            let param_val = fn_val
                .get_nth_param((i + 1) as u32)
                .expect("codegen bug: param count mismatch");
            let alloca = self.create_entry_block_alloca("param", *type_id);
            self.builder.build_store(alloca, param_val).unwrap();
            self.locals.insert(*sym_id, (alloca, *type_id));
        }

        // Body
        for stmt in &method.body {
            self.gen_stmt(stmt);
            if self.current_block_terminated() {
                break;
            }
        }

        // Implicit return if needed
        if !self.current_block_terminated() {
            if method.result.is_some() {
                self.builder.build_unreachable().unwrap();
            } else {
                self.builder.build_return(None).unwrap();
            }
        }

        self.current_fn = None;
    }

    pub fn write_ir(&self, path: &Path) -> Result<(), String> {
        self.module.print_to_file(path).map_err(|e| e.to_string())
    }

    pub fn write_object(&self, path: &Path) -> Result<(), String> {
        let machine = self.get_target_machine()?;
        self.module
            .run_passes("default<O3>", &machine, PassBuilderOptions::create())
            .expect("Failed to run optimization passes");
        machine
            .write_to_file(&self.module, FileType::Object, path)
            .map_err(|e| e.to_string())
    }

    fn get_target_machine(&self) -> Result<TargetMachine, String> {
        Target::initialize_native(&InitializationConfig::default()).map_err(|e| e.to_string())?;

        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).map_err(|e| e.to_string())?;

        target
            .create_target_machine(
                &triple,
                "generic",
                "",
                OptimizationLevel::Aggressive,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .ok_or_else(|| "Failed to create target machine".to_string())
    }
}
