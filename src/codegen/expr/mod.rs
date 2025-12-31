//! Expression code generation from TIR

mod binary;
mod call;
mod composite;
mod convert;
mod index;
mod literal;
mod slice;
mod unary;

use crate::analysis::{SymbolId, TypeId};
use crate::codegen::Codegen;
use crate::codegen::runtime::names;
use crate::tir::{TExpr, TLValue};
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, PointerValue};

impl<'ctx> Codegen<'ctx> {
    pub fn gen_expr(&mut self, expr: &TExpr) -> BasicValueEnum<'ctx> {
        match expr {
            // Literals
            TExpr::Int { value, type_id } => self.gen_int(*value, *type_id),
            TExpr::Uint { value, type_id } => self.gen_uint(*value, *type_id),
            TExpr::Float { value, type_id } => self.gen_float(*value, *type_id),
            TExpr::String { value, type_id: _ } => self.gen_string_literal(value),
            TExpr::Nil { type_id } => self.gen_nil(*type_id),

            // Variable/constant reference
            TExpr::Var { symbol_id, type_id } => self.gen_var(*symbol_id, *type_id),
            TExpr::Const { value, type_id, .. } => self.gen_const_value(value, *type_id),

            // Operations
            TExpr::Binary {
                op,
                left,
                right,
                type_id,
            } => self.gen_binary(op, left, right, *type_id),
            TExpr::Unary {
                op,
                operand,
                type_id,
            } => self.gen_unary(*op, operand, *type_id),

            // Calls
            TExpr::Call {
                kind,
                args,
                type_id,
            } => self.gen_call(kind, args, *type_id),

            // Type conversion
            TExpr::Convert {
                kind,
                expr,
                type_id,
            } => self.gen_convert(*kind, expr, *type_id),

            // Field access
            TExpr::Field {
                base,
                struct_type,
                field_index,
                auto_deref,
                type_id,
            } => self.gen_field(base, *struct_type, *field_index, *auto_deref, *type_id),

            // Index operation
            TExpr::Index {
                kind,
                base,
                index,
                type_id,
            } => self.gen_index(kind, base, index, *type_id),

            // Slice operation
            TExpr::Slice {
                kind,
                base,
                low,
                high,
                type_id: _,
            } => self.gen_slice_expr(kind, base, low, high),

            // Composite literal
            TExpr::Composite { kind, type_id } => self.gen_composite(kind, *type_id),

            // Address-of
            TExpr::AddrOf {
                operand,
                type_id: _,
            } => self.gen_addr_of(operand).into(),

            // Dereference
            TExpr::Deref { operand, type_id } => self.gen_deref(operand, *type_id),
        }
    }

    /// Generate an address for a TIR lvalue
    pub fn gen_lvalue(&mut self, lvalue: &TLValue) -> PointerValue<'ctx> {
        match lvalue {
            TLValue::Var { symbol_id, .. } => self.gen_var_addr(*symbol_id),
            TLValue::Field {
                base,
                struct_type,
                field_index,
                auto_deref,
                ..
            } => self.gen_field_addr(base, *struct_type, *field_index, *auto_deref),
            TLValue::Index {
                kind,
                base,
                index,
                type_id,
            } => self.gen_index_addr(kind, base, index, *type_id),
            TLValue::Deref { operand, .. } => self.gen_expr(operand).into_pointer_value(),
            TLValue::Blank => {
                // Allocate dummy storage
                let i64_type = self.context.i64_type();
                self.builder.build_alloca(i64_type, "blank").unwrap()
            }
        }
    }

    /// Generate address from an expression
    pub fn gen_addr_of(&mut self, expr: &TExpr) -> PointerValue<'ctx> {
        match expr {
            TExpr::Var { symbol_id, .. } => self.gen_var_addr(*symbol_id),
            TExpr::Field {
                base,
                struct_type,
                field_index,
                auto_deref,
                ..
            } => self.gen_field_addr(base, *struct_type, *field_index, *auto_deref),
            TExpr::Index {
                kind,
                base,
                index,
                type_id,
            } => self.gen_index_addr(kind, base, index, *type_id),
            TExpr::Deref { operand, .. } => self.gen_expr(operand).into_pointer_value(),
            TExpr::Composite { kind, type_id } => {
                // Allocate temp storage and return pointer
                let val = self.gen_composite(kind, *type_id);
                let llvm_type = self.llvm_type(*type_id);
                let ptr = self.builder.build_alloca(llvm_type, "lit_temp").unwrap();
                self.builder.build_store(ptr, val).unwrap();
                ptr
            }
            // Allocate temp storage
            _ => {
                let val = self.gen_expr(expr);
                let type_id = expr.type_id();
                let llvm_type = self.llvm_type(type_id);
                let ptr = self.builder.build_alloca(llvm_type, "temp").unwrap();
                self.builder.build_store(ptr, val).unwrap();
                ptr
            }
        }
    }

    /// Generate code for variable load
    fn gen_var(&mut self, symbol_id: SymbolId, type_id: TypeId) -> BasicValueEnum<'ctx> {
        // Check local variables first
        if let Some((ptr, tid)) = self.get_local(symbol_id) {
            return self.build_load(ptr, tid, "var");
        }

        // Check globals
        if let Some(ptr) = self.get_global(symbol_id) {
            return self.build_load(ptr, type_id, "global");
        }

        // Check functions
        if let Some(&fn_val) = self.functions.get(&symbol_id) {
            return fn_val.as_global_value().as_pointer_value().into();
        }

        panic!("codegen bug: variable not found: {:?}", symbol_id);
    }

    /// Get address of a variable
    fn gen_var_addr(&self, symbol_id: SymbolId) -> PointerValue<'ctx> {
        // Check local variables first
        if let Some((ptr, _)) = self.get_local(symbol_id) {
            return ptr;
        }

        // Check globals
        if let Some(ptr) = self.get_global(symbol_id) {
            return ptr;
        }

        panic!("codegen bug: variable address not found: {:?}", symbol_id);
    }

    /// Generate field access
    fn gen_field(
        &mut self,
        base: &TExpr,
        struct_type: TypeId,
        field_index: u32,
        auto_deref: u32,
        type_id: TypeId,
    ) -> BasicValueEnum<'ctx> {
        let ptr = self.gen_field_addr(base, struct_type, field_index, auto_deref);
        self.build_load(ptr, type_id, "field")
    }

    /// Get address of a field
    fn gen_field_addr(
        &mut self,
        base: &TExpr,
        struct_type: TypeId,
        field_index: u32,
        auto_deref: u32,
    ) -> PointerValue<'ctx> {
        let mut base_ptr = self.gen_addr_of(base);

        // Auto-dereference pointers
        for _ in 0..auto_deref {
            base_ptr = self
                .builder
                .build_load(self.ptr_type(), base_ptr, "deref")
                .unwrap()
                .into_pointer_value();
        }

        let llvm_struct_type = self.llvm_type(struct_type);
        if let BasicTypeEnum::StructType(st) = llvm_struct_type {
            self.builder
                .build_struct_gep(st, base_ptr, field_index, "field_ptr")
                .unwrap()
        } else {
            base_ptr
        }
    }

    /// Generate dereference
    fn gen_deref(&mut self, operand: &TExpr, type_id: TypeId) -> BasicValueEnum<'ctx> {
        let ptr = self.gen_expr(operand).into_pointer_value();
        self.gen_nil_check(ptr);
        self.build_load(ptr, type_id, "deref")
    }

    /// Generate nil pointer check
    pub fn gen_nil_check(&mut self, ptr: PointerValue<'ctx>) {
        let current_fn = self.current_function();
        let panic_bb = self.context.append_basic_block(current_fn, "nil.panic");
        let ok_bb = self.context.append_basic_block(current_fn, "nil.ok");

        let is_null = self.builder.build_is_null(ptr, "is_nil").unwrap();

        self.builder
            .build_conditional_branch(is_null, panic_bb, ok_bb)
            .unwrap();

        // Panic block
        self.builder.position_at_end(panic_bb);
        let panic_fn = self.get_runtime_fn(names::GO_PANIC_NIL);
        self.builder.build_call(panic_fn, &[], "").unwrap();
        self.builder.build_unreachable().unwrap();

        // Continue in ok block
        self.builder.position_at_end(ok_bb);
    }
}
