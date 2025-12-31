//! Compilation context and helper methods

use inkwell::basic_block::BasicBlock;
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue};

use crate::analysis::{SymbolId, TypeId};
use crate::codegen::Codegen;

impl<'ctx> Codegen<'ctx> {
    pub fn append_block(&self, name: &str) -> BasicBlock<'ctx> {
        self.context
            .append_basic_block(self.current_function(), name)
    }

    pub fn position_at_end(&self, block: BasicBlock<'ctx>) {
        self.builder.position_at_end(block);
    }

    pub fn current_block_terminated(&self) -> bool {
        self.builder
            .get_insert_block()
            .map(|b| b.get_terminator().is_some())
            .unwrap_or(true)
    }

    pub fn build_br_if_needed(&self, dest: BasicBlock<'ctx>) {
        if !self.current_block_terminated() {
            self.builder.build_unconditional_branch(dest).unwrap();
        }
    }

    /// Conditional branch
    pub fn build_cond_br(
        &self,
        cond: IntValue<'ctx>,
        then_block: BasicBlock<'ctx>,
        else_block: BasicBlock<'ctx>,
    ) {
        self.builder
            .build_conditional_branch(cond, then_block, else_block)
            .unwrap();
    }

    #[inline]
    pub(crate) fn current_function(&self) -> FunctionValue<'ctx> {
        self.current_fn.expect("codegen bug: no current function")
    }

    pub(crate) fn enter_loop(
        &mut self,
        continue_block: BasicBlock<'ctx>,
        break_block: BasicBlock<'ctx>,
    ) {
        self.continue_stack.push(continue_block);
        self.break_stack.push(break_block);
    }

    pub(crate) fn exit_loop(&mut self) {
        self.continue_stack.pop();
        self.break_stack.pop();
    }

    pub(crate) fn enter_switch(&mut self, break_block: BasicBlock<'ctx>) {
        self.break_stack.push(break_block);
    }

    pub(crate) fn exit_switch(&mut self) {
        self.break_stack.pop();
    }

    pub(crate) fn continue_target(&self) -> BasicBlock<'ctx> {
        *self
            .continue_stack
            .last()
            .expect("codegen bug: continue outside loop")
    }

    pub(crate) fn break_target(&self) -> BasicBlock<'ctx> {
        *self
            .break_stack
            .last()
            .expect("codegen bug: break outside loop/switch")
    }

    pub fn get_global(&self, sym_id: SymbolId) -> Option<PointerValue<'ctx>> {
        self.globals.get(&sym_id).copied()
    }
    pub fn get_local(&self, sym_id: SymbolId) -> Option<(PointerValue<'ctx>, TypeId)> {
        self.locals.get(&sym_id).copied()
    }
    pub fn set_local(&mut self, sym_id: SymbolId, ptr: PointerValue<'ctx>, type_id: TypeId) {
        self.locals.insert(sym_id, (ptr, type_id));
    }
    pub fn create_entry_block_alloca(&mut self, name: &str, type_id: TypeId) -> PointerValue<'ctx> {
        let llvm_type = self.llvm_type(type_id);
        let function = self.current_function();
        let entry = function.get_first_basic_block().unwrap();

        // Save current insert point
        let current_block = self.builder.get_insert_block();

        // Position at start of entry block
        match entry.get_first_instruction() {
            Some(inst) => self.builder.position_before(&inst),
            None => self.builder.position_at_end(entry),
        }

        let alloca = self.builder.build_alloca(llvm_type, name).unwrap();

        // Restore original position
        if let Some(block) = current_block {
            self.builder.position_at_end(block);
        }

        alloca
    }

    pub fn build_load(
        &mut self,
        ptr: PointerValue<'ctx>,
        type_id: TypeId,
        name: &str,
    ) -> BasicValueEnum<'ctx> {
        let llvm_type = self.llvm_type(type_id);
        self.builder.build_load(llvm_type, ptr, name).unwrap()
    }
}
