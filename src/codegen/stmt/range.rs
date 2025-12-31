//! Range loop code generation from TIR

use inkwell::IntPredicate;
use inkwell::values::{IntValue, PointerValue};

use crate::analysis::{SymbolId, TypeId};
use crate::codegen::Codegen;
use crate::codegen::runtime::names;
use crate::codegen::types::{SLICE_DATA_IDX, SLICE_LEN_IDX};
use crate::tir::{TExpr, TStmt};

impl<'ctx> Codegen<'ctx> {
    pub fn gen_range_array(
        &mut self,
        array_len: u64,
        elem_type: TypeId,
        range_expr: &TExpr,
        key_sym: Option<SymbolId>,
        val_sym: Option<SymbolId>,
        body: &[TStmt],
    ) {
        let cond_block = self.append_block("range.cond");
        let body_block = self.append_block("range.body");
        let post_block = self.append_block("range.post");
        let after_block = self.append_block("range.after");

        // Init
        let i64_type = self.context.i64_type();
        let idx_ptr = self.builder.build_alloca(i64_type, "range.idx").unwrap();
        self.builder
            .build_store(idx_ptr, i64_type.const_zero())
            .unwrap();

        let len = i64_type.const_int(array_len, false);
        let array_ptr = self.gen_addr_of(range_expr);

        let array_type = self.llvm_type(range_expr.type_id());
        let zero = i64_type.const_zero();
        let data_ptr = unsafe {
            self.builder
                .build_gep(array_type, array_ptr, &[zero, zero], "arr.data")
                .unwrap()
        };

        let key_ptr = self.maybe_alloca_range_var(key_sym, TypeId::INT);
        let value_ptr = self.maybe_alloca_range_var(val_sym, elem_type);

        self.builder.build_unconditional_branch(cond_block).unwrap();
        self.enter_loop(post_block, after_block);

        // Condition
        self.position_at_end(cond_block);
        let idx = self.build_load_i64(idx_ptr);
        let cmp = self
            .builder
            .build_int_compare(IntPredicate::SLT, idx, len, "range.cmp")
            .unwrap();
        self.build_cond_br(cmp, body_block, after_block);

        // Body
        self.position_at_end(body_block);
        self.bind_range_vars(key_ptr, value_ptr, idx_ptr, Some(data_ptr), elem_type);
        self.gen_block(body);
        self.build_br_if_needed(post_block);

        // Post
        self.position_at_end(post_block);
        let idx = self.build_load_i64(idx_ptr);
        let next = self
            .builder
            .build_int_add(idx, i64_type.const_int(1, false), "idx.next")
            .unwrap();
        self.builder.build_store(idx_ptr, next).unwrap();
        self.builder.build_unconditional_branch(cond_block).unwrap();

        self.exit_loop();
        self.position_at_end(after_block);
    }

    pub fn gen_range_slice(
        &mut self,
        elem_type: TypeId,
        range_expr: &TExpr,
        key_sym: Option<SymbolId>,
        val_sym: Option<SymbolId>,
        body: &[TStmt],
    ) {
        let cond_block = self.append_block("range.cond");
        let body_block = self.append_block("range.body");
        let post_block = self.append_block("range.post");
        let after_block = self.append_block("range.after");

        // Init
        let i64_type = self.context.i64_type();
        let idx_ptr = self.builder.build_alloca(i64_type, "range.idx").unwrap();
        self.builder
            .build_store(idx_ptr, i64_type.const_zero())
            .unwrap();

        let collection = self.gen_expr(range_expr);
        let slice_struct = collection.into_struct_value();
        let len = self
            .builder
            .build_extract_value(slice_struct, SLICE_LEN_IDX, "len")
            .unwrap()
            .into_int_value();
        let data_ptr = self
            .builder
            .build_extract_value(slice_struct, SLICE_DATA_IDX, "data")
            .unwrap()
            .into_pointer_value();

        let key_ptr = self.maybe_alloca_range_var(key_sym, TypeId::INT);
        let value_ptr = self.maybe_alloca_range_var(val_sym, elem_type);

        self.builder.build_unconditional_branch(cond_block).unwrap();
        self.enter_loop(post_block, after_block);

        // Condition
        self.position_at_end(cond_block);
        let idx = self.build_load_i64(idx_ptr);
        let cmp = self
            .builder
            .build_int_compare(IntPredicate::SLT, idx, len, "range.cmp")
            .unwrap();
        self.build_cond_br(cmp, body_block, after_block);

        // Body
        self.position_at_end(body_block);
        self.bind_range_vars(key_ptr, value_ptr, idx_ptr, Some(data_ptr), elem_type);
        self.gen_block(body);
        self.build_br_if_needed(post_block);

        // Post
        self.position_at_end(post_block);
        let idx = self.build_load_i64(idx_ptr);
        let next = self
            .builder
            .build_int_add(idx, i64_type.const_int(1, false), "idx.next")
            .unwrap();
        self.builder.build_store(idx_ptr, next).unwrap();
        self.builder.build_unconditional_branch(cond_block).unwrap();

        self.exit_loop();
        self.position_at_end(after_block);
    }

    pub fn gen_range_string(
        &mut self,
        range_expr: &TExpr,
        key_sym: Option<SymbolId>,
        val_sym: Option<SymbolId>,
        body: &[TStmt],
    ) {
        let cond_block = self.append_block("range.cond");
        let body_block = self.append_block("range.body");
        let post_block = self.append_block("range.post");
        let after_block = self.append_block("range.after");

        // Init
        let i64_type = self.context.i64_type();
        let idx_ptr = self.builder.build_alloca(i64_type, "range.idx").unwrap();
        self.builder
            .build_store(idx_ptr, i64_type.const_zero())
            .unwrap();

        let string_val = self.gen_expr(range_expr);
        let string_struct = string_val.into_struct_value();
        let data_ptr = self
            .builder
            .build_extract_value(string_struct, SLICE_DATA_IDX, "str.data")
            .unwrap()
            .into_pointer_value();
        let str_len = self
            .builder
            .build_extract_value(string_struct, SLICE_LEN_IDX, "str.len")
            .unwrap()
            .into_int_value();

        let key_ptr = self.maybe_alloca_range_var(key_sym, TypeId::INT);
        let value_ptr = self.maybe_alloca_range_var(val_sym, TypeId::INT32);

        let width_ptr = self.builder.build_alloca(i64_type, "rune.width").unwrap();

        self.builder.build_unconditional_branch(cond_block).unwrap();
        self.enter_loop(post_block, after_block);

        // Condition
        self.position_at_end(cond_block);
        let idx = self.build_load_i64(idx_ptr);
        let cmp = self
            .builder
            .build_int_compare(IntPredicate::SLT, idx, str_len, "range.cmp")
            .unwrap();
        self.build_cond_br(cmp, body_block, after_block);

        // Body
        self.position_at_end(body_block);
        let idx = self.build_load_i64(idx_ptr);

        let curr_ptr = unsafe {
            self.builder
                .build_gep(self.context.i8_type(), data_ptr, &[idx], "curr.ptr")
                .unwrap()
        };

        let remaining = self
            .builder
            .build_int_sub(str_len, idx, "remaining")
            .unwrap();

        let decode_fn = self.get_runtime_fn(names::GO_STRING_DECODE_RUNE);
        let result = self
            .builder
            .build_call(decode_fn, &[curr_ptr.into(), remaining.into()], "decoded")
            .unwrap()
            .try_as_basic_value()
            .basic()
            .unwrap()
            .into_int_value();

        let rune = self
            .builder
            .build_int_truncate(result, self.context.i32_type(), "rune")
            .unwrap();
        let width = self
            .builder
            .build_right_shift(result, i64_type.const_int(32, false), false, "width")
            .unwrap();

        if let Some(kp) = key_ptr {
            self.builder.build_store(kp, idx).unwrap();
        }

        if let Some(vp) = value_ptr {
            self.builder.build_store(vp, rune).unwrap();
        }

        self.builder.build_store(width_ptr, width).unwrap();
        self.gen_block(body);
        self.build_br_if_needed(post_block);

        // Post
        self.position_at_end(post_block);
        let idx = self.build_load_i64(idx_ptr);
        let width = self.build_load_i64(width_ptr);
        let next = self.builder.build_int_add(idx, width, "idx.next").unwrap();
        self.builder.build_store(idx_ptr, next).unwrap();
        self.builder.build_unconditional_branch(cond_block).unwrap();

        self.exit_loop();
        self.position_at_end(after_block);
    }

    pub fn gen_range_map(
        &mut self,
        key_type: TypeId,
        val_type: TypeId,
        range_expr: &TExpr,
        key_sym: Option<SymbolId>,
        val_sym: Option<SymbolId>,
        body: &[TStmt],
    ) {
        let cond_block = self.append_block("range.cond");
        let body_block = self.append_block("range.body");
        let after_block = self.append_block("range.after");

        // Init
        let map_ptr = self.gen_expr(range_expr).into_pointer_value();
        let iter_init_fn = self.get_runtime_fn(names::GO_MAP_ITER_INIT);
        let iter = self
            .builder
            .build_call(iter_init_fn, &[map_ptr.into()], "iter")
            .unwrap()
            .try_as_basic_value()
            .basic()
            .unwrap()
            .into_pointer_value();

        let key_ptr = self.maybe_alloca_range_var(key_sym, key_type);
        let value_ptr = self.maybe_alloca_range_var(val_sym, val_type);

        self.builder.build_unconditional_branch(cond_block).unwrap();
        self.enter_loop(cond_block, after_block);

        // Condition
        self.position_at_end(cond_block);
        let iter_next_fn = self.get_runtime_fn(names::GO_MAP_ITER_NEXT);
        let has_next = self
            .builder
            .build_call(iter_next_fn, &[iter.into()], "has_next")
            .unwrap()
            .try_as_basic_value()
            .basic()
            .unwrap()
            .into_int_value();
        let zero = self.context.i32_type().const_zero();
        let cmp = self
            .builder
            .build_int_compare(IntPredicate::NE, has_next, zero, "")
            .unwrap();
        self.build_cond_br(cmp, body_block, after_block);

        // Body
        self.position_at_end(body_block);

        // Key
        if let Some(kp) = key_ptr {
            let iter_key_fn = self.get_runtime_fn(names::GO_MAP_ITER_KEY);
            let key_raw_ptr = self
                .builder
                .build_call(iter_key_fn, &[iter.into()], "key_ptr")
                .unwrap()
                .try_as_basic_value()
                .basic()
                .unwrap()
                .into_pointer_value();
            let key_llvm_type = self.llvm_type(key_type);
            let key_val = self
                .builder
                .build_load(key_llvm_type, key_raw_ptr, "key")
                .unwrap();
            self.builder.build_store(kp, key_val).unwrap();
        }

        // Value
        if let Some(vp) = value_ptr {
            let iter_val_fn = self.get_runtime_fn(names::GO_MAP_ITER_VALUE);
            let val_raw_ptr = self
                .builder
                .build_call(iter_val_fn, &[iter.into()], "val_ptr")
                .unwrap()
                .try_as_basic_value()
                .basic()
                .unwrap()
                .into_pointer_value();
            let val_llvm_type = self.llvm_type(val_type);
            let val_val = self
                .builder
                .build_load(val_llvm_type, val_raw_ptr, "val")
                .unwrap();
            self.builder.build_store(vp, val_val).unwrap();
        }

        self.gen_block(body);
        self.build_br_if_needed(cond_block);

        self.exit_loop();
        self.position_at_end(after_block);
    }

    fn maybe_alloca_range_var(
        &mut self,
        sym: Option<SymbolId>,
        type_id: TypeId,
    ) -> Option<PointerValue<'ctx>> {
        sym.map(|symbol_id| {
            let llvm_type = self.llvm_type(type_id);
            let ptr = self.builder.build_alloca(llvm_type, "range.var").unwrap();
            self.set_local(symbol_id, ptr, type_id);
            ptr
        })
    }

    fn build_load_i64(&self, ptr: PointerValue<'ctx>) -> IntValue<'ctx> {
        self.builder
            .build_load(self.context.i64_type(), ptr, "")
            .unwrap()
            .into_int_value()
    }

    fn bind_range_vars(
        &mut self,
        key_ptr: Option<PointerValue<'ctx>>,
        value_ptr: Option<PointerValue<'ctx>>,
        idx_ptr: PointerValue<'ctx>,
        data_ptr: Option<PointerValue<'ctx>>,
        elem_type: TypeId,
    ) {
        // Key
        if let Some(kp) = key_ptr {
            let idx = self.build_load_i64(idx_ptr);
            self.builder.build_store(kp, idx).unwrap();
        }

        // Value
        if let (Some(vp), Some(dp)) = (value_ptr, data_ptr) {
            let idx = self.build_load_i64(idx_ptr);
            let elem_llvm_type = self.llvm_type(elem_type);

            let elem_ptr = unsafe {
                self.builder
                    .build_gep(elem_llvm_type, dp, &[idx], "elem.ptr")
                    .unwrap()
            };

            let elem_val = self
                .builder
                .build_load(elem_llvm_type, elem_ptr, "elem")
                .unwrap();
            self.builder.build_store(vp, elem_val).unwrap();
        }
    }
}
