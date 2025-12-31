//! Index operation code generation from TIR

use inkwell::IntPredicate;
use inkwell::values::{BasicValueEnum, IntValue, PointerValue};

use crate::analysis::TypeId;
use crate::codegen::Codegen;
use crate::codegen::runtime::names;
use crate::codegen::types::{SLICE_DATA_IDX, SLICE_LEN_IDX};
use crate::tir::{TExpr, TIndexKind};

impl<'ctx> Codegen<'ctx> {
    pub fn gen_index(
        &mut self,
        kind: &TIndexKind,
        base: &TExpr,
        index: &TExpr,
        type_id: TypeId,
    ) -> BasicValueEnum<'ctx> {
        let ptr = self.gen_index_addr(kind, base, index, type_id);
        self.build_load(ptr, type_id, "idx")
    }

    pub fn gen_index_addr(
        &mut self,
        kind: &TIndexKind,
        base: &TExpr,
        index: &TExpr,
        _type_id: TypeId,
    ) -> PointerValue<'ctx> {
        match kind {
            TIndexKind::Array { len } => {
                let base_type = base.type_id();
                let array_llvm_type = self.llvm_type(base_type);
                let base_ptr = self.gen_addr_of_inner(base);
                let idx = self.gen_expr(index).into_int_value();

                let len_val = self.context.i64_type().const_int(*len, false);
                self.gen_bounds_check(idx, len_val);

                let zero = self.context.i64_type().const_zero();
                unsafe {
                    self.builder
                        .build_gep(array_llvm_type, base_ptr, &[zero, idx], "elem")
                        .unwrap()
                }
            }

            TIndexKind::Slice { elem_type } => {
                let elem_llvm_type = self.llvm_type(*elem_type);
                let base_val = self.gen_expr(base).into_struct_value();
                let idx = self.gen_expr(index).into_int_value();

                let data_ptr = self
                    .builder
                    .build_extract_value(base_val, SLICE_DATA_IDX, "data")
                    .unwrap()
                    .into_pointer_value();
                let len = self
                    .builder
                    .build_extract_value(base_val, SLICE_LEN_IDX, "len")
                    .unwrap()
                    .into_int_value();

                self.gen_bounds_check(idx, len);

                unsafe {
                    self.builder
                        .build_gep(elem_llvm_type, data_ptr, &[idx], "elem")
                        .unwrap()
                }
            }

            TIndexKind::String => {
                let base_val = self.gen_expr(base).into_struct_value();
                let idx = self.gen_expr(index).into_int_value();

                let data_ptr = self
                    .builder
                    .build_extract_value(base_val, SLICE_DATA_IDX, "data")
                    .unwrap()
                    .into_pointer_value();
                let len = self
                    .builder
                    .build_extract_value(base_val, SLICE_LEN_IDX, "len")
                    .unwrap()
                    .into_int_value();

                self.gen_bounds_check(idx, len);

                let i8_type = self.context.i8_type();
                unsafe {
                    self.builder
                        .build_gep(i8_type, data_ptr, &[idx], "elem")
                        .unwrap()
                }
            }

            TIndexKind::Map {
                key_type,
                value_type,
            } => {
                let map_ptr = self.gen_expr(base).into_pointer_value();
                let key_val = self.gen_expr(index);

                // Allocate temp storage for key
                let key_llvm_type = self.llvm_type(*key_type);
                let key_ptr = self.builder.build_alloca(key_llvm_type, "key_tmp").unwrap();
                self.builder.build_store(key_ptr, key_val).unwrap();

                // Get key and value sizes
                let key_size = self.type_size(*key_type);
                let value_size = self.type_size(*value_type);

                // Call go_map_get
                let map_get_fn = self.get_runtime_fn(names::GO_MAP_GET);
                let call_site = self
                    .builder
                    .build_call(
                        map_get_fn,
                        &[
                            map_ptr.into(),
                            key_ptr.into(),
                            self.context.i64_type().const_int(key_size, false).into(),
                            self.context.i64_type().const_int(value_size, false).into(),
                        ],
                        "map_get",
                    )
                    .unwrap();
                let value_ptr = call_site
                    .try_as_basic_value()
                    .basic()
                    .expect("codegen bug: go_map_get should return a value")
                    .into_pointer_value();

                // Return zero value for missing keys
                let value_llvm_type = self.llvm_type(*value_type);
                let is_null = self.builder.build_is_null(value_ptr, "is_null").unwrap();

                let current_fn = self.current_function();
                let found_bb = self.context.append_basic_block(current_fn, "map.found");
                let missing_bb = self.context.append_basic_block(current_fn, "map.missing");
                let merge_bb = self.context.append_basic_block(current_fn, "map.merge");

                self.builder
                    .build_conditional_branch(is_null, missing_bb, found_bb)
                    .unwrap();

                // Found block
                self.builder.position_at_end(found_bb);
                self.builder.build_unconditional_branch(merge_bb).unwrap();
                let found_bb_end = self.builder.get_insert_block().unwrap();

                // Missing block
                self.builder.position_at_end(missing_bb);
                let zero_ptr = self
                    .builder
                    .build_alloca(value_llvm_type, "zero_val")
                    .unwrap();
                let zero_val = self.gen_zero(*value_type);
                self.builder.build_store(zero_ptr, zero_val).unwrap();
                self.builder.build_unconditional_branch(merge_bb).unwrap();
                let missing_bb_end = self.builder.get_insert_block().unwrap();

                // Merge block
                self.builder.position_at_end(merge_bb);
                let phi = self
                    .builder
                    .build_phi(self.ptr_type(), "result_ptr")
                    .unwrap();
                phi.add_incoming(&[(&value_ptr, found_bb_end), (&zero_ptr, missing_bb_end)]);

                phi.as_basic_value().into_pointer_value()
            }
        }
    }

    fn gen_addr_of_inner(&mut self, expr: &TExpr) -> PointerValue<'ctx> {
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

    pub fn gen_bounds_check(&mut self, index: IntValue<'ctx>, len: IntValue<'ctx>) {
        let current_fn = self.current_function();
        let panic_bb = self.context.append_basic_block(current_fn, "bounds.panic");
        let ok_bb = self.context.append_basic_block(current_fn, "bounds.ok");

        // Check index < 0
        let zero = self.context.i64_type().const_zero();
        let is_neg = self
            .builder
            .build_int_compare(IntPredicate::SLT, index, zero, "idx.neg")
            .unwrap();

        // Check index >= len
        let is_oob = self
            .builder
            .build_int_compare(IntPredicate::UGE, index, len, "idx.oob")
            .unwrap();

        // Panic if negative or out of bounds
        let should_panic = self
            .builder
            .build_or(is_neg, is_oob, "bounds.fail")
            .unwrap();

        self.builder
            .build_conditional_branch(should_panic, panic_bb, ok_bb)
            .unwrap();

        // Panic block
        self.builder.position_at_end(panic_bb);
        let panic_fn = self.get_runtime_fn(names::GO_PANIC_BOUNDS);
        self.builder
            .build_call(panic_fn, &[index.into(), len.into()], "")
            .unwrap();
        self.builder.build_unreachable().unwrap();

        // Continue in ok block
        self.builder.position_at_end(ok_bb);
    }
}
