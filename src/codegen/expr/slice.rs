//! Slice expression code generation from TIR

use inkwell::IntPredicate;
use inkwell::values::{BasicValueEnum, IntValue};

use crate::analysis::TypeId;
use crate::codegen::Codegen;
use crate::codegen::runtime::names;
use crate::codegen::types::{
    SLICE_CAP_IDX, SLICE_DATA_IDX, SLICE_LEN_IDX, STRING_DATA_IDX, STRING_LEN_IDX,
};
use crate::tir::{TExpr, TSliceKind};

impl<'ctx> Codegen<'ctx> {
    pub fn gen_slice_expr(
        &mut self,
        kind: &TSliceKind,
        base: &TExpr,
        low: &Option<Box<TExpr>>,
        high: &Option<Box<TExpr>>,
    ) -> BasicValueEnum<'ctx> {
        // Compute low index (default to 0)
        let low_idx = low
            .as_ref()
            .map(|e| self.gen_expr(e).into_int_value())
            .unwrap_or_else(|| self.context.i64_type().const_zero());

        // Get element info based on slice kind
        let (elem_type, data_ptr, orig_cap, orig_len, is_string) = match kind {
            TSliceKind::Array { len, elem_type } => {
                let base_type = base.type_id();
                let array_llvm_type = self.llvm_type(base_type);
                let array_ptr = self.gen_addr_of(base);
                let zero = self.context.i64_type().const_zero();
                let data_ptr = unsafe {
                    self.builder
                        .build_gep(array_llvm_type, array_ptr, &[zero, zero], "arr.data")
                        .unwrap()
                };
                let len_val = self.context.i64_type().const_int(*len, false);
                (*elem_type, data_ptr, len_val, len_val, false)
            }
            TSliceKind::Slice { elem_type } => {
                let base_val = self.gen_expr(base).into_struct_value();
                let ptr = self
                    .builder
                    .build_extract_value(base_val, SLICE_DATA_IDX, "data")
                    .unwrap()
                    .into_pointer_value();
                let len = self
                    .builder
                    .build_extract_value(base_val, SLICE_LEN_IDX, "len")
                    .unwrap()
                    .into_int_value();
                let cap = self
                    .builder
                    .build_extract_value(base_val, SLICE_CAP_IDX, "cap")
                    .unwrap()
                    .into_int_value();
                (*elem_type, ptr, cap, len, false)
            }
            TSliceKind::String => {
                // For string slicing, elem_type is uint8
                let base_val = self.gen_expr(base).into_struct_value();
                let ptr = self
                    .builder
                    .build_extract_value(base_val, STRING_DATA_IDX, "data")
                    .unwrap()
                    .into_pointer_value();
                let len = self
                    .builder
                    .build_extract_value(base_val, STRING_LEN_IDX, "len")
                    .unwrap()
                    .into_int_value();
                (TypeId::UINT8, ptr, len, len, true)
            }
        };

        let elem_llvm_type = self.llvm_type(elem_type);

        // Compute high index (default to len)
        let high_idx = high
            .as_ref()
            .map(|e| self.gen_expr(e).into_int_value())
            .unwrap_or(orig_len);

        // Bounds check
        self.gen_slice_bounds_check(low_idx, high_idx, orig_cap);

        // Compute new length and capacity
        let new_len = self
            .builder
            .build_int_sub(high_idx, low_idx, "newlen")
            .unwrap();

        let new_cap = self
            .builder
            .build_int_sub(orig_cap, low_idx, "newcap")
            .unwrap();

        // Compute new data pointer
        let new_ptr = unsafe {
            self.builder
                .build_gep(elem_llvm_type, data_ptr, &[low_idx], "slice.ptr")
                .unwrap()
        };

        // Build result struct
        if is_string {
            // String: {ptr, len}
            let string_type = self.string_type();
            let mut s = string_type.get_undef();
            s = self
                .builder
                .build_insert_value(s, new_ptr, SLICE_DATA_IDX, "")
                .unwrap()
                .into_struct_value();
            s = self
                .builder
                .build_insert_value(s, new_len, SLICE_LEN_IDX, "")
                .unwrap()
                .into_struct_value();
            s.into()
        } else {
            // Slice: {ptr, len, cap}
            let slice_type = self.slice_type();
            let mut slice = slice_type.get_undef();
            slice = self
                .builder
                .build_insert_value(slice, new_ptr, SLICE_DATA_IDX, "")
                .unwrap()
                .into_struct_value();
            slice = self
                .builder
                .build_insert_value(slice, new_len, SLICE_LEN_IDX, "")
                .unwrap()
                .into_struct_value();
            slice = self
                .builder
                .build_insert_value(slice, new_cap, SLICE_CAP_IDX, "")
                .unwrap()
                .into_struct_value();
            slice.into()
        }
    }

    fn gen_slice_bounds_check(
        &mut self,
        low: IntValue<'ctx>,
        high: IntValue<'ctx>,
        cap: IntValue<'ctx>,
    ) {
        let current_fn = self.current_function();
        let panic_bb = self.context.append_basic_block(current_fn, "slice.panic");
        let ok_bb = self.context.append_basic_block(current_fn, "slice.ok");

        // Check low > high
        let low_gt_high = self
            .builder
            .build_int_compare(IntPredicate::SGT, low, high, "low_gt_high")
            .unwrap();

        // Check high > cap
        let high_gt_cap = self
            .builder
            .build_int_compare(IntPredicate::SGT, high, cap, "high_gt_cap")
            .unwrap();

        // Panic if either check fails
        let bounds_invalid = self
            .builder
            .build_or(low_gt_high, high_gt_cap, "bounds_invalid")
            .unwrap();

        self.builder
            .build_conditional_branch(bounds_invalid, panic_bb, ok_bb)
            .unwrap();

        // Panic block
        self.builder.position_at_end(panic_bb);
        let panic_fn = self.get_runtime_fn(names::GO_PANIC_BOUNDS);
        self.builder
            .build_call(panic_fn, &[high.into(), cap.into()], "")
            .unwrap();
        self.builder.build_unreachable().unwrap();

        // Continue in ok block
        self.builder.position_at_end(ok_bb);
    }
}
