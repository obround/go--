//! Binary operation code generation from TIR

use inkwell::values::{BasicValueEnum, IntValue, PointerValue};
use inkwell::{FloatPredicate, IntPredicate};

use crate::analysis::TypeId;
use crate::codegen::Codegen;
use crate::codegen::runtime::names;
use crate::codegen::types::{SLICE_DATA_IDX, STRING_DATA_IDX, STRING_LEN_IDX};
use crate::tir::{TBinaryOp, TEqualityInfo, TExpr, TPrimitiveEq};
use inkwell::basic_block::BasicBlock;

impl<'ctx> Codegen<'ctx> {
    pub fn gen_binary(
        &mut self,
        op: &TBinaryOp,
        left: &TExpr,
        right: &TExpr,
        _type_id: TypeId,
    ) -> BasicValueEnum<'ctx> {
        match op {
            // Integer arithmetic
            TBinaryOp::IntAdd => {
                let l = self.gen_expr(left).into_int_value();
                let r = self.gen_expr(right).into_int_value();
                self.builder.build_int_add(l, r, "add").unwrap().into()
            }
            TBinaryOp::IntSub => {
                let l = self.gen_expr(left).into_int_value();
                let r = self.gen_expr(right).into_int_value();
                self.builder.build_int_sub(l, r, "sub").unwrap().into()
            }
            TBinaryOp::IntMul => {
                let l = self.gen_expr(left).into_int_value();
                let r = self.gen_expr(right).into_int_value();
                self.builder.build_int_mul(l, r, "mul").unwrap().into()
            }
            TBinaryOp::IntDiv { signed } => {
                let l = self.gen_expr(left).into_int_value();
                let r = self.gen_expr(right).into_int_value();
                if *signed {
                    self.builder
                        .build_int_signed_div(l, r, "div")
                        .unwrap()
                        .into()
                } else {
                    self.builder
                        .build_int_unsigned_div(l, r, "div")
                        .unwrap()
                        .into()
                }
            }
            TBinaryOp::IntMod { signed } => {
                let l = self.gen_expr(left).into_int_value();
                let r = self.gen_expr(right).into_int_value();
                if *signed {
                    self.builder
                        .build_int_signed_rem(l, r, "mod")
                        .unwrap()
                        .into()
                } else {
                    self.builder
                        .build_int_unsigned_rem(l, r, "mod")
                        .unwrap()
                        .into()
                }
            }

            // Float arithmetic
            TBinaryOp::FloatAdd => {
                let l = self.gen_expr(left).into_float_value();
                let r = self.gen_expr(right).into_float_value();
                self.builder.build_float_add(l, r, "fadd").unwrap().into()
            }
            TBinaryOp::FloatSub => {
                let l = self.gen_expr(left).into_float_value();
                let r = self.gen_expr(right).into_float_value();
                self.builder.build_float_sub(l, r, "fsub").unwrap().into()
            }
            TBinaryOp::FloatMul => {
                let l = self.gen_expr(left).into_float_value();
                let r = self.gen_expr(right).into_float_value();
                self.builder.build_float_mul(l, r, "fmul").unwrap().into()
            }
            TBinaryOp::FloatDiv => {
                let l = self.gen_expr(left).into_float_value();
                let r = self.gen_expr(right).into_float_value();
                self.builder.build_float_div(l, r, "fdiv").unwrap().into()
            }

            // String concatenation
            TBinaryOp::StringConcat => {
                let l = self.gen_expr(left);
                let r = self.gen_expr(right);
                self.gen_string_concat(l, r)
            }

            // Integer comparisons
            TBinaryOp::IntEq => {
                let l = self.gen_expr(left).into_int_value();
                let r = self.gen_expr(right).into_int_value();
                self.builder
                    .build_int_compare(IntPredicate::EQ, l, r, "eq")
                    .unwrap()
                    .into()
            }
            TBinaryOp::IntNe => {
                let l = self.gen_expr(left).into_int_value();
                let r = self.gen_expr(right).into_int_value();
                self.builder
                    .build_int_compare(IntPredicate::NE, l, r, "ne")
                    .unwrap()
                    .into()
            }
            TBinaryOp::IntLt { signed } => {
                let l = self.gen_expr(left).into_int_value();
                let r = self.gen_expr(right).into_int_value();
                let pred = if *signed {
                    IntPredicate::SLT
                } else {
                    IntPredicate::ULT
                };
                self.builder
                    .build_int_compare(pred, l, r, "lt")
                    .unwrap()
                    .into()
            }
            TBinaryOp::IntLe { signed } => {
                let l = self.gen_expr(left).into_int_value();
                let r = self.gen_expr(right).into_int_value();
                let pred = if *signed {
                    IntPredicate::SLE
                } else {
                    IntPredicate::ULE
                };
                self.builder
                    .build_int_compare(pred, l, r, "le")
                    .unwrap()
                    .into()
            }
            TBinaryOp::IntGt { signed } => {
                let l = self.gen_expr(left).into_int_value();
                let r = self.gen_expr(right).into_int_value();
                let pred = if *signed {
                    IntPredicate::SGT
                } else {
                    IntPredicate::UGT
                };
                self.builder
                    .build_int_compare(pred, l, r, "gt")
                    .unwrap()
                    .into()
            }
            TBinaryOp::IntGe { signed } => {
                let l = self.gen_expr(left).into_int_value();
                let r = self.gen_expr(right).into_int_value();
                let pred = if *signed {
                    IntPredicate::SGE
                } else {
                    IntPredicate::UGE
                };
                self.builder
                    .build_int_compare(pred, l, r, "ge")
                    .unwrap()
                    .into()
            }

            // Float comparisons
            TBinaryOp::FloatEq => {
                let l = self.gen_expr(left).into_float_value();
                let r = self.gen_expr(right).into_float_value();
                self.builder
                    .build_float_compare(FloatPredicate::OEQ, l, r, "feq")
                    .unwrap()
                    .into()
            }
            TBinaryOp::FloatNe => {
                let l = self.gen_expr(left).into_float_value();
                let r = self.gen_expr(right).into_float_value();
                self.builder
                    .build_float_compare(FloatPredicate::ONE, l, r, "fne")
                    .unwrap()
                    .into()
            }
            TBinaryOp::FloatLt => {
                let l = self.gen_expr(left).into_float_value();
                let r = self.gen_expr(right).into_float_value();
                self.builder
                    .build_float_compare(FloatPredicate::OLT, l, r, "flt")
                    .unwrap()
                    .into()
            }
            TBinaryOp::FloatLe => {
                let l = self.gen_expr(left).into_float_value();
                let r = self.gen_expr(right).into_float_value();
                self.builder
                    .build_float_compare(FloatPredicate::OLE, l, r, "fle")
                    .unwrap()
                    .into()
            }
            TBinaryOp::FloatGt => {
                let l = self.gen_expr(left).into_float_value();
                let r = self.gen_expr(right).into_float_value();
                self.builder
                    .build_float_compare(FloatPredicate::OGT, l, r, "fgt")
                    .unwrap()
                    .into()
            }
            TBinaryOp::FloatGe => {
                let l = self.gen_expr(left).into_float_value();
                let r = self.gen_expr(right).into_float_value();
                self.builder
                    .build_float_compare(FloatPredicate::OGE, l, r, "fge")
                    .unwrap()
                    .into()
            }

            // String comparisons
            TBinaryOp::StringEq => {
                let l = self.gen_expr(left);
                let r = self.gen_expr(right);
                self.gen_string_eq(l, r).into()
            }
            TBinaryOp::StringNe => {
                let l = self.gen_expr(left);
                let r = self.gen_expr(right);
                let eq = self.gen_string_eq(l, r);
                self.builder.build_not(eq, "str_ne").unwrap().into()
            }
            TBinaryOp::StringLt => {
                let l = self.gen_expr(left);
                let r = self.gen_expr(right);
                self.gen_string_ordering(l, r, IntPredicate::SLT)
            }
            TBinaryOp::StringLe => {
                let l = self.gen_expr(left);
                let r = self.gen_expr(right);
                self.gen_string_ordering(l, r, IntPredicate::SLE)
            }
            TBinaryOp::StringGt => {
                let l = self.gen_expr(left);
                let r = self.gen_expr(right);
                self.gen_string_ordering(l, r, IntPredicate::SGT)
            }
            TBinaryOp::StringGe => {
                let l = self.gen_expr(left);
                let r = self.gen_expr(right);
                self.gen_string_ordering(l, r, IntPredicate::SGE)
            }

            // Bool comparisons
            TBinaryOp::BoolEq => {
                let l = self.gen_expr(left).into_int_value();
                let r = self.gen_expr(right).into_int_value();
                self.builder
                    .build_int_compare(IntPredicate::EQ, l, r, "bool_eq")
                    .unwrap()
                    .into()
            }
            TBinaryOp::BoolNe => {
                let l = self.gen_expr(left).into_int_value();
                let r = self.gen_expr(right).into_int_value();
                self.builder
                    .build_int_compare(IntPredicate::NE, l, r, "bool_ne")
                    .unwrap()
                    .into()
            }

            // Pointer comparisons
            TBinaryOp::PointerEq => {
                let l = self.gen_expr(left).into_pointer_value();
                let r = self.gen_expr(right).into_pointer_value();
                self.gen_pointer_eq(l, r).into()
            }
            TBinaryOp::PointerNe => {
                let l = self.gen_expr(left).into_pointer_value();
                let r = self.gen_expr(right).into_pointer_value();
                let eq = self.gen_pointer_eq(l, r);
                self.builder.build_not(eq, "ptr_ne").unwrap().into()
            }

            // Struct/array equality
            TBinaryOp::StructEq { equality } => {
                let l = self.gen_expr(left);
                let r = self.gen_expr(right);
                self.gen_equality(equality, l, r).into()
            }
            TBinaryOp::StructNe { equality } => {
                let l = self.gen_expr(left);
                let r = self.gen_expr(right);
                let eq = self.gen_equality(equality, l, r);
                self.builder.build_not(eq, "ne").unwrap().into()
            }
            TBinaryOp::ArrayEq { equality } => {
                let l = self.gen_expr(left);
                let r = self.gen_expr(right);
                self.gen_equality(equality, l, r).into()
            }
            TBinaryOp::ArrayNe { equality } => {
                let l = self.gen_expr(left);
                let r = self.gen_expr(right);
                let eq = self.gen_equality(equality, l, r);
                self.builder.build_not(eq, "ne").unwrap().into()
            }

            // Nil comparisons for slices
            TBinaryOp::SliceNilEq { slice_on_left } => {
                let l = self.gen_expr(left);
                let r = self.gen_expr(right);
                self.gen_slice_nil_eq(l, r, *slice_on_left).into()
            }
            TBinaryOp::SliceNilNe { slice_on_left } => {
                let l = self.gen_expr(left);
                let r = self.gen_expr(right);
                let eq = self.gen_slice_nil_eq(l, r, *slice_on_left);
                self.builder.build_not(eq, "ne").unwrap().into()
            }

            // Nil comparisons for maps
            TBinaryOp::MapNilEq { map_on_left } => {
                let l = self.gen_expr(left);
                let r = self.gen_expr(right);
                self.gen_map_nil_eq(l, r, *map_on_left).into()
            }
            TBinaryOp::MapNilNe { map_on_left } => {
                let l = self.gen_expr(left);
                let r = self.gen_expr(right);
                let eq = self.gen_map_nil_eq(l, r, *map_on_left);
                self.builder.build_not(eq, "ne").unwrap().into()
            }

            // Nil comparisons for pointers
            TBinaryOp::PointerNilEq { pointer_on_left } => {
                let l = self.gen_expr(left);
                let r = self.gen_expr(right);
                self.gen_ptr_nil_eq(l, r, *pointer_on_left).into()
            }
            TBinaryOp::PointerNilNe { pointer_on_left } => {
                let l = self.gen_expr(left);
                let r = self.gen_expr(right);
                let eq = self.gen_ptr_nil_eq(l, r, *pointer_on_left);
                self.builder.build_not(eq, "ne").unwrap().into()
            }

            // Nil comparisons for functions
            TBinaryOp::FuncNilEq { func_on_left } => {
                let l = self.gen_expr(left);
                let r = self.gen_expr(right);
                self.gen_ptr_nil_eq(l, r, *func_on_left).into()
            }
            TBinaryOp::FuncNilNe { func_on_left } => {
                let l = self.gen_expr(left);
                let r = self.gen_expr(right);
                let eq = self.gen_ptr_nil_eq(l, r, *func_on_left);
                self.builder.build_not(eq, "ne").unwrap().into()
            }

            // Logical operators
            TBinaryOp::And => self.gen_short_circuit_and(left, right),
            TBinaryOp::Or => self.gen_short_circuit_or(left, right),
        }
    }

    pub fn gen_string_concat(
        &mut self,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let (left_ptr, left_len) = self.extract_string_parts(left);
        let (right_ptr, right_len) = self.extract_string_parts(right);

        let total_len = self
            .builder
            .build_int_add(left_len, right_len, "total.len")
            .unwrap();

        // Allocate new buffer
        let new_ptr = self.call_gc_malloc(total_len);
        let i8_type = self.context.i8_type();

        let memcpy = self.get_runtime_fn(names::MEMCPY);

        // Copy left string
        self.builder
            .build_call(
                memcpy,
                &[new_ptr.into(), left_ptr.into(), left_len.into()],
                "",
            )
            .unwrap();

        // Copy right string
        let dest_offset = unsafe {
            self.builder
                .build_gep(i8_type, new_ptr, &[left_len], "concat.offset")
                .unwrap()
        };
        self.builder
            .build_call(
                memcpy,
                &[dest_offset.into(), right_ptr.into(), right_len.into()],
                "",
            )
            .unwrap();

        // Build result string struct
        let string_type = self.string_type();
        let mut result = string_type.const_zero();
        result = self
            .builder
            .build_insert_value(result, new_ptr, STRING_DATA_IDX, "")
            .unwrap()
            .into_struct_value();
        result = self
            .builder
            .build_insert_value(result, total_len, STRING_LEN_IDX, "")
            .unwrap()
            .into_struct_value();

        result.into()
    }

    pub fn gen_string_eq(
        &mut self,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> IntValue<'ctx> {
        let (left_ptr, left_len) = self.extract_string_parts(left);
        let (right_ptr, right_len) = self.extract_string_parts(right);

        // Compare lengths
        let len_equal = self
            .builder
            .build_int_compare(IntPredicate::EQ, left_len, right_len, "len.eq")
            .unwrap();

        let current_fn = self.current_function();
        let len_check_block = self.builder.get_insert_block().unwrap();
        let cmp_block = self.context.append_basic_block(current_fn, "str.cmp");
        let result_block = self.context.append_basic_block(current_fn, "str.result");

        self.builder
            .build_conditional_branch(len_equal, cmp_block, result_block)
            .unwrap();

        // Compare contents
        self.builder.position_at_end(cmp_block);
        let memcmp = self.get_runtime_fn(names::MEMCMP);

        let cmp_result = self
            .builder
            .build_call(
                memcmp,
                &[left_ptr.into(), right_ptr.into(), left_len.into()],
                "cmp",
            )
            .unwrap()
            .try_as_basic_value()
            .basic()
            .unwrap()
            .into_int_value();

        let zero_i32 = self.context.i32_type().const_zero();
        let is_equal = self
            .builder
            .build_int_compare(IntPredicate::EQ, cmp_result, zero_i32, "data.eq")
            .unwrap();

        self.builder
            .build_unconditional_branch(result_block)
            .unwrap();
        let cmp_end_block = self.builder.get_insert_block().unwrap();

        // Result block
        self.builder.position_at_end(result_block);
        let phi = self
            .builder
            .build_phi(self.context.bool_type(), "str.eq.result")
            .unwrap();

        phi.add_incoming(&[
            (&self.context.bool_type().const_zero(), len_check_block),
            (&is_equal, cmp_end_block),
        ]);

        phi.as_basic_value().into_int_value()
    }

    fn gen_string_ordering(
        &mut self,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
        pred: IntPredicate,
    ) -> BasicValueEnum<'ctx> {
        let (left_ptr, left_len) = self.extract_string_parts(left);
        let (right_ptr, right_len) = self.extract_string_parts(right);

        // Compare using min length
        let min_len = self
            .builder
            .build_select(
                self.builder
                    .build_int_compare(IntPredicate::SLT, left_len, right_len, "")
                    .unwrap(),
                left_len,
                right_len,
                "min.len",
            )
            .unwrap()
            .into_int_value();

        let memcmp = self.get_runtime_fn(names::MEMCMP);

        let cmp_result = self
            .builder
            .build_call(
                memcmp,
                &[left_ptr.into(), right_ptr.into(), min_len.into()],
                "cmp",
            )
            .unwrap()
            .try_as_basic_value()
            .basic()
            .unwrap()
            .into_int_value();

        let zero = self.context.i32_type().const_zero();
        let cmp_is_zero = self
            .builder
            .build_int_compare(IntPredicate::EQ, cmp_result, zero, "cmp.zero")
            .unwrap();

        let cmp_ext = self
            .builder
            .build_int_s_extend(cmp_result, self.context.i64_type(), "cmp.ext")
            .unwrap();

        let len_diff = self
            .builder
            .build_int_sub(left_len, right_len, "len.diff")
            .unwrap();

        let final_cmp = self
            .builder
            .build_select(cmp_is_zero, len_diff, cmp_ext, "final.cmp")
            .unwrap()
            .into_int_value();

        let zero64 = self.context.i64_type().const_zero();
        self.builder
            .build_int_compare(pred, final_cmp, zero64, "result")
            .unwrap()
            .into()
    }

    /// Extract (ptr, len) from a Go string struct value
    pub fn extract_string_parts(
        &mut self,
        s: BasicValueEnum<'ctx>,
    ) -> (PointerValue<'ctx>, IntValue<'ctx>) {
        let s_struct = s.into_struct_value();
        let ptr = self
            .builder
            .build_extract_value(s_struct, STRING_DATA_IDX, "str.ptr")
            .unwrap()
            .into_pointer_value();
        let len = self
            .builder
            .build_extract_value(s_struct, STRING_LEN_IDX, "str.len")
            .unwrap()
            .into_int_value();
        (ptr, len)
    }

    fn gen_pointer_eq(
        &self,
        left: PointerValue<'ctx>,
        right: PointerValue<'ctx>,
    ) -> IntValue<'ctx> {
        let l = self
            .builder
            .build_ptr_to_int(left, self.context.i64_type(), "ptr_l")
            .unwrap();
        let r = self
            .builder
            .build_ptr_to_int(right, self.context.i64_type(), "ptr_r")
            .unwrap();
        self.builder
            .build_int_compare(IntPredicate::EQ, l, r, "ptr_eq")
            .unwrap()
    }

    fn gen_slice_nil_eq(
        &mut self,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
        slice_on_left: bool,
    ) -> IntValue<'ctx> {
        let struct_val = if slice_on_left {
            left.into_struct_value()
        } else {
            right.into_struct_value()
        };

        // Get data pointer
        let data_ptr = self
            .builder
            .build_extract_value(struct_val, SLICE_DATA_IDX, "data_ptr")
            .unwrap()
            .into_pointer_value();

        let data_int = self
            .builder
            .build_ptr_to_int(data_ptr, self.context.i64_type(), "ptrtoint")
            .unwrap();
        let zero = self.context.i64_type().const_zero();

        self.builder
            .build_int_compare(IntPredicate::EQ, data_int, zero, "nil_eq")
            .unwrap()
    }

    fn gen_map_nil_eq(
        &mut self,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
        map_on_left: bool,
    ) -> IntValue<'ctx> {
        let map_ptr = if map_on_left {
            left.into_pointer_value()
        } else {
            right.into_pointer_value()
        };
        self.builder.build_is_null(map_ptr, "is_nil").unwrap()
    }

    /// Generate nil comparison for pointers and functions
    fn gen_ptr_nil_eq(
        &mut self,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
        ptr_on_left: bool,
    ) -> IntValue<'ctx> {
        let ptr = if ptr_on_left {
            left.into_pointer_value()
        } else {
            right.into_pointer_value()
        };
        self.builder.build_is_null(ptr, "is_nil").unwrap()
    }

    fn gen_short_circuit_and(&mut self, lhs: &TExpr, rhs: &TExpr) -> BasicValueEnum<'ctx> {
        let rhs_block = self.append_block("and.rhs");
        let merge_block = self.append_block("and.merge");

        let lhs_val = self.gen_expr(lhs).into_int_value();
        let lhs_end_block = self.builder.get_insert_block().unwrap();
        self.build_cond_br(lhs_val, rhs_block, merge_block);

        self.position_at_end(rhs_block);
        let rhs_val = self.gen_expr(rhs).into_int_value();
        let rhs_end_block = self.builder.get_insert_block().unwrap();
        self.build_br_if_needed(merge_block);

        self.position_at_end(merge_block);
        let phi = self
            .builder
            .build_phi(self.context.bool_type(), "and.result")
            .unwrap();
        phi.add_incoming(&[
            (&self.context.bool_type().const_zero(), lhs_end_block),
            (&rhs_val, rhs_end_block),
        ]);

        phi.as_basic_value()
    }

    fn gen_short_circuit_or(&mut self, lhs: &TExpr, rhs: &TExpr) -> BasicValueEnum<'ctx> {
        let rhs_block = self.append_block("or.rhs");
        let merge_block = self.append_block("or.merge");

        let lhs_val = self.gen_expr(lhs).into_int_value();
        let lhs_end_block = self.builder.get_insert_block().unwrap();
        self.build_cond_br(lhs_val, merge_block, rhs_block);

        self.position_at_end(rhs_block);
        let rhs_val = self.gen_expr(rhs).into_int_value();
        let rhs_end_block = self.builder.get_insert_block().unwrap();
        self.build_br_if_needed(merge_block);

        self.position_at_end(merge_block);
        let phi = self
            .builder
            .build_phi(self.context.bool_type(), "or.result")
            .unwrap();
        phi.add_incoming(&[
            (&self.context.bool_type().const_int(1, false), lhs_end_block),
            (&rhs_val, rhs_end_block),
        ]);

        phi.as_basic_value()
    }

    pub fn gen_equality(
        &mut self,
        info: &TEqualityInfo,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> IntValue<'ctx> {
        match info {
            TEqualityInfo::Primitive(prim) => self.gen_primitive_eq(*prim, left, right),
            TEqualityInfo::Struct { fields } => {
                if fields.is_empty() {
                    return self.context.bool_type().const_int(1, false);
                }

                let left_sv = left.into_struct_value();
                let right_sv = right.into_struct_value();

                let current_fn = self.current_function();
                let result_block = self.context.append_basic_block(current_fn, "struct.result");

                let field_blocks: Vec<_> = (0..fields.len())
                    .map(|i| {
                        self.context
                            .append_basic_block(current_fn, &format!("field.{}", i))
                    })
                    .collect();

                let mut incoming: Vec<(IntValue<'ctx>, BasicBlock<'ctx>)> = Vec::new();

                self.builder
                    .build_unconditional_branch(field_blocks[0])
                    .unwrap();

                for (i, (field_idx, field_eq)) in fields.iter().enumerate() {
                    self.builder.position_at_end(field_blocks[i]);

                    let left_field = self
                        .builder
                        .build_extract_value(left_sv, *field_idx, "lf")
                        .unwrap();
                    let right_field = self
                        .builder
                        .build_extract_value(right_sv, *field_idx, "rf")
                        .unwrap();

                    let eq = self.gen_equality(field_eq, left_field, right_field);

                    let is_last = i == fields.len() - 1;
                    if is_last {
                        incoming.push((eq, self.builder.get_insert_block().unwrap()));
                        self.builder
                            .build_unconditional_branch(result_block)
                            .unwrap();
                    } else {
                        let false_val = self.context.bool_type().const_zero();
                        let current_block = self.builder.get_insert_block().unwrap();
                        self.builder
                            .build_conditional_branch(eq, field_blocks[i + 1], result_block)
                            .unwrap();
                        incoming.push((false_val, current_block));
                    }
                }

                self.builder.position_at_end(result_block);
                let phi = self
                    .builder
                    .build_phi(self.context.bool_type(), "struct.eq")
                    .unwrap();
                for (val, block) in &incoming {
                    phi.add_incoming(&[(val, *block)]);
                }
                phi.as_basic_value().into_int_value()
            }
            TEqualityInfo::Array { len, elem } => {
                if *len == 0 {
                    return self.context.bool_type().const_int(1, false);
                }

                let left_arr = left.into_array_value();
                let right_arr = right.into_array_value();

                let current_fn = self.current_function();
                let cond_block = self.context.append_basic_block(current_fn, "arr.cond");
                let body_block = self.context.append_basic_block(current_fn, "arr.body");
                let true_block = self.context.append_basic_block(current_fn, "arr.true");
                let false_block = self.context.append_basic_block(current_fn, "arr.false");
                let result_block = self.context.append_basic_block(current_fn, "arr.result");

                let idx_ptr = self
                    .builder
                    .build_alloca(self.context.i64_type(), "idx")
                    .unwrap();
                self.builder
                    .build_store(idx_ptr, self.context.i64_type().const_zero())
                    .unwrap();

                // Store arrays to memory for GEP
                let left_ptr = self
                    .builder
                    .build_alloca(left_arr.get_type(), "left_arr")
                    .unwrap();
                let right_ptr = self
                    .builder
                    .build_alloca(right_arr.get_type(), "right_arr")
                    .unwrap();
                self.builder.build_store(left_ptr, left_arr).unwrap();
                self.builder.build_store(right_ptr, right_arr).unwrap();

                self.builder.build_unconditional_branch(cond_block).unwrap();

                self.builder.position_at_end(cond_block);
                let idx = self
                    .builder
                    .build_load(self.context.i64_type(), idx_ptr, "idx")
                    .unwrap()
                    .into_int_value();
                let len_val = self.context.i64_type().const_int(*len, false);
                let cmp = self
                    .builder
                    .build_int_compare(IntPredicate::ULT, idx, len_val, "")
                    .unwrap();
                self.builder
                    .build_conditional_branch(cmp, body_block, true_block)
                    .unwrap();

                self.builder.position_at_end(body_block);
                let idx = self
                    .builder
                    .build_load(self.context.i64_type(), idx_ptr, "idx")
                    .unwrap()
                    .into_int_value();
                let zero = self.context.i64_type().const_zero();
                let array_type = left_arr.get_type();

                let left_elem_ptr = unsafe {
                    self.builder
                        .build_gep(array_type, left_ptr, &[zero, idx], "le.ptr")
                        .unwrap()
                };
                let right_elem_ptr = unsafe {
                    self.builder
                        .build_gep(array_type, right_ptr, &[zero, idx], "re.ptr")
                        .unwrap()
                };

                let elem_type = array_type.get_element_type();
                let left_elem = self
                    .builder
                    .build_load(elem_type, left_elem_ptr, "le")
                    .unwrap();
                let right_elem = self
                    .builder
                    .build_load(elem_type, right_elem_ptr, "re")
                    .unwrap();

                let elem_eq = self.gen_equality(elem, left_elem, right_elem);

                let next_idx = self
                    .builder
                    .build_int_add(idx, self.context.i64_type().const_int(1, false), "")
                    .unwrap();
                self.builder.build_store(idx_ptr, next_idx).unwrap();

                self.builder
                    .build_conditional_branch(elem_eq, cond_block, false_block)
                    .unwrap();

                self.builder.position_at_end(true_block);
                self.builder
                    .build_unconditional_branch(result_block)
                    .unwrap();

                self.builder.position_at_end(false_block);
                self.builder
                    .build_unconditional_branch(result_block)
                    .unwrap();

                self.builder.position_at_end(result_block);
                let phi = self
                    .builder
                    .build_phi(self.context.bool_type(), "arr.eq")
                    .unwrap();
                phi.add_incoming(&[
                    (&self.context.bool_type().const_int(1, false), true_block),
                    (&self.context.bool_type().const_zero(), false_block),
                ]);

                phi.as_basic_value().into_int_value()
            }
        }
    }

    fn gen_primitive_eq(
        &mut self,
        prim: TPrimitiveEq,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> IntValue<'ctx> {
        match prim {
            TPrimitiveEq::Int | TPrimitiveEq::Bool => {
                let l = left.into_int_value();
                let r = right.into_int_value();
                self.builder
                    .build_int_compare(IntPredicate::EQ, l, r, "eq")
                    .unwrap()
            }
            TPrimitiveEq::Float => {
                let l = left.into_float_value();
                let r = right.into_float_value();
                self.builder
                    .build_float_compare(FloatPredicate::OEQ, l, r, "eq")
                    .unwrap()
            }
            TPrimitiveEq::Pointer => {
                let l = left.into_pointer_value();
                let r = right.into_pointer_value();
                self.gen_pointer_eq(l, r)
            }
            TPrimitiveEq::String => self.gen_string_eq(left, right),
        }
    }
}
