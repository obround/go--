//! Assignment statement code generation from TIR

use inkwell::values::BasicValueEnum;

use crate::analysis::TypeId;
use crate::codegen::Codegen;
use crate::codegen::runtime::names;
use crate::tir::{TCompoundKind, TExpr, TIncDecKind, TIndexKind, TLValue};

impl<'ctx> Codegen<'ctx> {
    pub fn gen_assign(&mut self, lhs: &TLValue, rhs: &TExpr) {
        if matches!(lhs, TLValue::Blank) {
            self.gen_expr(rhs);
            return;
        }

        // Map assignment
        if let TLValue::Index {
            kind:
                TIndexKind::Map {
                    key_type,
                    value_type,
                },
            base,
            index,
            ..
        } = lhs
        {
            self.gen_map_assign(base, index, rhs, *key_type, *value_type);
            return;
        }

        // Regular assignment
        let ptr = self.gen_lvalue(lhs);
        let val = self.gen_expr(rhs);
        self.builder.build_store(ptr, val).unwrap();
    }

    pub fn gen_compound_assign(&mut self, kind: TCompoundKind, lhs: &TLValue, rhs: &TExpr) {
        // Get lvalue address
        let ptr = self.gen_lvalue(lhs);
        let type_id = lhs.type_id();
        let llvm_type = self.llvm_type(type_id);

        // Load current value
        let lhs_val = self.builder.build_load(llvm_type, ptr, "").unwrap();
        let rhs_val = self.gen_expr(rhs);

        // Compute new value
        let result = self.gen_compound_op(kind, lhs_val, rhs_val);

        // Store result
        self.builder.build_store(ptr, result).unwrap();
    }

    fn gen_compound_op(
        &mut self,
        kind: TCompoundKind,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        match kind {
            TCompoundKind::IntAdd => {
                let l = lhs.into_int_value();
                let r = rhs.into_int_value();
                self.builder.build_int_add(l, r, "").unwrap().into()
            }
            TCompoundKind::IntSub => {
                let l = lhs.into_int_value();
                let r = rhs.into_int_value();
                self.builder.build_int_sub(l, r, "").unwrap().into()
            }
            TCompoundKind::IntMul => {
                let l = lhs.into_int_value();
                let r = rhs.into_int_value();
                self.builder.build_int_mul(l, r, "").unwrap().into()
            }
            TCompoundKind::IntDiv { signed } => {
                let l = lhs.into_int_value();
                let r = rhs.into_int_value();
                if signed {
                    self.builder.build_int_signed_div(l, r, "").unwrap().into()
                } else {
                    self.builder
                        .build_int_unsigned_div(l, r, "")
                        .unwrap()
                        .into()
                }
            }
            TCompoundKind::IntMod { signed } => {
                let l = lhs.into_int_value();
                let r = rhs.into_int_value();
                if signed {
                    self.builder.build_int_signed_rem(l, r, "").unwrap().into()
                } else {
                    self.builder
                        .build_int_unsigned_rem(l, r, "")
                        .unwrap()
                        .into()
                }
            }
            TCompoundKind::FloatAdd => {
                let l = lhs.into_float_value();
                let r = rhs.into_float_value();
                self.builder.build_float_add(l, r, "").unwrap().into()
            }
            TCompoundKind::FloatSub => {
                let l = lhs.into_float_value();
                let r = rhs.into_float_value();
                self.builder.build_float_sub(l, r, "").unwrap().into()
            }
            TCompoundKind::FloatMul => {
                let l = lhs.into_float_value();
                let r = rhs.into_float_value();
                self.builder.build_float_mul(l, r, "").unwrap().into()
            }
            TCompoundKind::FloatDiv => {
                let l = lhs.into_float_value();
                let r = rhs.into_float_value();
                self.builder.build_float_div(l, r, "").unwrap().into()
            }
            TCompoundKind::StringConcat => self.gen_string_concat(lhs, rhs),
        }
    }

    pub fn gen_inc_dec(&mut self, kind: TIncDecKind, operand: &TLValue) {
        let ptr = self.gen_lvalue(operand);
        let type_id = operand.type_id();
        let llvm_type = self.llvm_type(type_id);
        let val = self.builder.build_load(llvm_type, ptr, "").unwrap();

        let result: BasicValueEnum<'ctx> = match kind {
            TIncDecKind::IncSigned | TIncDecKind::IncUnsigned => {
                let int_val = val.into_int_value();
                let one = int_val.get_type().const_int(1, false);
                self.builder
                    .build_int_add(int_val, one, "inc")
                    .unwrap()
                    .into()
            }
            TIncDecKind::DecSigned | TIncDecKind::DecUnsigned => {
                let int_val = val.into_int_value();
                let one = int_val.get_type().const_int(1, false);
                self.builder
                    .build_int_sub(int_val, one, "dec")
                    .unwrap()
                    .into()
            }
            TIncDecKind::IncFloat => {
                let float_val = val.into_float_value();
                let one = float_val.get_type().const_float(1.0);
                self.builder
                    .build_float_add(float_val, one, "inc")
                    .unwrap()
                    .into()
            }
            TIncDecKind::DecFloat => {
                let float_val = val.into_float_value();
                let one = float_val.get_type().const_float(1.0);
                self.builder
                    .build_float_sub(float_val, one, "dec")
                    .unwrap()
                    .into()
            }
        };

        self.builder.build_store(ptr, result).unwrap();
    }

    fn gen_map_assign(
        &mut self,
        base: &TExpr,
        index: &TExpr,
        value: &TExpr,
        key_type: TypeId,
        value_type: TypeId,
    ) {
        let map_ptr = self.gen_expr(base).into_pointer_value();

        // Nil map write check
        self.gen_nil_check(map_ptr);

        let key_val = self.gen_expr(index);
        let val_val = self.gen_expr(value);

        // Allocate temp storage
        let key_llvm_type = self.llvm_type(key_type);
        let val_llvm_type = self.llvm_type(value_type);
        let key_ptr = self.builder.build_alloca(key_llvm_type, "key").unwrap();
        let val_ptr = self.builder.build_alloca(val_llvm_type, "val").unwrap();
        self.builder.build_store(key_ptr, key_val).unwrap();
        self.builder.build_store(val_ptr, val_val).unwrap();

        // Get sizes
        let key_size = self.type_size(key_type);
        let val_size = self.type_size(value_type);

        // Call go_map_set
        let map_set_fn = self.get_runtime_fn(names::GO_MAP_SET);
        self.builder
            .build_call(
                map_set_fn,
                &[
                    map_ptr.into(),
                    key_ptr.into(),
                    val_ptr.into(),
                    self.context.i64_type().const_int(key_size, false).into(),
                    self.context.i64_type().const_int(val_size, false).into(),
                ],
                "",
            )
            .unwrap();
    }
}
