//! Unary operation code generation from TIR

use inkwell::values::BasicValueEnum;

use crate::analysis::TypeId;
use crate::codegen::Codegen;
use crate::tir::{TExpr, TUnaryOp};

impl<'ctx> Codegen<'ctx> {
    pub fn gen_unary(
        &mut self,
        op: TUnaryOp,
        operand: &TExpr,
        _type_id: TypeId,
    ) -> BasicValueEnum<'ctx> {
        match op {
            TUnaryOp::IntNeg => {
                let val = self.gen_expr(operand).into_int_value();
                self.builder.build_int_neg(val, "neg").unwrap().into()
            }
            TUnaryOp::FloatNeg => {
                let val = self.gen_expr(operand).into_float_value();
                self.builder.build_float_neg(val, "fneg").unwrap().into()
            }
            TUnaryOp::BoolNot => {
                let val = self.gen_expr(operand).into_int_value();
                self.builder.build_not(val, "not").unwrap().into()
            }
        }
    }
}
