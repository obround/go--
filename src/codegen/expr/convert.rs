//! Type conversion code generation from TIR

use inkwell::values::BasicValueEnum;

use crate::analysis::TypeId;
use crate::codegen::Codegen;
use crate::codegen::types::{STRING_DATA_IDX, STRING_LEN_IDX};
use crate::tir::{TConversionKind, TExpr};

impl<'ctx> Codegen<'ctx> {
    pub fn gen_convert(
        &mut self,
        kind: TConversionKind,
        expr: &TExpr,
        target_type: TypeId,
    ) -> BasicValueEnum<'ctx> {
        let val = self.gen_expr(expr);

        match kind {
            TConversionKind::Identity => val,

            TConversionKind::IntToInt { src_signed } => {
                let target_llvm = self.llvm_type(target_type).into_int_type();
                let int_val = val.into_int_value();
                let src_bits = int_val.get_type().get_bit_width();
                let dst_bits = target_llvm.get_bit_width();

                if src_bits < dst_bits {
                    if src_signed {
                        self.builder
                            .build_int_s_extend(int_val, target_llvm, "sext")
                            .unwrap()
                            .into()
                    } else {
                        self.builder
                            .build_int_z_extend(int_val, target_llvm, "zext")
                            .unwrap()
                            .into()
                    }
                } else if src_bits > dst_bits {
                    self.builder
                        .build_int_truncate(int_val, target_llvm, "trunc")
                        .unwrap()
                        .into()
                } else {
                    val
                }
            }

            TConversionKind::FloatToInt { dst_signed } => {
                let target_llvm = self.llvm_type(target_type).into_int_type();
                if dst_signed {
                    self.builder
                        .build_float_to_signed_int(val.into_float_value(), target_llvm, "fptosi")
                        .unwrap()
                        .into()
                } else {
                    self.builder
                        .build_float_to_unsigned_int(val.into_float_value(), target_llvm, "fptoui")
                        .unwrap()
                        .into()
                }
            }

            TConversionKind::IntToFloat { src_signed } => {
                let target_llvm = self.llvm_type(target_type).into_float_type();
                if src_signed {
                    self.builder
                        .build_signed_int_to_float(val.into_int_value(), target_llvm, "sitofp")
                        .unwrap()
                        .into()
                } else {
                    self.builder
                        .build_unsigned_int_to_float(val.into_int_value(), target_llvm, "uitofp")
                        .unwrap()
                        .into()
                }
            }

            TConversionKind::FloatToFloat => {
                let target_llvm = self.llvm_type(target_type).into_float_type();
                let float_val = val.into_float_value();
                let src_type = float_val.get_type();

                if src_type == self.context.f32_type() && target_llvm == self.context.f64_type() {
                    self.builder
                        .build_float_ext(float_val, target_llvm, "fpext")
                        .unwrap()
                        .into()
                } else if src_type == self.context.f64_type()
                    && target_llvm == self.context.f32_type()
                {
                    self.builder
                        .build_float_trunc(float_val, target_llvm, "fptrunc")
                        .unwrap()
                        .into()
                } else {
                    val
                }
            }

            TConversionKind::IntToString => {
                let int_val = val.into_int_value();
                let bit_width = int_val.get_type().get_bit_width();

                // Convert to single byte
                let byte_val = if bit_width > 8 {
                    self.builder
                        .build_int_truncate(int_val, self.context.i8_type(), "byte")
                        .unwrap()
                } else if bit_width < 8 {
                    self.builder
                        .build_int_z_extend(int_val, self.context.i8_type(), "byte")
                        .unwrap()
                } else {
                    int_val
                };

                // Allocate 1 byte and store
                let one = self.context.i64_type().const_int(1, false);
                let data_ptr = self.call_gc_malloc(one);
                self.builder.build_store(data_ptr, byte_val).unwrap();

                // Build string struct
                let string_type = self.string_type();
                let mut result = string_type.const_zero();
                result = self
                    .builder
                    .build_insert_value(result, data_ptr, STRING_DATA_IDX, "str.data")
                    .unwrap()
                    .into_struct_value();
                result = self
                    .builder
                    .build_insert_value(result, one, STRING_LEN_IDX, "str.len")
                    .unwrap()
                    .into_struct_value();
                result.into()
            }

            TConversionKind::StringToBytes => {
                // String to []byte is a simple reinterpret (same struct layout)
                val
            }

            TConversionKind::BytesToString => {
                // []byte to string is a simple reinterpret (same struct layout for data/len)
                val
            }
        }
    }
}
