//! Literal and constant value generation

use inkwell::module::Linkage;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, IntValue};

use crate::analysis::TypeId;
use crate::codegen::Codegen;
use crate::tir::{ConstValue, TCompositeKind, TExpr};

impl<'ctx> Codegen<'ctx> {
    pub fn gen_const_value(
        &mut self,
        value: &ConstValue,
        target_type: TypeId,
    ) -> BasicValueEnum<'ctx> {
        let llvm_type = self.llvm_type(target_type);

        // TODO: Implement a cleaner way to handle coercions
        match value {
            ConstValue::Bool(b) => self.gen_bool(*b),
            ConstValue::Int(i) => {
                // int-to-float coercion
                if let BasicTypeEnum::FloatType(ft) = llvm_type {
                    return ft.const_float(*i as f64).into();
                }

                if self.is_unsigned_type(target_type) {
                    self.gen_uint(*i as u64, target_type)
                } else {
                    self.gen_int(*i as i64, target_type)
                }
            }
            ConstValue::Uint(u) => {
                // uint-to-float coercion
                if let BasicTypeEnum::FloatType(ft) = llvm_type {
                    return ft.const_float(*u as f64).into();
                }
                self.gen_uint(*u as u64, target_type)
            }
            ConstValue::Float(f) => self.gen_float(*f, target_type),
            ConstValue::String(s) => self.gen_string_literal(s),
            ConstValue::Nil => self.gen_nil(target_type),
        }
    }

    pub fn gen_const_init(&mut self, expr: &TExpr) -> BasicValueEnum<'ctx> {
        match expr {
            TExpr::Int { value, type_id } => self.gen_int(*value, *type_id),
            TExpr::Uint { value, type_id } => self.gen_uint(*value, *type_id),
            TExpr::Float { value, type_id } => self.gen_float(*value, *type_id),
            TExpr::String { value, .. } => self.gen_string_literal(value),
            TExpr::Nil { type_id } => self.gen_nil(*type_id),
            TExpr::Const { value, type_id, .. } => self.gen_const_value(value, *type_id),
            TExpr::Composite { kind, type_id } => self.gen_const_composite(kind, *type_id),
            _ => {
                // For complex expressions, generate zero and initialize at runtime
                let type_id = expr.type_id();
                self.gen_zero(type_id)
            }
        }
    }

    fn gen_const_composite(
        &mut self,
        kind: &TCompositeKind,
        type_id: TypeId,
    ) -> BasicValueEnum<'ctx> {
        match kind {
            TCompositeKind::Array { elements, .. } => {
                let llvm_type = self.llvm_type(type_id);
                let array_type = llvm_type.into_array_type();
                let elem_type = array_type.get_element_type();
                let array_len = array_type.len() as usize;

                // Collect constant element values
                let mut const_vals: Vec<BasicValueEnum<'ctx>> = Vec::with_capacity(array_len);
                for elem in elements {
                    const_vals.push(self.gen_const_init(elem));
                }

                // Pad with zeros if needed
                let zero = self.gen_zero_for_basic_type(elem_type);
                while const_vals.len() < array_len {
                    const_vals.push(zero);
                }

                // Build constant array based on element type
                if elem_type.is_float_type() {
                    let float_vals: Vec<_> =
                        const_vals.iter().map(|v| v.into_float_value()).collect();
                    elem_type.into_float_type().const_array(&float_vals).into()
                } else if elem_type.is_int_type() {
                    let int_vals: Vec<_> = const_vals.iter().map(|v| v.into_int_value()).collect();
                    elem_type.into_int_type().const_array(&int_vals).into()
                } else {
                    // For other types like structs or pointers
                    self.gen_zero(type_id)
                }
            }
            TCompositeKind::Struct { fields } => {
                let llvm_type = self.llvm_type(type_id);
                let struct_type = llvm_type.into_struct_type();
                let field_count = struct_type.count_fields() as usize;

                // Start with all zeros
                let mut field_vals: Vec<BasicValueEnum<'ctx>> = (0..field_count)
                    .map(|i| {
                        let ft = struct_type.get_field_type_at_index(i as u32).unwrap();
                        self.gen_zero_for_basic_type(ft)
                    })
                    .collect();

                // Fill in specified fields
                for (field_idx, field_expr) in fields {
                    field_vals[*field_idx as usize] = self.gen_const_init(field_expr);
                }

                struct_type.const_named_struct(&field_vals).into()
            }
            TCompositeKind::Slice { .. } | TCompositeKind::Map { .. } => {
                // Slices and maps require runtime allocation
                self.gen_zero(type_id)
            }
        }
    }

    fn gen_zero_for_basic_type(&self, basic_type: BasicTypeEnum<'ctx>) -> BasicValueEnum<'ctx> {
        match basic_type {
            BasicTypeEnum::IntType(it) => it.const_zero().into(),
            BasicTypeEnum::FloatType(ft) => ft.const_zero().into(),
            BasicTypeEnum::PointerType(pt) => pt.const_null().into(),
            BasicTypeEnum::ArrayType(at) => at.const_zero().into(),
            BasicTypeEnum::StructType(st) => st.const_zero().into(),
            BasicTypeEnum::VectorType(vt) => vt.const_zero().into(),
            BasicTypeEnum::ScalableVectorType(svt) => svt.const_zero().into(),
        }
    }

    pub fn gen_bool(&self, value: bool) -> BasicValueEnum<'ctx> {
        self.context
            .bool_type()
            .const_int(u64::from(value), false)
            .into()
    }

    pub fn gen_int(&mut self, value: i64, type_id: TypeId) -> BasicValueEnum<'ctx> {
        let llvm_type = self.llvm_type(type_id);
        if let BasicTypeEnum::IntType(int_type) = llvm_type {
            int_type.const_int(value as u64, true).into()
        } else {
            panic!("codegen bug: gen_int called with non-integer type")
        }
    }

    pub fn gen_uint(&mut self, value: u64, type_id: TypeId) -> BasicValueEnum<'ctx> {
        let llvm_type = self.llvm_type(type_id);
        if let BasicTypeEnum::IntType(int_type) = llvm_type {
            int_type.const_int(value, false).into()
        } else {
            panic!("codegen bug: gen_uint called with non-integer type")
        }
    }

    pub fn gen_float(&mut self, value: f64, type_id: TypeId) -> BasicValueEnum<'ctx> {
        let llvm_type = self.llvm_type(type_id);
        if let BasicTypeEnum::FloatType(ft) = llvm_type {
            ft.const_float(value).into()
        } else {
            panic!("codegen bug: gen_float called with non-float type")
        }
    }

    /// Generate a string literal as a global constant
    pub fn gen_string_literal(&mut self, s: &str) -> BasicValueEnum<'ctx> {
        let len = s.len();

        // Check intern table for existing global
        let data_ptr = if let Some(&existing_ptr) = self.string_intern.get(s) {
            existing_ptr
        } else {
            // Create new global string data
            let bytes = s.as_bytes();
            let i8_type = self.context.i8_type();
            let array_type = i8_type.array_type(len as u32);
            let array_values: Vec<IntValue> = bytes
                .iter()
                .map(|b| i8_type.const_int(*b as u64, false))
                .collect();
            let array_const = i8_type.const_array(&array_values);

            let global = self.module.add_global(array_type, None, "str");
            global.set_initializer(&array_const);
            global.set_constant(true);
            global.set_linkage(Linkage::Private);

            // Get pointer to first element
            let zero = self.context.i64_type().const_zero();
            let ptr = unsafe {
                global
                    .as_pointer_value()
                    .const_gep(array_type, &[zero, zero])
            };

            // Cache for reuse
            self.string_intern.insert(s.to_string(), ptr);
            ptr
        };

        // Build string struct { ptr, len }
        let len_val = self.context.i64_type().const_int(len as u64, false);
        let string_type = self.string_type();
        string_type
            .const_named_struct(&[data_ptr.into(), len_val.into()])
            .into()
    }

    pub fn gen_nil(&mut self, type_id: TypeId) -> BasicValueEnum<'ctx> {
        let llvm_type = self.llvm_type(type_id);
        match llvm_type {
            BasicTypeEnum::PointerType(pt) => pt.const_null().into(),
            BasicTypeEnum::StructType(st) => st.const_zero().into(),
            _ => self.ptr_type().const_null().into(),
        }
    }

    pub fn gen_zero(&mut self, type_id: TypeId) -> BasicValueEnum<'ctx> {
        let llvm_type = self.llvm_type(type_id);
        match llvm_type {
            BasicTypeEnum::PointerType(t) => t.const_null().into(),
            t => t.const_zero(),
        }
    }
}
