//! Composite literal code generation from TIR

use inkwell::values::BasicValueEnum;

use crate::analysis::{BasicType, Type, TypeId};
use crate::codegen::Codegen;
use crate::codegen::runtime::names;
use crate::codegen::types::{SLICE_CAP_IDX, SLICE_DATA_IDX, SLICE_LEN_IDX};
use crate::tir::{TCompositeKind, TExpr};

impl<'ctx> Codegen<'ctx> {
    pub fn gen_composite(
        &mut self,
        kind: &TCompositeKind,
        type_id: TypeId,
    ) -> BasicValueEnum<'ctx> {
        match kind {
            TCompositeKind::Struct { fields } => self.gen_struct_literal(type_id, fields),
            TCompositeKind::Array {
                elem_type,
                elements,
            } => self.gen_array_literal(type_id, *elem_type, elements),
            TCompositeKind::Slice {
                elem_type,
                elements,
            } => self.gen_slice_literal(*elem_type, elements),
            TCompositeKind::Map {
                key_type,
                value_type,
                entries,
            } => self.gen_map_literal(*key_type, *value_type, entries),
        }
    }

    fn gen_struct_literal(
        &mut self,
        struct_type_id: TypeId,
        fields: &[(u32, TExpr)],
    ) -> BasicValueEnum<'ctx> {
        let llvm_type = self.llvm_type(struct_type_id);
        let struct_type = llvm_type.into_struct_type();

        // Allocate on stack
        // TODO: Use GC
        let alloca = self
            .builder
            .build_alloca(struct_type, "struct.lit")
            .unwrap();

        // Zero out fields
        self.builder
            .build_store(alloca, struct_type.const_zero())
            .unwrap();

        // Set specified fields
        for (field_idx, field_expr) in fields {
            let field_ptr = self
                .builder
                .build_struct_gep(struct_type, alloca, *field_idx, "field")
                .unwrap();
            let val = self.gen_expr(field_expr);
            self.builder.build_store(field_ptr, val).unwrap();
        }

        self.builder
            .build_load(struct_type, alloca, "struct")
            .unwrap()
    }

    fn gen_array_literal(
        &mut self,
        array_type_id: TypeId,
        _elem_type_id: TypeId,
        elements: &[TExpr],
    ) -> BasicValueEnum<'ctx> {
        let llvm_type = self.llvm_type(array_type_id);
        let array_type = llvm_type.into_array_type();

        // Allocate on stack
        let alloca = self.builder.build_alloca(array_type, "array.lit").unwrap();

        // Zero out fields
        self.builder
            .build_store(alloca, array_type.const_zero())
            .unwrap();

        // Set specified elements
        for (i, elem_expr) in elements.iter().enumerate() {
            let idx_val = self.context.i64_type().const_int(i as u64, false);
            let elem_ptr = unsafe {
                self.builder
                    .build_gep(
                        array_type,
                        alloca,
                        &[self.context.i64_type().const_zero(), idx_val],
                        "elem",
                    )
                    .unwrap()
            };
            let val = self.gen_expr(elem_expr);
            self.builder.build_store(elem_ptr, val).unwrap();
        }

        self.builder
            .build_load(array_type, alloca, "array")
            .unwrap()
    }

    fn gen_slice_literal(
        &mut self,
        elem_type_id: TypeId,
        elements: &[TExpr],
    ) -> BasicValueEnum<'ctx> {
        let len = elements.len() as u64;
        let elem_size = self.type_size(elem_type_id);
        let elem_llvm_type = self.llvm_type(elem_type_id);

        // Allocate
        let total_size = self.context.i64_type().const_int(len * elem_size, false);
        let data_ptr = self.call_gc_malloc(total_size);

        // Populate
        for (i, elem_expr) in elements.iter().enumerate() {
            let idx_val = self.context.i64_type().const_int(i as u64, false);
            let elem_ptr = unsafe {
                self.builder
                    .build_gep(elem_llvm_type, data_ptr, &[idx_val], "elem")
                    .unwrap()
            };
            let val = self.gen_expr(elem_expr);
            self.builder.build_store(elem_ptr, val).unwrap();
        }

        // Build slice struct { ptr, len, cap }
        let slice_type = self.slice_type();
        let len_val = self.context.i64_type().const_int(len, false);
        let mut slice = slice_type.get_undef();
        slice = self
            .builder
            .build_insert_value(slice, data_ptr, SLICE_DATA_IDX, "")
            .unwrap()
            .into_struct_value();
        slice = self
            .builder
            .build_insert_value(slice, len_val, SLICE_LEN_IDX, "")
            .unwrap()
            .into_struct_value();
        slice = self
            .builder
            .build_insert_value(slice, len_val, SLICE_CAP_IDX, "")
            .unwrap()
            .into_struct_value();

        slice.into()
    }

    fn gen_map_literal(
        &mut self,
        key_type_id: TypeId,
        val_type_id: TypeId,
        entries: &[(TExpr, TExpr)],
    ) -> BasicValueEnum<'ctx> {
        let key_size = self.type_size(key_type_id);
        let val_size = self.type_size(val_type_id);
        let hint = entries.len() as u64;

        let key_type_val = if self.is_string_type(key_type_id) {
            1u64 // String
        } else {
            0u64
        };

        // Allocate
        let func = self.get_runtime_fn(names::GO_MAP_MAKE);
        let call = self
            .builder
            .build_call(
                func,
                &[
                    self.context.i64_type().const_int(key_size, false).into(),
                    self.context.i64_type().const_int(val_size, false).into(),
                    self.context
                        .i64_type()
                        .const_int(key_type_val, false)
                        .into(),
                    self.context.i64_type().const_int(hint, false).into(),
                ],
                "map",
            )
            .unwrap();
        let map_ptr = call
            .try_as_basic_value()
            .basic()
            .expect("codegen bug: cgo_map_make should return a value")
            .into_pointer_value();

        // Populate
        for (key_expr, val_expr) in entries {
            let key_val = self.gen_expr(key_expr);
            let val_val = self.gen_expr(val_expr);

            let key_ptr = self
                .builder
                .build_alloca(key_val.get_type(), "key")
                .unwrap();
            let val_ptr = self
                .builder
                .build_alloca(val_val.get_type(), "val")
                .unwrap();
            self.builder.build_store(key_ptr, key_val).unwrap();
            self.builder.build_store(val_ptr, val_val).unwrap();

            let set_func = self.get_runtime_fn(names::GO_MAP_SET);
            self.builder
                .build_call(
                    set_func,
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

        map_ptr.into()
    }

    fn is_string_type(&self, type_id: TypeId) -> bool {
        let underlying = self.types.underlying(type_id);
        matches!(
            self.types.get(underlying),
            Type::Basic {
                kind: BasicType::String
            }
        )
    }
}
