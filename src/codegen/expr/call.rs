//! Call expression code generation from TIR

use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum, FunctionType};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum};

use crate::analysis::{Type, TypeId};
use crate::codegen::Codegen;
use crate::codegen::runtime::names;
use crate::codegen::types::{SLICE_CAP_IDX, SLICE_DATA_IDX, SLICE_LEN_IDX, STRING_LEN_IDX};
use crate::tir::{TCallKind, TCapTarget, TExpr, TLenTarget, TMakeKind, TPrintKind};
use inkwell::values::PointerValue;

impl<'ctx> Codegen<'ctx> {
    pub fn gen_call(
        &mut self,
        kind: &TCallKind,
        args: &[TExpr],
        _type_id: TypeId,
    ) -> BasicValueEnum<'ctx> {
        match kind {
            TCallKind::Function { symbol_id } => {
                let fn_val = *self
                    .functions
                    .get(symbol_id)
                    .expect("codegen bug: function not registered");
                let arg_vals: Vec<BasicMetadataValueEnum> =
                    args.iter().map(|a| self.gen_expr(a).into()).collect();
                let call = self.builder.build_call(fn_val, &arg_vals, "call").unwrap();
                call.try_as_basic_value()
                    .basic()
                    .unwrap_or_else(|| self.context.i8_type().const_zero().into())
            }

            TCallKind::Method {
                receiver,
                mangled_name,
                receiver_is_pointer,
            } => {
                let fn_val = self
                    .module
                    .get_function(mangled_name)
                    .or_else(|| self.methods.get(mangled_name).copied())
                    .expect("codegen bug: method not registered");

                let recv_val = if *receiver_is_pointer {
                    // Pointer receiver
                    self.gen_expr(receiver)
                } else {
                    // Value receiver
                    self.gen_addr_of_expr(receiver).into()
                };

                let mut arg_vals: Vec<BasicMetadataValueEnum> = vec![recv_val.into()];
                arg_vals.extend(
                    args.iter()
                        .map(|a| -> BasicMetadataValueEnum { self.gen_expr(a).into() }),
                );

                let call = self.builder.build_call(fn_val, &arg_vals, "call").unwrap();
                call.try_as_basic_value()
                    .basic()
                    .unwrap_or_else(|| self.context.i8_type().const_zero().into())
            }

            TCallKind::Indirect { callee } => {
                let func_ptr = self.gen_expr(callee).into_pointer_value();
                let fn_type = self.build_indirect_call_type(callee.type_id());
                let arg_vals: Vec<BasicMetadataValueEnum> =
                    args.iter().map(|a| self.gen_expr(a).into()).collect();

                let call = self
                    .builder
                    .build_indirect_call(fn_type, func_ptr, &arg_vals, "call")
                    .unwrap();
                call.try_as_basic_value()
                    .basic()
                    .unwrap_or_else(|| self.context.i8_type().const_zero().into())
            }

            // Builtins
            TCallKind::Len(target) => self.gen_len(target),
            TCallKind::Cap(target) => self.gen_cap(target),
            TCallKind::New { elem_type } => self.gen_new(*elem_type),
            TCallKind::Make(info) => self.gen_make(&info.kind),
            TCallKind::Append {
                slice,
                elems,
                elem_type,
            } => self.gen_append(slice, elems, *elem_type),
            TCallKind::Copy {
                dst,
                src,
                elem_type,
            } => self.gen_copy(dst, src, *elem_type),
            TCallKind::Delete { map, key, key_type } => self.gen_delete(map, key, *key_type),
            TCallKind::Print {
                newline,
                args: print_args,
            } => {
                self.gen_print(print_args, *newline);
                self.context.i8_type().const_zero().into()
            }
            TCallKind::Panic { msg } => {
                self.gen_panic(msg);
                self.context.i8_type().const_zero().into()
            }
        }
    }

    fn gen_addr_of_expr(&mut self, expr: &TExpr) -> PointerValue<'ctx> {
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

    fn build_indirect_call_type(&mut self, callee_type: TypeId) -> FunctionType<'ctx> {
        let (param_ids, result_id): (Vec<TypeId>, Option<TypeId>) = {
            let ty = self.types.get(callee_type);

            // Handle named types
            let underlying_ty = match ty {
                Type::Named { underlying, .. } => self.types.get(*underlying),
                other => other,
            };

            if let Type::Signature { params, result } = underlying_ty {
                (
                    params.iter().map(|p| p.type_id).collect(),
                    result.as_ref().copied(),
                )
            } else {
                panic!("codegen bug: indirect call to non-signature type")
            }
        };

        let param_types: Vec<BasicMetadataTypeEnum> = param_ids
            .iter()
            .map(|&id| self.llvm_type(id).into())
            .collect();

        match result_id {
            Some(ret_id) => {
                let ret_type = self.llvm_type(ret_id);
                match ret_type {
                    BasicTypeEnum::IntType(t) => t.fn_type(&param_types, false),
                    BasicTypeEnum::FloatType(t) => t.fn_type(&param_types, false),
                    BasicTypeEnum::PointerType(t) => t.fn_type(&param_types, false),
                    BasicTypeEnum::StructType(t) => t.fn_type(&param_types, false),
                    BasicTypeEnum::ArrayType(t) => t.fn_type(&param_types, false),
                    BasicTypeEnum::VectorType(t) => t.fn_type(&param_types, false),
                    BasicTypeEnum::ScalableVectorType(t) => t.fn_type(&param_types, false),
                }
            }
            None => self.context.void_type().fn_type(&param_types, false),
        }
    }

    fn gen_len(&mut self, target: &TLenTarget) -> BasicValueEnum<'ctx> {
        match target {
            TLenTarget::Array { len } => self.context.i64_type().const_int(*len, false).into(),
            TLenTarget::Slice { expr } => {
                let val = self.gen_expr(expr).into_struct_value();
                self.builder
                    .build_extract_value(val, SLICE_LEN_IDX, "len")
                    .unwrap()
            }
            TLenTarget::String { expr } => {
                let val = self.gen_expr(expr).into_struct_value();
                self.builder
                    .build_extract_value(val, STRING_LEN_IDX, "len")
                    .unwrap()
            }
            TLenTarget::Map { expr } => {
                let val = self.gen_expr(expr).into_pointer_value();
                let func = self.get_runtime_fn(names::GO_MAP_LEN);
                let call = self.builder.build_call(func, &[val.into()], "len").unwrap();
                call.try_as_basic_value()
                    .basic()
                    .unwrap_or_else(|| self.context.i64_type().const_zero().into())
            }
        }
    }

    fn gen_cap(&mut self, target: &TCapTarget) -> BasicValueEnum<'ctx> {
        match target {
            TCapTarget::Array { len } => self.context.i64_type().const_int(*len, false).into(),
            TCapTarget::Slice { expr } => {
                let val = self.gen_expr(expr).into_struct_value();
                self.builder
                    .build_extract_value(val, SLICE_CAP_IDX, "cap")
                    .unwrap()
            }
        }
    }

    fn gen_new(&mut self, elem_type: TypeId) -> BasicValueEnum<'ctx> {
        let size_val = self.type_size(elem_type);
        let size = self.context.i64_type().const_int(size_val, false);
        self.call_gc_malloc(size).into()
    }

    fn gen_make(&mut self, kind: &TMakeKind) -> BasicValueEnum<'ctx> {
        match kind {
            TMakeKind::Slice {
                elem_type,
                len,
                cap,
            } => {
                let len_val = self.gen_expr(len).into_int_value();
                let cap_val = cap
                    .as_ref()
                    .map(|c| self.gen_expr(c).into_int_value())
                    .unwrap_or(len_val);

                let elem_size_val = self.type_size(*elem_type);
                let elem_size = self.context.i64_type().const_int(elem_size_val, false);
                let total_size = self
                    .builder
                    .build_int_mul(cap_val, elem_size, "total_size")
                    .unwrap();
                let data_ptr = self.call_gc_malloc(total_size);

                let slice_type = self.slice_type();
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
                    .build_insert_value(slice, cap_val, SLICE_CAP_IDX, "")
                    .unwrap()
                    .into_struct_value();
                slice.into()
            }
            TMakeKind::Map {
                key_type,
                value_type,
                key_is_string,
            } => {
                let key_size = self.type_size(*key_type);
                let val_size = self.type_size(*value_type);
                let key_type_flag = if *key_is_string { 1u64 } else { 0u64 };

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
                                .const_int(key_type_flag, false)
                                .into(),
                            self.context.i64_type().const_int(0, false).into(),
                        ],
                        "map",
                    )
                    .unwrap();
                call.try_as_basic_value()
                    .basic()
                    .expect("codegen bug: go_map_make should return a value")
            }
        }
    }

    fn gen_append(
        &mut self,
        slice: &TExpr,
        elems: &[TExpr],
        elem_type: TypeId,
    ) -> BasicValueEnum<'ctx> {
        let elem_size_val = self.type_size(elem_type);
        let elem_size = self.context.i64_type().const_int(elem_size_val, false);
        let elem_llvm_type = self.llvm_type(elem_type);

        let slice_ptr = self
            .builder
            .build_alloca(self.slice_type(), "slice")
            .unwrap();
        let slice_val = self.gen_expr(slice);
        self.builder.build_store(slice_ptr, slice_val).unwrap();

        let result_ptr = self
            .builder
            .build_alloca(self.slice_type(), "result")
            .unwrap();
        let elem_ptr = self.builder.build_alloca(elem_llvm_type, "elem").unwrap();

        let func = self.get_runtime_fn(names::GO_SLICE_APPEND);

        for elem in elems {
            let elem_val = self.gen_expr(elem);
            self.builder.build_store(elem_ptr, elem_val).unwrap();

            self.builder
                .build_call(
                    func,
                    &[
                        result_ptr.into(),
                        slice_ptr.into(),
                        elem_ptr.into(),
                        elem_size.into(),
                    ],
                    "",
                )
                .unwrap();

            let new_slice = self
                .builder
                .build_load(self.slice_type(), result_ptr, "")
                .unwrap();
            self.builder.build_store(slice_ptr, new_slice).unwrap();
        }

        self.builder
            .build_load(self.slice_type(), result_ptr, "append")
            .unwrap()
    }

    fn gen_copy(&mut self, dst: &TExpr, src: &TExpr, elem_type: TypeId) -> BasicValueEnum<'ctx> {
        let dst_val = self.gen_expr(dst);
        let src_val = self.gen_expr(src);

        let dst_ptr = self.builder.build_alloca(self.slice_type(), "dst").unwrap();
        self.builder.build_store(dst_ptr, dst_val).unwrap();
        let src_ptr = self.builder.build_alloca(self.slice_type(), "src").unwrap();
        self.builder.build_store(src_ptr, src_val).unwrap();

        let elem_size_val = self.type_size(elem_type);
        let elem_size = self.context.i64_type().const_int(elem_size_val, false);

        let func = self.get_runtime_fn(names::GO_SLICE_COPY);
        let call = self
            .builder
            .build_call(
                func,
                &[dst_ptr.into(), src_ptr.into(), elem_size.into()],
                "copy",
            )
            .unwrap();
        call.try_as_basic_value()
            .basic()
            .unwrap_or_else(|| self.context.i64_type().const_zero().into())
    }

    fn gen_delete(&mut self, map: &TExpr, key: &TExpr, key_type: TypeId) -> BasicValueEnum<'ctx> {
        let map_ptr = self.gen_expr(map).into_pointer_value();
        let key_val = self.gen_expr(key);

        let key_size_val = self.type_size(key_type);
        let key_ptr = self
            .builder
            .build_alloca(key_val.get_type(), "key")
            .unwrap();
        self.builder.build_store(key_ptr, key_val).unwrap();
        let key_size = self.context.i64_type().const_int(key_size_val, false);

        let func = self.get_runtime_fn(names::GO_MAP_DELETE);
        self.builder
            .build_call(func, &[map_ptr.into(), key_ptr.into(), key_size.into()], "")
            .unwrap();
        self.context.i8_type().const_zero().into()
    }

    fn gen_print(&mut self, args: &[(TExpr, TPrintKind)], newline: bool) {
        for (i, (arg, kind)) in args.iter().enumerate() {
            if newline && i > 0 {
                // println adds spaces between arguments
                let space_fn = self.get_runtime_fn(names::GO_PRINT_SPACE);
                self.builder.build_call(space_fn, &[], "").unwrap();
            }

            let val = self.gen_expr(arg);
            self.gen_print_value(val, *kind);
        }

        if newline {
            let newline_fn = self.get_runtime_fn(names::GO_PRINT_NEWLINE);
            self.builder.build_call(newline_fn, &[], "").unwrap();
        }
    }

    fn gen_print_value(&self, val: BasicValueEnum<'ctx>, kind: TPrintKind) {
        match kind {
            TPrintKind::Bool => {
                let func = self.get_runtime_fn(names::GO_PRINT_BOOL);
                let int_val = val.into_int_value();
                let extended = self
                    .builder
                    .build_int_z_extend(int_val, self.context.i8_type(), "")
                    .unwrap();
                self.builder
                    .build_call(func, &[extended.into()], "")
                    .unwrap();
            }
            TPrintKind::SignedInt => {
                let func = self.get_runtime_fn(names::GO_PRINT_INT);
                let int_val = val.into_int_value();
                let bit_width = int_val.get_type().get_bit_width();
                let extended = if bit_width < 64 {
                    self.builder
                        .build_int_s_extend(int_val, self.context.i64_type(), "")
                        .unwrap()
                } else {
                    int_val
                };
                self.builder
                    .build_call(func, &[extended.into()], "")
                    .unwrap();
            }
            TPrintKind::UnsignedInt => {
                let func = self.get_runtime_fn(names::GO_PRINT_UINT);
                let int_val = val.into_int_value();
                let bit_width = int_val.get_type().get_bit_width();
                let extended = if bit_width < 64 {
                    self.builder
                        .build_int_z_extend(int_val, self.context.i64_type(), "")
                        .unwrap()
                } else {
                    int_val
                };
                self.builder
                    .build_call(func, &[extended.into()], "")
                    .unwrap();
            }
            TPrintKind::Float => {
                let func = self.get_runtime_fn(names::GO_PRINT_FLOAT);
                let float_val = val.into_float_value();
                let extended = if float_val.get_type().get_bit_width() == 32 {
                    self.builder
                        .build_float_ext(float_val, self.context.f64_type(), "")
                        .unwrap()
                } else {
                    float_val
                };
                self.builder
                    .build_call(func, &[extended.into()], "")
                    .unwrap();
            }
            TPrintKind::String => {
                let func = self.get_runtime_fn(names::GO_PRINT_STRING);
                self.builder.build_call(func, &[val.into()], "").unwrap();
            }
            TPrintKind::Pointer => {
                let func = self.get_runtime_fn(names::GO_PRINT_POINTER);
                self.builder.build_call(func, &[val.into()], "").unwrap();
            }
        }
    }

    fn gen_panic(&mut self, msg: &TExpr) {
        let msg_val = self.gen_expr(msg);
        let func = self.get_runtime_fn(names::GO_PANIC);
        self.builder
            .build_call(func, &[msg_val.into()], "")
            .unwrap();
        self.builder.build_unreachable().unwrap();
    }
}
