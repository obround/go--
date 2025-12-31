//! Go type to LLVM type mapping

use inkwell::AddressSpace;
use inkwell::types::{
    BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, PointerType, StructType,
};

use crate::analysis::{BasicType as GoBasicType, SymbolId, Type, TypeId, UntypedKind};
use crate::codegen::Codegen;

/// String struct field indices: { data: ptr, len: i64 }
pub const STRING_DATA_IDX: u32 = 0;
pub const STRING_LEN_IDX: u32 = 1;

/// Slice struct field indices: { data: ptr, len: i64, cap: i64 }
pub const SLICE_DATA_IDX: u32 = 0;
pub const SLICE_LEN_IDX: u32 = 1;
pub const SLICE_CAP_IDX: u32 = 2;

impl<'ctx> Codegen<'ctx> {
    pub fn init_type_cache(&mut self) {
        let _ = self.llvm_type(self.universe.bool_type);
        let _ = self.llvm_type(self.universe.int_type);
        let _ = self.llvm_type(self.universe.int8_type);
        let _ = self.llvm_type(self.universe.int16_type);
        let _ = self.llvm_type(self.universe.int32_type);
        let _ = self.llvm_type(self.universe.int64_type);
        let _ = self.llvm_type(self.universe.uint_type);
        let _ = self.llvm_type(self.universe.uint8_type);
        let _ = self.llvm_type(self.universe.uint16_type);
        let _ = self.llvm_type(self.universe.uint32_type);
        let _ = self.llvm_type(self.universe.uint64_type);
        let _ = self.llvm_type(self.universe.uintptr_type);
        let _ = self.llvm_type(self.universe.float32_type);
        let _ = self.llvm_type(self.universe.float64_type);
        let _ = self.llvm_type(self.universe.string_type);

        let _ = self.llvm_type(self.universe.untyped_bool);
        let _ = self.llvm_type(self.universe.untyped_int);
        let _ = self.llvm_type(self.universe.untyped_float);
        let _ = self.llvm_type(self.universe.untyped_string);
        let _ = self.llvm_type(self.universe.untyped_nil);
    }

    pub fn llvm_type(&mut self, type_id: TypeId) -> BasicTypeEnum<'ctx> {
        if let Some(&cached) = self.llvm_types.get(&type_id) {
            return cached;
        }
        let llvm_type = self.generate_llvm_type(type_id);
        self.llvm_types.insert(type_id, llvm_type);
        llvm_type
    }

    fn generate_llvm_type(&mut self, type_id: TypeId) -> BasicTypeEnum<'ctx> {
        let type_info = self.types.get(type_id).clone();

        match type_info {
            Type::Invalid => unreachable!("codegen bug: invalid type reached codegen"),
            Type::Basic { kind } => self.basic_to_llvm(kind),
            Type::Untyped { kind } => {
                // Default type representation
                match kind {
                    UntypedKind::Bool => self.context.bool_type().into(),
                    UntypedKind::Int => self.context.i64_type().into(),
                    UntypedKind::Float => self.context.f64_type().into(),
                    UntypedKind::String => self.string_type().into(),
                    UntypedKind::Nil => self.ptr_type().into(),
                }
            }
            Type::Named { underlying, .. } => self.llvm_type(underlying),
            Type::Pointer { .. } => self.ptr_type().into(),
            Type::Array { len, elem } => {
                let elem_type = self.llvm_type(elem);
                elem_type.array_type(len as u32).into()
            }
            Type::Slice { .. } => self.slice_type().into(),
            Type::Struct { ref fields } => {
                // Anonymous struct
                let field_types: Vec<BasicTypeEnum> =
                    fields.iter().map(|f| self.llvm_type(f.type_id)).collect();
                self.context.struct_type(&field_types, false).into()
            }
            Type::Map { .. } => self.ptr_type().into(),
            Type::Signature { .. } => self.ptr_type().into(),
            // Builtins don't have runtime representation
            Type::Builtin { .. } => unreachable!("codegen bug: builtin type reached codegen"),
        }
    }

    fn basic_to_llvm(&self, basic: GoBasicType) -> BasicTypeEnum<'ctx> {
        match basic {
            GoBasicType::Bool => self.context.bool_type().into(),
            GoBasicType::Int8 | GoBasicType::Uint8 => self.context.i8_type().into(),
            GoBasicType::Int16 | GoBasicType::Uint16 => self.context.i16_type().into(),
            GoBasicType::Int32 | GoBasicType::Uint32 => self.context.i32_type().into(),
            GoBasicType::Int64
            | GoBasicType::Uint64
            | GoBasicType::Int
            | GoBasicType::Uint
            | GoBasicType::Uintptr => self.context.i64_type().into(),
            GoBasicType::Float32 => self.context.f32_type().into(),
            GoBasicType::Float64 => self.context.f64_type().into(),
            GoBasicType::String => self.string_type().into(),
        }
    }

    pub fn ptr_type(&self) -> PointerType<'ctx> {
        self.context.ptr_type(AddressSpace::default())
    }

    /// { data: ptr, len: i64 }
    pub fn string_type(&self) -> StructType<'ctx> {
        self.context.struct_type(
            &[self.ptr_type().into(), self.context.i64_type().into()],
            false,
        )
    }

    /// { data: ptr, len: i64, cap: i64 }
    pub fn slice_type(&self) -> StructType<'ctx> {
        self.context.struct_type(
            &[
                self.ptr_type().into(),
                self.context.i64_type().into(),
                self.context.i64_type().into(),
            ],
            false,
        )
    }

    pub fn build_function_type(
        &mut self,
        params: &[(SymbolId, TypeId)],
        result: Option<TypeId>,
    ) -> FunctionType<'ctx> {
        let param_types: Vec<BasicMetadataTypeEnum> = params
            .iter()
            .map(|(_, type_id)| self.llvm_type(*type_id).into())
            .collect();

        match result {
            Some(ret_id) => self.llvm_type(ret_id).fn_type(&param_types, false),
            None => self.context.void_type().fn_type(&param_types, false),
        }
    }

    pub fn build_method_type(
        &mut self,
        _receiver_type: TypeId,
        params: &[(SymbolId, TypeId)],
        result: Option<TypeId>,
    ) -> FunctionType<'ctx> {
        // Receiver is always passed as pointer
        let mut param_types: Vec<BasicMetadataTypeEnum> =
            vec![self.context.ptr_type(AddressSpace::default()).into()];
        param_types.extend(
            params
                .iter()
                .map(|(_, type_id)| -> BasicMetadataTypeEnum { self.llvm_type(*type_id).into() }),
        );

        match result {
            Some(ret_id) => self.llvm_type(ret_id).fn_type(&param_types, false),
            None => self.context.void_type().fn_type(&param_types, false),
        }
    }

    /// Size of a Go type in bytes
    pub fn type_size(&mut self, type_id: TypeId) -> u64 {
        let llvm_type = self.llvm_type(type_id);
        self.size_of_llvm_type(llvm_type)
    }

    /// Size of an LLVM type in bytes
    fn size_of_llvm_type(&self, ty: BasicTypeEnum<'ctx>) -> u64 {
        match ty {
            BasicTypeEnum::IntType(t) => (t.get_bit_width() as u64).div_ceil(8),
            BasicTypeEnum::FloatType(t) => {
                if t == self.context.f32_type() {
                    4
                } else {
                    8
                }
            }
            BasicTypeEnum::PointerType(_) => 8,
            BasicTypeEnum::ArrayType(t) => {
                let elem_size = self.size_of_llvm_type(t.get_element_type());
                elem_size * t.len() as u64
            }
            BasicTypeEnum::StructType(t) => {
                let mut size = 0u64;
                for i in 0..t.count_fields() {
                    if let Some(field_type) = t.get_field_type_at_index(i) {
                        size += self.size_of_llvm_type(field_type);
                    }
                }
                size
            }
            BasicTypeEnum::VectorType(_) => 16,
            BasicTypeEnum::ScalableVectorType(_) => 16,
        }
    }

    pub fn is_unsigned_type(&self, type_id: TypeId) -> bool {
        let ty = self.types.get(type_id);
        match ty {
            Type::Basic { kind } => matches!(
                kind,
                GoBasicType::Uint
                    | GoBasicType::Uint8
                    | GoBasicType::Uint16
                    | GoBasicType::Uint32
                    | GoBasicType::Uint64
                    | GoBasicType::Uintptr
            ),
            Type::Named { underlying, .. } => self.is_unsigned_type(*underlying),
            _ => false,
        }
    }
}
