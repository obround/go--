//! Type representation and arena

use crate::token::Span;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(u32);

impl TypeId {
    pub const INVALID: TypeId = TypeId(0);
    pub const BOOL: TypeId = TypeId(1);
    pub const INT: TypeId = TypeId(2);
    pub const INT8: TypeId = TypeId(3);
    pub const INT16: TypeId = TypeId(4);
    pub const INT32: TypeId = TypeId(5);
    pub const INT64: TypeId = TypeId(6);
    pub const UINT: TypeId = TypeId(7);
    pub const UINT8: TypeId = TypeId(8);
    pub const UINT16: TypeId = TypeId(9);
    pub const UINT32: TypeId = TypeId(10);
    pub const UINT64: TypeId = TypeId(11);
    pub const UINTPTR: TypeId = TypeId(12);
    pub const FLOAT32: TypeId = TypeId(13);
    pub const FLOAT64: TypeId = TypeId(14);
    pub const STRING: TypeId = TypeId(15);

    pub fn index(self) -> usize {
        self.0 as usize
    }
}

/// Basic types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BasicType {
    Bool,
    Int,
    Int8,
    Int16,
    Int32,
    Int64,
    Uint,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Uintptr,
    Float32,
    Float64,
    String,
}

impl BasicType {
    pub fn is_numeric(self) -> bool {
        !matches!(self, BasicType::Bool | BasicType::String)
    }

    pub fn is_integer(self) -> bool {
        matches!(
            self,
            BasicType::Int
                | BasicType::Int8
                | BasicType::Int16
                | BasicType::Int32
                | BasicType::Int64
                | BasicType::Uint
                | BasicType::Uint8
                | BasicType::Uint16
                | BasicType::Uint32
                | BasicType::Uint64
                | BasicType::Uintptr
        )
    }

    pub fn is_signed(self) -> bool {
        matches!(
            self,
            BasicType::Int
                | BasicType::Int8
                | BasicType::Int16
                | BasicType::Int32
                | BasicType::Int64
        )
    }

    pub fn is_float(self) -> bool {
        matches!(self, BasicType::Float32 | BasicType::Float64)
    }
}

/// Untyped kinds
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UntypedKind {
    Bool,
    Int,
    Float,
    String,
    Nil,
}

/// Builtin functions
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinKind {
    Println,
    Print,
    Len,
    Cap,
    New,
    Make,
    Append,
    Copy,
    Delete,
    Panic,
}

impl BuiltinKind {
    pub fn name(self) -> &'static str {
        match self {
            BuiltinKind::Println => "println",
            BuiltinKind::Print => "print",
            BuiltinKind::Len => "len",
            BuiltinKind::Cap => "cap",
            BuiltinKind::New => "new",
            BuiltinKind::Make => "make",
            BuiltinKind::Append => "append",
            BuiltinKind::Copy => "copy",
            BuiltinKind::Delete => "delete",
            BuiltinKind::Panic => "panic",
        }
    }
}

/// Struct field
#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub type_id: TypeId,
    pub span: Span,
}

/// Function parameter
#[derive(Debug, Clone)]
pub struct Param {
    #[allow(dead_code)]
    pub name: String,
    pub type_id: TypeId,
}

/// Method definition
#[derive(Debug, Clone)]
pub struct MethodInfo {
    pub name: String,
    pub signature: TypeId,
    pub receiver_is_pointer: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Type {
    Invalid,
    Basic {
        kind: BasicType,
    },
    Untyped {
        kind: UntypedKind,
    },
    Builtin {
        kind: BuiltinKind,
    },
    Pointer {
        base: TypeId,
    },
    Array {
        len: u64,
        elem: TypeId,
    },
    Slice {
        elem: TypeId,
    },
    Map {
        key: TypeId,
        value: TypeId,
    },
    Struct {
        fields: Vec<StructField>,
    },
    Signature {
        params: Vec<Param>,
        result: Option<TypeId>,
    },
    Named {
        name: String,
        underlying: TypeId,
        methods: Vec<MethodInfo>,
    },
}

pub struct TypeArena {
    types: Vec<Type>,
    // Consing caches
    pointer_cache: HashMap<TypeId, TypeId>,
    slice_cache: HashMap<TypeId, TypeId>,
    array_cache: HashMap<(u64, TypeId), TypeId>,
    map_cache: HashMap<(TypeId, TypeId), TypeId>,
}

impl TypeArena {
    pub fn new() -> Self {
        let mut arena = Self {
            types: Vec::new(),
            pointer_cache: HashMap::new(),
            slice_cache: HashMap::new(),
            array_cache: HashMap::new(),
            map_cache: HashMap::new(),
        };

        // Built-ins
        arena.types.push(Type::Invalid);
        arena.types.push(Type::Basic {
            kind: BasicType::Bool,
        });
        arena.types.push(Type::Basic {
            kind: BasicType::Int,
        });
        arena.types.push(Type::Basic {
            kind: BasicType::Int8,
        });
        arena.types.push(Type::Basic {
            kind: BasicType::Int16,
        });
        arena.types.push(Type::Basic {
            kind: BasicType::Int32,
        });
        arena.types.push(Type::Basic {
            kind: BasicType::Int64,
        });
        arena.types.push(Type::Basic {
            kind: BasicType::Uint,
        });
        arena.types.push(Type::Basic {
            kind: BasicType::Uint8,
        });
        arena.types.push(Type::Basic {
            kind: BasicType::Uint16,
        });
        arena.types.push(Type::Basic {
            kind: BasicType::Uint32,
        });
        arena.types.push(Type::Basic {
            kind: BasicType::Uint64,
        });
        arena.types.push(Type::Basic {
            kind: BasicType::Uintptr,
        });
        arena.types.push(Type::Basic {
            kind: BasicType::Float32,
        });
        arena.types.push(Type::Basic {
            kind: BasicType::Float64,
        });
        arena.types.push(Type::Basic {
            kind: BasicType::String,
        });

        arena
    }

    fn alloc(&mut self, ty: Type) -> TypeId {
        let id = TypeId(self.types.len() as u32);
        self.types.push(ty);
        id
    }

    pub fn get(&self, id: TypeId) -> &Type {
        &self.types[id.index()]
    }

    pub fn get_mut(&mut self, id: TypeId) -> &mut Type {
        &mut self.types[id.index()]
    }

    pub fn invalid(&self) -> TypeId {
        TypeId::INVALID
    }

    pub fn untyped(&mut self, kind: UntypedKind) -> TypeId {
        self.alloc(Type::Untyped { kind })
    }

    pub fn builtin(&mut self, kind: BuiltinKind) -> TypeId {
        self.alloc(Type::Builtin { kind })
    }

    pub fn pointer(&mut self, base: TypeId) -> TypeId {
        if let Some(&cached) = self.pointer_cache.get(&base) {
            return cached;
        }
        let id = self.alloc(Type::Pointer { base });
        self.pointer_cache.insert(base, id);
        id
    }

    pub fn slice(&mut self, elem: TypeId) -> TypeId {
        if let Some(&cached) = self.slice_cache.get(&elem) {
            return cached;
        }
        let id = self.alloc(Type::Slice { elem });
        self.slice_cache.insert(elem, id);
        id
    }

    pub fn array(&mut self, len: u64, elem: TypeId) -> TypeId {
        let key = (len, elem);
        if let Some(&cached) = self.array_cache.get(&key) {
            return cached;
        }
        let id = self.alloc(Type::Array { len, elem });
        self.array_cache.insert(key, id);
        id
    }

    pub fn map(&mut self, key: TypeId, value: TypeId) -> TypeId {
        let cache_key = (key, value);
        if let Some(&cached) = self.map_cache.get(&cache_key) {
            return cached;
        }
        let id = self.alloc(Type::Map { key, value });
        self.map_cache.insert(cache_key, id);
        id
    }

    pub fn structure(&mut self, fields: Vec<StructField>) -> TypeId {
        self.alloc(Type::Struct { fields })
    }

    pub fn signature(&mut self, params: Vec<Param>, result: Option<TypeId>) -> TypeId {
        self.alloc(Type::Signature { params, result })
    }

    pub fn named(&mut self, name: String, underlying: TypeId) -> TypeId {
        self.alloc(Type::Named {
            name,
            underlying,
            methods: Vec::new(),
        })
    }

    pub fn underlying(&self, id: TypeId) -> TypeId {
        match self.get(id) {
            Type::Named { underlying, .. } => self.underlying(*underlying),
            _ => id,
        }
    }

    pub fn is_invalid(&self, id: TypeId) -> bool {
        matches!(self.get(id), Type::Invalid)
    }

    pub fn is_basic(&self, id: TypeId) -> Option<BasicType> {
        match self.get(self.underlying(id)) {
            Type::Basic { kind } => Some(*kind),
            _ => None,
        }
    }

    pub fn is_boolean(&self, id: TypeId) -> bool {
        matches!(
            self.get(self.underlying(id)),
            Type::Basic {
                kind: BasicType::Bool,
            } | Type::Untyped {
                kind: UntypedKind::Bool,
            }
        )
    }

    pub fn is_integer(&self, id: TypeId) -> bool {
        match self.get(self.underlying(id)) {
            Type::Basic { kind } => kind.is_integer(),
            Type::Untyped {
                kind: UntypedKind::Int,
            } => true,
            _ => false,
        }
    }

    pub fn is_signed_int(&self, id: TypeId) -> bool {
        match self.get(self.underlying(id)) {
            Type::Basic { kind } => kind.is_signed(),
            // Untyped int constants should be treated as signed
            Type::Untyped {
                kind: UntypedKind::Int,
            } => true,
            _ => false,
        }
    }

    pub fn is_unsigned_int(&self, id: TypeId) -> bool {
        match self.get(self.underlying(id)) {
            Type::Basic { kind } => kind.is_integer() && !kind.is_signed(),
            _ => false,
        }
    }

    pub fn is_float(&self, id: TypeId) -> bool {
        match self.get(self.underlying(id)) {
            Type::Basic { kind } => kind.is_float(),
            Type::Untyped {
                kind: UntypedKind::Float,
            } => true,
            _ => false,
        }
    }

    pub fn is_numeric(&self, id: TypeId) -> bool {
        match self.get(self.underlying(id)) {
            Type::Basic { kind } => kind.is_numeric(),
            Type::Untyped {
                kind: UntypedKind::Int | UntypedKind::Float,
            } => true,
            _ => false,
        }
    }

    pub fn is_string(&self, id: TypeId) -> bool {
        matches!(
            self.get(self.underlying(id)),
            Type::Basic {
                kind: BasicType::String,
            } | Type::Untyped {
                kind: UntypedKind::String,
            }
        )
    }

    pub fn is_untyped(&self, id: TypeId) -> Option<UntypedKind> {
        match self.get(id) {
            Type::Untyped { kind } => Some(*kind),
            _ => None,
        }
    }

    pub fn is_untyped_nil(&self, id: TypeId) -> bool {
        matches!(
            self.get(id),
            Type::Untyped {
                kind: UntypedKind::Nil
            }
        )
    }

    pub fn is_untyped_bool(&self, id: TypeId) -> bool {
        matches!(
            self.get(id),
            Type::Untyped {
                kind: UntypedKind::Bool
            }
        )
    }

    /// Is named type
    pub fn is_named(&self, id: TypeId) -> bool {
        matches!(self.get(id), Type::Named { .. })
    }

    pub fn is_builtin(&self, id: TypeId) -> Option<BuiltinKind> {
        match self.get(id) {
            Type::Builtin { kind } => Some(*kind),
            _ => None,
        }
    }

    pub fn is_pointer(&self, id: TypeId) -> Option<TypeId> {
        match self.get(self.underlying(id)) {
            Type::Pointer { base } => Some(*base),
            _ => None,
        }
    }

    pub fn is_slice(&self, id: TypeId) -> Option<TypeId> {
        match self.get(self.underlying(id)) {
            Type::Slice { elem } => Some(*elem),
            _ => None,
        }
    }

    pub fn is_array(&self, id: TypeId) -> Option<(u64, TypeId)> {
        match self.get(self.underlying(id)) {
            Type::Array { len, elem } => Some((*len, *elem)),
            _ => None,
        }
    }

    pub fn is_map(&self, id: TypeId) -> Option<(TypeId, TypeId)> {
        match self.get(self.underlying(id)) {
            Type::Map { key, value } => Some((*key, *value)),
            _ => None,
        }
    }

    pub fn is_struct(&self, id: TypeId) -> bool {
        matches!(self.get(self.underlying(id)), Type::Struct { .. })
    }

    pub fn struct_fields(&self, id: TypeId) -> Option<&[StructField]> {
        match self.get(self.underlying(id)) {
            Type::Struct { fields } => Some(fields),
            _ => None,
        }
    }

    pub fn is_signature(&self, id: TypeId) -> Option<(&[Param], Option<TypeId>)> {
        match self.get(self.underlying(id)) {
            Type::Signature { params, result } => Some((params, *result)),
            _ => None,
        }
    }

    pub fn is_comparable(&self, id: TypeId) -> bool {
        let underlying = self.underlying(id);
        match self.get(underlying) {
            Type::Invalid => true,
            Type::Basic { .. } => true,
            Type::Untyped { .. } => true,
            Type::Pointer { .. } => true,
            Type::Array { elem, .. } => self.is_comparable(*elem),
            Type::Struct { fields } => fields.iter().all(|f| self.is_comparable(f.type_id)),
            // Slices, maps, and functions are not comparable
            Type::Slice { .. } | Type::Map { .. } | Type::Signature { .. } => false,
            Type::Named { underlying, .. } => self.is_comparable(*underlying),
            Type::Builtin { .. } => false,
        }
    }

    pub fn is_ordered(&self, id: TypeId) -> bool {
        let underlying = self.underlying(id);
        match self.get(underlying) {
            Type::Basic { kind } => kind.is_numeric() || matches!(kind, BasicType::String),
            Type::Untyped { kind } => matches!(
                kind,
                UntypedKind::Int | UntypedKind::Float | UntypedKind::String
            ),
            _ => false,
        }
    }

    /// Human-readable type name
    pub fn type_name(&self, id: TypeId) -> String {
        match self.get(id) {
            Type::Invalid => "<invalid>".to_string(),
            Type::Basic { kind } => match kind {
                BasicType::Bool => "bool".to_string(),
                BasicType::Int => "int".to_string(),
                BasicType::Int8 => "int8".to_string(),
                BasicType::Int16 => "int16".to_string(),
                BasicType::Int32 => "int32".to_string(),
                BasicType::Int64 => "int64".to_string(),
                BasicType::Uint => "uint".to_string(),
                BasicType::Uint8 => "uint8".to_string(),
                BasicType::Uint16 => "uint16".to_string(),
                BasicType::Uint32 => "uint32".to_string(),
                BasicType::Uint64 => "uint64".to_string(),
                BasicType::Uintptr => "uintptr".to_string(),
                BasicType::Float32 => "float32".to_string(),
                BasicType::Float64 => "float64".to_string(),
                BasicType::String => "string".to_string(),
            },
            Type::Untyped { kind } => match kind {
                UntypedKind::Bool => "untyped bool".to_string(),
                UntypedKind::Int => "untyped int".to_string(),
                UntypedKind::Float => "untyped float".to_string(),
                UntypedKind::String => "untyped string".to_string(),
                UntypedKind::Nil => "untyped nil".to_string(),
            },
            Type::Builtin { kind } => format!("builtin {}", kind.name()),
            Type::Pointer { base } => format!("*{}", self.type_name(*base)),
            Type::Array { len, elem } => format!("[{}]{}", len, self.type_name(*elem)),
            Type::Slice { elem } => format!("[]{}", self.type_name(*elem)),
            Type::Map { key, value } => {
                format!("map[{}]{}", self.type_name(*key), self.type_name(*value))
            }
            Type::Struct { fields } => {
                if fields.is_empty() {
                    "struct{}".to_string()
                } else {
                    format!("struct{{...{} fields}}", fields.len())
                }
            }
            Type::Signature { params, result } => {
                let params_str: Vec<_> = params.iter().map(|p| self.type_name(p.type_id)).collect();
                let ret = result.map(|r| self.type_name(r)).unwrap_or_default();
                if ret.is_empty() {
                    format!("func({})", params_str.join(", "))
                } else {
                    format!("func({}) {}", params_str.join(", "), ret)
                }
            }
            Type::Named { name, .. } => name.clone(),
        }
    }

    /// Check type identity
    pub fn identical(&self, a: TypeId, b: TypeId) -> bool {
        if a == b {
            return true;
        }

        let type_a = self.get(a);
        let type_b = self.get(b);

        match (type_a, type_b) {
            (Type::Pointer { base: a }, Type::Pointer { base: b }) => self.identical(*a, *b),
            (Type::Slice { elem: a }, Type::Slice { elem: b }) => self.identical(*a, *b),
            (Type::Array { len: la, elem: ea }, Type::Array { len: lb, elem: eb }) => {
                la == lb && self.identical(*ea, *eb)
            }
            (Type::Map { key: ka, value: va }, Type::Map { key: kb, value: vb }) => {
                self.identical(*ka, *kb) && self.identical(*va, *vb)
            }
            (Type::Struct { fields: fa }, Type::Struct { fields: fb }) => {
                if fa.len() != fb.len() {
                    return false;
                }
                fa.iter()
                    .zip(fb.iter())
                    .all(|(a, b)| a.name == b.name && self.identical(a.type_id, b.type_id))
            }
            (
                Type::Signature {
                    params: pa,
                    result: ra,
                },
                Type::Signature {
                    params: pb,
                    result: rb,
                },
            ) => {
                if pa.len() != pb.len() {
                    return false;
                }
                let params_match = pa
                    .iter()
                    .zip(pb.iter())
                    .all(|(a, b)| self.identical(a.type_id, b.type_id));
                let result_match = match (ra, rb) {
                    (Some(a), Some(b)) => self.identical(*a, *b),
                    (None, None) => true,
                    _ => false,
                };
                params_match && result_match
            }
            _ => false,
        }
    }

    /// Register method on type
    pub fn add_method(&mut self, type_id: TypeId, method: MethodInfo) {
        if let Type::Named { methods, .. } = self.get_mut(type_id) {
            methods.push(method);
        }
    }
}

impl Default for TypeArena {
    fn default() -> Self {
        Self::new()
    }
}
