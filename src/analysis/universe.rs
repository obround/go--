//! Universe scope initialization

use crate::analysis::scope::{ScopeId, ScopeKind, ScopeTree};
use crate::analysis::symbol::{Symbol, SymbolId, SymbolKind, SymbolTable};
use crate::analysis::types::{BuiltinKind, TypeArena, TypeId, UntypedKind};
use crate::analysis::value::ConstValue;
use crate::token::Span;

/// Universe scope with pre-declared identifiers
pub struct Universe {
    pub scope_id: ScopeId,

    // Basic types
    pub bool_type: TypeId,
    pub int_type: TypeId,
    pub int8_type: TypeId,
    pub int16_type: TypeId,
    pub int32_type: TypeId,
    pub int64_type: TypeId,
    pub uint_type: TypeId,
    pub uint8_type: TypeId,
    pub uint16_type: TypeId,
    pub uint32_type: TypeId,
    pub uint64_type: TypeId,
    pub uintptr_type: TypeId,
    pub float32_type: TypeId,
    pub float64_type: TypeId,
    pub string_type: TypeId,

    // Untyped types
    pub untyped_bool: TypeId,
    pub untyped_int: TypeId,
    pub untyped_float: TypeId,
    pub untyped_string: TypeId,
    pub untyped_nil: TypeId,

    // Constant symbols
    // TODO: incorporate these
    #[allow(dead_code)]
    pub true_sym: SymbolId,
    #[allow(dead_code)]
    pub false_sym: SymbolId,
    #[allow(dead_code)]
    pub nil_sym: SymbolId,
}

impl Universe {
    /// Create universe scope
    pub fn new(types: &mut TypeArena, symbols: &mut SymbolTable, scopes: &mut ScopeTree) -> Self {
        let scope_id = scopes.new_scope(None, ScopeKind::Universe);
        let span = Span::default();

        // Well-known IDs
        let bool_type = TypeId::BOOL;
        let int_type = TypeId::INT;
        let int8_type = TypeId::INT8;
        let int16_type = TypeId::INT16;
        let int32_type = TypeId::INT32;
        let int64_type = TypeId::INT64;
        let uint_type = TypeId::UINT;
        let uint8_type = TypeId::UINT8;
        let uint16_type = TypeId::UINT16;
        let uint32_type = TypeId::UINT32;
        let uint64_type = TypeId::UINT64;
        let uintptr_type = TypeId::UINTPTR;
        let float32_type = TypeId::FLOAT32;
        let float64_type = TypeId::FLOAT64;
        let string_type = TypeId::STRING;

        // Register types
        let type_names = [
            ("bool", bool_type),
            ("int", int_type),
            ("int8", int8_type),
            ("int16", int16_type),
            ("int32", int32_type),
            ("int64", int64_type),
            ("uint", uint_type),
            ("uint8", uint8_type),
            ("uint16", uint16_type),
            ("uint32", uint32_type),
            ("uint64", uint64_type),
            ("uintptr", uintptr_type),
            ("float32", float32_type),
            ("float64", float64_type),
            ("string", string_type),
            ("byte", uint8_type), // alias
            ("rune", int32_type), // alias
        ];

        for (name, type_id) in type_names {
            let sym = symbols.add(Symbol::new(
                name.to_string(),
                span,
                SymbolKind::Type { type_id },
            ));
            scopes.define(scope_id, name.to_string(), sym);
        }

        // Untyped types
        let untyped_bool = types.untyped(UntypedKind::Bool);
        let untyped_int = types.untyped(UntypedKind::Int);
        let untyped_float = types.untyped(UntypedKind::Float);
        let untyped_string = types.untyped(UntypedKind::String);
        let untyped_nil = types.untyped(UntypedKind::Nil);

        // true
        let true_sym = symbols.add(Symbol::new(
            "true".to_string(),
            span,
            SymbolKind::Const {
                type_id: untyped_bool,
                value: ConstValue::Bool(true),
            },
        ));
        scopes.define(scope_id, "true".to_string(), true_sym);

        // false
        let false_sym = symbols.add(Symbol::new(
            "false".to_string(),
            span,
            SymbolKind::Const {
                type_id: untyped_bool,
                value: ConstValue::Bool(false),
            },
        ));
        scopes.define(scope_id, "false".to_string(), false_sym);

        // nil
        let nil_sym = symbols.add(Symbol::new(
            "nil".to_string(),
            span,
            SymbolKind::Const {
                type_id: untyped_nil,
                value: ConstValue::Nil,
            },
        ));
        scopes.define(scope_id, "nil".to_string(), nil_sym);

        // Builtins
        let builtins = [
            ("println", BuiltinKind::Println),
            ("print", BuiltinKind::Print),
            ("len", BuiltinKind::Len),
            ("cap", BuiltinKind::Cap),
            ("new", BuiltinKind::New),
            ("make", BuiltinKind::Make),
            ("append", BuiltinKind::Append),
            ("copy", BuiltinKind::Copy),
            ("delete", BuiltinKind::Delete),
            ("panic", BuiltinKind::Panic),
        ];

        for (name, kind) in builtins {
            let builtin_type = types.builtin(kind);
            let sym = symbols.add(Symbol::new(
                name.to_string(),
                span,
                SymbolKind::Builtin {
                    type_id: builtin_type,
                },
            ));
            scopes.define(scope_id, name.to_string(), sym);
        }

        Self {
            scope_id,
            bool_type,
            int_type,
            int8_type,
            int16_type,
            int32_type,
            int64_type,
            uint_type,
            uint8_type,
            uint16_type,
            uint32_type,
            uint64_type,
            uintptr_type,
            float32_type,
            float64_type,
            string_type,
            untyped_bool,
            untyped_int,
            untyped_float,
            untyped_string,
            untyped_nil,
            true_sym,
            false_sym,
            nil_sym,
        }
    }
}
