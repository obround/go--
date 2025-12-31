//! Type operations

use crate::analysis::types::{BasicType, TypeArena, TypeId, UntypedKind};
use crate::analysis::value::ConstValue;
use crate::ast::BinaryOp;

/// Check assignability
pub fn is_assignable_to(
    types: &TypeArena,
    value_type: TypeId,
    target_type: TypeId,
    const_val: Option<&ConstValue>,
) -> bool {
    if types.is_invalid(value_type) || types.is_invalid(target_type) {
        return true;
    }

    // Identity
    if types.identical(value_type, target_type) {
        return true;
    }

    // Untyped
    if let Some(kind) = types.is_untyped(value_type) {
        return is_untyped_assignable(types, kind, target_type, const_val);
    }

    // Nil assignable
    if types.is_untyped_nil(value_type) {
        return types.is_pointer(target_type).is_some()
            || types.is_slice(target_type).is_some()
            || types.is_map(target_type).is_some()
            || types.is_signature(target_type).is_some();
    }

    let val_underlying = types.underlying(value_type);
    let tgt_underlying = types.underlying(target_type);

    if types.identical(val_underlying, tgt_underlying) {
        // Unnamed check
        let val_is_named = types.is_named(value_type);
        let tgt_is_named = types.is_named(target_type);

        if !val_is_named || !tgt_is_named {
            return !val_is_named;
        }
    }

    false
}

fn is_untyped_assignable(
    types: &TypeArena,
    kind: UntypedKind,
    target: TypeId,
    const_val: Option<&ConstValue>,
) -> bool {
    let underlying = types.underlying(target);

    match kind {
        UntypedKind::Bool => types.is_boolean(underlying),
        UntypedKind::Int => {
            if let Some(basic) = types.is_basic(underlying)
                && basic.is_numeric()
            {
                return const_val.map(|cv| cv.fits_type(basic)).unwrap_or(true);
            }
            false
        }
        UntypedKind::Float => {
            if let Some(basic) = types.is_basic(underlying)
                && basic.is_float()
            {
                return const_val.map(|cv| cv.fits_type(basic)).unwrap_or(true);
            }
            false
        }
        UntypedKind::String => types.is_string(underlying),
        UntypedKind::Nil => {
            types.is_pointer(underlying).is_some()
                || types.is_slice(underlying).is_some()
                || types.is_map(underlying).is_some()
                || types.is_signature(underlying).is_some()
        }
    }
}

/// Check convertibility
pub fn can_convert(types: &TypeArena, from: TypeId, to: TypeId) -> bool {
    if types.is_invalid(from) || types.is_invalid(to) {
        return true;
    }

    if is_assignable_to(types, from, to, None) {
        return true;
    }

    let from_u = types.underlying(from);
    let to_u = types.underlying(to);

    // Identical underlying
    if types.identical(from_u, to_u) {
        return true;
    }

    // Numeric -> Numeric
    if types.is_numeric(from_u) && types.is_numeric(to_u) {
        return true;
    }

    // integer -> string
    if types.is_integer(from_u) && types.is_string(to_u) {
        return true;
    }

    // string -> []byte
    if types.is_string(from_u)
        && let Some(elem) = types.is_slice(to_u)
        && types.is_basic(elem) == Some(BasicType::Uint8)
    {
        return true;
    }

    // []byte -> string
    if let Some(elem) = types.is_slice(from_u)
        && types.is_basic(elem) == Some(BasicType::Uint8)
        && types.is_string(to_u)
    {
        return true;
    }

    // Pointer (same underlying base)
    if let (Some(from_base), Some(to_base)) = (types.is_pointer(from_u), types.is_pointer(to_u))
        && types.identical(types.underlying(from_base), types.underlying(to_base))
    {
        return true;
    }

    false
}

/// Default type for untyped
pub fn default_type(types: &TypeArena, type_id: TypeId) -> TypeId {
    match types.is_untyped(type_id) {
        Some(UntypedKind::Bool) => TypeId::BOOL,
        Some(UntypedKind::Int) => TypeId::INT,
        Some(UntypedKind::Float) => TypeId::FLOAT64,
        Some(UntypedKind::String) => TypeId::STRING,
        Some(UntypedKind::Nil) => type_id, // nil stays nil
        None => type_id,
    }
}

/// Check binary op validity
pub fn check_binary_op(
    types: &TypeArena,
    op: BinaryOp,
    lhs: TypeId,
    rhs: TypeId,
    bool_type: TypeId,
) -> Option<TypeId> {
    if types.is_invalid(lhs) || types.is_invalid(rhs) {
        return Some(types.invalid());
    }

    let lhs_u = types.underlying(lhs);
    let rhs_u = types.underlying(rhs);

    match op {
        // Logical
        BinaryOp::And | BinaryOp::Or => {
            if types.is_boolean(lhs_u) && types.is_boolean(rhs_u) {
                Some(bool_type)
            } else {
                None
            }
        }

        // Equality
        BinaryOp::Eq | BinaryOp::Ne => check_equality_op(types, lhs, rhs, bool_type),

        // Comparison
        BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
            if types.is_ordered(lhs) && types.is_ordered(rhs) && compatible_types(types, lhs, rhs) {
                Some(bool_type)
            } else {
                None
            }
        }

        // Arithmetic
        BinaryOp::Add => {
            let is_numeric = types.is_numeric(lhs) && types.is_numeric(rhs);
            let is_string = (types.is_string(lhs_u)
                || types.is_untyped(lhs) == Some(UntypedKind::String))
                && (types.is_string(rhs_u) || types.is_untyped(rhs) == Some(UntypedKind::String));

            if (is_numeric || is_string) && compatible_types(types, lhs, rhs) {
                Some(result_type(types, lhs, rhs))
            } else {
                None
            }
        }

        BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
            if types.is_numeric(lhs) && types.is_numeric(rhs) && compatible_types(types, lhs, rhs) {
                Some(result_type(types, lhs, rhs))
            } else {
                None
            }
        }
    }
}

fn check_equality_op(
    types: &TypeArena,
    lhs: TypeId,
    rhs: TypeId,
    bool_type: TypeId,
) -> Option<TypeId> {
    // nil op pointer/slice/map/func
    let lhs_nil = types.is_untyped_nil(lhs);
    let rhs_nil = types.is_untyped_nil(rhs);

    if lhs_nil || rhs_nil {
        let other = if lhs_nil { rhs } else { lhs };
        let other_u = types.underlying(other);
        if types.is_pointer(other_u).is_some()
            || types.is_slice(other_u).is_some()
            || types.is_map(other_u).is_some()
            || types.is_signature(other_u).is_some()
            || types.is_untyped_nil(other)
        {
            return Some(bool_type);
        }
    }

    if types.is_comparable(lhs) && types.is_comparable(rhs) && compatible_types(types, lhs, rhs) {
        Some(bool_type)
    } else {
        None
    }
}

fn compatible_types(types: &TypeArena, a: TypeId, b: TypeId) -> bool {
    // Same types
    if types.identical(a, b) {
        return true;
    }

    // Same underlying
    if types.identical(types.underlying(a), types.underlying(b)) {
        return true;
    }

    // Untyped -> typed/untyped check
    let a_untyped = types.is_untyped(a);
    let b_untyped = types.is_untyped(b);

    match (a_untyped, b_untyped) {
        (Some(ak), Some(bk)) => {
            // Both untyped
            match (ak, bk) {
                // Same kind is compatible
                (UntypedKind::Bool, UntypedKind::Bool) => true,
                (UntypedKind::String, UntypedKind::String) => true,
                (UntypedKind::Nil, UntypedKind::Nil) => true,
                // Numeric kinds are compatible with each other
                (UntypedKind::Int, UntypedKind::Int) => true,
                (UntypedKind::Float, UntypedKind::Float) => true,
                (UntypedKind::Int, UntypedKind::Float) => true,
                (UntypedKind::Float, UntypedKind::Int) => true,
                // Different kinds are not compatible
                _ => false,
            }
        }
        (Some(kind), None) => untyped_compatible_with(types, kind, b),
        (None, Some(kind)) => untyped_compatible_with(types, kind, a),
        (None, None) => false,
    }
}

fn untyped_compatible_with(types: &TypeArena, kind: UntypedKind, typed: TypeId) -> bool {
    let underlying = types.underlying(typed);
    match kind {
        UntypedKind::Bool => types.is_boolean(underlying),
        UntypedKind::Int => types.is_numeric(underlying),
        UntypedKind::Float => types.is_numeric(underlying),
        UntypedKind::String => types.is_string(underlying),
        UntypedKind::Nil => {
            types.is_pointer(underlying).is_some()
                || types.is_slice(underlying).is_some()
                || types.is_map(underlying).is_some()
                || types.is_signature(underlying).is_some()
        }
    }
}

fn result_type(types: &TypeArena, a: TypeId, b: TypeId) -> TypeId {
    // If one is typed, use that
    if types.is_untyped(a).is_some() && types.is_untyped(b).is_none() {
        return b;
    }
    if types.is_untyped(b).is_some() && types.is_untyped(a).is_none() {
        return a;
    }
    // Both untyped or both typed
    a
}
