//! Constant values

use crate::analysis::types::BasicType;
use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::{ToPrimitive, Zero};
use std::fmt;

/// Constant value
#[derive(Debug, Clone)]
pub enum ConstValue {
    Int(BigInt),
    Float(BigRational),
    String(String),
    Bool(bool),
    Nil,
}

impl ConstValue {
    pub fn to_i64(&self) -> Option<i64> {
        match self {
            ConstValue::Int(i) => i.to_i64(),
            ConstValue::Float(f) if f.is_integer() => f.to_integer().to_i64(),
            _ => None,
        }
    }

    pub fn negate(&self) -> Option<ConstValue> {
        match self {
            ConstValue::Int(i) => Some(ConstValue::Int(-i)),
            ConstValue::Float(f) => Some(ConstValue::Float(-f)),
            _ => None,
        }
    }

    pub fn not(&self) -> Option<ConstValue> {
        match self {
            ConstValue::Bool(b) => Some(ConstValue::Bool(!b)),
            _ => None,
        }
    }

    pub fn fits_type(&self, basic: BasicType) -> bool {
        match (self, basic) {
            (ConstValue::Bool(_), BasicType::Bool) => true,
            (ConstValue::String(_), BasicType::String) => true,
            (ConstValue::Float(f), t) if t.is_integer() => {
                if !f.is_integer() {
                    return false;
                }
                fits_int(&f.to_integer(), t)
            }
            (ConstValue::Int(i), t) if t.is_integer() => fits_int(i, t),
            (ConstValue::Int(_), t) if t.is_float() => true,
            (ConstValue::Float(f), t) if t.is_float() => f
                .to_f64()
                .map(|v| match t {
                    BasicType::Float32 => !(v as f32).is_infinite() || v.is_infinite(),
                    BasicType::Float64 => true,
                    _ => false,
                })
                .unwrap_or(false),
            _ => false,
        }
    }

    pub fn binary_op(&self, op: &str, other: &ConstValue) -> Option<ConstValue> {
        match (self, other) {
            (ConstValue::Int(a), ConstValue::Int(b)) => binary_int(a, op, b),
            (ConstValue::Float(a), ConstValue::Float(b)) => binary_float(a, op, b),
            (ConstValue::Int(a), ConstValue::Float(b)) => {
                binary_float(&BigRational::from(a.clone()), op, b)
            }
            (ConstValue::Float(a), ConstValue::Int(b)) => {
                binary_float(a, op, &BigRational::from(b.clone()))
            }
            (ConstValue::String(a), ConstValue::String(b)) => binary_string(a, op, b),
            (ConstValue::Bool(a), ConstValue::Bool(b)) => binary_bool(*a, op, *b),
            _ => None,
        }
    }
}

impl fmt::Display for ConstValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConstValue::Int(i) => write!(f, "{}", i),
            ConstValue::Float(r) => r
                .to_f64()
                .map(|v| write!(f, "{}", v))
                .unwrap_or_else(|| write!(f, "{}/{}", r.numer(), r.denom())),
            ConstValue::String(s) => write!(f, "{:?}", s),
            ConstValue::Bool(b) => write!(f, "{}", b),
            ConstValue::Nil => write!(f, "nil"),
        }
    }
}

impl PartialEq for ConstValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ConstValue::Int(a), ConstValue::Int(b)) => a == b,
            (ConstValue::Float(a), ConstValue::Float(b)) => a == b,
            (ConstValue::String(a), ConstValue::String(b)) => a == b,
            (ConstValue::Bool(a), ConstValue::Bool(b)) => a == b,
            (ConstValue::Nil, ConstValue::Nil) => true,
            _ => false,
        }
    }
}

fn fits_int(i: &BigInt, basic: BasicType) -> bool {
    let signed_range: Option<(i128, i128)> = match basic {
        BasicType::Int8 => Some((i8::MIN as i128, i8::MAX as i128)),
        BasicType::Int16 => Some((i16::MIN as i128, i16::MAX as i128)),
        BasicType::Int32 => Some((i32::MIN as i128, i32::MAX as i128)),
        BasicType::Int | BasicType::Int64 => Some((i64::MIN as i128, i64::MAX as i128)),
        _ => None,
    };

    if let Some((min, max)) = signed_range {
        // Signed types: check if value fits in signed range
        return i.to_i128().map(|v| v >= min && v <= max).unwrap_or(false);
    }

    // Unsigned types
    let max: u128 = match basic {
        BasicType::Uint8 => u8::MAX as u128,
        BasicType::Uint16 => u16::MAX as u128,
        BasicType::Uint32 => u32::MAX as u128,
        BasicType::Uint | BasicType::Uint64 | BasicType::Uintptr => u64::MAX as u128,
        _ => return false,
    };

    // For unsigned types, value must be non-negative and fit in range
    i.to_u128().map(|v| v <= max).unwrap_or(false)
}

fn binary_int(a: &BigInt, op: &str, b: &BigInt) -> Option<ConstValue> {
    Some(match op {
        "+" => ConstValue::Int(a + b),
        "-" => ConstValue::Int(a - b),
        "*" => ConstValue::Int(a * b),
        "/" if !b.is_zero() => ConstValue::Int(a / b),
        "%" if !b.is_zero() => ConstValue::Int(a % b),
        "==" => ConstValue::Bool(a == b),
        "!=" => ConstValue::Bool(a != b),
        "<" => ConstValue::Bool(a < b),
        "<=" => ConstValue::Bool(a <= b),
        ">" => ConstValue::Bool(a > b),
        ">=" => ConstValue::Bool(a >= b),
        _ => return None,
    })
}

fn binary_float(a: &BigRational, op: &str, b: &BigRational) -> Option<ConstValue> {
    Some(match op {
        "+" => ConstValue::Float(a + b),
        "-" => ConstValue::Float(a - b),
        "*" => ConstValue::Float(a * b),
        "/" if !b.is_zero() => ConstValue::Float(a / b),
        "==" => ConstValue::Bool(a == b),
        "!=" => ConstValue::Bool(a != b),
        "<" => ConstValue::Bool(a < b),
        "<=" => ConstValue::Bool(a <= b),
        ">" => ConstValue::Bool(a > b),
        ">=" => ConstValue::Bool(a >= b),
        _ => return None,
    })
}

fn binary_string(a: &str, op: &str, b: &str) -> Option<ConstValue> {
    Some(match op {
        "+" => ConstValue::String(format!("{}{}", a, b)),
        "==" => ConstValue::Bool(a == b),
        "!=" => ConstValue::Bool(a != b),
        "<" => ConstValue::Bool(a < b),
        "<=" => ConstValue::Bool(a <= b),
        ">" => ConstValue::Bool(a > b),
        ">=" => ConstValue::Bool(a >= b),
        _ => return None,
    })
}

fn binary_bool(a: bool, op: &str, b: bool) -> Option<ConstValue> {
    Some(match op {
        "&&" => ConstValue::Bool(a && b),
        "||" => ConstValue::Bool(a || b),
        "==" => ConstValue::Bool(a == b),
        "!=" => ConstValue::Bool(a != b),
        _ => return None,
    })
}
