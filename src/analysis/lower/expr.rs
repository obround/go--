use crate::analysis::Analyzer;
use crate::analysis::error::ErrorCode;
use crate::analysis::symbol::SymbolKind;
use crate::analysis::typeops;
use crate::analysis::types::{BasicType, BuiltinKind, Type, TypeId, UntypedKind};
use crate::analysis::value::ConstValue;
use crate::ast::{BinaryOp, Expr, UnaryOp};
use crate::tir::{
    ConstValue as TConstValue, TBinaryOp, TCallKind, TCapTarget, TConversionKind, TEqualityInfo,
    TExpr, TIndexKind, TLenTarget, TMakeInfo, TMakeKind, TPrimitiveEq, TPrintKind, TSliceKind,
    TUnaryOp,
};
use crate::token::Span;
use num_bigint::BigInt;
use num_rational::BigRational;

use num_traits::{Num, Signed, ToPrimitive, Zero};

impl<'a> Analyzer<'a> {
    pub fn lower_expr(&mut self, expr: &Expr, type_hint: Option<TypeId>) -> TExpr {
        match expr {
            Expr::Ident(span, name) => self.lower_ident(*span, name),
            Expr::IntLit(_, s) => self.lower_int_lit(s, type_hint),
            Expr::FloatLit(_, s) => self.lower_float_lit(s, type_hint),
            Expr::StringLit(_, s) => self.lower_string_lit(s, type_hint),
            Expr::Binary(span, lhs, op, rhs) => self.lower_binary(*span, lhs, *op, rhs),
            Expr::Unary(span, op, operand) => self.lower_unary(*span, *op, operand, type_hint),
            Expr::Call(span, callee, args) => self.lower_call(*span, callee, args),
            Expr::Selector(span, base, field) => self.lower_selector(*span, base, field),
            Expr::Index(span, base, index) => self.lower_index(*span, base, index),
            Expr::Slice(span, base, low, high) => {
                self.lower_slice(*span, base, low.as_deref(), high.as_deref())
            }
            Expr::CompositeLit(span, lit_type, value) => {
                self.lower_composite_lit(*span, lit_type, value)
            }
            Expr::Paren(_, inner) => self.lower_expr(inner, type_hint),
            Expr::TypeVal(span, _) => {
                self.error(ErrorCode::NotAnExpr, *span, "type is not an expression");
                TExpr::Nil {
                    type_id: TypeId::INVALID,
                }
            }
        }
    }

    pub fn lower_lhs_expr(&mut self, expr: &Expr) -> TExpr {
        match expr {
            Expr::Ident(span, name) => self.lower_lhs_ident(*span, name),
            // For selectors, we DO read the base to access the field
            Expr::Selector(span, base, field) => self.lower_selector(*span, base, field),
            // For indices, we DO read the base and index
            Expr::Index(span, base, index) => self.lower_index(*span, base, index),
            Expr::Paren(_, inner) => self.lower_lhs_expr(inner),
            Expr::Unary(span, op, operand) => self.lower_unary(*span, *op, operand, None),
            _ => self.lower_expr(expr, None),
        }
    }

    fn lower_lhs_ident(&mut self, span: Span, name: &str) -> TExpr {
        // Note: _ IS allowed on LHS of assignments
        if name == "_" {
            return TExpr::Nil {
                type_id: TypeId::INVALID,
            };
        }

        match self.lookup(name) {
            Some((sym_id, _)) => {
                // Do not mark used
                let kind = self.symbols.get(sym_id).kind.clone();
                match kind {
                    SymbolKind::Var { type_id } => TExpr::Var {
                        symbol_id: sym_id,
                        type_id,
                    },
                    SymbolKind::Const { type_id, .. } => {
                        // Constants are not assignable but still return valid type
                        TExpr::Var {
                            symbol_id: sym_id,
                            type_id,
                        }
                    }
                    _ => TExpr::Nil {
                        type_id: TypeId::INVALID,
                    },
                }
            }
            None => {
                self.error(
                    ErrorCode::UndeclaredName,
                    span,
                    format!("undefined: {}", name),
                );
                TExpr::Nil {
                    type_id: TypeId::INVALID,
                }
            }
        }
    }

    fn lower_ident(&mut self, span: Span, name: &str) -> TExpr {
        if name == "_" {
            self.error(
                ErrorCode::InvalidBlank,
                span,
                "cannot use _ as value or type",
            );
            return TExpr::Nil {
                type_id: TypeId::INVALID,
            };
        }

        match self.lookup(name) {
            Some((sym_id, _)) => {
                self.symbols.mark_used(sym_id);
                // Copy out the data we need to avoid borrowing conflicts
                let kind = self.symbols.get(sym_id).kind.clone();
                match kind {
                    SymbolKind::Var { type_id } | SymbolKind::Param { type_id } => TExpr::Var {
                        symbol_id: sym_id,
                        type_id,
                    },
                    SymbolKind::Const { type_id, value } => TExpr::Const {
                        value: Box::new(self.convert_const_value(&value)),
                        type_id,
                    },
                    SymbolKind::Func { signature } => TExpr::Var {
                        symbol_id: sym_id,
                        type_id: signature,
                    },
                    SymbolKind::Type { .. } => {
                        self.error(
                            ErrorCode::NotAnExpr,
                            span,
                            format!("{} (type) is not an expression", name),
                        );
                        TExpr::Nil {
                            type_id: TypeId::INVALID,
                        }
                    }
                    SymbolKind::Builtin { type_id } => {
                        self.error(
                            ErrorCode::UncalledBuiltin,
                            span,
                            format!("{} (built-in) must be called", name),
                        );
                        TExpr::Nil { type_id }
                    }
                }
            }
            None => {
                self.error(
                    ErrorCode::UndeclaredName,
                    span,
                    format!("undefined: {}", name),
                );
                TExpr::Nil {
                    type_id: TypeId::INVALID,
                }
            }
        }
    }

    fn lower_int_lit(&mut self, s: &str, hint: Option<TypeId>) -> TExpr {
        let s = s.replace("_", "");
        let big = if s.starts_with("0x") || s.starts_with("0X") {
            BigInt::from_str_radix(&s[2..], 16).ok()
        } else if s.starts_with("0o") || s.starts_with("0O") {
            BigInt::from_str_radix(&s[2..], 8).ok()
        } else if s.starts_with("0b") || s.starts_with("0B") {
            BigInt::from_str_radix(&s[2..], 2).ok()
        } else if s.starts_with('0') && s.len() > 1 && s.chars().skip(1).all(|c| c.is_ascii_digit())
        {
            BigInt::from_str_radix(&s[1..], 8).ok()
        } else {
            s.parse::<BigInt>().ok()
        };

        // Check hint
        let type_id = match hint {
            Some(h) if self.types.is_numeric(h) => h,
            _ => self.universe.untyped_int,
        };

        match big {
            Some(i) => {
                if i.is_negative() || self.types.is_signed_int(type_id) {
                    TExpr::Int {
                        value: i.to_i64().unwrap_or(0),
                        type_id,
                    }
                } else {
                    TExpr::Uint {
                        value: i.to_u64().unwrap_or(0),
                        type_id,
                    }
                }
            }
            None => TExpr::Int { value: 0, type_id },
        }
    }

    fn lower_float_lit(&mut self, s: &str, hint: Option<TypeId>) -> TExpr {
        let s = s.replace("_", "");
        let val = s.parse::<f64>().unwrap_or(0.0);
        // Check hint
        let type_id = match hint {
            Some(h) if self.types.is_float(h) => h,
            _ => self.universe.untyped_float,
        };
        TExpr::Float {
            value: val,
            type_id,
        }
    }

    fn lower_string_lit(&mut self, s: &str, hint: Option<TypeId>) -> TExpr {
        let content = if s.starts_with('`') {
            s.trim_matches('`').to_string()
        } else {
            unescape_string(s.trim_matches('"'))
        };
        // Check hint
        let type_id = match hint {
            Some(h) if self.types.is_string(h) => h,
            _ => self.universe.untyped_string,
        };
        TExpr::String {
            value: content,
            type_id,
        }
    }

    fn convert_const_value(&self, cv: &ConstValue) -> TConstValue {
        match cv {
            ConstValue::Int(i) => {
                if i.is_negative() {
                    TConstValue::Int(i.to_i128().unwrap_or(0))
                } else {
                    TConstValue::Uint(i.to_u128().unwrap_or(0))
                }
            }
            ConstValue::Float(f) => TConstValue::Float(f.to_f64().unwrap_or(0.0)),
            ConstValue::String(s) => TConstValue::String(s.clone()),
            ConstValue::Bool(b) => TConstValue::Bool(*b),
            ConstValue::Nil => TConstValue::Nil,
        }
    }

    fn lower_binary(&mut self, span: Span, lhs: &Expr, op: BinaryOp, rhs: &Expr) -> TExpr {
        let left = self.lower_expr(lhs, None);
        let right = self.lower_expr(rhs, None);
        let lhs_type = left.type_id();
        let rhs_type = right.type_id();

        // Division by zero
        if matches!(op, BinaryOp::Div | BinaryOp::Mod) {
            // Use eval_const_from_ast to evaluate expressions like 2 - 2 = 0
            let rhs_is_zero = self.eval_const_from_ast(rhs).is_some_and(|cv| {
                matches!(&cv, ConstValue::Int(i) if i.is_zero())
                    || matches!(&cv, ConstValue::Float(f) if f.is_zero())
            });
            if rhs_is_zero {
                self.error(
                    ErrorCode::DivByZero,
                    span,
                    "invalid operation: division by zero",
                );
            }
        }

        // Determine result type
        let bool_type = self.universe.bool_type;
        match typeops::check_binary_op(&self.types, op, lhs_type, rhs_type, bool_type) {
            Some(result_type) => {
                let tir_op = self.resolve_binary_op(op, lhs_type, rhs_type);
                TExpr::Binary {
                    op: tir_op,
                    left: Box::new(left),
                    right: Box::new(right),
                    type_id: result_type,
                }
            }
            None => {
                let is_untyped_nil = |t| self.types.is_untyped(t) == Some(UntypedKind::Nil);
                let is_incomparable = |t| {
                    matches!(
                        self.types.get(t),
                        Type::Slice { .. } | Type::Map { .. } | Type::Signature { .. }
                    )
                };

                if matches!(op, BinaryOp::Eq | BinaryOp::Ne)
                    && (is_incomparable(lhs_type) || is_incomparable(rhs_type))
                    && !is_untyped_nil(lhs_type)
                    && !is_untyped_nil(rhs_type)
                {
                    let t_idx = if is_incomparable(lhs_type) {
                        lhs_type
                    } else {
                        rhs_type
                    };
                    let detail = match self.types.get(t_idx) {
                        Type::Slice { .. } => "slice can only be compared to nil",
                        Type::Map { .. } => "map can only be compared to nil",
                        Type::Signature { .. } => "func can only be compared to nil",
                        _ => "invalid operation",
                    };
                    let lhs_str = self.expr_string(lhs);
                    let rhs_str = self.expr_string(rhs);
                    self.error(
                        ErrorCode::UndefinedOp,
                        span,
                        format!(
                            "invalid operation: {} {} {} ({})",
                            lhs_str,
                            op.name(),
                            rhs_str,
                            detail
                        ),
                    );
                    return TExpr::Nil {
                        type_id: TypeId::INVALID,
                    };
                }

                if !self.types.is_invalid(lhs_type) && !self.types.is_invalid(rhs_type) {
                    let lhs_str = self.expr_string(lhs);
                    let rhs_str = self.expr_string(rhs);

                    // Check if types are mismatched (both untyped but different, or both typed but different)
                    let lhs_untyped = self.types.is_untyped(lhs_type);
                    let rhs_untyped = self.types.is_untyped(rhs_type);
                    let types_differ = !self.types.identical(
                        self.types.underlying(lhs_type),
                        self.types.underlying(rhs_type),
                    );
                    let is_mismatched = types_differ
                        && ((lhs_untyped.is_some() && rhs_untyped.is_some())
                            || (lhs_untyped.is_none() && rhs_untyped.is_none()));

                    if is_mismatched {
                        self.error(
                            ErrorCode::MismatchedTypes,
                            span,
                            format!(
                                "invalid operation: {} {} {} (mismatched types {} and {})",
                                lhs_str,
                                op.name(),
                                rhs_str,
                                self.types.type_name(lhs_type),
                                self.types.type_name(rhs_type)
                            ),
                        );
                    } else {
                        // Operator not defined for the type
                        let type_desc = self.type_description(lhs_type);
                        self.error(
                            ErrorCode::UndefinedOp,
                            span,
                            format!(
                                "invalid operation: operator {} not defined on {} ({})",
                                op.name(),
                                lhs_str,
                                type_desc,
                            ),
                        );
                    }
                }
                TExpr::Nil {
                    type_id: TypeId::INVALID,
                }
            }
        }
    }

    fn resolve_binary_op(&mut self, op: BinaryOp, lhs_type: TypeId, rhs_type: TypeId) -> TBinaryOp {
        let lhs_u = self.types.underlying(lhs_type);
        let signed = self.types.is_signed_int(lhs_u);
        let is_float = self.types.is_float(lhs_u)
            || self.types.is_untyped(lhs_type) == Some(UntypedKind::Float);
        let is_string = self.types.is_string(lhs_u)
            || self.types.is_untyped(lhs_type) == Some(UntypedKind::String);

        match op {
            BinaryOp::Add if is_string => TBinaryOp::StringConcat,
            BinaryOp::Add if is_float => TBinaryOp::FloatAdd,
            BinaryOp::Add => TBinaryOp::IntAdd,
            BinaryOp::Sub if is_float => TBinaryOp::FloatSub,
            BinaryOp::Sub => TBinaryOp::IntSub,
            BinaryOp::Mul if is_float => TBinaryOp::FloatMul,
            BinaryOp::Mul => TBinaryOp::IntMul,
            BinaryOp::Div if is_float => TBinaryOp::FloatDiv,
            BinaryOp::Div => TBinaryOp::IntDiv { signed },
            BinaryOp::Mod => TBinaryOp::IntMod { signed },
            BinaryOp::Lt if is_float => TBinaryOp::FloatLt,
            BinaryOp::Lt if is_string => TBinaryOp::StringLt,
            BinaryOp::Lt => TBinaryOp::IntLt { signed },
            BinaryOp::Le if is_float => TBinaryOp::FloatLe,
            BinaryOp::Le if is_string => TBinaryOp::StringLe,
            BinaryOp::Le => TBinaryOp::IntLe { signed },
            BinaryOp::Gt if is_float => TBinaryOp::FloatGt,
            BinaryOp::Gt if is_string => TBinaryOp::StringGt,
            BinaryOp::Gt => TBinaryOp::IntGt { signed },
            BinaryOp::Ge if is_float => TBinaryOp::FloatGe,
            BinaryOp::Ge if is_string => TBinaryOp::StringGe,
            BinaryOp::Ge => TBinaryOp::IntGe { signed },
            BinaryOp::Eq | BinaryOp::Ne => self.resolve_equality_op(op, lhs_type, rhs_type),
            BinaryOp::And => TBinaryOp::And,
            BinaryOp::Or => TBinaryOp::Or,
        }
    }

    fn resolve_equality_op(
        &mut self,
        op: BinaryOp,
        lhs_type: TypeId,
        rhs_type: TypeId,
    ) -> TBinaryOp {
        let is_eq = op == BinaryOp::Eq;
        let lhs_nil = self.types.is_untyped_nil(lhs_type);
        let rhs_nil = self.types.is_untyped_nil(rhs_type);
        let lhs_u = self.types.underlying(lhs_type);
        let rhs_u = self.types.underlying(rhs_type);

        // Nil comparisons
        if lhs_nil || rhs_nil {
            if self.types.is_slice(lhs_u).is_some() || self.types.is_slice(rhs_u).is_some() {
                return if is_eq {
                    TBinaryOp::SliceNilEq {
                        slice_on_left: !lhs_nil,
                    }
                } else {
                    TBinaryOp::SliceNilNe {
                        slice_on_left: !lhs_nil,
                    }
                };
            }
            if self.types.is_map(lhs_u).is_some() || self.types.is_map(rhs_u).is_some() {
                return if is_eq {
                    TBinaryOp::MapNilEq {
                        map_on_left: !lhs_nil,
                    }
                } else {
                    TBinaryOp::MapNilNe {
                        map_on_left: !lhs_nil,
                    }
                };
            }
            // Pointer nil
            if self.types.is_pointer(lhs_u).is_some() || self.types.is_pointer(rhs_u).is_some() {
                return if is_eq {
                    TBinaryOp::PointerNilEq {
                        pointer_on_left: !lhs_nil,
                    }
                } else {
                    TBinaryOp::PointerNilNe {
                        pointer_on_left: !lhs_nil,
                    }
                };
            }
            // Function nil
            if self.types.is_signature(lhs_u).is_some() || self.types.is_signature(rhs_u).is_some()
            {
                return if is_eq {
                    TBinaryOp::FuncNilEq {
                        func_on_left: !lhs_nil,
                    }
                } else {
                    TBinaryOp::FuncNilNe {
                        func_on_left: !lhs_nil,
                    }
                };
            }
            // Pointer equality (fallback)
            return if is_eq {
                TBinaryOp::PointerEq
            } else {
                TBinaryOp::PointerNe
            };
        }

        // Type specific
        if self.types.is_boolean(lhs_u) {
            return if is_eq {
                TBinaryOp::BoolEq
            } else {
                TBinaryOp::BoolNe
            };
        }
        if self.types.is_float(lhs_u) {
            return if is_eq {
                TBinaryOp::FloatEq
            } else {
                TBinaryOp::FloatNe
            };
        }
        if self.types.is_string(lhs_u) {
            return if is_eq {
                TBinaryOp::StringEq
            } else {
                TBinaryOp::StringNe
            };
        }
        if self.types.is_pointer(lhs_u).is_some() {
            return if is_eq {
                TBinaryOp::PointerEq
            } else {
                TBinaryOp::PointerNe
            };
        }
        if self.types.is_struct(lhs_u) {
            let eq = self.compute_equality_info(lhs_type);
            return if is_eq {
                TBinaryOp::StructEq { equality: eq }
            } else {
                TBinaryOp::StructNe { equality: eq }
            };
        }
        if self.types.is_array(lhs_u).is_some() {
            let eq = self.compute_equality_info(lhs_type);
            return if is_eq {
                TBinaryOp::ArrayEq { equality: eq }
            } else {
                TBinaryOp::ArrayNe { equality: eq }
            };
        }
        if is_eq {
            TBinaryOp::IntEq
        } else {
            TBinaryOp::IntNe
        }
    }

    fn compute_equality_info(&mut self, type_id: TypeId) -> TEqualityInfo {
        let underlying = self.types.underlying(type_id);
        if self.types.is_boolean(underlying) {
            return TEqualityInfo::Primitive(TPrimitiveEq::Bool);
        }
        if self.types.is_integer(underlying) {
            return TEqualityInfo::Primitive(TPrimitiveEq::Int);
        }
        if self.types.is_float(underlying) {
            return TEqualityInfo::Primitive(TPrimitiveEq::Float);
        }
        if self.types.is_string(underlying) {
            return TEqualityInfo::Primitive(TPrimitiveEq::String);
        }
        if self.types.is_pointer(underlying).is_some() {
            return TEqualityInfo::Primitive(TPrimitiveEq::Pointer);
        }
        if let Some(fields) = self.types.struct_fields(underlying) {
            let field_types: Vec<_> = fields
                .iter()
                .enumerate()
                .map(|(i, f)| (i, f.type_id))
                .collect();
            let fields: Vec<_> = field_types
                .into_iter()
                .map(|(i, tid)| (i as u32, self.compute_equality_info(tid)))
                .collect();
            return TEqualityInfo::Struct { fields };
        }
        if let Some((len, elem)) = self.types.is_array(underlying) {
            return TEqualityInfo::Array {
                len,
                elem: Box::new(self.compute_equality_info(elem)),
            };
        }
        TEqualityInfo::Primitive(TPrimitiveEq::Int)
    }

    fn lower_unary(
        &mut self,
        span: Span,
        op: UnaryOp,
        operand: &Expr,
        type_hint: Option<TypeId>,
    ) -> TExpr {
        match op {
            UnaryOp::Addr => {
                let inner = self.lower_expr(operand, None);
                let inner_type = inner.type_id();

                // Check addressability
                if !self.types.is_invalid(inner_type) && !self.is_expr_addressable(operand) {
                    let expr_str = self.expr_string(operand);
                    // Check for specific unaddressable cases
                    let type_desc = if self.is_map_index_expr(operand) {
                        format!(
                            "map index expression of type {}",
                            self.types.type_name(inner_type)
                        )
                    } else if self.is_string_index_expr(operand) {
                        "value of type byte".to_string()
                    } else {
                        // Constant value
                        if let Expr::Binary(_, _, _, _) | Expr::Paren(_, _) = operand {
                            if let Some(uk) = self.types.is_untyped(inner_type) {
                                let (_, const_val) = self.eval_const_with_type(operand, false);
                                let base_desc = match uk {
                                    UntypedKind::Bool => "untyped bool constant",
                                    UntypedKind::Int => "untyped int constant",
                                    UntypedKind::Float => "untyped float constant",
                                    UntypedKind::String => "untyped string constant",
                                    UntypedKind::Nil => "untyped nil",
                                };
                                // Include the evaluated value if we have one
                                match const_val {
                                    ConstValue::Int(ref n) => {
                                        format!("{} {}", base_desc, n)
                                    }
                                    _ => base_desc.to_string(),
                                }
                            } else {
                                self.expr_type_description(operand, inner_type)
                            }
                        } else {
                            self.expr_type_description(operand, inner_type)
                        }
                    };
                    self.error(
                        ErrorCode::UnaddressableOperand,
                        span,
                        format!(
                            "invalid operation: cannot take address of {} ({})",
                            expr_str, type_desc
                        ),
                    );
                }

                let type_id = self.types.pointer(inner_type);
                TExpr::AddrOf {
                    operand: Box::new(inner),
                    type_id,
                }
            }
            UnaryOp::Deref => {
                let inner = self.lower_expr(operand, None);
                match self.types.is_pointer(inner.type_id()) {
                    Some(base) => TExpr::Deref {
                        operand: Box::new(inner),
                        type_id: base,
                    },
                    None => {
                        if !self.types.is_invalid(inner.type_id()) {
                            // Check nil
                            if self.types.is_untyped_nil(inner.type_id())
                                && matches!(operand, Expr::Ident(_, name) if name == "nil")
                            {
                                self.error(
                                    ErrorCode::InvalidIndirection,
                                    span,
                                    "invalid operation: cannot indirect nil",
                                );
                            } else {
                                self.error(
                                    ErrorCode::InvalidIndirection,
                                    span,
                                    format!(
                                        "invalid operation: cannot indirect {} ({})",
                                        self.expr_string(operand),
                                        self.type_description(inner.type_id())
                                    ),
                                );
                            }
                        }
                        TExpr::Nil {
                            type_id: TypeId::INVALID,
                        }
                    }
                }
            }
            UnaryOp::Neg => {
                // Pass type hint
                let inner = self.lower_expr(operand, type_hint);
                let type_id = inner.type_id();

                // Constant folding
                match &inner {
                    TExpr::Int { value, type_id } => {
                        return TExpr::Int {
                            value: value.wrapping_neg(),
                            type_id: *type_id,
                        };
                    }
                    TExpr::Uint { value, type_id } => {
                        // -uint becomes Int
                        return TExpr::Int {
                            value: -(*value as i64),
                            type_id: *type_id,
                        };
                    }
                    TExpr::Float { value, type_id } => {
                        return TExpr::Float {
                            value: -value,
                            type_id: *type_id,
                        };
                    }
                    _ => {}
                }

                let is_float = self.types.is_float(self.types.underlying(type_id));
                let tir_op = if is_float {
                    TUnaryOp::FloatNeg
                } else {
                    TUnaryOp::IntNeg
                };
                TExpr::Unary {
                    op: tir_op,
                    operand: Box::new(inner),
                    type_id,
                }
            }
            UnaryOp::Not => {
                let inner = self.lower_expr(operand, None);
                let type_id = inner.type_id();
                TExpr::Unary {
                    op: TUnaryOp::BoolNot,
                    operand: Box::new(inner),
                    type_id,
                }
            }
            UnaryOp::Pos => self.lower_expr(operand, type_hint),
        }
    }

    pub fn lower_call(&mut self, span: Span, callee: &Expr, args: &[Expr]) -> TExpr {
        // Check for type conversion
        if let Some(type_id) = self.callee_as_type(callee) {
            return self.lower_conversion(span, type_id, args);
        }
        // Check for builtin
        if let Expr::Ident(_, name) = callee
            && let Some((sym_id, _)) = self.lookup(name)
            && let SymbolKind::Builtin { type_id } = self.symbols.get(sym_id).kind
            && let Some(b) = self.types.is_builtin(type_id)
        {
            return self.lower_builtin(span, b, args);
        }

        // Method call check
        let method_resolve = if let Expr::Selector(_, base, field) = callee {
            let base_expr = self.lower_expr(base, None);
            self.resolve_method_call(base_expr, field)
        } else {
            None
        };

        let (callee_kind, params, result_type) = if let Some((
            mangled_name,
            sig,
            is_ptr,
            receiver,
        )) = method_resolve
        {
            let (params, result) = self
                .types
                .is_signature(sig)
                .map(|(p, r)| (p.to_vec(), r.unwrap_or(TypeId::INVALID)))
                .unwrap_or_else(|| (vec![], TypeId::INVALID));
            (
                TCallKind::Method {
                    receiver: Box::new(receiver),
                    mangled_name,
                    receiver_is_pointer: is_ptr,
                },
                params,
                result,
            )
        } else {
            // Regular call
            let callee_expr = self.lower_expr(callee, None);
            let callee_type = callee_expr.type_id();
            let (params, result) = match self.types.is_signature(callee_type) {
                Some((p, r)) => (p.to_vec(), r.unwrap_or(TypeId::INVALID)),
                None => {
                    if !self.types.is_invalid(callee_type) {
                        let type_name = self.types.type_name(callee_type);
                        self.error(
                                ErrorCode::InvalidCall,
                                span,
                                format!(
                                "invalid operation: cannot call {} (variable of type {}): {} is not a function",
                                self.expr_string(callee),
                                type_name,
                                type_name
                            ),
                            );
                    }
                    return TExpr::Nil {
                        type_id: TypeId::INVALID,
                    };
                }
            };

            let kind = if let TExpr::Var { symbol_id, .. } = &callee_expr {
                TCallKind::Function {
                    symbol_id: *symbol_id,
                }
            } else {
                TCallKind::Indirect {
                    callee: Box::new(callee_expr),
                }
            };
            (kind, params, result)
        };

        // Check argument count
        if args.len() != params.len() {
            self.error(
                ErrorCode::WrongArgCount,
                span,
                format!(
                    "{} arguments in call to {}",
                    if args.len() < params.len() {
                        "not enough"
                    } else {
                        "too many"
                    },
                    self.expr_string(callee),
                ),
            );
        }

        // Lower and check arguments
        let lowered: Vec<TExpr> = args
            .iter()
            .enumerate()
            .map(|(i, a)| {
                let param_type = params.get(i).map(|p| p.type_id);
                let arg_expr = self.lower_expr(a, param_type);
                let arg_type = arg_expr.type_id();

                // Check argument assignability to parameter type
                if let Some(pt) = param_type
                    && !self.is_assignable_to(arg_type, pt, None)
                    && !self.types.is_invalid(arg_type)
                    && !self.types.is_invalid(pt)
                {
                    // Check for float truncation
                    let is_float_truncation = self.types.is_untyped(arg_type)
                        == Some(UntypedKind::Float)
                        && self.types.is_integer(pt);
                    let truncated = if is_float_truncation {
                        " (truncated)"
                    } else {
                        ""
                    };
                    self.error(
                        ErrorCode::IncompatibleArg,
                        a.span(),
                        format!(
                            "cannot use {} ({}) as {} value in argument to {}{}",
                            self.expr_string(a),
                            self.type_description(arg_type),
                            self.types.type_name(pt),
                            self.expr_string(callee),
                            truncated
                        ),
                    );
                }
                arg_expr
            })
            .collect();

        TExpr::Call {
            kind: callee_kind,
            args: lowered,
            type_id: result_type,
        }
    }

    fn resolve_method_call(
        &mut self,
        base: TExpr,
        method_name: &str,
    ) -> Option<(String, TypeId, bool, TExpr)> {
        let base_type = base.type_id();

        let find_method = |ty: TypeId| -> Option<(TypeId, bool)> {
            if let Type::Named { methods, .. } = self.types.get(ty) {
                for m in methods {
                    if m.name == method_name {
                        return Some((m.signature, m.receiver_is_pointer));
                    }
                }
            }
            None
        };

        if let Some(elem) = self.types.is_pointer(base_type) {
            // Receiver is *T
            if let Some((sig, rx_ptr)) = find_method(elem) {
                let mangled = format!("{}.{}", self.types.type_name(elem), method_name);
                if rx_ptr {
                    Some((mangled, sig, true, base))
                } else {
                    Some((
                        mangled,
                        sig,
                        false,
                        TExpr::Deref {
                            operand: Box::new(base),
                            type_id: elem,
                        },
                    ))
                }
            } else {
                None
            }
        } else {
            // Receiver is T
            if let Some((sig, rx_ptr)) = find_method(base_type) {
                let mangled = format!("{}.{}", self.types.type_name(base_type), method_name);
                if !rx_ptr {
                    Some((mangled, sig, false, base))
                } else {
                    // Check addressability if possible
                    let is_addr = matches!(
                        base,
                        TExpr::Var { .. }
                            | TExpr::Field { .. }
                            | TExpr::Index { .. }
                            | TExpr::Deref { .. }
                    );
                    if is_addr {
                        let ptr = self.types.pointer(base_type);
                        Some((
                            mangled,
                            sig,
                            true,
                            TExpr::AddrOf {
                                operand: Box::new(base),
                                type_id: ptr,
                            },
                        ))
                    } else {
                        None
                    }
                }
            } else {
                None
            }
        }
    }

    fn lower_builtin(&mut self, span: Span, b: BuiltinKind, args: &[Expr]) -> TExpr {
        match b {
            BuiltinKind::Println | BuiltinKind::Print => {
                let nl = b == BuiltinKind::Println;
                let pa: Vec<_> = args
                    .iter()
                    .map(|a| {
                        let e = self.lower_expr(a, None);
                        let k = self.print_kind(e.type_id());
                        (e, k)
                    })
                    .collect();
                TExpr::Call {
                    kind: TCallKind::Print {
                        newline: nl,
                        args: pa,
                    },
                    args: vec![],
                    type_id: TypeId::INVALID,
                }
            }
            BuiltinKind::Len => self.lower_len(span, args),
            BuiltinKind::Cap => self.lower_cap(span, args),
            BuiltinKind::New => self.lower_new(span, args),
            BuiltinKind::Make => self.lower_make(span, args),
            BuiltinKind::Append => self.lower_append(span, args),
            BuiltinKind::Copy => self.lower_copy(span, args),
            BuiltinKind::Delete => self.lower_delete(span, args),
            BuiltinKind::Panic => {
                let msg = if args.is_empty() {
                    TExpr::String {
                        value: "panic".into(),
                        type_id: self.universe.string_type,
                    }
                } else {
                    self.lower_expr(&args[0], None)
                };
                TExpr::Call {
                    kind: TCallKind::Panic { msg: Box::new(msg) },
                    args: vec![],
                    type_id: TypeId::INVALID,
                }
            }
        }
    }

    fn lower_len(&mut self, span: Span, args: &[Expr]) -> TExpr {
        if args.len() != 1 {
            let msg = if args.is_empty() {
                "invalid operation: not enough arguments for len() (expected 1, found 0)"
                    .to_string()
            } else {
                format!(
                    "invalid operation: too many arguments for {} (expected 1, found {})",
                    self.builtin_call_text("len", args),
                    args.len()
                )
            };
            self.error(ErrorCode::WrongArgCount, span, msg);
            return TExpr::Nil {
                type_id: TypeId::INVALID,
            };
        }
        let a = self.lower_expr(&args[0], None);
        let arg_type = a.type_id();
        let u = self.types.underlying(arg_type);
        let t = if let Some((l, _)) = self.types.is_array(u) {
            TLenTarget::Array { len: l }
        } else if self.types.is_slice(u).is_some() {
            TLenTarget::Slice {
                expr: Box::new(a.clone()),
            }
        } else if self.types.is_string(u)
            || self.types.is_untyped(arg_type) == Some(UntypedKind::String)
        {
            TLenTarget::String {
                expr: Box::new(a.clone()),
            }
        } else if self.types.is_map(u).is_some() {
            TLenTarget::Map {
                expr: Box::new(a.clone()),
            }
        } else {
            if !self.types.is_invalid(arg_type) {
                self.error(
                    ErrorCode::InvalidCall,
                    span,
                    format!(
                        "invalid argument: {} ({}) for built-in len",
                        self.expr_string(&args[0]),
                        self.type_description(arg_type)
                    ),
                );
            }
            return TExpr::Int {
                value: 0,
                type_id: self.universe.int_type,
            };
        };
        TExpr::Call {
            kind: TCallKind::Len(t),
            args: vec![],
            type_id: self.universe.int_type,
        }
    }

    fn lower_cap(&mut self, span: Span, args: &[Expr]) -> TExpr {
        if args.len() != 1 {
            let msg = if args.is_empty() {
                "invalid operation: not enough arguments for cap() (expected 1, found 0)"
                    .to_string()
            } else {
                format!(
                    "invalid operation: too many arguments for cap() (expected 1, found {})",
                    args.len()
                )
            };
            self.error(ErrorCode::WrongArgCount, span, msg);
            return TExpr::Nil {
                type_id: TypeId::INVALID,
            };
        }
        let a = self.lower_expr(&args[0], None);
        let arg_type = a.type_id();
        let u = self.types.underlying(arg_type);
        let t = if let Some((l, _)) = self.types.is_array(u) {
            TCapTarget::Array { len: l }
        } else if self.types.is_slice(u).is_some() {
            TCapTarget::Slice {
                expr: Box::new(a.clone()),
            }
        } else {
            if !self.types.is_invalid(arg_type) {
                self.error(
                    ErrorCode::InvalidCall,
                    args[0].span(),
                    format!(
                        "invalid argument: {} ({}) for built-in cap",
                        self.expr_string(&args[0]),
                        self.type_description(arg_type)
                    ),
                );
            }
            return TExpr::Int {
                value: 0,
                type_id: self.universe.int_type,
            };
        };
        TExpr::Call {
            kind: TCallKind::Cap(t),
            args: vec![],
            type_id: self.universe.int_type,
        }
    }

    fn lower_new(&mut self, span: Span, args: &[Expr]) -> TExpr {
        if args.len() != 1 {
            let msg = if args.is_empty() {
                "invalid operation: not enough arguments for new() (expected 1, found 0)"
                    .to_string()
            } else {
                format!(
                    "invalid operation: too many arguments for new() (expected 1, found {})",
                    args.len()
                )
            };
            self.error(ErrorCode::WrongArgCount, span, msg);
            return TExpr::Nil {
                type_id: TypeId::INVALID,
            };
        }

        let type_id = match &args[0] {
            Expr::TypeVal(_, ty) => self.resolve_ast_type(ty),
            Expr::Ident(s, name) => {
                if let Some((sym_id, _)) = self.lookup(name) {
                    let sym = self.symbols.get(sym_id);
                    match &sym.kind {
                        SymbolKind::Type { type_id } => *type_id,
                        _ => {
                            self.error(ErrorCode::NotAType, *s, format!("{} is not a type", name));
                            return TExpr::Nil {
                                type_id: TypeId::INVALID,
                            };
                        }
                    }
                } else {
                    self.error(
                        ErrorCode::UndeclaredName,
                        *s,
                        format!("undefined: {}", name),
                    );
                    return TExpr::Nil {
                        type_id: TypeId::INVALID,
                    };
                }
            }
            other => {
                self.error(
                    ErrorCode::NotAType,
                    other.span(),
                    "new requires a type argument",
                );
                return TExpr::Nil {
                    type_id: TypeId::INVALID,
                };
            }
        };

        let ptr = self.types.pointer(type_id);
        TExpr::Call {
            kind: TCallKind::New { elem_type: type_id },
            args: vec![],
            type_id: ptr,
        }
    }

    fn lower_make(&mut self, span: Span, args: &[Expr]) -> TExpr {
        if args.is_empty() {
            self.error(
                ErrorCode::WrongArgCount,
                span,
                "invalid operation: not enough arguments for make() (expected 1, found 0)",
            );
            return TExpr::Nil {
                type_id: TypeId::INVALID,
            };
        }

        let type_id = match &args[0] {
            Expr::TypeVal(_, ty) => self.resolve_ast_type(ty),
            Expr::Ident(s, name) => {
                if let Some((sym_id, _)) = self.lookup(name) {
                    let sym = self.symbols.get(sym_id);
                    match &sym.kind {
                        SymbolKind::Type { type_id } => *type_id,
                        _ => {
                            self.error(ErrorCode::NotAType, *s, format!("{} is not a type", name));
                            return TExpr::Nil {
                                type_id: TypeId::INVALID,
                            };
                        }
                    }
                } else {
                    self.error(
                        ErrorCode::UndeclaredName,
                        *s,
                        format!("undefined: {}", name),
                    );
                    return TExpr::Nil {
                        type_id: TypeId::INVALID,
                    };
                }
            }
            other => {
                self.error(
                    ErrorCode::NotAType,
                    other.span(),
                    "make requires a type argument",
                );
                return TExpr::Nil {
                    type_id: TypeId::INVALID,
                };
            }
        };

        let underlying = self.types.underlying(type_id);
        let is_slice = self.types.is_slice(underlying).is_some();
        let is_map = self.types.is_map(underlying).is_some();

        if !is_slice && !is_map && !self.types.is_invalid(type_id) {
            self.error(
                ErrorCode::InvalidCall,
                span,
                format!(
                    "cannot make type {} (must be slice or map)",
                    self.types.type_name(type_id)
                ),
            );
            return TExpr::Nil {
                type_id: TypeId::INVALID,
            };
        }

        // Check argument counts for slice
        if is_slice {
            if args.len() < 2 {
                self.error(
                    ErrorCode::WrongArgCount,
                    span,
                    format!(
                        "invalid operation: make({}) expects 2 or 3 arguments; found 1",
                        self.types.type_name(type_id)
                    ),
                );
                return TExpr::Nil { type_id };
            }
            if args.len() > 3 {
                self.error(
                    ErrorCode::WrongArgCount,
                    span,
                    format!(
                        "invalid operation: make({}) expects 2 or 3 arguments; found {}",
                        self.types.type_name(type_id),
                        args.len()
                    ),
                );
            }
        } else if is_map && args.len() > 2 {
            self.error(
                ErrorCode::WrongArgCount,
                span,
                format!(
                    "too many arguments to make (expected 1 or 2, got {})",
                    args.len()
                ),
            );
        }

        // Check size arguments
        let mut len_val: Option<i64> = None;
        let mut cap_val: Option<i64> = None;

        for (i, arg) in args.iter().skip(1).enumerate() {
            let arg_lowered = self.lower_expr(arg, Some(self.universe.int_type));
            let arg_type = arg_lowered.type_id();

            if !self.types.is_integer(arg_type)
                && self.types.is_untyped(arg_type) != Some(UntypedKind::Int)
                && !self.types.is_invalid(arg_type)
            {
                let expr_str = self.expr_string(arg);
                let type_desc = self.type_description(arg_type);
                self.error(
                    ErrorCode::InvalidIndex,
                    arg.span(),
                    format!("cannot convert {} ({}) to type int", expr_str, type_desc),
                );
            }

            // Check for negative constants
            if let Some(v) = self.const_index_value(&arg_lowered) {
                if v < 0 {
                    self.error(
                        ErrorCode::InvalidMakeArg,
                        arg.span(),
                        format!(
                            "invalid argument: index {} (constant of type int) must not be negative",
                            v
                        ),
                    );
                }
                if i == 0 {
                    len_val = Some(v);
                } else if i == 1 {
                    cap_val = Some(v);
                }
            }
        }

        // Check len <= cap
        if is_slice
            && let (Some(len), Some(cap)) = (len_val, cap_val)
            && len > cap
            && cap >= 0
        {
            self.error(
                ErrorCode::InvalidMakeArg,
                span,
                format!(
                    "len larger than cap in make({})",
                    self.types.type_name(type_id)
                ),
            );
        }

        let kind = if let Some(elem) = self.types.is_slice(underlying) {
            let len = if args.len() > 1 {
                self.lower_expr(&args[1], Some(self.universe.int_type))
            } else {
                TExpr::Int {
                    value: 0,
                    type_id: self.universe.int_type,
                }
            };
            let cap = if args.len() > 2 {
                Some(Box::new(
                    self.lower_expr(&args[2], Some(self.universe.int_type)),
                ))
            } else {
                None
            };
            TMakeKind::Slice {
                elem_type: elem,
                len: Box::new(len),
                cap,
            }
        } else if let Some((k, v)) = self.types.is_map(underlying) {
            TMakeKind::Map {
                key_type: k,
                value_type: v,
                key_is_string: self.types.is_string(k),
            }
        } else {
            return TExpr::Nil {
                type_id: TypeId::INVALID,
            };
        };

        TExpr::Call {
            kind: TCallKind::Make(TMakeInfo { kind }),
            args: vec![],
            type_id,
        }
    }

    fn lower_append(&mut self, span: Span, args: &[Expr]) -> TExpr {
        if args.is_empty() {
            self.error(
                ErrorCode::WrongArgCount,
                span,
                "invalid operation: not enough arguments for append() (expected 1, found 0)",
            );
            return TExpr::Nil {
                type_id: TypeId::INVALID,
            };
        }
        let s = self.lower_expr(&args[0], None);
        let st = s.type_id();
        let elem = match self.types.is_slice(self.types.underlying(st)) {
            Some(e) => e,
            None => {
                if !self.types.is_invalid(st) {
                    self.error(
                        ErrorCode::InvalidCall,
                        args[0].span(),
                        format!(
                            "invalid append: argument must be a slice; have {} ({})",
                            self.expr_string(&args[0]),
                            self.type_description(st)
                        ),
                    );
                }
                return TExpr::Nil {
                    type_id: TypeId::INVALID,
                };
            }
        };

        // Check element assignability
        let mut elems = Vec::new();
        for arg in args.iter().skip(1) {
            let e = self.lower_expr(arg, Some(elem));
            let arg_type = e.type_id();
            if !self.is_assignable_to(arg_type, elem, None) && !self.types.is_invalid(arg_type) {
                self.error(
                    ErrorCode::IncompatibleArg,
                    arg.span(),
                    format!(
                        "cannot use {} ({}) as {} value in argument to append",
                        self.expr_string(arg),
                        self.type_description(arg_type),
                        self.types.type_name(elem)
                    ),
                );
            }
            elems.push(e);
        }

        TExpr::Call {
            kind: TCallKind::Append {
                slice: Box::new(s),
                elems,
                elem_type: elem,
            },
            args: vec![],
            type_id: st,
        }
    }

    fn lower_copy(&mut self, span: Span, args: &[Expr]) -> TExpr {
        if args.len() != 2 {
            let msg = if args.is_empty() || args.len() == 1 {
                format!(
                    "invalid operation: not enough arguments for copy() (expected 2, found {})",
                    args.len()
                )
            } else {
                format!(
                    "invalid operation: too many arguments for copy() (expected 2, found {})",
                    args.len()
                )
            };
            self.error(ErrorCode::WrongArgCount, span, msg);
            return TExpr::Nil {
                type_id: TypeId::INVALID,
            };
        }

        let d = self.lower_expr(&args[0], None);
        let sr = self.lower_expr(&args[1], None);
        let dst_type = d.type_id();
        let src_type = sr.type_id();

        let dst_elem = self.types.is_slice(self.types.underlying(dst_type));
        let src_elem = self.types.is_slice(self.types.underlying(src_type));

        // Check dst is a slice
        if dst_elem.is_none() && !self.types.is_invalid(dst_type) {
            self.error(
                ErrorCode::IncompatibleArg,
                args[0].span(),
                format!(
                    "invalid copy: argument must be a slice; have {} ({})",
                    self.expr_string(&args[0]),
                    self.type_description(dst_type)
                ),
            );
            return TExpr::Nil {
                type_id: TypeId::INVALID,
            };
        }

        // Check src is a slice or string
        if src_elem.is_none() && !self.types.is_string(src_type) && !self.types.is_invalid(src_type)
        {
            self.error(
                ErrorCode::IncompatibleArg,
                args[1].span(),
                format!(
                    "invalid copy: argument must be a slice; have {} ({})",
                    self.expr_string(&args[1]),
                    self.type_description(src_type)
                ),
            );
            return TExpr::Nil {
                type_id: TypeId::INVALID,
            };
        }

        // Check element type compatibility
        match (dst_elem, src_elem) {
            (Some(d), Some(s)) if !self.types.identical(d, s) => {
                if !self.types.is_invalid(dst_type) && !self.types.is_invalid(src_type) {
                    self.error(
                        ErrorCode::IncompatibleArg,
                        span,
                        format!(
                            "arguments to copy have different element types ({} and {})",
                            self.types.type_name(dst_type),
                            self.types.type_name(src_type)
                        ),
                    );
                }
            }
            _ => {}
        }

        let elem = dst_elem.unwrap_or(TypeId::INVALID);
        TExpr::Call {
            kind: TCallKind::Copy {
                dst: Box::new(d),
                src: Box::new(sr),
                elem_type: elem,
            },
            args: vec![],
            type_id: self.universe.int_type,
        }
    }

    fn lower_delete(&mut self, span: Span, args: &[Expr]) -> TExpr {
        // Check all args to report errors before returning
        for arg in args {
            let _ = self.lower_expr(arg, None);
        }

        if args.len() != 2 {
            let args_text = self.builtin_call_text("delete", args);
            let msg = if args.is_empty() || args.len() == 1 {
                format!(
                    "invalid operation: not enough arguments for {} (expected 2, found {})",
                    args_text,
                    args.len()
                )
            } else {
                format!(
                    "invalid operation: too many arguments for {} (expected 2, found {})",
                    args_text,
                    args.len()
                )
            };
            self.error(ErrorCode::WrongArgCount, span, msg);
            return TExpr::Nil {
                type_id: TypeId::INVALID,
            };
        }

        let m = self.lower_expr(&args[0], None);
        let map_type = m.type_id();

        let (key_type, _) = match self.types.is_map(self.types.underlying(map_type)) {
            Some((k, v)) => (k, v),
            None => {
                if !self.types.is_invalid(map_type) {
                    self.error(
                        ErrorCode::InvalidCall,
                        args[0].span(),
                        format!(
                            "invalid argument: {} ({}) is not a map",
                            self.expr_string(&args[0]),
                            self.type_description(map_type)
                        ),
                    );
                }
                return TExpr::Nil {
                    type_id: TypeId::INVALID,
                };
            }
        };

        let k = self.lower_expr(&args[1], Some(key_type));
        let key_arg_type = k.type_id();

        // Check key assignability
        if !self.is_assignable_to(key_arg_type, key_type, None)
            && !self.types.is_invalid(key_arg_type)
        {
            self.error(
                ErrorCode::InvalidMapKey,
                args[1].span(),
                format!(
                    "cannot use {} ({}) as {} value in argument to delete",
                    self.expr_string(&args[1]),
                    self.type_description(key_arg_type),
                    self.types.type_name(key_type)
                ),
            );
        }

        TExpr::Call {
            kind: TCallKind::Delete {
                map: Box::new(m),
                key: Box::new(k),
                key_type,
            },
            args: vec![],
            type_id: TypeId::INVALID,
        }
    }

    fn lower_conversion(&mut self, span: Span, target: TypeId, args: &[Expr]) -> TExpr {
        if args.len() != 1 {
            self.error(
                ErrorCode::WrongArgCount,
                span,
                format!(
                    "conversion to {} requires exactly one argument",
                    self.types.type_name(target)
                ),
            );
            return TExpr::Nil {
                type_id: TypeId::INVALID,
            };
        }

        let e = self.lower_expr(&args[0], None);
        let src = e.type_id();

        // Check if conversion is valid
        if !self.can_convert(src, target)
            && !self.types.is_invalid(src)
            && !self.types.is_invalid(target)
        {
            self.error(
                ErrorCode::InvalidConversion,
                span,
                format!(
                    "cannot convert {} (variable of type {}) to type {}",
                    self.expr_string(&args[0]),
                    self.types.type_name(src),
                    self.types.type_name(target)
                ),
            );
        }

        // Check for constant overflow/truncation
        if let Some(cv) = self.tir_const_to_analysis(&e)
            && let Some(basic) = self.types.is_basic(self.types.underlying(target))
            && !self.const_representable(&cv, basic)
        {
            let is_truncation =
                matches!(&cv, ConstValue::Float(f) if !f.is_integer()) && basic.is_integer();
            let msg = if is_truncation {
                format!(
                    "constant {} truncated to integer",
                    self.expr_string(&args[0])
                )
            } else {
                format!(
                    "constant {} overflows {}",
                    self.expr_string(&args[0]),
                    self.types.type_name(target)
                )
            };
            self.error(ErrorCode::NumericOverflow, span, msg);
        }

        let kind = self.conv_kind(src, target);
        TExpr::Convert {
            kind,
            expr: Box::new(e),
            type_id: target,
        }
    }

    /// Convert TExpr constant to ConstValue for validation
    fn tir_const_to_analysis(&self, expr: &TExpr) -> Option<ConstValue> {
        match expr {
            TExpr::Int { value, .. } => Some(ConstValue::Int(BigInt::from(*value))),
            TExpr::Uint { value, .. } => Some(ConstValue::Int(BigInt::from(*value))),
            TExpr::Float { value, .. } => BigRational::from_float(*value).map(ConstValue::Float),
            TExpr::String { value, .. } => Some(ConstValue::String(value.clone())),
            TExpr::Const { value, .. } => match value.as_ref() {
                TConstValue::Int(i) => Some(ConstValue::Int(BigInt::from(*i))),
                TConstValue::Uint(u) => Some(ConstValue::Int(BigInt::from(*u))),
                TConstValue::Float(f) => BigRational::from_float(*f).map(ConstValue::Float),
                TConstValue::String(s) => Some(ConstValue::String(s.clone())),
                TConstValue::Bool(b) => Some(ConstValue::Bool(*b)),
                TConstValue::Nil => Some(ConstValue::Nil),
            },
            _ => None,
        }
    }

    fn conv_kind(&self, s: TypeId, d: TypeId) -> TConversionKind {
        let su = self.types.underlying(s);
        let du = self.types.underlying(d);
        if self.types.is_integer(su) && self.types.is_integer(du) {
            TConversionKind::IntToInt {
                src_signed: self.types.is_signed_int(su),
            }
        } else if self.types.is_integer(su) && self.types.is_float(du) {
            TConversionKind::IntToFloat {
                src_signed: self.types.is_signed_int(su),
            }
        } else if self.types.is_float(su) && self.types.is_integer(du) {
            TConversionKind::FloatToInt {
                dst_signed: self.types.is_signed_int(du),
            }
        } else if self.types.is_float(su) && self.types.is_float(du) {
            TConversionKind::FloatToFloat
        } else if self.types.is_integer(su) && self.types.is_string(du) {
            TConversionKind::IntToString
        } else if self.types.is_string(su) && self.is_byte_slice(du) {
            TConversionKind::StringToBytes
        } else if self.is_byte_slice(su) && self.types.is_string(du) {
            TConversionKind::BytesToString
        } else {
            TConversionKind::Identity
        }
    }

    /// Check if type is []byte ([]uint8)
    fn is_byte_slice(&self, type_id: TypeId) -> bool {
        if let Some(elem) = self.types.is_slice(type_id) {
            self.types.is_basic(elem) == Some(BasicType::Uint8)
        } else {
            false
        }
    }

    pub fn lower_selector(&mut self, span: Span, base: &Expr, field: &str) -> TExpr {
        let b = self.lower_expr(base, None);
        let bt = b.type_id();
        let (st, ad) = if let Some(p) = self.types.is_pointer(bt) {
            (p, 1)
        } else {
            (bt, 0)
        };
        let u = self.types.underlying(st);
        if let Some(fs) = self.types.struct_fields(u) {
            for (i, f) in fs.iter().enumerate() {
                if f.name == field {
                    return TExpr::Field {
                        base: Box::new(b),
                        struct_type: u,
                        field_index: i as u32,
                        auto_deref: ad,
                        type_id: f.type_id,
                    };
                }
            }
        }
        self.error(
            ErrorCode::MissingFieldOrMethod,
            span,
            format!(
                "{}.{} undefined (type {} has no field or method {})",
                self.expr_string(base),
                field,
                self.types.type_name(st),
                field
            ),
        );
        TExpr::Nil {
            type_id: TypeId::INVALID,
        }
    }

    pub fn lower_index(&mut self, span: Span, base: &Expr, index: &Expr) -> TExpr {
        let b = self.lower_expr(base, None);
        let base_type = b.type_id();
        let u = self.types.underlying(base_type);

        // Check for map index first
        if let Some((key_type, value_type)) = self.types.is_map(u) {
            let i = self.lower_expr(index, Some(key_type));
            let index_type = i.type_id();

            // Check that index is assignable to key type
            if !self.is_assignable_to(index_type, key_type, None)
                && !self.types.is_invalid(index_type)
            {
                let type_desc = self.type_description(index_type);
                self.error(
                    ErrorCode::InvalidMapKey,
                    index.span(),
                    format!(
                        "cannot use {} ({}) as {} value in map index",
                        self.expr_string(index),
                        type_desc,
                        self.types.type_name(key_type)
                    ),
                );
            }

            return TExpr::Index {
                kind: TIndexKind::Map {
                    key_type,
                    value_type,
                },
                base: Box::new(b),
                index: Box::new(i),
                type_id: value_type,
            };
        }

        // For non-map types, index must be an integer
        let i = self.lower_expr(index, Some(self.universe.int_type));
        let index_type = i.type_id();

        // Check index is integer type
        if !self.types.is_integer(index_type)
            && self.types.is_untyped(index_type) != Some(UntypedKind::Int)
            && !self.types.is_invalid(index_type)
        {
            let expr_str = self.expr_string(index);
            let type_desc = self.type_description(index_type);
            let msg = if self.types.is_untyped(index_type) == Some(UntypedKind::Float) {
                format!("{} ({}) truncated to int", expr_str, type_desc)
            } else {
                format!("cannot convert {} ({}) to type int", expr_str, type_desc)
            };
            self.error(ErrorCode::InvalidIndex, index.span(), msg);
        }

        // Check for constant index values
        let const_idx = self.const_index_value(&i);
        if let Some(idx) = const_idx
            && idx < 0
        {
            self.error(
                ErrorCode::InvalidIndexNeg,
                index.span(),
                format!(
                    "invalid argument: index {} (constant of type int) must not be negative",
                    idx
                ),
            );
        }

        // Determine result type and check bounds
        let (k, et) = if let Some((len, elem)) = self.types.is_array(u) {
            // Check array bounds for constant index
            if let Some(idx) = const_idx
                && idx >= 0
                && idx >= len as i64
            {
                self.error(
                    ErrorCode::InvalidIndexBounds,
                    index.span(),
                    format!("invalid argument: index {} out of bounds [0:{}]", idx, len),
                );
            }
            (TIndexKind::Array { len }, elem)
        } else if let Some(elem) = self.types.is_slice(u) {
            (TIndexKind::Slice { elem_type: elem }, elem)
        } else if self.types.is_string(u) {
            (TIndexKind::String, self.universe.uint8_type)
        } else {
            if !self.types.is_invalid(base_type) {
                self.error(
                    ErrorCode::NonIndexableOperand,
                    span,
                    format!(
                        "cannot index {} ({})",
                        self.expr_string(base),
                        self.type_description(base_type)
                    ),
                );
            }
            return TExpr::Nil {
                type_id: TypeId::INVALID,
            };
        };

        TExpr::Index {
            kind: k,
            base: Box::new(b),
            index: Box::new(i),
            type_id: et,
        }
    }

    pub fn lower_slice(
        &mut self,
        span: Span,
        base: &Expr,
        low: Option<&Expr>,
        high: Option<&Expr>,
    ) -> TExpr {
        let b = self.lower_expr(base, None);
        let base_type = b.type_id();
        let u = self.types.underlying(base_type);

        // Determine slice kind and result type
        let array_len: Option<u64> = self.types.is_array(u).map(|(len, _)| len);

        let (k, rt) = if let Some((len, elem)) = self.types.is_array(u) {
            // Check that array is addressable before slicing
            if !self.is_expr_addressable(base) && !self.types.is_invalid(base_type) {
                self.error(
                    ErrorCode::UnaddressableOperand,
                    base.span(),
                    format!(
                        "cannot slice unaddressable value {} (value of type {})",
                        self.expr_string(base),
                        self.types.type_name(base_type)
                    ),
                );
            }
            (
                TSliceKind::Array {
                    len,
                    elem_type: elem,
                },
                self.types.slice(elem),
            )
        } else if let Some(elem) = self.types.is_slice(u) {
            (TSliceKind::Slice { elem_type: elem }, base_type)
        } else if self.types.is_string(u) {
            (TSliceKind::String, base_type)
        } else {
            if !self.types.is_invalid(base_type) {
                self.error(
                    ErrorCode::NonSliceableOperand,
                    span,
                    format!(
                        "cannot slice {} ({})",
                        self.expr_string(base),
                        self.type_description(base_type)
                    ),
                );
            }
            return TExpr::Nil {
                type_id: TypeId::INVALID,
            };
        };

        // Lower and validate slice indices
        let lo = low.map(|e| {
            let lowered = self.lower_expr(e, Some(self.universe.int_type));
            self.check_slice_index(e, &lowered, array_len);
            Box::new(lowered)
        });
        let hi = high.map(|e| {
            let lowered = self.lower_expr(e, Some(self.universe.int_type));
            self.check_slice_index(e, &lowered, array_len);
            Box::new(lowered)
        });

        // Check low <= high for constant indices
        let low_val = lo.as_ref().and_then(|e| self.const_index_value(e));
        let high_val = hi.as_ref().and_then(|e| self.const_index_value(e));

        if let (Some(l), Some(h)) = (low_val, high_val)
            && l > h
        {
            self.error(
                ErrorCode::SwappedSliceIndices,
                span,
                format!("invalid slice indices: {} < {}", h, l),
            );
        }

        TExpr::Slice {
            kind: k,
            base: Box::new(b),
            low: lo,
            high: hi,
            type_id: rt,
        }
    }

    /// Check a slice index for validity
    fn check_slice_index(&mut self, expr: &Expr, lowered: &TExpr, array_len: Option<u64>) {
        let index_type = lowered.type_id();

        // Check index is integer type
        if !self.types.is_integer(index_type)
            && self.types.is_untyped(index_type) != Some(UntypedKind::Int)
            && !self.types.is_invalid(index_type)
        {
            let expr_str = self.expr_string(expr);
            let type_desc = self.type_description(index_type);
            self.error(
                ErrorCode::InvalidIndex,
                expr.span(),
                format!("cannot convert {} ({}) to type int", expr_str, type_desc),
            );
            return;
        }

        // Check for negative constant
        if let Some(idx) = self.const_index_value(lowered) {
            if idx < 0 {
                self.error(
                    ErrorCode::InvalidIndexNeg,
                    expr.span(),
                    format!(
                        "invalid argument: index {} (constant of type int) must not be negative",
                        idx
                    ),
                );
            }

            // Check array bounds
            if let Some(len) = array_len
                && idx >= 0
                && idx > len as i64
            {
                self.error(
                    ErrorCode::InvalidIndexBounds,
                    expr.span(),
                    format!(
                        "invalid argument: index {} out of bounds [0:{}]",
                        idx,
                        len + 1
                    ),
                );
            }
        }
    }

    fn callee_as_type(&self, c: &Expr) -> Option<TypeId> {
        if let Expr::Ident(_, n) = c
            && let Some((sid, _)) = self.lookup(n)
            && let SymbolKind::Type { type_id } = self.symbols.get(sid).kind
        {
            return Some(type_id);
        }
        None
    }

    fn print_kind(&self, tid: TypeId) -> TPrintKind {
        let u = self.types.underlying(tid);
        if self.types.is_boolean(u) {
            TPrintKind::Bool
        } else if self.types.is_unsigned_int(u) {
            TPrintKind::UnsignedInt
        } else if self.types.is_integer(u) {
            TPrintKind::SignedInt
        } else if self.types.is_float(u) {
            TPrintKind::Float
        } else if self.types.is_string(u) {
            TPrintKind::String
        } else {
            TPrintKind::Pointer
        }
    }

    /// Extracts a constant value from an AST expression
    pub(crate) fn eval_const_with_type(
        &mut self,
        expr: &Expr,
        report_errors: bool,
    ) -> (TypeId, ConstValue) {
        match expr {
            Expr::IntLit(_, s) => {
                let s = s.replace("_", "");
                let big = if s.starts_with("0x") || s.starts_with("0X") {
                    BigInt::from_str_radix(&s[2..], 16).ok()
                } else if s.starts_with("0o") || s.starts_with("0O") {
                    BigInt::from_str_radix(&s[2..], 8).ok()
                } else if s.starts_with("0b") || s.starts_with("0B") {
                    BigInt::from_str_radix(&s[2..], 2).ok()
                } else if s.starts_with('0')
                    && s.len() > 1
                    && s.chars().skip(1).all(|c| c.is_ascii_digit())
                {
                    BigInt::from_str_radix(&s[1..], 8).ok()
                } else {
                    s.parse::<BigInt>().ok()
                };
                let cv = big.map(ConstValue::Int).unwrap_or(ConstValue::Nil);
                (self.types.untyped(UntypedKind::Int), cv)
            }
            Expr::FloatLit(_, s) => {
                let s = s.replace("_", "");
                let cv = s
                    .parse::<f64>()
                    .ok()
                    .map(|v| ConstValue::Float(BigRational::from_float(v).unwrap_or_default()))
                    .unwrap_or(ConstValue::Nil);
                (self.types.untyped(UntypedKind::Float), cv)
            }
            Expr::StringLit(_, s) => {
                let content = if s.starts_with('`') {
                    s.trim_matches('`').to_string()
                } else {
                    unescape_string(s.trim_matches('"'))
                };
                (
                    self.types.untyped(UntypedKind::String),
                    ConstValue::String(content),
                )
            }
            Expr::Ident(span, name) => {
                if name == "true" {
                    (
                        self.types.untyped(UntypedKind::Bool),
                        ConstValue::Bool(true),
                    )
                } else if name == "false" {
                    (
                        self.types.untyped(UntypedKind::Bool),
                        ConstValue::Bool(false),
                    )
                } else if name == "nil" {
                    (self.types.untyped(UntypedKind::Nil), ConstValue::Nil)
                } else if let Some((sym_id, _)) = self.lookup(name) {
                    let sym = self.symbols.get(sym_id);
                    if let Some(cv) = sym.const_value() {
                        (sym.type_id().unwrap_or(TypeId::INVALID), cv.clone())
                    } else {
                        // Not a constant
                        let var_type = sym.type_id().unwrap_or(TypeId::INVALID);
                        if report_errors {
                            self.error(
                                ErrorCode::InvalidConstExpr,
                                *span,
                                format!(
                                    "{} (variable of type {}) is not constant",
                                    name,
                                    self.types.type_name(var_type)
                                ),
                            );
                        }
                        (TypeId::INVALID, ConstValue::Nil)
                    }
                } else {
                    (TypeId::INVALID, ConstValue::Nil)
                }
            }
            Expr::Call(span, callee, _) => {
                let return_type = if let Expr::Ident(_, name) = callee.as_ref() {
                    if let Some((sym_id, _)) = self.lookup(name) {
                        let sig_type = self
                            .symbols
                            .get(sym_id)
                            .type_id()
                            .unwrap_or(TypeId::INVALID);
                        if let Some((_, result)) = self.types.is_signature(sig_type) {
                            result.unwrap_or(TypeId::INVALID)
                        } else {
                            TypeId::INVALID
                        }
                    } else {
                        TypeId::INVALID
                    }
                } else {
                    TypeId::INVALID
                };
                if report_errors {
                    let expr_str = self.expr_string(expr);
                    self.error(
                        ErrorCode::InvalidConstInit,
                        *span,
                        format!(
                            "{} (value of type {}) is not constant",
                            expr_str,
                            self.types.type_name(return_type)
                        ),
                    );
                }
                (return_type, ConstValue::Nil)
            }
            Expr::Unary(_, op, inner) => {
                let (type_id, val) = self.eval_const_with_type(inner, report_errors);
                let new_val = match op {
                    UnaryOp::Neg => val.negate().unwrap_or(ConstValue::Nil),
                    UnaryOp::Pos => val,
                    UnaryOp::Not => val.not().unwrap_or(ConstValue::Nil),
                    _ => ConstValue::Nil,
                };
                (type_id, new_val)
            }
            Expr::Binary(_, lhs, op, rhs) => {
                let (l_type, l_val) = self.eval_const_with_type(lhs, report_errors);
                let (_r_type, r_val) = self.eval_const_with_type(rhs, report_errors);

                let val = l_val
                    .binary_op(op.name(), &r_val)
                    .unwrap_or(ConstValue::Nil);

                if matches!(
                    op,
                    BinaryOp::Eq
                        | BinaryOp::Ne
                        | BinaryOp::Lt
                        | BinaryOp::Le
                        | BinaryOp::Gt
                        | BinaryOp::Ge
                ) {
                    (self.types.untyped(UntypedKind::Bool), val)
                } else {
                    (l_type, val)
                }
            }
            Expr::Paren(_, inner) => self.eval_const_with_type(inner, report_errors),
            _ => (TypeId::INVALID, ConstValue::Nil),
        }
    }

    pub fn eval_const_from_ast(&mut self, expr: &Expr) -> Option<ConstValue> {
        let (_, val) = self.eval_const_with_type(expr, false);
        if matches!(val, ConstValue::Nil) {
            None
        } else {
            Some(val)
        }
    }

    /// Returns the untyped kind of an AST expression, if it is an untyped constant
    pub fn expr_untyped_kind(&self, expr: &Expr) -> Option<UntypedKind> {
        match expr {
            Expr::IntLit(_, _) => Some(UntypedKind::Int),
            Expr::FloatLit(_, _) => Some(UntypedKind::Float),
            Expr::StringLit(_, _) => Some(UntypedKind::String),
            Expr::Ident(_, name) if name == "true" || name == "false" => Some(UntypedKind::Bool),
            Expr::Ident(_, name) if name == "nil" => Some(UntypedKind::Nil),
            Expr::Paren(_, inner) => self.expr_untyped_kind(inner),
            Expr::Unary(_, UnaryOp::Neg, inner) => match self.expr_untyped_kind(inner) {
                Some(UntypedKind::Int) => Some(UntypedKind::Int),
                Some(UntypedKind::Float) => Some(UntypedKind::Float),
                _ => None,
            },
            Expr::Unary(_, UnaryOp::Not, inner) => {
                if self.expr_untyped_kind(inner) == Some(UntypedKind::Bool) {
                    Some(UntypedKind::Bool)
                } else {
                    None
                }
            }
            Expr::Binary(_, lhs, _, rhs) => {
                let l = self.expr_untyped_kind(lhs);
                let r = self.expr_untyped_kind(rhs);
                // If both are untyped constants, result depends on types
                match (l, r) {
                    (Some(UntypedKind::Float), Some(UntypedKind::Int))
                    | (Some(UntypedKind::Int), Some(UntypedKind::Float)) => {
                        Some(UntypedKind::Float)
                    }
                    (Some(a), Some(_)) => Some(a),
                    (Some(a), None) | (None, Some(a)) => Some(a),
                    _ => None,
                }
            }
            _ => None,
        }
    }
}

pub fn unescape_string(s: &str) -> String {
    let mut res = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c != '\\' {
            res.push(c);
            continue;
        }

        match chars.peek().copied() {
            Some('n') => {
                chars.next();
                res.push('\n');
            }
            Some('r') => {
                chars.next();
                res.push('\r');
            }
            Some('t') => {
                chars.next();
                res.push('\t');
            }
            Some('\\') => {
                chars.next();
                res.push('\\');
            }
            Some('"') => {
                chars.next();
                res.push('"');
            }
            Some('\'') => {
                chars.next();
                res.push('\'');
            }
            Some('x') => {
                chars.next();
                let h = chars.next().and_then(|c| c.to_digit(16));
                let l = chars.next().and_then(|c| c.to_digit(16));
                if let (Some(h), Some(l)) = (h, l) {
                    res.push(((h << 4) | l) as u8 as char);
                }
            }
            Some('0'..='7') => {
                let mut v = 0;
                for _ in 0..3 {
                    if let Some(d) = chars.peek().and_then(|c| c.to_digit(8)) {
                        v = (v << 3) | d;
                        chars.next();
                    } else {
                        break;
                    }
                }
                if v <= 255 {
                    res.push(v as u8 as char);
                }
            }
            Some(c) => {
                chars.next();
                res.push('\\');
                res.push(c);
            }
            None => res.push('\\'),
        }
    }
    res
}
