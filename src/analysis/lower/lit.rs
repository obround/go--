//! Composite literal lowering

use crate::analysis::Analyzer;
use crate::analysis::error::ErrorCode;
use crate::analysis::symbol::SymbolKind;
use crate::analysis::types::{StructField, TypeId, UntypedKind};

use crate::ast::{Element, Expr, Key, LiteralType, LiteralValue};
use crate::tir::{ConstValue as TConstValue, TCompositeKind, TExpr, TUnaryOp};
use crate::token::Span;

use std::collections::HashSet;
use std::convert::TryInto;

impl<'a> Analyzer<'a> {
    pub(crate) fn lower_composite_lit(
        &mut self,
        span: Span,
        lt: &LiteralType,
        val: &LiteralValue,
    ) -> TExpr {
        let tid = self.resolve_lit_type(lt);
        if self.types.is_invalid(tid) {
            return TExpr::Nil {
                type_id: TypeId::INVALID,
            };
        }

        let u = self.types.underlying(tid);

        // Clone struct fields to avoid borrow conflict
        let struct_fields: Option<Vec<_>> = self.types.struct_fields(u).map(|fs| fs.to_vec());
        let array_info = self.types.is_array(u);
        let slice_elem = self.types.is_slice(u);
        let map_info = self.types.is_map(u);

        let kind = if let Some(fs) = struct_fields {
            self.lower_struct_lit(span, val, &fs, tid)
        } else if let Some((l, e)) = array_info {
            self.lower_array_lit(span, val, l, e)
        } else if let Some(e) = slice_elem {
            self.lower_slice_lit(span, val, e)
        } else if let Some((k, v)) = map_info {
            self.lower_map_lit(span, val, k, v)
        } else {
            self.error(
                ErrorCode::InvalidLit,
                span,
                format!(
                    "invalid composite literal type {}",
                    self.types.type_name(tid)
                ),
            );
            return TExpr::Nil {
                type_id: TypeId::INVALID,
            };
        };
        TExpr::Composite { kind, type_id: tid }
    }

    fn lower_struct_lit(
        &mut self,
        span: Span,
        val: &LiteralValue,
        fs: &[StructField],
        display_type: TypeId,
    ) -> TCompositeKind {
        let type_name = self.types.type_name(display_type).to_string();

        // Detect mixed keyed/positional elements
        let first_key = val.elements.iter().position(|e| e.key.is_some());
        let first_value = val.elements.iter().position(|e| e.key.is_none());

        if let (Some(key_idx), Some(val_idx)) = (first_key, first_value) {
            let bad_idx = if key_idx < val_idx { val_idx } else { key_idx };
            let bad_elem = &val.elements[bad_idx];
            let bad_span = if let Some(ref key) = bad_elem.key {
                match key {
                    Key::Field(s, _) => *s,
                    Key::Expr(e) => e.span(),
                }
            } else {
                self.element_span(&bad_elem.value)
            };
            self.error(
                ErrorCode::MixedStructLit,
                bad_span,
                "mixture of field:value and value elements in struct literal",
            );
            // Still produce valid TIR
            let r: Vec<_> = val
                .elements
                .iter()
                .enumerate()
                .map(|(i, e)| {
                    (
                        i as u32,
                        self.lower_elem(&e.value, fs.get(i).map(|f| f.type_id)),
                    )
                })
                .collect();
            return TCompositeKind::Struct { fields: r };
        }

        // Keyed struct literal
        if first_key.is_some() {
            let mut seen: HashSet<String> = HashSet::new();
            let mut result = Vec::new();

            for elem in &val.elements {
                let (field_span, field_name) = match &elem.key {
                    Some(Key::Field(s, n)) => (*s, n.as_str()),
                    Some(Key::Expr(e)) => {
                        if let Expr::Ident(s, n) = e {
                            (*s, n.as_str())
                        } else {
                            self.error(
                                ErrorCode::InvalidLitField,
                                e.span(),
                                "invalid field name in struct literal",
                            );
                            continue;
                        }
                    }
                    None => continue,
                };

                if !seen.insert(field_name.to_string()) {
                    self.error(
                        ErrorCode::DuplicateLitField,
                        field_span,
                        format!("duplicate field name {} in struct literal", field_name),
                    );
                    continue;
                }

                let (field_idx, field_type) =
                    match fs.iter().enumerate().find(|(_, f)| f.name == field_name) {
                        Some((i, f)) => (i, f.type_id),
                        None => {
                            self.error(
                                ErrorCode::MissingLitField,
                                field_span,
                                format!(
                                    "unknown field {} in struct literal of type {}",
                                    field_name, type_name
                                ),
                            );
                            continue;
                        }
                    };

                let val_expr = self.lower_elem(&elem.value, Some(field_type));
                let val_type = val_expr.type_id();
                if !self.is_assignable_to(val_type, field_type, None)
                    && !self.types.is_invalid(val_type)
                {
                    self.error(
                        ErrorCode::IncompatibleAssign,
                        self.element_span(&elem.value),
                        format!(
                            "cannot use {} as {} in struct literal field {}",
                            self.types.type_name(val_type),
                            self.types.type_name(field_type),
                            field_name
                        ),
                    );
                }
                result.push((field_idx as u32, val_expr));
            }
            TCompositeKind::Struct { fields: result }
        } else {
            // Positional struct literal
            if !val.elements.is_empty() && val.elements.len() != fs.len() {
                self.error(
                    ErrorCode::InvalidStructLit,
                    span,
                    format!(
                        "too {} values in struct literal of type {}",
                        if val.elements.len() < fs.len() {
                            "few"
                        } else {
                            "many"
                        },
                        type_name
                    ),
                );
            }

            let result: Vec<_> = val
                .elements
                .iter()
                .enumerate()
                .filter_map(|(i, elem)| {
                    if i >= fs.len() {
                        return None;
                    }
                    let field_type = fs[i].type_id;
                    let val_expr = self.lower_elem(&elem.value, Some(field_type));
                    let val_type = val_expr.type_id();
                    if !self.is_assignable_to(val_type, field_type, None)
                        && !self.types.is_invalid(val_type)
                    {
                        self.error(
                            ErrorCode::IncompatibleAssign,
                            self.element_span(&elem.value),
                            format!(
                                "cannot use {} as {} in struct literal field {}",
                                self.types.type_name(val_type),
                                self.types.type_name(field_type),
                                fs[i].name
                            ),
                        );
                    }
                    Some((i as u32, val_expr))
                })
                .collect();
            TCompositeKind::Struct { fields: result }
        }
    }

    fn lower_sequence_lit(
        &mut self,
        span: Span,
        val: &LiteralValue,
        elem_type: TypeId,
        max_len: Option<u64>,
        ctx: &str, // "array" or "slice"
    ) -> Vec<TExpr> {
        let mut max_index: i64 = -1;
        let mut seen_indices = HashSet::new();
        let mut elements = Vec::new();

        for elem_val in &val.elements {
            let has_explicit_key = elem_val.key.is_some();
            let index = match &elem_val.key {
                Some(Key::Expr(key_expr)) => {
                    // Get constant value for index
                    let key_lowered = self.lower_expr(key_expr, None);
                    let key_type = key_lowered.type_id();
                    match self.const_index_value(&key_lowered) {
                        Some(idx) => {
                            if idx < 0 {
                                self.error(
                                    ErrorCode::InvalidLitIndexNeg,
                                    key_expr.span(),
                                    format!("invalid argument: index {} (constant of type int) must not be negative", idx),
                                );
                                continue;
                            }
                            idx
                        }
                        None => {
                            // Check if it's a float truncation error
                            if self.types.is_untyped(key_type) == Some(UntypedKind::Float) {
                                self.error(
                                    ErrorCode::InvalidLitIndex,
                                    key_expr.span(),
                                    format!(
                                        "{} ({}) truncated to int",
                                        self.expr_string(key_expr),
                                        self.type_description(key_type)
                                    ),
                                );
                            } else {
                                self.error(
                                    ErrorCode::InvalidLitIndex,
                                    key_expr.span(),
                                    "index must be integer constant",
                                );
                            }
                            continue;
                        }
                    }
                }
                Some(Key::Field(fspan, _)) => {
                    self.error(
                        ErrorCode::InvalidLitField,
                        *fspan,
                        format!("invalid field name in {} literal", ctx),
                    );
                    continue;
                }
                None => max_index + 1,
            };

            if let Some(len) = max_len {
                if index >= len as i64 {
                    let msg = if has_explicit_key {
                        format!(
                            "invalid argument: index {} out of bounds [0:{}]",
                            index, len
                        )
                    } else {
                        format!("index {} is out of bounds (>= {})", index, len)
                    };
                    self.error(ErrorCode::InvalidLitIndexBounds, span, msg);
                }
            }

            if !seen_indices.insert(index) {
                self.error(
                    ErrorCode::DuplicateLitKey,
                    span,
                    format!("duplicate index {} in array or slice literal", index),
                );
            }

            max_index = max_index.max(index);

            let val_expr = self.lower_elem(&elem_val.value, Some(elem_type));
            let val_type = val_expr.type_id();
            if !self.is_assignable_to(val_type, elem_type, None) && !self.types.is_invalid(val_type)
            {
                self.error(
                    ErrorCode::IncompatibleAssign,
                    self.element_span(&elem_val.value),
                    format!(
                        "cannot use {} as {} in {} literal",
                        self.types.type_name(val_type),
                        self.types.type_name(elem_type),
                        ctx
                    ),
                );
            }
            elements.push(val_expr);
        }
        elements
    }

    fn lower_array_lit(
        &mut self,
        span: Span,
        val: &LiteralValue,
        len: u64,
        elem_type: TypeId,
    ) -> TCompositeKind {
        let elements = self.lower_sequence_lit(span, val, elem_type, Some(len), "array");
        TCompositeKind::Array {
            elem_type,
            elements,
        }
    }

    fn lower_slice_lit(
        &mut self,
        span: Span,
        val: &LiteralValue,
        elem_type: TypeId,
    ) -> TCompositeKind {
        let elements = self.lower_sequence_lit(span, val, elem_type, None, "slice");
        TCompositeKind::Slice {
            elem_type,
            elements,
        }
    }

    fn lower_map_lit(
        &mut self,
        span: Span,
        val: &LiteralValue,
        key_type: TypeId,
        value_type: TypeId,
    ) -> TCompositeKind {
        let mut seen_keys: Vec<TConstValue> = Vec::new();
        let mut entries = Vec::new();

        for elem_val in &val.elements {
            let key_expr = match &elem_val.key {
                Some(Key::Expr(k)) => k,
                Some(Key::Field(fspan, _)) => {
                    self.error(
                        ErrorCode::InvalidLitField,
                        *fspan,
                        "invalid field name in map literal",
                    );
                    continue;
                }
                None => {
                    self.error(ErrorCode::MissingLitKey, span, "missing key in map literal");
                    continue;
                }
            };

            let key_lowered = self.lower_expr(key_expr, Some(key_type));
            let key_lowered_type = key_lowered.type_id();

            if !self.is_assignable_to(key_lowered_type, key_type, None)
                && !self.types.is_invalid(key_lowered_type)
            {
                self.error(
                    ErrorCode::InvalidMapKey,
                    key_expr.span(),
                    format!(
                        "cannot use {} as {} in map literal key",
                        self.types.type_name(key_lowered_type),
                        self.types.type_name(key_type)
                    ),
                );
            }

            // Check for duplicate constant keys
            if let Some(cv) = self.texpr_const_value(&key_lowered) {
                if seen_keys.contains(&cv) {
                    self.error(
                        ErrorCode::DuplicateLitKey,
                        key_expr.span(),
                        format!(
                            "duplicate key {} in map literal",
                            self.expr_string(key_expr)
                        ),
                    );
                } else {
                    seen_keys.push(cv);
                }
            }

            let val_expr = self.lower_elem(&elem_val.value, Some(value_type));
            let val_type = val_expr.type_id();
            if !self.is_assignable_to(val_type, value_type, None)
                && !self.types.is_invalid(val_type)
            {
                self.error(
                    ErrorCode::IncompatibleAssign,
                    self.element_span(&elem_val.value),
                    format!(
                        "cannot use {} as {} in map literal value",
                        self.types.type_name(val_type),
                        self.types.type_name(value_type)
                    ),
                );
            }

            entries.push((key_lowered, val_expr));
        }

        TCompositeKind::Map {
            key_type,
            value_type,
            entries,
        }
    }

    pub(crate) fn texpr_const_value(&self, expr: &TExpr) -> Option<TConstValue> {
        match expr {
            TExpr::Int { value, .. } => Some(TConstValue::Int(*value as i128)),
            TExpr::Uint { value, .. } => Some(TConstValue::Uint(*value as u128)),
            TExpr::Float { value, .. } => Some(TConstValue::Float(*value)),
            TExpr::String { value, .. } => Some(TConstValue::String(value.clone())),
            TExpr::Const { value, .. } => Some(value.as_ref().clone()),
            TExpr::Unary { op, operand, .. } => {
                let val = self.texpr_const_value(operand)?;
                match op {
                    TUnaryOp::IntNeg => match val {
                        TConstValue::Int(i) => Some(TConstValue::Int(-i)),
                        _ => None,
                    },
                    TUnaryOp::FloatNeg => match val {
                        TConstValue::Float(f) => Some(TConstValue::Float(-f)),
                        _ => None,
                    },
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn element_span(&self, elem: &Element) -> Span {
        match elem {
            Element::Expr(e) => e.span(),
            Element::Lit(lit) => lit.span,
        }
    }

    fn lower_elem(&mut self, e: &Element, h: Option<TypeId>) -> TExpr {
        match e {
            Element::Expr(ex) => self.lower_expr(ex, h),
            Element::Lit(lit) => self.lower_nested_lit(lit, h.unwrap_or(TypeId::INVALID)),
        }
    }

    fn lower_nested_lit(&mut self, lit: &LiteralValue, tid: TypeId) -> TExpr {
        if self.types.is_invalid(tid) {
            self.error(
                ErrorCode::UntypedLit,
                lit.span,
                "missing type in composite literal",
            );
            return TExpr::Nil {
                type_id: TypeId::INVALID,
            };
        }

        let u = self.types.underlying(tid);

        // Clone struct fields to avoid borrow conflict
        let struct_fields: Option<Vec<_>> = self.types.struct_fields(u).map(|fs| fs.to_vec());
        let array_info = self.types.is_array(u);
        let slice_elem = self.types.is_slice(u);
        let map_info = self.types.is_map(u);

        let kind = if let Some(fs) = struct_fields {
            self.lower_struct_lit(lit.span, lit, &fs, tid)
        } else if let Some((l, e)) = array_info {
            self.lower_array_lit(lit.span, lit, l, e)
        } else if let Some(e) = slice_elem {
            self.lower_slice_lit(lit.span, lit, e)
        } else if let Some((k, v)) = map_info {
            self.lower_map_lit(lit.span, lit, k, v)
        } else {
            self.error(
                ErrorCode::InvalidLit,
                lit.span,
                format!(
                    "invalid composite literal type {}",
                    self.types.type_name(tid)
                ),
            );
            return TExpr::Nil {
                type_id: TypeId::INVALID,
            };
        };
        TExpr::Composite { kind, type_id: tid }
    }

    fn resolve_lit_type(&mut self, lt: &LiteralType) -> TypeId {
        match lt {
            LiteralType::Name(sp, n) => self.type_from_name(*sp, n),
            LiteralType::Array(sp, len, el) => {
                let et = self.resolve_ast_type(el);
                match self.eval_array_len_with_check(len, *sp) {
                    Some(l) => self.types.array(l, et),
                    None => TypeId::INVALID,
                }
            }
            LiteralType::Slice(_, el) => {
                let et = self.resolve_ast_type(el);
                self.types.slice(et)
            }
            LiteralType::Map(_sp, k, v) => {
                let kt = self.resolve_ast_type(k);
                let vt = self.resolve_ast_type(v);
                // Check that key type is comparable
                if !self.types.is_comparable(kt) && !self.types.is_invalid(kt) {
                    self.error(
                        ErrorCode::IncomparableMapKey,
                        k.span(),
                        format!("invalid map key type {}", self.types.type_name(kt)),
                    );
                }
                self.types.map(kt, vt)
            }
            LiteralType::Struct(sp, fs) => {
                // Check for duplicate fields
                let mut seen_fields: HashSet<&str> = HashSet::new();
                for field in fs {
                    for name in &field.names {
                        if !seen_fields.insert(name.as_str()) {
                            self.error(
                                ErrorCode::DuplicateField,
                                *sp,
                                format!("duplicate field {} in struct literal", name),
                            );
                        }
                    }
                }
                self.resolve_struct_type(fs)
            }
            LiteralType::Qualified(sp, _, _) => {
                self.error(ErrorCode::UndeclaredName, *sp, "qualified not supported");
                TypeId::INVALID
            }
        }
    }

    fn type_from_name(&mut self, span: Span, name: &str) -> TypeId {
        if let Some((sid, _)) = self.lookup(name)
            && let SymbolKind::Type { type_id } = self.symbols.get(sid).kind
        {
            return type_id;
        }
        self.error(
            ErrorCode::UndeclaredName,
            span,
            format!("undefined: {}", name),
        );
        TypeId::INVALID
    }

    pub(crate) fn const_index_value(&self, expr: &TExpr) -> Option<i64> {
        match expr {
            TExpr::Int { value, .. } => Some(*value),
            TExpr::Uint { value, .. } => (*value).try_into().ok(),
            TExpr::Const { value, .. } => match value.as_ref() {
                TConstValue::Int(i) => (*i).try_into().ok(),
                TConstValue::Uint(u) => (*u).try_into().ok(),
                _ => None,
            },
            TExpr::Unary { op, operand, .. } => {
                let inner = self.const_index_value(operand)?;
                match op {
                    TUnaryOp::IntNeg => Some(inner.wrapping_neg()),
                    _ => None,
                }
            }
            _ => None,
        }
    }
}
