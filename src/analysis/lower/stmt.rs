//! Statement lowering

use crate::analysis::Analyzer;
use crate::analysis::error::ErrorCode;
use crate::analysis::scope::ScopeKind;
use crate::analysis::symbol::{Symbol, SymbolKind};
use crate::analysis::typeops;
use crate::analysis::types::{TypeId, UntypedKind};
use crate::analysis::value::ConstValue;
use crate::ast::{
    AssignOp, BinaryOp, ConstDecl, Declaration, Else, Expr, ForClause, ForStmt, IfStmt, IncDecOp,
    SimpleStmt, Stmt, SwitchCase, SwitchStmt, TypeDecl, VarDecl,
};
use crate::tir::{
    ConstValue as TConstValue, TCase, TCompoundKind, TExpr, TForKind, TIncDecKind, TLValue, TStmt,
    TSwitchKind,
};
use crate::token::Span;

impl<'a> Analyzer<'a> {
    pub fn lower_stmt(&mut self, stmt: &Stmt) -> Vec<TStmt> {
        match stmt {
            Stmt::Decl(d) => self.lower_decl_stmt(d),
            Stmt::Simple(s) => self.lower_simple_stmt(s),
            Stmt::Return(span, expr) => vec![self.lower_return(*span, expr.as_ref())],
            Stmt::Break(span) => {
                if !self.scopes.is_in_switch_or_loop(self.current_scope) {
                    self.error(
                        ErrorCode::MisplacedBreak,
                        *span,
                        "break is not in a loop, switch, or select",
                    );
                }
                vec![TStmt::Break]
            }
            Stmt::Continue(span) => {
                if !self.scopes.is_in_loop(self.current_scope) {
                    self.error(
                        ErrorCode::MisplacedContinue,
                        *span,
                        "continue is not in a loop",
                    );
                }
                vec![TStmt::Continue]
            }
            Stmt::Block(b) => {
                let scope = self
                    .scopes
                    .new_scope(Some(self.current_scope), ScopeKind::Block);
                let old = self.current_scope;
                self.current_scope = scope;
                let body = b.stmts.iter().flat_map(|s| self.lower_stmt(s)).collect();
                self.check_unused_vars_in_scope(scope);
                self.current_scope = old;
                vec![TStmt::Block(body)]
            }
            Stmt::If(if_stmt) => vec![self.lower_if(if_stmt)],
            Stmt::Switch(sw) => vec![self.lower_switch(sw)],
            Stmt::For(for_stmt) => vec![self.lower_for(for_stmt)],
        }
    }

    fn lower_decl_stmt(&mut self, decl: &Declaration) -> Vec<TStmt> {
        match decl {
            Declaration::Var(v) => self.lower_local_var_stmt(v),
            Declaration::Const(c) => self.lower_local_const_stmt(c),
            Declaration::Type(t) => {
                self.lower_local_type_stmt(t);
                vec![]
            }
        }
    }

    fn lower_local_type_stmt(&mut self, decl: &TypeDecl) {
        let underlying = self.resolve_ast_type(&decl.typ);
        let type_id = self.types.named(decl.name.clone(), underlying);

        let sym = self.symbols.add(Symbol::new(
            decl.name.clone(),
            decl.span,
            SymbolKind::Type { type_id },
        ));

        self.try_define(&decl.name, sym, decl.span);
    }

    fn lower_local_var_stmt(&mut self, decl: &VarDecl) -> Vec<TStmt> {
        let explicit_type = decl.typ.as_ref().map(|t| self.resolve_ast_type(t));

        // Check void
        if let Some(ref val_expr) = decl.value
            && self.is_void_call(val_expr)
        {
            self.error(
                ErrorCode::IncompatibleAssign,
                val_expr.span(),
                format!("{} (no value) used as value", self.expr_string(val_expr)),
            );
        }

        // Always type-check the init expression even for blank identifier
        let init = decl
            .value
            .as_ref()
            .map(|e| self.lower_expr(e, explicit_type));

        // Handle blank identifier
        if decl.name == "_" {
            return vec![];
        }

        // Duplicate declaration
        if let Some(existing_id) = self.scopes.lookup_local(self.current_scope, &decl.name) {
            let prev_sym = self.symbols.get(existing_id);
            self.error_with_related(
                ErrorCode::DuplicateDecl,
                decl.span,
                format!("{} redeclared in this block", decl.name),
                prev_sym.span,
                format!("other declaration of {}", prev_sym.name),
            );
            return vec![];
        }

        // Check type
        let type_id = if let Some(et) = explicit_type {
            // Check assignability if there's an init expression
            if let Some(ref val_expr) = decl.value {
                // Check constant
                let const_val = self.eval_const_from_ast(val_expr);
                let untyped_kind = self.expr_untyped_kind(val_expr);
                let target_basic = self.types.is_basic(self.types.underlying(et));

                // Check truncation
                let is_truncation =
                    untyped_kind == Some(UntypedKind::Float) && self.types.is_integer(et);

                // Check overflow
                let is_overflow = !is_truncation
                    && const_val.as_ref().is_some_and(|cv| {
                        // Only check overflow for numeric const values
                        let is_numeric_const =
                            matches!(cv, ConstValue::Int(_) | ConstValue::Float(_));
                        is_numeric_const
                            && target_basic.is_some_and(|t| t.is_numeric() && !cv.fits_type(t))
                    });

                // Report error if truncation, overflow, or type mismatch
                if is_truncation || is_overflow {
                    let type_desc = self.expr_type_description(val_expr, TypeId::INVALID);
                    let suffix = if is_truncation {
                        " (truncated)"
                    } else {
                        " (overflows)"
                    };
                    self.error(
                        ErrorCode::IncompatibleAssign,
                        decl.span,
                        format!(
                            "cannot use {} ({}) as {} value in variable declaration{}",
                            self.expr_string(val_expr),
                            type_desc,
                            self.types.type_name(et),
                            suffix
                        ),
                    );
                } else {
                    // Check normal type compatibility
                    let init_type = init
                        .as_ref()
                        .map(|i| i.type_id())
                        .unwrap_or(TypeId::INVALID);
                    // Use const value for assignability check to detect overflow
                    if !self.is_assignable_to(init_type, et, const_val.as_ref())
                        && !self.types.is_invalid(init_type)
                        && !self.types.is_invalid(et)
                    {
                        let type_desc = self.expr_type_description(val_expr, init_type);
                        self.error(
                            ErrorCode::IncompatibleAssign,
                            decl.span,
                            format!(
                                "cannot use {} ({}) as {} value in variable declaration",
                                self.expr_string(val_expr),
                                type_desc,
                                self.types.type_name(et),
                            ),
                        );
                    }
                }
            }
            et
        } else if let Some(ref i) = init {
            let default = self.default_type(i.type_id());
            // Check for untyped nil
            if self.types.is_untyped_nil(default) {
                self.error(
                    ErrorCode::UntypedNilUse,
                    decl.span,
                    "use of untyped nil in variable declaration",
                );
            }
            default
        } else {
            // No type and no init
            self.error(
                ErrorCode::UntypedNilUse,
                decl.span,
                "missing type or initializer",
            );
            TypeId::INVALID
        };

        let sym = self.symbols.add(Symbol::new(
            decl.name.clone(),
            decl.span,
            SymbolKind::Var { type_id },
        ));

        if !self.try_define(&decl.name, sym, decl.span) {
            return vec![];
        }

        vec![TStmt::VarDecl {
            symbol_id: sym,
            type_id,
            init,
        }]
    }

    fn lower_local_const_stmt(&mut self, decl: &ConstDecl) -> Vec<TStmt> {
        let explicit_type = decl.typ.as_ref().map(|t| self.resolve_ast_type(t));

        // Check validity of const type
        if let Some(dt) = explicit_type
            && !self.is_valid_const_type(dt)
        {
            self.error(
                ErrorCode::InvalidConstType,
                decl.span,
                format!("invalid constant type {}", self.types.type_name(dt)),
            );
        }

        if decl.name == "_" {
            let _ = self.lower_expr(&decl.value, explicit_type);
            return vec![];
        }

        // Duplicate declaration
        if let Some(existing_id) = self.scopes.lookup_local(self.current_scope, &decl.name) {
            let prev_sym = self.symbols.get(existing_id);
            self.error_with_related(
                ErrorCode::DuplicateDecl,
                decl.span,
                format!("{} redeclared in this block", decl.name),
                prev_sym.span,
                format!("other declaration of {}", prev_sym.name),
            );
            return vec![];
        }

        let (type_id, value) = self.eval_const_with_type(&decl.value, true);
        let final_type = explicit_type.unwrap_or(type_id);

        // Check assignability
        if let Some(et) = explicit_type
            && !self.is_assignable_to(type_id, et, Some(&value))
            && !self.types.is_invalid(type_id)
            && !self.types.is_invalid(et)
        {
            self.error(
                ErrorCode::IncompatibleAssign,
                decl.value.span(),
                format!(
                    "cannot use {} as {} value in constant declaration",
                    self.types.type_name(type_id),
                    self.types.type_name(et)
                ),
            );
        }

        let sym = self.symbols.add(Symbol::new(
            decl.name.clone(),
            decl.span,
            SymbolKind::Const {
                type_id: final_type,
                value,
            },
        ));

        if !self.try_define(&decl.name, sym, decl.span) {
            return vec![];
        }

        vec![]
    }

    fn lower_simple_stmt(&mut self, stmt: &SimpleStmt) -> Vec<TStmt> {
        match stmt {
            SimpleStmt::Empty(_) => vec![],
            SimpleStmt::Expr(e) => vec![TStmt::Expr(self.lower_expr(e, None))],
            SimpleStmt::IncDec(span, e, op) => vec![self.lower_incdec(*span, e, *op)],
            SimpleStmt::Assign(span, lhs, op, rhs) => self.lower_assign(*span, lhs, *op, rhs),
            SimpleStmt::ShortVarDecl(span, name, value) => {
                self.lower_short_var_decl(*span, name, value)
            }
        }
    }

    fn lower_assign(&mut self, span: Span, lhs: &Expr, op: AssignOp, rhs: &Expr) -> Vec<TStmt> {
        if let Expr::Ident(_, name) = lhs
            && name == "_"
        {
            let r = self.lower_expr(rhs, None);
            return vec![TStmt::Expr(r)];
        }

        // Do not mark used
        let l = self.lower_lhs_expr(lhs);
        let l_type = l.type_id();
        let r = self.lower_expr(rhs, Some(l_type));
        let r_type = r.type_id();
        let lvalue = self.expr_to_lvalue(&l);

        // Check assignability
        if !self.is_expr_assignable(lhs) && !self.types.is_invalid(l_type) {
            let msg = if self.is_map_struct_field_access(lhs) {
                format!(
                    "cannot assign to struct field {} in map",
                    self.expr_string(lhs)
                )
            } else {
                format!(
                    "cannot assign to {} (neither addressable nor a map index expression)",
                    self.expr_string_parens(lhs)
                )
            };
            self.error(ErrorCode::UnassignableOperand, lhs.span(), msg);
        }

        // Simple assignment
        if op == AssignOp::Assign {
            if !self.is_assignable_to(r_type, l_type, None)
                && !self.types.is_invalid(l_type)
                && !self.types.is_invalid(r_type)
            {
                let type_desc = self.expr_type_description(rhs, r_type);
                self.error(
                    ErrorCode::IncompatibleAssign,
                    rhs.span(),
                    format!(
                        "cannot use {} ({}) as {} value in assignment",
                        self.expr_string(rhs),
                        type_desc,
                        self.types.type_name(l_type)
                    ),
                );
            }
            return vec![TStmt::Assign {
                lhs: lvalue,
                rhs: r,
            }];
        }

        // Compound assignment
        let bin_op = match op {
            AssignOp::Add => BinaryOp::Add,
            AssignOp::Sub => BinaryOp::Sub,
            AssignOp::Mul => BinaryOp::Mul,
            AssignOp::Div => BinaryOp::Div,
            AssignOp::Mod => BinaryOp::Mod,
            AssignOp::Assign => unreachable!(),
        };

        let bool_type = self.universe.bool_type;
        if typeops::check_binary_op(&self.types, bin_op, l_type, r_type, bool_type).is_none()
            && !self.types.is_invalid(l_type)
            && !self.types.is_invalid(r_type)
        {
            self.error(
                ErrorCode::UndefinedOp,
                span,
                format!(
                    "invalid operation: {} {}= {} (mismatched types {} and {})",
                    self.expr_string(lhs),
                    bin_op.name(),
                    self.expr_string(rhs),
                    self.types.type_name(l_type),
                    self.types.type_name(r_type)
                ),
            );
        }

        let kind = match op {
            AssignOp::Add => {
                if self.types.is_float(l_type) {
                    TCompoundKind::FloatAdd
                } else if self.types.is_string(l_type) {
                    TCompoundKind::StringConcat
                } else {
                    TCompoundKind::IntAdd
                }
            }
            AssignOp::Sub => {
                if self.types.is_float(l_type) {
                    TCompoundKind::FloatSub
                } else {
                    TCompoundKind::IntSub
                }
            }
            AssignOp::Mul => {
                if self.types.is_float(l_type) {
                    TCompoundKind::FloatMul
                } else {
                    TCompoundKind::IntMul
                }
            }
            AssignOp::Div => {
                if self.types.is_float(l_type) {
                    TCompoundKind::FloatDiv
                } else {
                    TCompoundKind::IntDiv {
                        signed: self.types.is_signed_int(l_type),
                    }
                }
            }
            AssignOp::Mod => TCompoundKind::IntMod {
                signed: self.types.is_signed_int(l_type),
            },
            AssignOp::Assign => unreachable!(),
        };

        vec![TStmt::CompoundAssign {
            kind,
            lhs: lvalue,
            rhs: r,
        }]
    }

    fn expr_to_lvalue(&self, expr: &TExpr) -> TLValue {
        match expr {
            TExpr::Var { symbol_id, type_id } => TLValue::Var {
                symbol_id: *symbol_id,
                type_id: *type_id,
            },
            TExpr::Field {
                base,
                struct_type,
                field_index,
                auto_deref,
                type_id,
            } => TLValue::Field {
                base: base.clone(),
                struct_type: *struct_type,
                field_index: *field_index,
                auto_deref: *auto_deref,
                type_id: *type_id,
            },
            TExpr::Index {
                kind,
                base,
                index,
                type_id,
            } => TLValue::Index {
                kind: kind.clone(),
                base: base.clone(),
                index: index.clone(),
                type_id: *type_id,
            },
            TExpr::Deref { operand, type_id } => TLValue::Deref {
                operand: operand.clone(),
                type_id: *type_id,
            },
            _ => TLValue::Blank,
        }
    }

    fn lower_short_var_decl(&mut self, span: Span, name: &str, value: &Expr) -> Vec<TStmt> {
        // Check void
        if self.is_void_call(value) {
            self.error(
                ErrorCode::IncompatibleAssign,
                value.span(),
                format!("{} (no value) used as value", self.expr_string(value)),
            );
        }

        let v = self.lower_expr(value, None);
        let expr_type = v.type_id();

        // Handle blank identifier
        if name == "_" {
            return vec![TStmt::Expr(v)];
        }

        // Get the default type
        let type_id = self.default_type(expr_type);

        // Check untyped nil
        if self.types.is_untyped_nil(type_id) {
            self.error(
                ErrorCode::UntypedNilUse,
                span,
                "use of untyped nil in assignment",
            );
        }

        let sym = self.symbols.add(Symbol::new(
            name.to_string(),
            span,
            SymbolKind::Var { type_id },
        ));

        if !self.try_define(name, sym, span) {
            return vec![TStmt::Expr(v)];
        }

        vec![TStmt::VarDecl {
            symbol_id: sym,
            type_id,
            init: Some(v),
        }]
    }

    fn lower_incdec(&mut self, span: Span, expr: &Expr, op: IncDecOp) -> TStmt {
        let target = self.lower_expr(expr, None);
        let type_id = target.type_id();
        let lvalue = self.expr_to_lvalue(&target);

        let op_str = match op {
            IncDecOp::Inc => "++",
            IncDecOp::Dec => "--",
        };

        // Check numeric
        let is_numeric = self.types.is_numeric(type_id);
        if !is_numeric && !self.types.is_invalid(type_id) {
            self.error(
                ErrorCode::NonNumericIncDec,
                span,
                format!(
                    "invalid operation: {}{} (non-numeric type {})",
                    self.expr_string(expr),
                    op_str,
                    self.types.type_name(type_id)
                ),
            );
        }

        // Check assignability
        if !self.is_expr_assignable(expr) && !self.types.is_invalid(type_id) {
            self.error(
                ErrorCode::UnassignableOperand,
                span,
                format!(
                    "cannot assign to {} (neither addressable nor a map index expression)",
                    self.expr_string_parens(expr)
                ),
            );
        }

        let is_inc = op == IncDecOp::Inc;
        let kind = if self.types.is_float(type_id) {
            if is_inc {
                TIncDecKind::IncFloat
            } else {
                TIncDecKind::DecFloat
            }
        } else if self.types.is_signed_int(type_id) {
            if is_inc {
                TIncDecKind::IncSigned
            } else {
                TIncDecKind::DecSigned
            }
        } else if is_inc {
            TIncDecKind::IncUnsigned
        } else {
            TIncDecKind::DecUnsigned
        };

        TStmt::IncDec {
            kind,
            operand: lvalue,
        }
    }

    fn lower_return(&mut self, span: Span, expr: Option<&Expr>) -> TStmt {
        let expected = self.current_func_result();

        match (expected, expr) {
            (Some(req), Some(e)) => {
                let lowered = self.lower_expr(e, Some(req));
                let actual = lowered.type_id();
                if !self.is_assignable_to(actual, req, None)
                    && !self.types.is_invalid(actual)
                    && !self.types.is_invalid(req)
                {
                    let type_desc = self.expr_type_description(e, actual);
                    self.error(
                        ErrorCode::IncompatibleAssign,
                        e.span(),
                        format!(
                            "cannot use {} ({}) as {} value in return statement",
                            self.expr_string(e),
                            type_desc,
                            self.types.type_name(req)
                        ),
                    );
                }
                TStmt::Return(Some(lowered))
            }
            (None, None) => TStmt::Return(None),
            (Some(_), None) => {
                self.error(
                    ErrorCode::WrongResultCount,
                    span,
                    "not enough return values",
                );
                TStmt::Return(None)
            }
            (None, Some(e)) => {
                self.error(
                    ErrorCode::WrongResultCount,
                    e.span(),
                    "too many return values",
                );
                // Still lower the expression to catch any errors in it
                let lowered = self.lower_expr(e, None);
                TStmt::Return(Some(lowered))
            }
        }
    }

    fn lower_if(&mut self, if_stmt: &IfStmt) -> TStmt {
        let if_scope = self
            .scopes
            .new_scope(Some(self.current_scope), ScopeKind::Block);
        let old = self.current_scope;
        self.current_scope = if_scope;

        // Handle init statement if present
        let init_stmts: Vec<TStmt> = if let Some(init) = &if_stmt.init {
            self.lower_simple_stmt(init)
        } else {
            vec![]
        };

        let cond = self.lower_expr(&if_stmt.cond, Some(self.universe.bool_type));
        let cond_type = cond.type_id();

        // Check condition is boolean
        if !self.types.is_boolean(cond_type)
            && !self.types.is_untyped_bool(cond_type)
            && !self.types.is_invalid(cond_type)
        {
            self.error(
                ErrorCode::InvalidCond,
                if_stmt.cond.span(),
                "non-boolean condition in if statement",
            );
        }

        let then_scope = self
            .scopes
            .new_scope(Some(self.current_scope), ScopeKind::Block);
        self.current_scope = then_scope;
        let then_body: Vec<TStmt> = if_stmt
            .body
            .stmts
            .iter()
            .flat_map(|s| self.lower_stmt(s))
            .collect();
        self.check_unused_vars_in_scope(then_scope);
        self.current_scope = if_scope;

        let else_body = if_stmt.else_branch.as_ref().map(|eb| {
            let else_scope = self.scopes.new_scope(Some(if_scope), ScopeKind::Block);
            self.current_scope = else_scope;
            let body = match eb {
                Else::Block(b) => b.stmts.iter().flat_map(|s| self.lower_stmt(s)).collect(),
                Else::If(nested) => vec![self.lower_if(nested)],
            };
            self.check_unused_vars_in_scope(else_scope);
            self.current_scope = if_scope;
            body
        });

        self.check_unused_vars_in_scope(if_scope);
        self.current_scope = old;

        let if_stmt = TStmt::If {
            cond,
            then_block: then_body,
            else_block: else_body,
        };

        // If there's an init statement wrap in a block
        if init_stmts.is_empty() {
            if_stmt
        } else {
            let mut block = init_stmts;
            block.push(if_stmt);
            TStmt::Block(block)
        }
    }

    fn lower_switch(&mut self, sw: &SwitchStmt) -> TStmt {
        let switch_scope = self
            .scopes
            .new_scope(Some(self.current_scope), ScopeKind::Switch);
        let old = self.current_scope;
        self.current_scope = switch_scope;

        // Handle init statement if present
        let init_stmts: Vec<TStmt> = if let Some(init) = &sw.init {
            self.lower_simple_stmt(init)
        } else {
            vec![]
        };

        let tag = sw.tag.as_ref().map(|e| self.lower_expr(e, None));
        let tag_type = tag
            .as_ref()
            .map(|e| self.default_type(e.type_id()))
            .unwrap_or(self.universe.bool_type);
        let tag_kind = self.determine_switch_kind(tag_type);

        // Keep track of the tag expression string for error messages
        let tag_expr_str = sw.tag.as_ref().map(|e| self.expr_string(e));

        let mut has_default = false;
        let mut default_span: Option<Span> = None;
        let mut seen_cases: Vec<(TConstValue, Span)> = Vec::new();

        let mut cases: Vec<TCase> = Vec::new();

        for c in &sw.cases {
            let case_scope = self
                .scopes
                .new_scope(Some(self.current_scope), ScopeKind::Block);
            let old_scope = self.current_scope;
            self.current_scope = case_scope;

            let values = match &c.case {
                SwitchCase::Expr(e) => {
                    let lowered = self.lower_expr(e, Some(tag_type));
                    let expr_type = lowered.type_id();

                    let assignable = self.is_assignable_to(expr_type, tag_type, None);

                    // Check case type is compatible with tag
                    if !assignable
                        && !self.types.is_invalid(expr_type)
                        && !self.types.is_invalid(tag_type)
                    {
                        let expr_str = self.expr_string(e);
                        let type_desc = self.type_description(expr_type);
                        let target_type = self.default_type(tag_type);
                        self.error(
                            ErrorCode::InvalidCaseType,
                            e.span(),
                            format!(
                                "cannot convert {} ({}) to type {}",
                                expr_str,
                                type_desc,
                                self.types.type_name(target_type)
                            ),
                        );
                    }

                    // Check that the implicit comparison in switch is valid
                    let bool_type = self.universe.bool_type;
                    if assignable
                        && typeops::check_binary_op(
                            &self.types,
                            BinaryOp::Eq,
                            tag_type,
                            expr_type,
                            bool_type,
                        )
                        .is_none()
                        && !self.types.is_invalid(expr_type)
                        && !self.types.is_invalid(tag_type)
                    {
                        let tag_text = tag_expr_str.as_deref().unwrap_or("<switch>");
                        let expr_text = self.expr_string(e);

                        let tag_u = self.types.underlying(tag_type);
                        let case_u = self.types.underlying(expr_type);
                        let note = if self.types.is_map(tag_u).is_some()
                            && self.types.is_map(case_u).is_some()
                        {
                            "map can only be compared to nil"
                        } else if self.types.is_slice(tag_u).is_some()
                            && self.types.is_slice(case_u).is_some()
                        {
                            "slice can only be compared to nil"
                        } else if self.types.is_signature(tag_u).is_some()
                            && self.types.is_signature(case_u).is_some()
                        {
                            "func can only be compared to nil"
                        } else {
                            "invalid comparison"
                        };

                        self.error(
                            ErrorCode::InvalidCaseType,
                            e.span(),
                            format!(
                                "invalid case {} in switch on {} ({})",
                                expr_text, tag_text, note
                            ),
                        );
                    }

                    // Check for duplicate case constant values
                    if let Some(cv) = self.texpr_const_value(&lowered) {
                        if let Some((_, prev_span)) = seen_cases.iter().find(|(c, _)| *c == cv) {
                            self.error_with_related(
                                ErrorCode::DuplicateCase,
                                e.span(),
                                format!(
                                    "duplicate case {} (constant of type {}) in expression switch",
                                    self.expr_string(e),
                                    self.types.type_name(tag_type)
                                ),
                                *prev_span,
                                "previous case",
                            );
                        } else {
                            seen_cases.push((cv, e.span()));
                        }
                    }

                    vec![lowered]
                }
                SwitchCase::Default => {
                    if has_default {
                        if let Some(first_span) = default_span {
                            self.error(
                                ErrorCode::DuplicateDefault,
                                c.span,
                                format!(
                                    "multiple defaults (first at {}:{}:{})",
                                    self.file(),
                                    first_span.line,
                                    first_span.col
                                ),
                            );
                        }
                    } else {
                        has_default = true;
                        default_span = Some(c.span);
                    }
                    vec![]
                }
            };
            let body: Vec<TStmt> = c.body.iter().flat_map(|s| self.lower_stmt(s)).collect();
            self.check_unused_vars_in_scope(case_scope);

            self.current_scope = old_scope;
            cases.push(TCase { values, body });
        }

        self.check_unused_vars_in_scope(switch_scope);
        self.current_scope = old;

        let switch_stmt = TStmt::Switch {
            tag,
            tag_kind,
            cases,
        };

        // If there's an init statement, wrap in a block
        if init_stmts.is_empty() {
            switch_stmt
        } else {
            let mut block = init_stmts;
            block.push(switch_stmt);
            TStmt::Block(block)
        }
    }

    fn determine_switch_kind(&self, type_id: TypeId) -> TSwitchKind {
        let u = self.types.underlying(type_id);
        if self.types.is_boolean(u) {
            TSwitchKind::Bool
        } else if self.types.is_integer(u) {
            TSwitchKind::Int
        } else if self.types.is_float(u) {
            TSwitchKind::Float
        } else if self.types.is_string(u) {
            TSwitchKind::String
        } else if self.types.is_pointer(u).is_some() {
            TSwitchKind::Pointer
        } else {
            TSwitchKind::Int
        }
    }

    fn lower_for(&mut self, for_stmt: &ForStmt) -> TStmt {
        let for_scope = self
            .scopes
            .new_scope(Some(self.current_scope), ScopeKind::For);
        let old = self.current_scope;
        self.current_scope = for_scope;

        let (kind, body) = match &for_stmt.clause {
            ForClause::Infinite => {
                let body: Vec<TStmt> = for_stmt
                    .body
                    .stmts
                    .iter()
                    .flat_map(|s| self.lower_stmt(s))
                    .collect();
                (TForKind::Infinite, body)
            }
            ForClause::Condition(cond) => {
                let c = self.lower_expr(cond, Some(self.universe.bool_type));
                let cond_type = c.type_id();

                // Check condition is boolean
                if !self.types.is_boolean(cond_type)
                    && !self.types.is_untyped_bool(cond_type)
                    && !self.types.is_invalid(cond_type)
                {
                    self.error(
                        ErrorCode::InvalidCond,
                        cond.span(),
                        "non-boolean condition in for statement",
                    );
                }

                let body: Vec<TStmt> = for_stmt
                    .body
                    .stmts
                    .iter()
                    .flat_map(|s| self.lower_stmt(s))
                    .collect();
                (TForKind::Condition { cond: c }, body)
            }
            ForClause::Classic(init, cond, post) => {
                let init_stmt = init.as_ref().map(|s| {
                    let stmts = self.lower_simple_stmt(s);
                    Box::new(TStmt::Block(stmts))
                });

                let cond_expr = cond.as_ref().map(|e| {
                    let c = self.lower_expr(e, Some(self.universe.bool_type));
                    let cond_type = c.type_id();

                    // Check condition is boolean
                    if !self.types.is_boolean(cond_type)
                        && !self.types.is_untyped_bool(cond_type)
                        && !self.types.is_invalid(cond_type)
                    {
                        self.error(
                            ErrorCode::InvalidCond,
                            e.span(),
                            "non-boolean condition in for statement",
                        );
                    }
                    c
                });

                // Check that post statement is not a short var decl
                if let Some(p) = post
                    && let SimpleStmt::ShortVarDecl(span, _, _) = p.as_ref()
                {
                    self.error(
                        ErrorCode::InvalidPostDecl,
                        *span,
                        "syntax error: cannot declare in post statement of for loop",
                    );
                }

                let post_stmt = post.as_ref().map(|s| {
                    let stmts = self.lower_simple_stmt(s);
                    Box::new(TStmt::Block(stmts))
                });

                let body: Vec<TStmt> = for_stmt
                    .body
                    .stmts
                    .iter()
                    .flat_map(|s| self.lower_stmt(s))
                    .collect();
                (
                    TForKind::Classic {
                        init: init_stmt,
                        cond: cond_expr,
                        post: post_stmt,
                    },
                    body,
                )
            }
            ForClause::Range(range_clause, range_expr) => {
                let range_lowered = self.lower_expr(range_expr, None);
                let range_type = range_lowered.type_id();
                let u = self.types.underlying(range_type);

                let (key_type, val_type, is_rangeable) =
                    if let Some((_, elem)) = self.types.is_array(u) {
                        (self.universe.int_type, elem, true)
                    } else if let Some(elem) = self.types.is_slice(u) {
                        (self.universe.int_type, elem, true)
                    } else if self.types.is_string(u) {
                        (self.universe.int_type, self.universe.int32_type, true)
                    } else if let Some((k, v)) = self.types.is_map(u) {
                        (k, v, true)
                    } else {
                        (self.universe.int_type, TypeId::INVALID, false)
                    };

                // Check range expression is rangeable
                if !is_rangeable && !self.types.is_invalid(range_type) {
                    // Special formatting for struct types
                    let type_desc = if self.types.struct_fields(range_type).is_some() {
                        format!(
                            "variable of struct type {}",
                            self.types.type_name(range_type)
                        )
                    } else {
                        self.type_description(range_type).to_string()
                    };
                    self.error(
                        ErrorCode::InvalidRangeExpr,
                        range_expr.span(),
                        format!(
                            "cannot range over {} ({})",
                            self.expr_string(range_expr),
                            type_desc
                        ),
                    );
                }

                let key_sym = range_clause
                    .key
                    .as_ref()
                    .filter(|n| n.as_str() != "_")
                    .map(|name| {
                        let sym = self.symbols.add(Symbol::new(
                            name.clone(),
                            range_clause.span,
                            SymbolKind::Var { type_id: key_type },
                        ));
                        self.scopes.define(self.current_scope, name.clone(), sym);
                        sym
                    });

                // Check for duplicate key/value names
                let val_sym = range_clause
                    .value
                    .as_ref()
                    .filter(|n| n.as_str() != "_")
                    .map(|name| {
                        if range_clause
                            .key
                            .as_ref()
                            .is_some_and(|k| k == name && k != "_")
                        {
                            self.error_with_related(
                                ErrorCode::DuplicateDecl,
                                range_clause.span,
                                format!("{} redeclared in this block", name),
                                range_clause.span,
                                format!("other declaration of {}", name),
                            );
                        }
                        let sym = self.symbols.add(Symbol::new(
                            name.clone(),
                            range_clause.span,
                            SymbolKind::Var { type_id: val_type },
                        ));
                        self.scopes.define(self.current_scope, name.clone(), sym);
                        sym
                    });

                let body: Vec<TStmt> = for_stmt
                    .body
                    .stmts
                    .iter()
                    .flat_map(|s| self.lower_stmt(s))
                    .collect();

                let kind = if let Some((len, elem)) = self.types.is_array(u) {
                    TForKind::RangeArray {
                        len,
                        elem_type: elem,
                        expr: range_lowered,
                        key_sym,
                        val_sym,
                    }
                } else if let Some(elem) = self.types.is_slice(u) {
                    TForKind::RangeSlice {
                        elem_type: elem,
                        expr: range_lowered,
                        key_sym,
                        val_sym,
                    }
                } else if self.types.is_string(u) {
                    TForKind::RangeString {
                        expr: range_lowered,
                        key_sym,
                        val_sym,
                    }
                } else if let Some((k, v)) = self.types.is_map(u) {
                    TForKind::RangeMap {
                        key_type: k,
                        value_type: v,
                        expr: range_lowered,
                        key_sym,
                        val_sym,
                    }
                } else {
                    TForKind::Infinite
                };

                (kind, body)
            }
        };

        self.check_unused_vars_in_scope(for_scope);
        self.current_scope = old;

        TStmt::For { kind, body }
    }
}
