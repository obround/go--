//! Declaration lowering

use crate::analysis::error::ErrorCode;
use crate::analysis::scope::ScopeKind;
use crate::analysis::symbol::{Symbol, SymbolKind};
use crate::analysis::types::{MethodInfo, Param, Type, TypeId};
use crate::analysis::value::ConstValue;
use crate::analysis::{Analyzer, FuncContext};
use crate::ast::{
    Block, ConstDecl, Else, Expr, FunctionDecl, IfStmt, MethodDecl, ParameterDecl, Stmt,
    SwitchCase, SwitchStmt, TopLevelDecl, TypeDecl, VarDecl,
};
use crate::tir::{TFunction, TGlobal, TMethod, TStmt};
use crate::token::Span;
use std::collections::HashMap;

impl<'a> Analyzer<'a> {
    pub fn collect_decl(&mut self, decl: &TopLevelDecl) {
        match decl {
            TopLevelDecl::Type(t) => self.collect_type_decl(t),
            TopLevelDecl::Const(c) => self.collect_const_decl(c),
            TopLevelDecl::Var(v) => self.collect_var_decl(v),
            TopLevelDecl::Function(f) => self.collect_func_decl(f),
            TopLevelDecl::Method(_) => {}
        }
    }

    fn collect_type_decl(&mut self, decl: &TypeDecl) {
        let type_id = self.types.named(decl.name.clone(), TypeId::INVALID);
        let sym = self.symbols.add(Symbol::new(
            decl.name.clone(),
            decl.span,
            SymbolKind::Type { type_id },
        ));
        self.try_define(&decl.name, sym, decl.span);
    }

    fn collect_const_decl(&mut self, decl: &ConstDecl) {
        let sym = self.symbols.add(Symbol::new(
            decl.name.clone(),
            decl.span,
            SymbolKind::Const {
                type_id: TypeId::INVALID,
                value: ConstValue::Nil,
            },
        ));
        self.try_define(&decl.name, sym, decl.span);
    }

    fn collect_var_decl(&mut self, decl: &VarDecl) {
        let sym = self.symbols.add(Symbol::new(
            decl.name.clone(),
            decl.span,
            SymbolKind::Var {
                type_id: TypeId::INVALID,
            },
        ));
        self.try_define(&decl.name, sym, decl.span);
    }

    fn collect_func_decl(&mut self, decl: &FunctionDecl) {
        let sym = self.symbols.add(Symbol::new(
            decl.name.clone(),
            decl.span,
            SymbolKind::Func {
                signature: TypeId::INVALID,
            },
        ));
        self.try_define(&decl.name, sym, decl.span);
    }

    pub fn resolve_const_decl(&mut self, decl: &ConstDecl) {
        let explicit_type = decl.typ.as_ref().map(|t| self.resolve_ast_type(t));
        let Some((sym_id, _)) = self.lookup(&decl.name) else {
            return;
        };

        // Check valid type
        if let Some(dt) = explicit_type
            && !self.is_valid_const_type(dt)
        {
            self.error(
                ErrorCode::InvalidConstType,
                decl.span,
                format!("invalid constant type {}", self.types.type_name(dt)),
            );
        }

        let (type_id, value) = self.eval_const_with_type(&decl.value, true);
        let final_type = explicit_type.unwrap_or(type_id);

        self.symbols.get_mut(sym_id).kind = SymbolKind::Const {
            type_id: final_type,
            value,
        };
    }

    pub fn resolve_var_decl_types(&mut self, decl: &VarDecl) {
        let explicit_type = decl.typ.as_ref().map(|t| self.resolve_ast_type(t));
        let Some((sym_id, _)) = self.lookup(&decl.name) else {
            return;
        };

        let value_type = decl.value.as_ref().map(|e| self.infer_expr_type(e));
        let final_type = explicit_type.or(value_type).unwrap_or(TypeId::INVALID);

        self.symbols.update_type(sym_id, final_type);
    }

    pub fn resolve_func_signature(&mut self, decl: &FunctionDecl) {
        let Some((sym_id, _)) = self.lookup(&decl.name) else {
            return;
        };

        let params = self.resolve_params_with_duplicate_check(&decl.signature.params);
        let result = decl
            .signature
            .result
            .as_ref()
            .map(|t| self.resolve_ast_type(t));
        let sig = self.types.signature(params.clone(), result);

        // Check main
        if decl.name == "main" && (!params.is_empty() || result.is_some()) {
            self.error(
                ErrorCode::InvalidMainDecl,
                decl.span,
                "func main must have no arguments and no return values",
            );
            self.errored_funcs.insert(decl.span);
        }

        self.symbols.update_type(sym_id, sig);
    }

    pub fn resolve_method_signature(&mut self, decl: &MethodDecl) {
        let receiver_type = self.resolve_ast_type(&decl.receiver.typ);

        // Check pointer receiver
        let (is_pointer, base_type) = match self.types.get(receiver_type) {
            Type::Pointer { base } => (true, *base),
            _ => (false, receiver_type),
        };

        // Check invalid receiver
        if is_pointer && self.types.is_pointer(base_type).is_some() {
            self.error(
                ErrorCode::InvalidRecv,
                decl.receiver.span,
                format!(
                    "invalid receiver type {}",
                    self.types.type_name(receiver_type)
                ),
            );
            self.errored_methods.insert(decl.name_span);
            return;
        }

        // Check named receiver
        let type_name = match self.types.get(base_type) {
            Type::Named {
                name, underlying, ..
            } => {
                // Underlying not pointer
                let u = self.types.underlying(*underlying);
                if self.types.is_pointer(u).is_some() {
                    self.error(
                        ErrorCode::InvalidRecv,
                        decl.receiver.span,
                        format!("invalid receiver type {} (pointer or interface type)", name),
                    );
                    self.errored_methods.insert(decl.name_span);
                    return;
                }
                name.clone()
            }
            Type::Basic { .. } => {
                self.error(
                    ErrorCode::InvalidRecvBaseType,
                    decl.receiver.span,
                    format!(
                        "cannot define new methods on non-local type {}",
                        self.types.type_name(receiver_type)
                    ),
                );
                self.errored_methods.insert(decl.name_span);
                return;
            }
            Type::Invalid => {
                self.errored_methods.insert(decl.name_span);
                return;
            }
            _ => {
                self.error(
                    ErrorCode::InvalidRecvBaseType,
                    decl.receiver.span,
                    format!(
                        "invalid receiver type {}",
                        self.types.type_name(receiver_type)
                    ),
                );
                self.errored_methods.insert(decl.name_span);
                return;
            }
        };

        // Check duplicate method
        let existing_method = if let Type::Named { methods, .. } = self.types.get(base_type) {
            methods.iter().find(|m| m.name == decl.name).cloned()
        } else {
            None
        };

        if let Some(existing) = existing_method {
            self.error(
                ErrorCode::DuplicateMethod,
                decl.name_span,
                format!(
                    "method {}.{} already declared at {}:{}:{}",
                    type_name,
                    decl.name,
                    self.reporter.file_name(),
                    existing.span.line,
                    existing.span.col
                ),
            );
            self.errored_methods.insert(decl.name_span);
            return;
        }

        // Check field conflict
        if let Some(fields) = self.types.struct_fields(self.types.underlying(base_type))
            && let Some(field) = fields.iter().find(|f| f.name == decl.name)
        {
            self.error_with_related(
                ErrorCode::DuplicateFieldAndMethod,
                decl.name_span,
                format!("field and method with the same name {}", decl.name),
                field.span,
                format!("other declaration of {}", decl.name),
            );
            self.errored_methods.insert(decl.name_span);
            return;
        }

        let params = self.resolve_params_with_duplicate_check(&decl.signature.params);
        let result = decl
            .signature
            .result
            .as_ref()
            .map(|t| self.resolve_ast_type(t));
        let sig = self.types.signature(params, result);

        let method_info = MethodInfo {
            name: decl.name.clone(),
            signature: sig,
            receiver_is_pointer: is_pointer,
            span: decl.name_span,
        };
        self.types.add_method(base_type, method_info);
    }

    pub fn resolve_params(&mut self, params: &[ParameterDecl]) -> Vec<Param> {
        params
            .iter()
            .map(|p| Param {
                name: p.name.clone().unwrap_or_default(),
                type_id: self.resolve_ast_type(&p.typ),
            })
            .collect()
    }

    /// Resolve parameters with duplicate check
    fn resolve_params_with_duplicate_check(&mut self, params: &[ParameterDecl]) -> Vec<Param> {
        let mut seen_params: HashMap<String, Span> = HashMap::new();
        let mut result = Vec::new();

        for param in params {
            let param_type = self.resolve_ast_type(&param.typ);

            if let Some(ref name) = param.name {
                if name != "_" {
                    if let Some(&prev_span) = seen_params.get(name) {
                        self.error_with_related(
                            ErrorCode::DuplicateParam,
                            param.span,
                            format!("{} redeclared in this block", name),
                            prev_span,
                            format!("other declaration of {}", name),
                        );
                    } else {
                        seen_params.insert(name.clone(), param.span);
                    }
                }
                result.push(Param {
                    name: name.clone(),
                    type_id: param_type,
                });
            } else {
                result.push(Param {
                    name: String::new(),
                    type_id: param_type,
                });
            }
        }

        result
    }

    pub fn lower_function(&mut self, decl: &FunctionDecl) -> Option<TFunction> {
        let (sym_id, _) = self.lookup(&decl.name)?;

        // Check previous errors
        let had_sig_errors = self.errored_funcs.contains(&decl.span);

        let sig = self.symbols.get(sym_id).type_id()?;
        let (sig_params, result) = self.types.is_signature(sig)?;

        let func_scope = self
            .scopes
            .new_scope(Some(self.current_scope), ScopeKind::Function);
        let old_scope = self.current_scope;
        self.current_scope = func_scope;
        self.current_func = Some(FuncContext {
            result_type: result,
        });

        let mut params = Vec::new();
        for (i, pd) in decl.signature.params.iter().enumerate() {
            let param_type = sig_params
                .get(i)
                .map(|p| p.type_id)
                .unwrap_or(TypeId::INVALID);
            let name = pd.name.clone().unwrap_or_default();
            let param_sym = self.symbols.add(Symbol::new(
                name.clone(),
                pd.span,
                SymbolKind::Param {
                    type_id: param_type,
                },
            ));
            if !name.is_empty() && name != "_" {
                self.scopes.define(self.current_scope, name, param_sym);
            }
            params.push((param_sym, param_type));
        }

        let body = self.lower_block(&decl.body);

        if result.is_some() && !self.block_returns_ast(&decl.body.stmts) {
            self.error(ErrorCode::MissingReturn, decl.body.end, "missing return");
        }

        // Unused vars
        if !had_sig_errors {
            self.check_unused_vars_in_scope(func_scope);
        }

        self.current_scope = old_scope;
        self.current_func = None;

        Some(TFunction {
            symbol_id: sym_id,
            name: decl.name.clone(),
            params,
            result,
            body,
        })
    }

    pub fn lower_method(&mut self, decl: &MethodDecl) -> Option<TMethod> {
        // Check previous errors
        let had_sig_errors = self.errored_methods.contains(&decl.name_span);

        let receiver_type = self.resolve_ast_type(&decl.receiver.typ);
        let receiver_is_pointer = self.types.is_pointer(receiver_type).is_some();
        let base_type = self
            .types
            .is_pointer(receiver_type)
            .unwrap_or(receiver_type);

        let type_name = match self.types.get(base_type) {
            Type::Named { name, .. } => name.clone(),
            _ => return None,
        };

        let mangled_name = format!("{}.{}", type_name, decl.name);

        let func_scope = self
            .scopes
            .new_scope(Some(self.current_scope), ScopeKind::Function);
        let old_scope = self.current_scope;
        self.current_scope = func_scope;

        let receiver_sym = self.symbols.add(Symbol::new(
            decl.receiver.name.clone(),
            decl.receiver.span,
            SymbolKind::Param {
                type_id: receiver_type,
            },
        ));
        // Mark receiver as used - Go doesn't report unused receiver variables
        self.symbols.mark_used(receiver_sym);
        if !decl.receiver.name.is_empty() && decl.receiver.name != "_" {
            self.scopes
                .define(self.current_scope, decl.receiver.name.clone(), receiver_sym);
        }

        let result = decl
            .signature
            .result
            .as_ref()
            .map(|t| self.resolve_ast_type(t));
        self.current_func = Some(FuncContext {
            result_type: result,
        });

        let sig_params: Vec<_> = decl
            .signature
            .params
            .iter()
            .map(|p| self.resolve_ast_type(&p.typ))
            .collect();
        let mut params = Vec::new();
        for (i, pd) in decl.signature.params.iter().enumerate() {
            let param_type = sig_params.get(i).copied().unwrap_or(TypeId::INVALID);
            let name = pd.name.clone().unwrap_or_default();
            let param_sym = self.symbols.add(Symbol::new(
                name.clone(),
                pd.span,
                SymbolKind::Param {
                    type_id: param_type,
                },
            ));
            if !name.is_empty() && name != "_" {
                self.scopes.define(self.current_scope, name, param_sym);
            }
            params.push((param_sym, param_type));
        }

        let body = self.lower_block(&decl.body);

        if result.is_some() && !self.block_returns_ast(&decl.body.stmts) {
            self.error(ErrorCode::MissingReturn, decl.body.end, "missing return");
        }

        // Only check unused vars if no signature-level errors occurred
        if !had_sig_errors {
            self.check_unused_vars_in_scope(func_scope);
        }

        self.current_scope = old_scope;
        self.current_func = None;

        Some(TMethod {
            receiver_sym,
            receiver_type,
            receiver_is_pointer,
            name: decl.name.clone(),
            mangled_name,
            params,
            result,
            body,
        })
    }

    pub fn lower_global_var(&mut self, decl: &VarDecl) -> Option<TGlobal> {
        let (sym_id, _) = self.lookup(&decl.name)?;

        // Get the already-resolved type from the symbol
        let type_id = self
            .symbols
            .get(sym_id)
            .type_id()
            .unwrap_or(TypeId::INVALID);

        let init = decl
            .value
            .as_ref()
            .map(|e| self.lower_expr(e, Some(type_id)));

        Some(TGlobal {
            symbol_id: sym_id,
            name: decl.name.clone(),
            type_id,
            init,
        })
    }

    /// Evaluate array length
    pub(crate) fn eval_array_len_with_check(&mut self, expr: &Expr, span: Span) -> Option<u64> {
        let (type_id, cv) = self.eval_const_with_type(expr, false);

        // Check constant
        if cv == ConstValue::Nil && type_id == TypeId::INVALID {
            // Non-constant value
            let expr_str = self.expr_string(expr);
            self.error(
                ErrorCode::InvalidArrayLen,
                span,
                format!("invalid array length {}", expr_str),
            );
            return None;
        }

        // Check negative value
        if let Some(n) = cv.to_i64() {
            if n < 0 {
                let type_desc = self.type_description(type_id);
                self.error(
                    ErrorCode::InvalidArrayLen,
                    span,
                    format!("invalid array length {} ({})", n, type_desc),
                );
                return None;
            }
            return Some(n as u64);
        }

        // Check integer type
        let expr_str = self.expr_string(expr);
        let type_desc = self.type_description(type_id);
        self.error(
            ErrorCode::InvalidArrayLen,
            span,
            format!("array length {} ({}) must be integer", expr_str, type_desc),
        );
        None
    }

    pub(crate) fn block_returns_ast(&self, stmts: &[Stmt]) -> bool {
        for stmt in stmts.iter().rev() {
            match stmt {
                Stmt::Return(_, _) => return true,
                Stmt::If(if_stmt) => {
                    // if-else must have both branches return for the whole to return
                    if self.if_returns_ast(if_stmt) {
                        return true;
                    }
                }
                Stmt::Block(b) => {
                    if self.block_returns_ast(&b.stmts) {
                        return true;
                    }
                }
                Stmt::Switch(sw) => {
                    if self.switch_returns_ast(sw) {
                        return true;
                    }
                }
                _ => {}
            }
        }
        false
    }

    /// Check if an if statement returns on all paths
    fn if_returns_ast(&self, if_stmt: &IfStmt) -> bool {
        // The then branch must return
        if !self.block_returns_ast(&if_stmt.body.stmts) {
            return false;
        }
        // There must be an else branch and it must return
        match &if_stmt.else_branch {
            Some(Else::Block(eb)) => self.block_returns_ast(&eb.stmts),
            Some(Else::If(nested)) => self.if_returns_ast(nested),
            None => false,
        }
    }

    /// Check if a switch statement returns on all paths
    fn switch_returns_ast(&self, sw: &SwitchStmt) -> bool {
        let mut has_default = false;
        for case in &sw.cases {
            match &case.case {
                SwitchCase::Default => has_default = true,
                SwitchCase::Expr(_) => {}
            }
            // Each case body must return (or fallthrough which we don't support)
            if !self.block_returns_ast(&case.body) {
                return false;
            }
        }
        // Must have a default case for the switch to be exhaustive
        has_default
    }

    pub(crate) fn lower_block(&mut self, block: &Block) -> Vec<TStmt> {
        block
            .stmts
            .iter()
            .flat_map(|s| self.lower_stmt(s))
            .collect()
    }
}
