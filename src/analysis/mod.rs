//! Semantic analysis
//!
//! Analysis proceeds in three phases:
//! 1. Collect declarations
//! 2. Resolve types
//! 3. Lower to TIR

mod error;
mod lower;
mod scope;
mod symbol;
mod typeops;
mod types;
mod universe;
mod value;

pub use error::{ErrorCode, ErrorReporter, SemanticError};
pub use scope::{ScopeId, ScopeKind, ScopeTree};
pub use symbol::{SymbolId, SymbolKind, SymbolTable};
pub use types::{BasicType, Type, TypeArena, TypeId, UntypedKind};
pub use universe::Universe;
pub use value::ConstValue;

use std::borrow::Cow;
use std::collections::HashSet;

use crate::analysis::types::BuiltinKind;
use crate::ast::{Expr, SourceFile, UnaryOp};
use crate::tir::TProgram;
use crate::token::Span;

/// The semantic analyzer that checks the AST and produces TIR
pub struct Analyzer<'a> {
    // Input
    pub(crate) ast: &'a SourceFile,

    // Type system
    pub(crate) types: TypeArena,
    pub(crate) symbols: SymbolTable,
    pub(crate) scopes: ScopeTree,
    pub(crate) universe: Universe,

    // Current analysis state
    pub(crate) current_scope: ScopeId,
    pub(crate) current_func: Option<FuncContext>,

    // Error reporting
    pub(crate) reporter: ErrorReporter,

    // Methods with signature errors
    pub(crate) errored_methods: HashSet<Span>,

    // Functions with signature errors
    pub(crate) errored_funcs: HashSet<Span>,
}

/// Analysis result
pub struct AnalysisResult {
    pub program: TProgram,
    pub types: TypeArena,
    pub universe: Universe,
}

pub struct FuncContext {
    pub result_type: Option<TypeId>,
}

impl<'a> Analyzer<'a> {
    pub fn new(ast: &'a SourceFile, file: &str) -> Self {
        let mut types = TypeArena::new();
        let mut symbols = SymbolTable::new();
        let mut scopes = ScopeTree::new();

        let universe = Universe::new(&mut types, &mut symbols, &mut scopes);
        let pkg_scope = scopes.new_scope(Some(universe.scope_id), ScopeKind::Package);

        Self {
            ast,
            types,
            symbols,
            scopes,
            universe,
            current_scope: pkg_scope,
            current_func: None,
            reporter: ErrorReporter::new(file),
            errored_methods: HashSet::new(),
            errored_funcs: HashSet::new(),
        }
    }

    pub fn analyze(mut self) -> Result<AnalysisResult, Vec<SemanticError>> {
        // Phase 1: Collect all top-level declarations
        self.collect_declarations();

        // Phase 2: Resolve type definitions and global declarations
        self.resolve_types();

        // Phase 3: Semantic checking and lower to TIR
        let program = self.lower_program();

        // Collect all errors from all phases
        let errors = self.reporter.take_errors();
        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(AnalysisResult {
            program,
            types: self.types,
            universe: self.universe,
        })
    }

    pub(crate) fn error(&mut self, code: ErrorCode, span: Span, message: impl Into<String>) {
        self.reporter.error(code, span, message);
    }

    pub(crate) fn error_with_related(
        &mut self,
        code: ErrorCode,
        span: Span,
        message: impl Into<String>,
        related_span: Span,
        related_message: impl Into<String>,
    ) {
        self.reporter
            .error_with_related(code, span, message, related_span, related_message);
    }

    pub(crate) fn file(&self) -> &str {
        self.reporter.file()
    }

    #[inline]
    pub(crate) fn is_assignable_to(
        &self,
        value_type: TypeId,
        target_type: TypeId,
        const_val: Option<&ConstValue>,
    ) -> bool {
        typeops::is_assignable_to(&self.types, value_type, target_type, const_val)
    }

    #[inline]
    pub(crate) fn can_convert(&self, from: TypeId, to: TypeId) -> bool {
        typeops::can_convert(&self.types, from, to)
    }

    #[inline]
    pub(crate) fn default_type(&self, type_id: TypeId) -> TypeId {
        typeops::default_type(&self.types, type_id)
    }

    #[inline]
    pub(crate) fn try_define(&mut self, name: &str, sym_id: SymbolId, span: Span) -> bool {
        if name == "_" {
            return true;
        }
        if let Some((existing_id, scope)) = self.scopes.lookup(self.current_scope, name)
            && scope == self.current_scope
        {
            let existing = self.symbols.get(existing_id);
            self.error_with_related(
                ErrorCode::DuplicateDecl,
                span,
                format!("{} redeclared in this block", name),
                existing.span,
                format!("other declaration of {}", name),
            );
            return false;
        }
        self.scopes
            .define(self.current_scope, name.to_string(), sym_id);
        true
    }

    pub(crate) fn infer_expr_type(&mut self, expr: &Expr) -> TypeId {
        match expr {
            Expr::IntLit(_, _) => self.universe.untyped_int,
            Expr::FloatLit(_, _) => self.universe.untyped_float,
            Expr::StringLit(_, _) => self.universe.untyped_string,
            Expr::Ident(_, name) => {
                if let Some((sym_id, _)) = self.lookup(name) {
                    self.symbols
                        .get(sym_id)
                        .type_id()
                        .unwrap_or(TypeId::INVALID)
                } else {
                    TypeId::INVALID
                }
            }
            Expr::Paren(_, inner) => self.infer_expr_type(inner),
            _ => TypeId::INVALID,
        }
    }

    pub(crate) fn is_valid_const_type(&self, type_id: TypeId) -> bool {
        let underlying = self.types.underlying(type_id);
        matches!(
            self.types.get(underlying),
            Type::Basic { .. } | Type::Invalid
        )
    }

    pub(crate) fn const_representable(&self, cv: &ConstValue, basic: BasicType) -> bool {
        cv.fits_type(basic)
    }

    pub(crate) fn current_func_result(&self) -> Option<TypeId> {
        self.current_func.as_ref().and_then(|f| f.result_type)
    }

    pub(crate) fn lookup(&self, name: &str) -> Option<(SymbolId, ScopeId)> {
        self.scopes.lookup(self.current_scope, name)
    }

    /// String representation of expression for error messages
    pub(crate) fn expr_string(&self, expr: &Expr) -> String {
        match expr {
            Expr::Ident(_span, name) => name.clone(),
            Expr::IntLit(_span, s) | Expr::FloatLit(_span, s) | Expr::StringLit(_span, s) => {
                s.clone()
            }
            Expr::Binary(_span, lhs, op, rhs) => {
                format!(
                    "{} {} {}",
                    self.expr_string(lhs),
                    op.name(),
                    self.expr_string(rhs)
                )
            }
            Expr::Unary(_span, op, operand) => {
                format!("{}{}", op.name(), self.expr_string(operand))
            }
            Expr::Call(_span, callee, _) => format!("{}()", self.expr_string(callee)),
            Expr::Selector(_span, base, field) => {
                format!("{}.{}", self.expr_string(base), field)
            }
            Expr::Index(_span, base, index) => {
                format!("{}[{}]", self.expr_string(base), self.expr_string(index))
            }
            Expr::Slice(_span, base, _, _) => format!("{}[:]", self.expr_string(base)),
            Expr::Paren(_span, inner) => self.expr_string(inner),
            Expr::CompositeLit(_span, _, _) => "{...}".to_string(),
            Expr::TypeVal(_span, _) => "<type>".to_string(),
        }
    }

    pub(crate) fn expr_string_parens(&self, expr: &Expr) -> String {
        match expr {
            Expr::Binary(_span, _, _, _) => format!("({})", self.expr_string(expr)),
            Expr::Paren(_span, inner) => self.expr_string_parens(inner),
            _ => self.expr_string(expr),
        }
    }

    pub(crate) fn builtin_call_text(&self, name: &str, args: &[Expr]) -> String {
        let args_text: Vec<String> = args.iter().map(|a| self.expr_string(a)).collect();
        format!("{}({})", name, args_text.join(", "))
    }

    pub(crate) fn type_description(&self, type_id: TypeId) -> Cow<'_, str> {
        if let Some(uk) = self.types.is_untyped(type_id) {
            (match uk {
                UntypedKind::Bool => "untyped bool constant",
                UntypedKind::Int => "untyped int constant",
                UntypedKind::Float => "untyped float constant",
                UntypedKind::String => "untyped string constant",
                UntypedKind::Nil => "untyped nil",
            })
            .into()
        } else {
            format!("variable of type {}", self.types.type_name(type_id)).into()
        }
    }

    /// Type description for expression
    pub(crate) fn expr_type_description(&self, expr: &Expr, type_id: TypeId) -> String {
        match expr {
            Expr::IntLit(_span, _) => "untyped int constant".to_string(),
            Expr::FloatLit(_span, _) => "untyped float constant".to_string(),
            Expr::StringLit(_span, _) => "untyped string constant".to_string(),
            Expr::Ident(_span, name) if name == "true" || name == "false" => {
                "untyped bool constant".to_string()
            }
            Expr::Paren(_span, inner) => self.expr_type_description(inner, type_id),
            Expr::Unary(_span, UnaryOp::Neg, inner) | Expr::Unary(_span, UnaryOp::Pos, inner) => {
                self.expr_type_description(inner, type_id)
            }
            Expr::Call(_span, _, _) => {
                format!("value of type {}", self.types.type_name(type_id))
            }
            Expr::Index(_span, base, _) => {
                if let Some(base_type) = self.infer_base_type(base) {
                    if self.types.is_map(base_type).is_some() {
                        return format!(
                            "map index expression of type {}",
                            self.types.type_name(type_id)
                        );
                    }
                    // String index returns byte
                    if self.types.is_string(base_type) {
                        return "value of type byte".to_string();
                    }
                }
                format!("value of type {}", self.types.type_name(type_id))
            }
            _ => {
                if let Some(uk) = self.types.is_untyped(type_id) {
                    match uk {
                        UntypedKind::Bool => "untyped bool constant".to_string(),
                        UntypedKind::Int => "untyped int constant".to_string(),
                        UntypedKind::Float => "untyped float constant".to_string(),
                        UntypedKind::String => "untyped string constant".to_string(),
                        UntypedKind::Nil => "untyped nil".to_string(),
                    }
                } else {
                    if let Type::Named {
                        name, underlying, ..
                    } = self.types.get(type_id)
                    {
                        let underlying_name = self.types.type_name(*underlying);
                        format!("variable of {} type {}", underlying_name, name)
                    } else {
                        format!("variable of type {}", self.types.type_name(type_id))
                    }
                }
            }
        }
    }

    pub(crate) fn is_expr_assignable(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Ident(_span, name) => {
                if name == "_" {
                    return true;
                }
                if let Some((sym_id, _)) = self.lookup(name) {
                    matches!(
                        self.symbols.get(sym_id).kind,
                        SymbolKind::Var { .. } | SymbolKind::Param { .. }
                    )
                } else {
                    false
                }
            }
            Expr::Selector(_span, base, _) => {
                // Struct field on map index is not assignable
                if let Expr::Index(_span, index_base, _) = base.as_ref()
                    && let Some(type_id) = self.infer_base_type(index_base)
                    && self.types.is_map(type_id).is_some()
                {
                    return false;
                }
                true
            }
            Expr::Index(_span, base, _) => {
                // String index not assignable
                if let Some(type_id) = self.infer_base_type(base)
                    && self.types.is_string(type_id)
                {
                    return false;
                }
                // Also check for string literal bases
                if matches!(base.as_ref(), Expr::StringLit(_span, _)) {
                    return false;
                }
                // Map indices and array/slice indices are assignable
                true
            }
            Expr::Unary(_span, UnaryOp::Deref, _) => true,
            Expr::Paren(_span, inner) => self.is_expr_assignable(inner),
            _ => false,
        }
    }

    /// Check if expression is addressable
    pub(crate) fn is_expr_addressable(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Ident(_span, name) => {
                if name == "_" {
                    return false;
                }
                if let Some((sym_id, _)) = self.lookup(name) {
                    // Variables are addressable, constants are not
                    matches!(
                        self.symbols.get(sym_id).kind,
                        SymbolKind::Var { .. } | SymbolKind::Param { .. }
                    )
                } else {
                    false
                }
            }
            Expr::Selector(_span, base, _) => self.is_expr_addressable(base),
            Expr::Index(_span, base, _) => {
                let base_type = self.infer_base_type(base);
                if let Some(underlying) = base_type {
                    if self.types.is_map(underlying).is_some() {
                        return false;
                    }
                    if self.types.is_string(underlying) {
                        return false;
                    }
                    if self.types.is_slice(underlying).is_some() {
                        return true;
                    }
                    if self.types.is_array(underlying).is_some() {
                        return self.is_expr_addressable(base);
                    }
                }
                // Default: not addressable
                false
            }
            Expr::Unary(_span, UnaryOp::Deref, _) => true,
            Expr::Paren(_span, inner) => self.is_expr_addressable(inner),
            Expr::CompositeLit(_span, _, _) => true,
            _ => false,
        }
    }

    /// Infer underlying type of expression base
    fn infer_base_type(&self, expr: &Expr) -> Option<TypeId> {
        match expr {
            Expr::Ident(_span, name) => {
                if let Some((sym_id, _)) = self.lookup(name) {
                    self.symbols
                        .get(sym_id)
                        .type_id()
                        .map(|t| self.types.underlying(t))
                } else {
                    None
                }
            }
            Expr::StringLit(_span, _) => Some(self.universe.string_type),
            Expr::Paren(_span, inner) => self.infer_base_type(inner),
            _ => None,
        }
    }

    /// Check for unused variables in scope
    pub(crate) fn check_unused_vars_in_scope(&mut self, scope_id: ScopeId) {
        let symbols: Vec<_> = self
            .scopes
            .local_symbols(scope_id)
            .map(|(name, &sym_id)| (name.clone(), sym_id))
            .collect();

        for (name, sym_id) in symbols {
            let sym = self.symbols.get(sym_id);
            if !sym.used && sym.is_var() && !name.starts_with('_') {
                self.error(
                    ErrorCode::UnusedVar,
                    sym.span,
                    format!("declared and not used: {}", name),
                );
            }
        }
    }

    pub(crate) fn is_map_struct_field_access(&self, expr: &Expr) -> bool {
        if let Expr::Selector(_span, base, _) = expr
            && let Expr::Index(_span, index_base, _) = base.as_ref()
        {
            if let Some(type_id) = self.infer_base_type(index_base) {
                return self.types.is_map(type_id).is_some();
            }
        }
        false
    }

    pub(crate) fn is_map_index_expr(&self, expr: &Expr) -> bool {
        if let Expr::Index(_span, base, _) = expr
            && let Some(type_id) = self.infer_base_type(base)
        {
            return self.types.is_map(type_id).is_some();
        }
        false
    }

    pub(crate) fn is_string_index_expr(&self, expr: &Expr) -> bool {
        if let Expr::Index(_span, base, _) = expr
            && let Some(type_id) = self.infer_base_type(base)
        {
            return self.types.is_string(type_id);
        }
        false
    }

    pub(crate) fn is_void_call(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Call(_span, callee, _) => {
                let sig_type = match callee.as_ref() {
                    Expr::Ident(_span, name) => {
                        if let Some((sym_id, _)) = self.lookup(name) {
                            let sym = self.symbols.get(sym_id);
                            match &sym.kind {
                                SymbolKind::Func { signature } => Some(*signature),
                                SymbolKind::Builtin { type_id } => {
                                    if let Some(b) = self.types.is_builtin(*type_id) {
                                        return matches!(
                                            b,
                                            BuiltinKind::Println
                                                | BuiltinKind::Print
                                                | BuiltinKind::Delete
                                                | BuiltinKind::Panic
                                        );
                                    }
                                    None
                                }
                                _ => None,
                            }
                        } else {
                            None
                        }
                    }
                    Expr::Selector(_, base, method_name) => {
                        if let Some(base_type) = self.infer_base_type(base) {
                            self.find_method_signature(base_type, method_name)
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

                if let Some(sig) = sig_type
                    && let Some((_, result)) = self.types.is_signature(sig)
                {
                    return result.is_none();
                }
                false
            }
            Expr::Paren(_, inner) => self.is_void_call(inner),
            _ => false,
        }
    }

    /// Find method signature on type
    fn find_method_signature(&self, type_id: TypeId, method_name: &str) -> Option<TypeId> {
        let check_type = |ty: TypeId| -> Option<TypeId> {
            if let Type::Named { methods, .. } = self.types.get(ty) {
                for m in methods {
                    if m.name == method_name {
                        return Some(m.signature);
                    }
                }
            }
            None
        };

        if let Some(sig) = check_type(type_id) {
            return Some(sig);
        }

        if let Some(elem) = self.types.is_pointer(type_id) {
            return check_type(elem);
        }

        None
    }
}
