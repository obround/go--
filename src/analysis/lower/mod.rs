//! TIR lowering from AST

mod decl;
mod expr;
mod lit;
mod stmt;

use crate::analysis::Analyzer;
use crate::analysis::error::ErrorCode;
use crate::analysis::symbol::SymbolKind;
use crate::analysis::types::{StructField, Type, TypeId};
use crate::ast::{FieldDecl, TopLevelDecl, Type as AstType, TypeDecl};
use crate::tir::TProgram;
use crate::token::Span;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TypeState {
    Unresolved,
    Resolving,
    Resolved,
}

pub(crate) struct TypeResolveInfo<'a> {
    pub name: String,
    pub type_id: TypeId,
    pub decl: &'a TypeDecl,
    pub state: TypeState,
}

pub(crate) struct ResolutionContext<'a> {
    pub type_infos: Vec<TypeResolveInfo<'a>>,
    pub resolving_stack: Vec<usize>,
}

impl<'a> ResolutionContext<'a> {
    pub(crate) fn new() -> Self {
        Self {
            type_infos: Vec::new(),
            resolving_stack: Vec::new(),
        }
    }

    pub(crate) fn add(&mut self, name: String, type_id: TypeId, decl: &'a TypeDecl) {
        self.type_infos.push(TypeResolveInfo {
            name,
            type_id,
            decl,
            state: TypeState::Unresolved,
        });
    }

    fn find(&self, name: &str) -> Option<usize> {
        self.type_infos.iter().position(|info| info.name == name)
    }

    pub(crate) fn len(&self) -> usize {
        self.type_infos.len()
    }
}

impl<'a> Analyzer<'a> {
    pub fn lower_program(&mut self) -> TProgram {
        let mut functions = Vec::new();
        let mut methods = Vec::new();
        let mut globals = Vec::new();

        for decl in &self.ast.decls {
            match decl {
                TopLevelDecl::Function(f) => {
                    if let Some(func) = self.lower_function(f) {
                        functions.push(func);
                    }
                }
                TopLevelDecl::Method(m) => {
                    if let Some(method) = self.lower_method(m) {
                        methods.push(method);
                    }
                }
                TopLevelDecl::Var(v) => {
                    if let Some(global) = self.lower_global_var(v) {
                        globals.push(global);
                    }
                }
                TopLevelDecl::Type(_) | TopLevelDecl::Const(_) => {}
            }
        }

        TProgram {
            functions,
            methods,
            globals,
        }
    }

    pub fn collect_declarations(&mut self) {
        for decl in &self.ast.decls {
            self.collect_decl(decl);
        }
    }

    pub fn resolve_types(&mut self) {
        let decls: Vec<_> = self.ast.decls.iter().collect();

        // Initialize context
        let mut ctx = ResolutionContext::new();
        for decl in &decls {
            if let TopLevelDecl::Type(type_decl) = decl
                && let Some((sym_id, _)) = self.lookup(&type_decl.name)
            {
                let sym = self.symbols.get(sym_id);
                if let SymbolKind::Type { type_id } = &sym.kind {
                    ctx.add(type_decl.name.clone(), *type_id, type_decl);
                }
            }
        }

        // Resolve all types
        for i in 0..ctx.len() {
            self.resolve_type_at_index(i, &mut ctx);
        }

        // Resolve signatures
        for decl in &decls {
            match decl {
                TopLevelDecl::Function(f) => self.resolve_func_signature(f),
                TopLevelDecl::Method(m) => self.resolve_method_signature(m),
                _ => {}
            }
        }

        // Resolve bodies/vars
        for decl in &decls {
            match decl {
                TopLevelDecl::Type(_) | TopLevelDecl::Function(_) | TopLevelDecl::Method(_) => {}
                TopLevelDecl::Const(c) => self.resolve_const_decl(c),
                TopLevelDecl::Var(v) => self.resolve_var_decl_types(v),
            }
        }
    }

    pub(crate) fn resolve_type_at_index(&mut self, idx: usize, ctx: &mut ResolutionContext<'_>) {
        // If already resolved, nothing to do
        if ctx.type_infos[idx].state == TypeState::Resolved {
            return;
        }

        // Direct cycle
        if ctx.type_infos[idx].state == TypeState::Resolving {
            self.error(
                ErrorCode::InvalidTypeCycle,
                ctx.type_infos[idx].decl.span,
                format!("invalid recursive type {}", ctx.type_infos[idx].name),
            );
            return;
        }

        // Mark resolving
        ctx.type_infos[idx].state = TypeState::Resolving;
        ctx.resolving_stack.push(idx);
        let underlying = self.resolve_ast_type_with_ctx(&ctx.type_infos[idx].decl.typ, Some(ctx));
        ctx.resolving_stack.pop();

        let type_id = ctx.type_infos[idx].type_id;
        if let Type::Named { underlying: u, .. } = self.types.get_mut(type_id) {
            *u = underlying;
        }

        ctx.type_infos[idx].state = TypeState::Resolved;
    }

    /// Resolve AST type
    pub(crate) fn resolve_ast_type_with_ctx(
        &mut self,
        ast_type: &AstType,
        mut ctx: Option<&mut ResolutionContext<'_>>,
    ) -> TypeId {
        match ast_type {
            AstType::Name(span, name) => {
                // User-defined type resolution
                if let Some(ref mut ctx) = ctx
                    && let Some(idx) = ctx.find(name)
                {
                    match ctx.type_infos[idx].state {
                        TypeState::Resolving => {
                            // Direct cycle
                            if let Some(start_pos) =
                                ctx.resolving_stack.iter().position(|&i| i == idx)
                            {
                                let cycle: Vec<usize> = ctx.resolving_stack[start_pos..].to_vec();

                                // Edges
                                for w in cycle.windows(2) {
                                    let from = &ctx.type_infos[w[0]];
                                    let to = &ctx.type_infos[w[1]];
                                    self.error(
                                        ErrorCode::InvalidTypeCycle,
                                        from.decl.span,
                                        format!("{} refers to {}", from.name, to.name),
                                    );
                                }
                                // Close cycle
                                if let (Some(&last_idx), Some(&first_idx)) =
                                    (cycle.last(), cycle.first())
                                {
                                    let from_name = ctx.type_infos[last_idx].name.clone();
                                    let from_span = ctx.type_infos[last_idx].decl.span;
                                    let to_name = ctx.type_infos[first_idx].name.clone();
                                    let to_span = ctx.type_infos[first_idx].decl.span;

                                    self.error(
                                        ErrorCode::InvalidTypeCycle,
                                        from_span,
                                        format!("{} refers to {}", from_name, to_name),
                                    );
                                    // Primary diagnostic
                                    self.error(
                                        ErrorCode::InvalidTypeCycle,
                                        to_span,
                                        format!("invalid recursive type {}", to_name),
                                    );

                                    // Mark invalid
                                    let invalid_id = self.types.invalid();
                                    for cyc_idx in cycle {
                                        ctx.type_infos[cyc_idx].state = TypeState::Resolved;
                                        let type_id = ctx.type_infos[cyc_idx].type_id;
                                        if let Type::Named { underlying: u, .. } =
                                            self.types.get_mut(type_id)
                                        {
                                            *u = invalid_id;
                                        }
                                    }
                                }
                            } else {
                                self.error(
                                    ErrorCode::InvalidTypeCycle,
                                    *span,
                                    format!("invalid recursive type {}", name),
                                );
                            }

                            return self.types.invalid();
                        }
                        TypeState::Unresolved => {
                            self.resolve_type_at_index(idx, ctx);
                        }
                        TypeState::Resolved => {}
                    }
                    return ctx.type_infos[idx].type_id;
                }
                self.resolve_type_name(*span, name)
            }

            AstType::Qualified(span, pkg, name) => {
                // Import check
                let pkg_is_imported = self
                    .ast
                    .imports
                    .iter()
                    .any(|imp| imp.path.rsplit('/').next().unwrap_or(&imp.path) == pkg);

                if !pkg_is_imported {
                    self.error(
                        ErrorCode::UndeclaredName,
                        *span,
                        format!("undefined: {}", pkg),
                    );
                } else {
                    self.error(
                        ErrorCode::UndeclaredImportedName,
                        *span,
                        format!("undefined: {}.{}", pkg, name),
                    );
                }
                self.types.invalid()
            }

            // Indirection breaks cycles
            AstType::Pointer(_, inner) => {
                let base = self.resolve_ast_type_with_ctx(inner, None);
                self.types.pointer(base)
            }
            AstType::Slice(_, elem) => {
                let elem_type = self.resolve_ast_type_with_ctx(elem, None);
                self.types.slice(elem_type)
            }
            AstType::Map(span, key, value) => {
                let key_type = self.resolve_ast_type_with_ctx(key, None);
                let value_type = self.resolve_ast_type_with_ctx(value, None);
                if !self.types.is_comparable(key_type) {
                    self.error(
                        ErrorCode::IncomparableMapKey,
                        *span,
                        format!("invalid map key type {}", self.types.type_name(key_type)),
                    );
                }
                self.types.map(key_type, value_type)
            }

            AstType::Array(span, len_expr, elem) => {
                let elem_type = self.resolve_ast_type_with_ctx(elem, ctx);
                match self.eval_array_len_with_check(len_expr, *span) {
                    Some(len) => self.types.array(len, elem_type),
                    None => self.types.invalid(),
                }
            }
            AstType::Struct(_, fields) => self.resolve_struct_type_with_ctx(fields, ctx),
            AstType::Function(_, sig) => {
                let params = self.resolve_params(&sig.params);
                let result = sig
                    .result
                    .as_ref()
                    .map(|t| self.resolve_ast_type_with_ctx(t, ctx));
                self.types.signature(params, result)
            }
            AstType::Paren(_, inner) => self.resolve_ast_type_with_ctx(inner, ctx),
        }
    }

    pub(crate) fn resolve_struct_type_with_ctx(
        &mut self,
        fields: &[FieldDecl],
        mut ctx: Option<&mut ResolutionContext<'_>>,
    ) -> TypeId {
        let mut struct_fields = Vec::new();
        let mut seen_fields: HashMap<String, Span> = HashMap::new();

        for f in fields {
            let field_type = self.resolve_ast_type_with_ctx(&f.typ, ctx.as_deref_mut());
            for name in &f.names {
                // Duplicate field
                if let Some(&prev_span) = seen_fields.get(name) {
                    self.error_with_related(
                        ErrorCode::DuplicateField,
                        f.span,
                        format!("{} redeclared", name),
                        prev_span,
                        format!("other declaration of {}", name),
                    );
                } else {
                    seen_fields.insert(name.clone(), f.span);
                }

                struct_fields.push(StructField {
                    name: name.clone(),
                    type_id: field_type,
                    span: f.span,
                });
            }
        }
        self.types.structure(struct_fields)
    }

    /// Simple type resolution (no cycle check)
    pub(crate) fn resolve_type_name(&mut self, span: Span, name: &str) -> TypeId {
        if let Some((sym_id, _)) = self.lookup(name) {
            if let SymbolKind::Type { type_id } = self.symbols.get(sym_id).kind {
                return type_id;
            }
            self.error(ErrorCode::NotAType, span, format!("{} is not a type", name));
        } else {
            self.error(
                ErrorCode::UndeclaredName,
                span,
                format!("undefined: {}", name),
            );
        }
        self.types.invalid()
    }

    pub(crate) fn resolve_ast_type(&mut self, ty: &AstType) -> TypeId {
        self.resolve_ast_type_with_ctx(ty, None)
    }

    pub(crate) fn resolve_struct_type(&mut self, fields: &[FieldDecl]) -> TypeId {
        self.resolve_struct_type_with_ctx(fields, None)
    }
}
