//! Scope tree

use crate::analysis::symbol::SymbolId;
use std::collections::HashMap;

/// Scope ID
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(u32);

impl ScopeId {
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    Universe,
    Package,
    Function,
    Block,
    For,
    Switch,
}

/// Lexical scope with definitions
#[derive(Debug)]
pub struct Scope {
    pub parent: Option<ScopeId>,
    pub kind: ScopeKind,
    symbols: HashMap<String, SymbolId>,
}

impl Scope {
    fn new(parent: Option<ScopeId>, kind: ScopeKind) -> Self {
        Self {
            parent,
            kind,
            symbols: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, sym_id: SymbolId) {
        self.symbols.insert(name, sym_id);
    }

    pub fn lookup_local(&self, name: &str) -> Option<SymbolId> {
        self.symbols.get(name).copied()
    }

    pub fn local_symbols(&self) -> impl Iterator<Item = (&String, &SymbolId)> {
        self.symbols.iter()
    }
}

/// Tree of lexical scopes
pub struct ScopeTree {
    scopes: Vec<Scope>,
}

impl ScopeTree {
    pub fn new() -> Self {
        Self { scopes: Vec::new() }
    }

    pub fn new_scope(&mut self, parent: Option<ScopeId>, kind: ScopeKind) -> ScopeId {
        let id = ScopeId(self.scopes.len() as u32);
        self.scopes.push(Scope::new(parent, kind));
        id
    }

    pub fn define(&mut self, scope_id: ScopeId, name: String, sym_id: SymbolId) {
        self.scopes[scope_id.index()].define(name, sym_id);
    }

    /// Lookup name walking up scope tree
    pub fn lookup(&self, scope_id: ScopeId, name: &str) -> Option<(SymbolId, ScopeId)> {
        let mut current = Some(scope_id);
        while let Some(id) = current {
            let scope = &self.scopes[id.index()];
            if let Some(sym_id) = scope.lookup_local(name) {
                return Some((sym_id, id));
            }
            current = scope.parent;
        }
        None
    }

    /// Lookup name in local scope only
    pub fn lookup_local(&self, scope_id: ScopeId, name: &str) -> Option<SymbolId> {
        self.scopes[scope_id.index()].lookup_local(name)
    }

    /// Iterator over local symbols
    pub fn local_symbols(&self, scope_id: ScopeId) -> impl Iterator<Item = (&String, &SymbolId)> {
        self.scopes[scope_id.index()].local_symbols()
    }

    /// Check if scope is inside a loop
    pub fn is_in_loop(&self, scope_id: ScopeId) -> bool {
        let mut current = Some(scope_id);
        while let Some(id) = current {
            let scope = &self.scopes[id.index()];
            match scope.kind {
                ScopeKind::For => return true,
                ScopeKind::Function => return false,
                _ => current = scope.parent,
            }
        }
        false
    }

    /// Check if scope is inside loop or switch
    pub fn is_in_switch_or_loop(&self, scope_id: ScopeId) -> bool {
        let mut current = Some(scope_id);
        while let Some(id) = current {
            let scope = &self.scopes[id.index()];
            match scope.kind {
                ScopeKind::For | ScopeKind::Switch => return true,
                ScopeKind::Function => return false,
                _ => current = scope.parent,
            }
        }
        false
    }
}

impl Default for ScopeTree {
    fn default() -> Self {
        Self::new()
    }
}
