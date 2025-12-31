//! Symbol table

use crate::analysis::types::TypeId;
use crate::analysis::value::ConstValue;
use crate::token::Span;

/// Symbol ID
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(u32);

impl SymbolId {
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone)]
pub enum SymbolKind {
    Var { type_id: TypeId },
    Param { type_id: TypeId },
    Const { type_id: TypeId, value: ConstValue },
    Type { type_id: TypeId },
    Func { signature: TypeId },
    Builtin { type_id: TypeId },
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub span: Span,
    pub kind: SymbolKind,
    pub used: bool,
}

impl Symbol {
    pub fn new(name: String, span: Span, kind: SymbolKind) -> Self {
        Self {
            name,
            span,
            kind,
            used: false,
        }
    }

    pub fn type_id(&self) -> Option<TypeId> {
        match &self.kind {
            SymbolKind::Var { type_id } => Some(*type_id),
            SymbolKind::Param { type_id } => Some(*type_id),
            SymbolKind::Const { type_id, .. } => Some(*type_id),
            SymbolKind::Type { type_id } => Some(*type_id),
            SymbolKind::Func { signature } => Some(*signature),
            SymbolKind::Builtin { type_id } => Some(*type_id),
        }
    }

    pub fn const_value(&self) -> Option<&ConstValue> {
        match &self.kind {
            SymbolKind::Const { value, .. } => Some(value),
            _ => None,
        }
    }

    pub fn is_var(&self) -> bool {
        matches!(&self.kind, SymbolKind::Var { .. })
    }
}

pub struct SymbolTable {
    symbols: Vec<Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: Vec::new(),
        }
    }

    pub fn add(&mut self, symbol: Symbol) -> SymbolId {
        let id = SymbolId(self.symbols.len() as u32);
        self.symbols.push(symbol);
        id
    }

    pub fn get(&self, id: SymbolId) -> &Symbol {
        &self.symbols[id.index()]
    }

    pub fn get_mut(&mut self, id: SymbolId) -> &mut Symbol {
        &mut self.symbols[id.index()]
    }

    pub fn mark_used(&mut self, id: SymbolId) {
        self.symbols[id.index()].used = true;
    }

    pub fn update_type(&mut self, id: SymbolId, new_type: TypeId) {
        match &mut self.symbols[id.index()].kind {
            SymbolKind::Var { type_id } => *type_id = new_type,
            SymbolKind::Param { type_id } => *type_id = new_type,
            SymbolKind::Const { type_id, .. } => *type_id = new_type,
            SymbolKind::Type { type_id } => *type_id = new_type,
            SymbolKind::Func { signature } => *signature = new_type,
            SymbolKind::Builtin { type_id } => *type_id = new_type,
        }
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}
