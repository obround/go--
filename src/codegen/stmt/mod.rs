//! Statement code generation from TIR

mod assign;
mod control;
mod range;

use crate::analysis::{SymbolId, TypeId};
use crate::codegen::Codegen;
use crate::tir::{TExpr, TStmt};

impl<'ctx> Codegen<'ctx> {
    pub fn gen_stmt(&mut self, stmt: &TStmt) {
        match stmt {
            TStmt::VarDecl {
                symbol_id,
                type_id,
                init,
            } => self.gen_var_decl(*symbol_id, *type_id, init.as_ref()),
            TStmt::Assign { lhs, rhs } => self.gen_assign(lhs, rhs),
            TStmt::CompoundAssign { kind, lhs, rhs } => self.gen_compound_assign(*kind, lhs, rhs),
            TStmt::IncDec { kind, operand } => self.gen_inc_dec(*kind, operand),
            TStmt::Expr(expr) => {
                self.gen_expr(expr);
            }
            TStmt::Return(expr) => self.gen_return(expr.as_ref()),
            TStmt::Block(stmts) => self.gen_block(stmts),
            TStmt::If {
                cond,
                then_block,
                else_block,
            } => self.gen_if(cond, then_block, else_block.as_deref()),
            TStmt::For { kind, body } => self.gen_for(kind, body),
            TStmt::Switch {
                tag,
                tag_kind,
                cases,
            } => self.gen_switch(tag.as_ref(), tag_kind, cases),
            TStmt::Break => self.gen_break(),
            TStmt::Continue => self.gen_continue(),
        }
    }

    pub fn gen_block(&mut self, stmts: &[TStmt]) {
        for stmt in stmts {
            self.gen_stmt(stmt);
            if self.current_block_terminated() {
                break;
            }
        }
    }

    fn gen_var_decl(&mut self, symbol_id: SymbolId, type_id: TypeId, init: Option<&TExpr>) {
        let alloca = self.create_entry_block_alloca("var", type_id);
        if let Some(init_expr) = init {
            let val = self.gen_expr(init_expr);
            self.builder.build_store(alloca, val).unwrap();
        } else {
            let zero = self.gen_zero(type_id);
            self.builder.build_store(alloca, zero).unwrap();
        }
        self.set_local(symbol_id, alloca, type_id);
    }
}
