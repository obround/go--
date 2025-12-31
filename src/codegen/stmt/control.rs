//! Control flow code generation from TIR

use inkwell::values::{BasicValueEnum, IntValue};
use inkwell::{FloatPredicate, IntPredicate};

use crate::codegen::Codegen;
use crate::tir::{TCase, TExpr, TForKind, TStmt, TSwitchKind};

impl<'ctx> Codegen<'ctx> {
    pub fn gen_return(&mut self, expr: Option<&TExpr>) {
        match expr {
            Some(e) => {
                let val = self.gen_expr(e);
                self.builder.build_return(Some(&val)).unwrap();
            }
            None => {
                self.builder.build_return(None).unwrap();
            }
        }
    }

    pub fn gen_break(&mut self) {
        let target = self.break_target();
        self.builder.build_unconditional_branch(target).unwrap();
    }

    pub fn gen_continue(&mut self) {
        let target = self.continue_target();
        self.builder.build_unconditional_branch(target).unwrap();
    }

    pub fn gen_if(&mut self, cond: &TExpr, then_block: &[TStmt], else_block: Option<&[TStmt]>) {
        let then_bb = self.append_block("if.then");
        let else_bb = self.append_block("if.else");
        let merge_bb = self.append_block("if.merge");

        // Condition
        let cond_val = self.gen_expr(cond).into_int_value();
        self.build_cond_br(cond_val, then_bb, else_bb);

        // Then block
        self.position_at_end(then_bb);
        self.gen_block(then_block);
        self.build_br_if_needed(merge_bb);

        // Else block
        self.position_at_end(else_bb);
        if let Some(else_stmts) = else_block {
            self.gen_block(else_stmts);
        }
        self.build_br_if_needed(merge_bb);

        self.position_at_end(merge_bb);
    }

    pub fn gen_switch(&mut self, tag: Option<&TExpr>, tag_kind: &TSwitchKind, cases: &[TCase]) {
        let merge_block = self.append_block("switch.merge");

        // Tag
        let tag_val = tag
            .map(|t| self.gen_expr(t))
            .unwrap_or_else(|| self.context.bool_type().const_int(1, false).into());

        // Default case
        let mut default_idx = None;
        for (i, case) in cases.iter().enumerate() {
            if case.values.is_empty() {
                default_idx = Some(i);
            }
        }

        // Case blocks
        let case_blocks: Vec<_> = (0..cases.len())
            .map(|i| self.append_block(&format!("case.{}", i)))
            .collect();

        let default_block = default_idx.map(|i| case_blocks[i]).unwrap_or(merge_block);

        // Case checks
        let mut current_check = self.builder.get_insert_block().unwrap();

        for (i, case) in cases.iter().enumerate() {
            if !case.values.is_empty() {
                self.position_at_end(current_check);

                // Case value
                let mut any_match: Option<IntValue<'ctx>> = None;

                for case_expr in &case.values {
                    let case_val = self.gen_expr(case_expr);
                    let matches = self.gen_switch_compare(tag_val, case_val, tag_kind);

                    any_match = Some(match any_match {
                        Some(prev) => self.builder.build_or(prev, matches, "").unwrap(),
                        None => matches,
                    });
                }

                let next_check = self.append_block(&format!("case.check.{}", i + 1));
                if let Some(cond) = any_match {
                    self.build_cond_br(cond, case_blocks[i], next_check);
                }
                current_check = next_check;
            }
        }

        // Last check falls through to default
        self.position_at_end(current_check);
        self.builder
            .build_unconditional_branch(default_block)
            .unwrap();

        // Generate case bodies
        self.enter_switch(merge_block);

        for (i, case) in cases.iter().enumerate() {
            self.position_at_end(case_blocks[i]);
            for stmt in &case.body {
                self.gen_stmt(stmt);
                if self.current_block_terminated() {
                    break;
                }
            }
            // Implicit break
            self.build_br_if_needed(merge_block);
        }

        self.exit_switch();
        self.position_at_end(merge_block);
    }

    fn gen_switch_compare(
        &mut self,
        tag_val: BasicValueEnum<'ctx>,
        case_val: BasicValueEnum<'ctx>,
        tag_kind: &TSwitchKind,
    ) -> IntValue<'ctx> {
        match tag_kind {
            TSwitchKind::Bool | TSwitchKind::Int => self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    tag_val.into_int_value(),
                    case_val.into_int_value(),
                    "case.cmp",
                )
                .unwrap(),

            TSwitchKind::Float => self
                .builder
                .build_float_compare(
                    FloatPredicate::OEQ,
                    tag_val.into_float_value(),
                    case_val.into_float_value(),
                    "case.cmp",
                )
                .unwrap(),

            TSwitchKind::String => self.gen_string_eq(tag_val, case_val),

            TSwitchKind::Pointer => {
                let tag_int = self
                    .builder
                    .build_ptr_to_int(tag_val.into_pointer_value(), self.context.i64_type(), "")
                    .unwrap();
                let case_int = self
                    .builder
                    .build_ptr_to_int(case_val.into_pointer_value(), self.context.i64_type(), "")
                    .unwrap();
                self.builder
                    .build_int_compare(IntPredicate::EQ, tag_int, case_int, "case.cmp")
                    .unwrap()
            }
        }
    }

    pub fn gen_for(&mut self, kind: &TForKind, body: &[TStmt]) {
        match kind {
            TForKind::Infinite => self.gen_infinite_loop(body),
            TForKind::Condition { cond } => self.gen_while_loop(cond, body),
            TForKind::Classic { init, cond, post } => {
                self.gen_classic_for(init.as_deref(), cond.as_ref(), post.as_deref(), body)
            }
            TForKind::RangeArray {
                len,
                elem_type,
                expr,
                key_sym,
                val_sym,
            } => self.gen_range_array(*len, *elem_type, expr, *key_sym, *val_sym, body),
            TForKind::RangeSlice {
                elem_type,
                expr,
                key_sym,
                val_sym,
            } => self.gen_range_slice(*elem_type, expr, *key_sym, *val_sym, body),
            TForKind::RangeString {
                expr,
                key_sym,
                val_sym,
            } => self.gen_range_string(expr, *key_sym, *val_sym, body),
            TForKind::RangeMap {
                key_type,
                value_type,
                expr,
                key_sym,
                val_sym,
            } => self.gen_range_map(*key_type, *value_type, expr, *key_sym, *val_sym, body),
        }
    }

    fn gen_infinite_loop(&mut self, body: &[TStmt]) {
        let loop_block = self.append_block("loop.body");
        let after_block = self.append_block("loop.after");

        self.builder.build_unconditional_branch(loop_block).unwrap();
        self.enter_loop(loop_block, after_block);

        self.position_at_end(loop_block);
        self.gen_block(body);
        self.build_br_if_needed(loop_block);

        self.exit_loop();
        self.position_at_end(after_block);
    }

    fn gen_while_loop(&mut self, cond: &TExpr, body: &[TStmt]) {
        let cond_block = self.append_block("loop.cond");
        let body_block = self.append_block("loop.body");
        let after_block = self.append_block("loop.after");

        self.builder.build_unconditional_branch(cond_block).unwrap();
        self.enter_loop(cond_block, after_block);

        // Condition
        self.position_at_end(cond_block);
        let cond_val = self.gen_expr(cond).into_int_value();
        self.build_cond_br(cond_val, body_block, after_block);

        // Body
        self.position_at_end(body_block);
        self.gen_block(body);
        self.build_br_if_needed(cond_block);

        self.exit_loop();
        self.position_at_end(after_block);
    }

    fn gen_classic_for(
        &mut self,
        init: Option<&TStmt>,
        cond: Option<&TExpr>,
        post: Option<&TStmt>,
        body: &[TStmt],
    ) {
        // Init
        if let Some(i) = init {
            self.gen_stmt(i);
        }

        let cond_block = self.append_block("for.cond");
        let body_block = self.append_block("for.body");
        let post_block = self.append_block("for.post");
        let after_block = self.append_block("for.after");

        self.builder.build_unconditional_branch(cond_block).unwrap();
        self.enter_loop(post_block, after_block);

        // Condition
        self.position_at_end(cond_block);
        if let Some(c) = cond {
            let cond_val = self.gen_expr(c).into_int_value();
            self.build_cond_br(cond_val, body_block, after_block);
        } else {
            self.builder.build_unconditional_branch(body_block).unwrap();
        }

        // Body
        self.position_at_end(body_block);
        self.gen_block(body);
        self.build_br_if_needed(post_block);

        // Post
        self.position_at_end(post_block);
        if let Some(p) = post {
            self.gen_stmt(p);
        }
        self.builder.build_unconditional_branch(cond_block).unwrap();

        self.exit_loop();
        self.position_at_end(after_block);
    }
}
