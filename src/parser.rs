use crate::ast::*;
use crate::errors::{Result, SyntaxError};
use crate::lexer::Lexer;
use crate::token::{Span, Token, TokenKind};
use std::mem;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    file: &'a str,
    curr: Token,
    peek: Token,
    in_condition: bool,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, file: &'a str) -> Self {
        let mut lexer = Lexer::new(source);
        let curr = lexer.next_token();
        let peek = lexer.next_token();
        Self {
            lexer,
            file,
            curr,
            peek,
            in_condition: false,
        }
    }

    fn advance(&mut self) {
        self.curr = mem::replace(&mut self.peek, self.lexer.next_token());
    }

    fn curr_kind(&self) -> TokenKind {
        self.curr.kind
    }

    fn peek_kind(&self) -> TokenKind {
        self.peek.kind
    }

    fn curr_span(&self) -> Span {
        self.curr.span
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token> {
        if self.curr_kind() == kind {
            let tok = self.curr.clone();
            self.advance();
            Ok(tok)
        } else {
            Err(SyntaxError::expected(
                self.file,
                self.curr_span(),
                kind.description(),
                &self.curr.literal,
            ))
        }
    }

    fn consume(&mut self, kind: TokenKind) -> bool {
        if self.curr_kind() == kind {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect_semi(&mut self) -> Result<()> {
        if self.curr_kind() == TokenKind::Semi {
            self.advance();
            Ok(())
        } else if self.curr_kind() == TokenKind::RBrace || self.curr_kind() == TokenKind::Eof {
            Ok(())
        } else {
            Err(SyntaxError::expected(
                self.file,
                self.curr_span(),
                ";",
                &self.curr.literal,
            ))
        }
    }

    // SourceFile = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" }
    pub fn parse_source_file(&mut self) -> Result<SourceFile> {
        self.parse_package_clause()?;
        self.expect_semi()?;

        let mut imports = Vec::new();
        while self.curr_kind() == TokenKind::Import {
            imports.push(self.parse_import_decl()?);
            self.expect_semi()?;
        }

        let mut decls = Vec::new();
        while self.curr_kind() != TokenKind::Eof {
            decls.push(self.parse_top_level_decl()?);
            self.expect_semi()?;
        }

        Ok(SourceFile { imports, decls })
    }

    // PackageClause = "package" "main"
    fn parse_package_clause(&mut self) -> Result<()> {
        self.expect(TokenKind::Package)?;
        let tok = self.expect(TokenKind::Ident)?;
        if tok.literal != "main" {
            return Err(SyntaxError::expected(
                self.file,
                tok.span,
                "main",
                &tok.literal,
            ));
        }
        Ok(())
    }

    // ImportDecl = "import" ImportSpec
    // ImportSpec = string_lit
    fn parse_import_decl(&mut self) -> Result<ImportDecl> {
        let span = self.curr_span();
        self.expect(TokenKind::Import)?;
        let path_tok = self.expect(TokenKind::StringLit)?;
        let path = path_tok.literal[1..path_tok.literal.len() - 1].to_string();
        Ok(ImportDecl { span, path })
    }

    // TopLevelDecl = ConstDecl | TypeDecl | VarDecl | FunctionDecl | MethodDecl
    fn parse_top_level_decl(&mut self) -> Result<TopLevelDecl> {
        match self.curr_kind() {
            TokenKind::Const => Ok(TopLevelDecl::Const(self.parse_const_decl()?)),
            TokenKind::Type => Ok(TopLevelDecl::Type(self.parse_type_decl()?)),
            TokenKind::Var => Ok(TopLevelDecl::Var(self.parse_var_decl()?)),
            TokenKind::Func => self.parse_func_or_method_decl(),
            _ => Err(SyntaxError::non_decl_outside_func(
                self.file,
                self.curr_span(),
            )),
        }
    }

    // ConstDecl = "const" identifier [ Type ] "=" Expression
    fn parse_const_decl(&mut self) -> Result<ConstDecl> {
        let span = self.curr_span();
        self.expect(TokenKind::Const)?;
        let name = self.expect(TokenKind::Ident)?.literal;
        let typ = if self.is_type_start() && self.curr_kind() != TokenKind::Eq {
            Some(self.parse_type()?)
        } else {
            None
        };
        self.expect(TokenKind::Eq)?;
        let value = self.parse_expression()?;
        Ok(ConstDecl {
            span,
            name,
            typ,
            value,
        })
    }

    // TypeDecl = "type" identifier Type
    fn parse_type_decl(&mut self) -> Result<TypeDecl> {
        let span = self.curr_span();
        self.expect(TokenKind::Type)?;
        let name = self.expect(TokenKind::Ident)?.literal;
        let typ = self.parse_type()?;
        Ok(TypeDecl { span, name, typ })
    }

    // VarDecl = "var" identifier ( Type [ "=" Expression ] | "=" Expression )
    fn parse_var_decl(&mut self) -> Result<VarDecl> {
        let span = self.curr_span();
        self.expect(TokenKind::Var)?;
        let name = self.expect(TokenKind::Ident)?.literal;

        if self.consume(TokenKind::Eq) {
            let value = self.parse_expression()?;
            Ok(VarDecl {
                span,
                name,
                typ: None,
                value: Some(value),
            })
        } else {
            let typ = Some(self.parse_type()?);
            let value = if self.consume(TokenKind::Eq) {
                Some(self.parse_expression()?)
            } else {
                None
            };
            Ok(VarDecl {
                span,
                name,
                typ,
                value,
            })
        }
    }

    // FunctionDecl = "func" identifier Signature Block
    // MethodDecl = "func" Receiver identifier Signature Block
    fn parse_func_or_method_decl(&mut self) -> Result<TopLevelDecl> {
        let span = self.curr_span();
        self.expect(TokenKind::Func)?;

        if self.curr_kind() == TokenKind::LParen {
            let receiver = self.parse_receiver()?;
            let name_span = self.curr_span();
            let name = self.expect(TokenKind::Ident)?.literal;
            let signature = self.parse_signature()?;
            let body = self.parse_block()?;
            Ok(TopLevelDecl::Method(MethodDecl {
                span,
                receiver,
                name,
                name_span,
                signature,
                body,
            }))
        } else {
            let name = self.expect(TokenKind::Ident)?.literal;
            let signature = self.parse_signature()?;
            let body = self.parse_block()?;
            Ok(TopLevelDecl::Function(FunctionDecl {
                span,
                name,
                signature,
                body,
            }))
        }
    }

    // Receiver = "(" identifier Type ")"
    fn parse_receiver(&mut self) -> Result<Receiver> {
        let span = self.curr_span();
        self.expect(TokenKind::LParen)?;

        let has_name = self.curr_kind() == TokenKind::Ident
            && self.peek_kind() != TokenKind::RParen
            && self.peek_kind() != TokenKind::Dot;

        if has_name {
            let name = self.expect(TokenKind::Ident)?.literal;
            let typ = self.parse_type()?;
            self.expect(TokenKind::RParen)?;
            Ok(Receiver { span, name, typ })
        } else {
            let typ = self.parse_type()?;
            self.expect(TokenKind::RParen)?;
            Ok(Receiver {
                span,
                name: String::new(),
                typ,
            })
        }
    }

    // Signature = Parameters [ Result ]
    fn parse_signature(&mut self) -> Result<Signature> {
        let params = self.parse_parameters()?;
        let result = self.parse_result()?;
        Ok(Signature { params, result })
    }

    // Result = Type
    fn parse_result(&mut self) -> Result<Option<Type>> {
        if self.is_type_start() {
            Ok(Some(self.parse_type()?))
        } else {
            Ok(None)
        }
    }

    // Parameters = "(" [ ParameterList [ "," ] ] ")"
    // ParameterList = ParameterDecl { "," ParameterDecl }
    // ParameterDecl = [ identifier ] Type
    fn parse_parameters(&mut self) -> Result<Vec<ParameterDecl>> {
        self.expect(TokenKind::LParen)?;
        let mut params = Vec::new();

        if self.curr_kind() == TokenKind::RParen {
            self.advance(); // )
            return Ok(params);
        }

        loop {
            let span = self.curr_span();

            // Check if we have "name Type" or just "Type"
            // If current is ident and next is a type-start or comma/rparen, it's ambiguous
            // We need to look ahead: if it's "ident Type", then ident is the name
            // If it's just "ident" followed by comma/rparen, then ident IS the type
            if self.curr_kind() == TokenKind::Ident {
                let ident = self.expect(TokenKind::Ident)?.literal;

                // Check if a type follows - if so, ident was the parameter name
                if self.is_type_start()
                    && !matches!(self.curr_kind(), TokenKind::Comma | TokenKind::RParen)
                {
                    let typ = self.parse_type()?;
                    params.push(ParameterDecl {
                        span,
                        name: Some(ident),
                        typ,
                    });
                } else {
                    // No type follows, so ident IS the type name
                    params.push(ParameterDecl {
                        span,
                        name: None,
                        typ: Type::Name(span, ident),
                    });
                }
            } else {
                // Non-ident start means it's definitely just a type (like *int, []byte, etc.)
                let typ = self.parse_type()?;
                params.push(ParameterDecl {
                    span,
                    name: None,
                    typ,
                });
            }

            if !self.consume(TokenKind::Comma) {
                break;
            }
            if self.curr_kind() == TokenKind::RParen {
                break;
            }
        }

        self.expect(TokenKind::RParen)?;
        Ok(params)
    }

    // IdentifierList = identifier { "," identifier }
    fn parse_identifier_list(&mut self) -> Result<Vec<String>> {
        let mut names = vec![self.expect(TokenKind::Ident)?.literal];
        while self.consume(TokenKind::Comma) {
            if self.curr_kind() != TokenKind::Ident {
                break;
            }
            names.push(self.expect(TokenKind::Ident)?.literal);
        }
        Ok(names)
    }

    fn is_type_start(&self) -> bool {
        matches!(
            self.curr_kind(),
            TokenKind::Ident
                | TokenKind::LBrack
                | TokenKind::Struct
                | TokenKind::Star
                | TokenKind::Func
                | TokenKind::Map
                | TokenKind::LParen
        )
    }

    // Type = TypeName | TypeLit | "(" Type ")"
    // TypeName = identifier | QualifiedIdent
    // TypeLit = ArrayType | SliceType | StructType | PointerType | FunctionType | MapType
    fn parse_type(&mut self) -> Result<Type> {
        let span = self.curr_span();
        match self.curr_kind() {
            TokenKind::Ident => {
                let name = self.expect(TokenKind::Ident)?.literal;
                if self.consume(TokenKind::Dot) {
                    let sel = self.expect(TokenKind::Ident)?.literal;
                    Ok(Type::Qualified(span, name, sel))
                } else {
                    Ok(Type::Name(span, name))
                }
            }
            TokenKind::LBrack => {
                self.advance(); // [
                if self.curr_kind() == TokenKind::RBrack {
                    self.advance(); // ]
                    let elem = self.parse_type()?;
                    Ok(Type::Slice(span, Box::new(elem)))
                } else {
                    let size = self.parse_expression()?;
                    self.expect(TokenKind::RBrack)?;
                    let elem = self.parse_type()?;
                    Ok(Type::Array(span, Box::new(size), Box::new(elem)))
                }
            }
            TokenKind::Struct => {
                self.advance(); // struct
                self.expect(TokenKind::LBrace)?;
                let mut fields = Vec::new();
                while self.curr_kind() != TokenKind::RBrace {
                    fields.push(self.parse_field_decl()?);
                    self.expect_semi()?;
                }
                self.expect(TokenKind::RBrace)?;
                Ok(Type::Struct(span, fields))
            }
            TokenKind::Star => {
                self.advance(); // *
                let elem = self.parse_type()?;
                Ok(Type::Pointer(span, Box::new(elem)))
            }
            TokenKind::Func => {
                self.advance(); // func
                let sig = self.parse_signature()?;
                Ok(Type::Function(span, Box::new(sig)))
            }
            TokenKind::Map => {
                self.advance(); // map
                self.expect(TokenKind::LBrack)?;
                let key = self.parse_type()?;
                self.expect(TokenKind::RBrack)?;
                let value = self.parse_type()?;
                Ok(Type::Map(span, Box::new(key), Box::new(value)))
            }
            TokenKind::LParen => {
                self.advance(); // (
                let inner = self.parse_type()?;
                self.expect(TokenKind::RParen)?;
                Ok(Type::Paren(span, Box::new(inner)))
            }
            _ => Err(SyntaxError::unexpected_expected(
                self.file,
                self.curr_span(),
                &self.curr.literal,
                "type",
            )),
        }
    }

    // FieldDecl = IdentifierList Type
    fn parse_field_decl(&mut self) -> Result<FieldDecl> {
        let span = self.curr_span();
        let names = self.parse_identifier_list()?;
        let typ = self.parse_type()?;
        Ok(FieldDecl { span, names, typ })
    }

    // Block = "{" StatementList "}"
    fn parse_block(&mut self) -> Result<Block> {
        let span = self.curr_span();
        self.expect(TokenKind::LBrace)?;
        let stmts = self.parse_statement_list()?;
        let end = self.expect(TokenKind::RBrace)?.span;
        Ok(Block { span, end, stmts })
    }

    // StatementList = { Statement ";" }
    fn parse_statement_list(&mut self) -> Result<Vec<Stmt>> {
        let mut stmts = Vec::new();
        while !matches!(
            self.curr_kind(),
            TokenKind::RBrace | TokenKind::Case | TokenKind::Default | TokenKind::Eof
        ) {
            stmts.push(self.parse_statement()?);
            self.expect_semi()?;
        }
        Ok(stmts)
    }

    // Statement = Declaration | SimpleStmt | ReturnStmt | BreakStmt | ContinueStmt | Block | IfStmt | SwitchStmt | ForStmt
    fn parse_statement(&mut self) -> Result<Stmt> {
        match self.curr_kind() {
            TokenKind::Const => Ok(Stmt::Decl(Declaration::Const(self.parse_const_decl()?))),
            TokenKind::Type => Ok(Stmt::Decl(Declaration::Type(self.parse_type_decl()?))),
            TokenKind::Var => Ok(Stmt::Decl(Declaration::Var(self.parse_var_decl()?))),
            TokenKind::Return => self.parse_return_stmt(),
            TokenKind::Break => {
                let span = self.curr_span();
                self.advance(); // break
                Ok(Stmt::Break(span))
            }
            TokenKind::Continue => {
                let span = self.curr_span();
                self.advance(); // continue
                Ok(Stmt::Continue(span))
            }
            TokenKind::LBrace => Ok(Stmt::Block(self.parse_block()?)),
            TokenKind::If => Ok(Stmt::If(self.parse_if_stmt()?)),
            TokenKind::Switch => Ok(Stmt::Switch(self.parse_switch_stmt()?)),
            TokenKind::For => Ok(Stmt::For(self.parse_for_stmt()?)),
            _ => Ok(Stmt::Simple(self.parse_simple_stmt()?)),
        }
    }

    // SimpleStmt = EmptyStmt | ExpressionStmt | IncDecStmt | Assignment | ShortVarDecl
    fn parse_simple_stmt(&mut self) -> Result<SimpleStmt> {
        let span = self.curr_span();
        if matches!(
            self.curr_kind(),
            TokenKind::Semi | TokenKind::RBrace | TokenKind::Colon
        ) {
            return Ok(SimpleStmt::Empty(span));
        }

        let expr = self.parse_expression()?;

        match self.curr_kind() {
            TokenKind::Inc => {
                self.advance();
                Ok(SimpleStmt::IncDec(span, expr, IncDecOp::Inc))
            }
            TokenKind::Dec => {
                self.advance();
                Ok(SimpleStmt::IncDec(span, expr, IncDecOp::Dec))
            }
            TokenKind::ColonEq => {
                self.advance();
                let name = match expr.as_ident() {
                    Some(name) => name.to_string(),
                    None => {
                        return Err(SyntaxError::unexpected_expected(
                            self.file,
                            expr.span(),
                            "expression",
                            "identifier",
                        ));
                    }
                };
                let value = self.parse_expression()?;
                Ok(SimpleStmt::ShortVarDecl(span, name, value))
            }
            kind if kind.is_assign_op() => {
                let op = match kind {
                    TokenKind::Eq => AssignOp::Assign,
                    TokenKind::PlusAssign => AssignOp::Add,
                    TokenKind::MinusAssign => AssignOp::Sub,
                    TokenKind::StarAssign => AssignOp::Mul,
                    TokenKind::SlashAssign => AssignOp::Div,
                    TokenKind::PercentAssign => AssignOp::Mod,
                    _ => unreachable!(),
                };
                self.advance();
                let rhs = self.parse_expression()?;
                Ok(SimpleStmt::Assign(span, expr, op, rhs))
            }
            _ => Ok(SimpleStmt::Expr(expr)),
        }
    }

    // ReturnStmt = "return" [ ExpressionList ]
    fn parse_return_stmt(&mut self) -> Result<Stmt> {
        let span = self.curr_span();
        self.expect(TokenKind::Return)?;

        let expr = if !matches!(
            self.curr_kind(),
            TokenKind::Semi | TokenKind::RBrace | TokenKind::Eof
        ) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.expect_semi()?;

        Ok(Stmt::Return(span, expr))
    }

    // IfStmt = "if" [ SimpleStmt ";" ] Expression Block [ "else" ( IfStmt | Block ) ]
    fn parse_if_stmt(&mut self) -> Result<IfStmt> {
        let span = self.curr_span();
        self.expect(TokenKind::If)?;

        self.in_condition = true;
        let (init, cond) = self.parse_init_and_cond()?;
        self.in_condition = false;

        let body = self.parse_block()?;

        let else_branch = if self.consume(TokenKind::Else) {
            if self.curr_kind() == TokenKind::If {
                Some(Else::If(Box::new(self.parse_if_stmt()?)))
            } else {
                Some(Else::Block(self.parse_block()?))
            }
        } else {
            None
        };

        Ok(IfStmt {
            span,
            init,
            cond,
            body,
            else_branch,
        })
    }

    fn parse_init_and_cond(&mut self) -> Result<(Option<Box<SimpleStmt>>, Expr)> {
        let stmt = self.parse_simple_stmt()?;

        if self.consume(TokenKind::Semi) {
            let cond = self.parse_expression()?;
            let init = match stmt {
                SimpleStmt::Empty(_) => None,
                _ => Some(Box::new(stmt)),
            };
            Ok((init, cond))
        } else {
            match stmt {
                SimpleStmt::Expr(expr) => Ok((None, expr)),
                _ => Err(SyntaxError::unexpected_expected(
                    self.file,
                    self.curr_span(),
                    &self.curr.literal,
                    "expression or ;",
                )),
            }
        }
    }

    // ExprSwitchStmt = "switch" [ SimpleStmt ";" ] [ Expression ] "{" { ExprCaseClause } "}"
    fn parse_switch_stmt(&mut self) -> Result<SwitchStmt> {
        let span = self.curr_span();
        self.expect(TokenKind::Switch)?;

        let (init, tag) = if self.curr_kind() == TokenKind::LBrace {
            (None, None)
        } else {
            self.in_condition = true;
            let stmt = self.parse_simple_stmt()?;

            if self.consume(TokenKind::Semi) {
                let init = match stmt {
                    SimpleStmt::Empty(_) => None,
                    _ => Some(Box::new(stmt)),
                };
                let tag = if self.curr_kind() != TokenKind::LBrace {
                    Some(self.parse_expression()?)
                } else {
                    None
                };
                self.in_condition = false;
                (init, tag)
            } else {
                self.in_condition = false;
                match stmt {
                    SimpleStmt::Expr(expr) => (None, Some(expr)),
                    SimpleStmt::Empty(_) => (None, None),
                    _ => {
                        return Err(SyntaxError::unexpected_expected(
                            self.file,
                            self.curr_span(),
                            &self.curr.literal,
                            "{",
                        ));
                    }
                }
            }
        };

        self.expect(TokenKind::LBrace)?;
        let mut cases = Vec::new();
        while self.curr_kind() != TokenKind::RBrace {
            cases.push(self.parse_case_clause()?);
        }
        self.expect(TokenKind::RBrace)?;

        Ok(SwitchStmt {
            span,
            init,
            tag,
            cases,
        })
    }

    // ExprCaseClause = ExprSwitchCase ":" StatementList
    // ExprSwitchCase = "case" Expression | "default"
    fn parse_case_clause(&mut self) -> Result<CaseClause> {
        let span = self.curr_span();
        let case = if self.consume(TokenKind::Default) {
            SwitchCase::Default
        } else {
            self.expect(TokenKind::Case)?;
            SwitchCase::Expr(self.parse_expression()?)
        };
        self.expect(TokenKind::Colon)?;
        let body = self.parse_statement_list()?;
        Ok(CaseClause { span, case, body })
    }

    // ForStmt = "for" [ Condition | ForClause | RangeClause ] Block
    // ForClause = [ InitStmt ] ";" [ Condition ] ";" [ PostStmt ]
    // RangeClause = [ identifier [ "," identifier ] ":=" ] "range" Expression
    fn parse_for_stmt(&mut self) -> Result<ForStmt> {
        let span = self.curr_span();
        self.expect(TokenKind::For)?;

        if self.curr_kind() == TokenKind::LBrace {
            let body = self.parse_block()?;
            return Ok(ForStmt {
                span,
                clause: ForClause::Infinite,
                body,
            });
        }

        self.in_condition = true;

        // Check for bare "range expr"
        if self.curr_kind() == TokenKind::Ident && self.curr.literal == "range" {
            self.advance();
            let expr = self.parse_expression()?;
            self.in_condition = false;
            let body = self.parse_block()?;
            return Ok(ForStmt {
                span,
                clause: ForClause::Range(
                    RangeClause {
                        span,
                        key: None,
                        value: None,
                    },
                    Box::new(expr),
                ),
                body,
            });
        }

        let first_expr = self.parse_expression()?;

        // Check for range with short var decl: "k := range" or "k, v := range"
        if self.curr_kind() == TokenKind::Comma || self.curr_kind() == TokenKind::ColonEq {
            let key_name = match first_expr.as_ident() {
                Some(name) => name.to_string(),
                None => {
                    return Err(SyntaxError::unexpected_expected(
                        self.file,
                        first_expr.span(),
                        "expression",
                        "identifier",
                    ));
                }
            };

            let value_name = if self.consume(TokenKind::Comma) {
                let val_expr = self.parse_expression()?;
                Some(match val_expr.as_ident() {
                    Some(name) => name.to_string(),
                    None => {
                        return Err(SyntaxError::unexpected_expected(
                            self.file,
                            val_expr.span(),
                            "expression",
                            "identifier",
                        ));
                    }
                })
            } else {
                None
            };

            if self.consume(TokenKind::ColonEq) {
                if self.curr_kind() == TokenKind::Ident && self.curr.literal == "range" {
                    self.advance();
                    let range_expr = self.parse_expression()?;
                    self.in_condition = false;
                    let body = self.parse_block()?;
                    return Ok(ForStmt {
                        span,
                        clause: ForClause::Range(
                            RangeClause {
                                span,
                                key: Some(key_name),
                                value: value_name,
                            },
                            Box::new(range_expr),
                        ),
                        body,
                    });
                }
                // Not a range, it's a short var decl in classic for
                let rhs = self.parse_expression()?;
                let init_stmt = SimpleStmt::ShortVarDecl(span, key_name, rhs);
                self.expect(TokenKind::Semi)?;
                return self.parse_classic_for_rest(span, Some(Box::new(init_stmt)));
            }
        }

        // Check for assignment init
        if self.curr_kind().is_assign_op() {
            let op = match self.curr_kind() {
                TokenKind::Eq => AssignOp::Assign,
                TokenKind::PlusAssign => AssignOp::Add,
                TokenKind::MinusAssign => AssignOp::Sub,
                TokenKind::StarAssign => AssignOp::Mul,
                TokenKind::SlashAssign => AssignOp::Div,
                TokenKind::PercentAssign => AssignOp::Mod,
                _ => unreachable!(),
            };
            self.advance();
            let rhs = self.parse_expression()?;
            let init_stmt = SimpleStmt::Assign(span, first_expr, op, rhs);
            self.expect(TokenKind::Semi)?;
            return self.parse_classic_for_rest(span, Some(Box::new(init_stmt)));
        }

        // Check for semicolon (classic for with expression init or empty)
        if self.consume(TokenKind::Semi) {
            let init = Some(Box::new(SimpleStmt::Expr(first_expr)));
            return self.parse_classic_for_rest(span, init);
        }

        // Must be condition-only form
        self.in_condition = false;
        let body = self.parse_block()?;
        Ok(ForStmt {
            span,
            clause: ForClause::Condition(first_expr),
            body,
        })
    }

    fn parse_classic_for_rest(
        &mut self,
        span: Span,
        init: Option<Box<SimpleStmt>>,
    ) -> Result<ForStmt> {
        let cond = if self.curr_kind() != TokenKind::Semi {
            Some(self.parse_expression()?)
        } else {
            None
        };
        self.expect(TokenKind::Semi)?;
        let post = if self.curr_kind() != TokenKind::LBrace {
            let stmt = self.parse_simple_stmt()?;
            match stmt {
                SimpleStmt::Empty(_) => None,
                _ => Some(Box::new(stmt)),
            }
        } else {
            None
        };
        self.in_condition = false;
        let body = self.parse_block()?;
        Ok(ForStmt {
            span,
            clause: ForClause::Classic(init, cond, post),
            body,
        })
    }

    // Expression = LogicalOrExpr
    fn parse_expression(&mut self) -> Result<Expr> {
        self.parse_logical_or_expr()
    }

    // LogicalOrExpr = LogicalAndExpr { "||" LogicalAndExpr }
    fn parse_logical_or_expr(&mut self) -> Result<Expr> {
        let mut left = self.parse_logical_and_expr()?;
        while self.curr_kind() == TokenKind::PipePipe {
            let span = self.curr_span();
            self.advance(); // ||
            let right = self.parse_logical_and_expr()?;
            left = Expr::Binary(span, Box::new(left), BinaryOp::Or, Box::new(right));
        }
        Ok(left)
    }

    // LogicalAndExpr = RelationalExpr { "&&" RelationalExpr }
    fn parse_logical_and_expr(&mut self) -> Result<Expr> {
        let mut left = self.parse_relational_expr()?;
        while self.curr_kind() == TokenKind::AmpAmp {
            let span = self.curr_span();
            self.advance(); // &&
            let right = self.parse_relational_expr()?;
            left = Expr::Binary(span, Box::new(left), BinaryOp::And, Box::new(right));
        }
        Ok(left)
    }

    // RelationalExpr = AdditiveExpr { rel_op AdditiveExpr }
    fn parse_relational_expr(&mut self) -> Result<Expr> {
        let mut left = self.parse_additive_expr()?;
        while let Some(op) = self.match_rel_op() {
            let span = self.curr_span();
            self.advance(); // rel_op
            let right = self.parse_additive_expr()?;
            left = Expr::Binary(span, Box::new(left), op, Box::new(right));
        }
        Ok(left)
    }

    fn match_rel_op(&self) -> Option<BinaryOp> {
        match self.curr_kind() {
            TokenKind::EqEq => Some(BinaryOp::Eq),
            TokenKind::NotEq => Some(BinaryOp::Ne),
            TokenKind::Lt => Some(BinaryOp::Lt),
            TokenKind::LtEq => Some(BinaryOp::Le),
            TokenKind::Gt => Some(BinaryOp::Gt),
            TokenKind::GtEq => Some(BinaryOp::Ge),
            _ => None,
        }
    }

    // AdditiveExpr = MultiplicativeExpr { add_op MultiplicativeExpr }
    fn parse_additive_expr(&mut self) -> Result<Expr> {
        let mut left = self.parse_multiplicative_expr()?;
        while let Some(op) = self.match_add_op() {
            let span = self.curr_span();
            self.advance(); // add_op
            let right = self.parse_multiplicative_expr()?;
            left = Expr::Binary(span, Box::new(left), op, Box::new(right));
        }
        Ok(left)
    }

    fn match_add_op(&self) -> Option<BinaryOp> {
        match self.curr_kind() {
            TokenKind::Plus => Some(BinaryOp::Add),
            TokenKind::Minus => Some(BinaryOp::Sub),
            _ => None,
        }
    }

    // MultiplicativeExpr = UnaryExpr { mul_op UnaryExpr }
    fn parse_multiplicative_expr(&mut self) -> Result<Expr> {
        let mut left = self.parse_unary_expr()?;
        while let Some(op) = self.match_mul_op() {
            let span = self.curr_span();
            self.advance(); // mul_op
            let right = self.parse_unary_expr()?;
            left = Expr::Binary(span, Box::new(left), op, Box::new(right));
        }
        Ok(left)
    }

    fn match_mul_op(&self) -> Option<BinaryOp> {
        match self.curr_kind() {
            TokenKind::Star => Some(BinaryOp::Mul),
            TokenKind::Slash => Some(BinaryOp::Div),
            TokenKind::Percent => Some(BinaryOp::Mod),
            _ => None,
        }
    }

    // UnaryExpr = PrimaryExpr | unary_op UnaryExpr
    fn parse_unary_expr(&mut self) -> Result<Expr> {
        if let Some(op) = self.match_unary_op() {
            let span = self.curr_span();
            self.advance(); // unary_op
            let operand = self.parse_unary_expr()?;
            Ok(Expr::Unary(span, op, Box::new(operand)))
        } else {
            self.parse_primary_expr()
        }
    }

    fn match_unary_op(&self) -> Option<UnaryOp> {
        match self.curr_kind() {
            TokenKind::Plus => Some(UnaryOp::Pos),
            TokenKind::Minus => Some(UnaryOp::Neg),
            TokenKind::Bang => Some(UnaryOp::Not),
            TokenKind::Star => Some(UnaryOp::Deref),
            TokenKind::Amp => Some(UnaryOp::Addr),
            _ => None,
        }
    }

    // PrimaryExpr = Operand { Selector | Index | Slice | Call }
    fn parse_primary_expr(&mut self) -> Result<Expr> {
        let mut expr = self.parse_operand()?;

        loop {
            match self.curr_kind() {
                TokenKind::Dot => {
                    let span = self.curr_span();
                    self.advance(); // .
                    let sel = self.expect(TokenKind::Ident)?.literal;
                    expr = Expr::Selector(span, Box::new(expr), sel);
                }
                TokenKind::LBrack => {
                    let span = self.curr_span();
                    self.advance(); // [

                    if self.curr_kind() == TokenKind::Colon {
                        self.advance(); // :
                        let high = if self.curr_kind() != TokenKind::RBrack {
                            Some(Box::new(self.parse_expression()?))
                        } else {
                            None
                        };
                        self.expect(TokenKind::RBrack)?;
                        expr = Expr::Slice(span, Box::new(expr), None, high);
                    } else {
                        let index = self.parse_expression()?;
                        if self.consume(TokenKind::Colon) {
                            let low = Some(Box::new(index));
                            let high = if self.curr_kind() != TokenKind::RBrack {
                                Some(Box::new(self.parse_expression()?))
                            } else {
                                None
                            };
                            self.expect(TokenKind::RBrack)?;
                            expr = Expr::Slice(span, Box::new(expr), low, high);
                        } else {
                            self.expect(TokenKind::RBrack)?;
                            expr = Expr::Index(span, Box::new(expr), Box::new(index));
                        }
                    }
                }
                TokenKind::LParen => {
                    let span = self.curr_span();
                    self.advance(); // (
                    let mut args = Vec::new();
                    if self.curr_kind() != TokenKind::RParen {
                        if self.is_make_call_target(&expr) {
                            // make() takes a type as first argument
                            args.push(self.parse_make_new_type_expr()?);
                        } else if self.is_new_call_target(&expr) {
                            // new() takes any type as argument
                            args.push(self.parse_new_type_expr()?);
                        } else {
                            args.push(self.parse_expression()?);
                        }
                        while self.consume(TokenKind::Comma) {
                            if self.curr_kind() == TokenKind::RParen {
                                break;
                            }
                            args.push(self.parse_expression()?);
                        }
                    }
                    self.expect(TokenKind::RParen)?;
                    expr = Expr::Call(span, Box::new(expr), args);
                }
                TokenKind::LBrace if !self.in_condition => {
                    if let Some(lit_type) = self.expr_to_literal_type(&expr) {
                        let lit_val = self.parse_literal_value()?;
                        let span = expr.span();
                        expr = Expr::CompositeLit(span, lit_type, lit_val);
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn is_make_call_target(&self, expr: &Expr) -> bool {
        matches!(expr, Expr::Ident(_, name) if name == "make")
    }

    fn is_new_call_target(&self, expr: &Expr) -> bool {
        matches!(expr, Expr::Ident(_, name) if name == "new")
    }

    /// Parse type expression for new() - any type is allowed
    fn parse_new_type_expr(&mut self) -> Result<Expr> {
        let typ = self.parse_type()?;
        let span = typ.span();
        Ok(Expr::TypeVal(span, typ))
    }

    fn parse_make_new_type_expr(&mut self) -> Result<Expr> {
        if !matches!(
            self.curr_kind(),
            TokenKind::Map | TokenKind::LBrack | TokenKind::LParen
        ) {
            return Err(SyntaxError::expected(
                self.file,
                self.curr_span(),
                "array, slice, or map type",
                &self.curr.literal,
            ));
        }

        let typ = self.parse_type()?;
        if self.is_make_new_type(&typ) {
            let span = typ.span();
            Ok(Expr::TypeVal(span, typ))
        } else {
            Err(SyntaxError::unexpected_expected(
                self.file,
                typ.span(),
                "type",
                "array, slice, or map type",
            ))
        }
    }

    fn is_make_new_type(&self, typ: &Type) -> bool {
        match typ {
            Type::Array(_, _, _) | Type::Slice(_, _) | Type::Map(_, _, _) => true,
            Type::Paren(_, inner) => self.is_make_new_type(inner),
            _ => false,
        }
    }

    fn expr_to_literal_type(&self, expr: &Expr) -> Option<LiteralType> {
        match expr {
            Expr::Ident(span, name) => Some(LiteralType::Name(*span, name.clone())),
            Expr::Selector(span, base, field) => {
                // Handle pkg.Type{} composite literals
                if let Expr::Ident(_, pkg) = base.as_ref() {
                    Some(LiteralType::Qualified(*span, pkg.clone(), field.clone()))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    // Operand = Literal | identifier | QualifiedIdent | "(" Expression ")"
    // Literal = BasicLit | CompositeLit
    fn parse_operand(&mut self) -> Result<Expr> {
        let span = self.curr_span();
        match self.curr_kind() {
            TokenKind::Ident => {
                let name = self.expect(TokenKind::Ident)?.literal;
                if self.curr_kind() == TokenKind::LBrace && !self.in_condition {
                    let lit_type = LiteralType::Name(span, name);
                    let lit_val = self.parse_literal_value()?;
                    Ok(Expr::CompositeLit(span, lit_type, lit_val))
                } else {
                    Ok(Expr::Ident(span, name))
                }
            }
            TokenKind::IntLit => {
                let val = self.curr.literal.clone();
                self.advance(); // int_lit
                Ok(Expr::IntLit(span, val))
            }
            TokenKind::FloatLit => {
                let val = self.curr.literal.clone();
                self.advance(); // float_lit
                Ok(Expr::FloatLit(span, val))
            }
            TokenKind::StringLit => {
                let val = self.curr.literal.clone();
                self.advance(); // string_lit
                Ok(Expr::StringLit(span, val))
            }
            TokenKind::LParen => {
                self.advance(); // (
                let prev_in_condition = self.in_condition;
                self.in_condition = false;
                let expr = self.parse_expression()?;
                self.in_condition = prev_in_condition;
                self.expect(TokenKind::RParen)?;
                Ok(Expr::Paren(span, Box::new(expr)))
            }
            TokenKind::LBrack => {
                self.advance(); // [
                if self.curr_kind() == TokenKind::RBrack {
                    self.advance(); // ]
                    let elem = self.parse_type()?;
                    let lit_type = LiteralType::Slice(span, Box::new(elem));
                    let lit_val = self.parse_literal_value()?;
                    Ok(Expr::CompositeLit(span, lit_type, lit_val))
                } else {
                    let size = self.parse_expression()?;
                    self.expect(TokenKind::RBrack)?;
                    let elem = self.parse_type()?;
                    let lit_type = LiteralType::Array(span, Box::new(size), Box::new(elem));
                    let lit_val = self.parse_literal_value()?;
                    Ok(Expr::CompositeLit(span, lit_type, lit_val))
                }
            }
            TokenKind::Struct => {
                self.advance(); // struct
                self.expect(TokenKind::LBrace)?;
                let mut fields = Vec::new();
                while self.curr_kind() != TokenKind::RBrace {
                    fields.push(self.parse_field_decl()?);
                    self.expect_semi()?;
                }
                self.expect(TokenKind::RBrace)?;
                let lit_type = LiteralType::Struct(span, fields);
                let lit_val = self.parse_literal_value()?;
                Ok(Expr::CompositeLit(span, lit_type, lit_val))
            }
            TokenKind::Map => {
                self.advance(); // map
                self.expect(TokenKind::LBrack)?;
                let key = self.parse_type()?;
                self.expect(TokenKind::RBrack)?;
                let value = self.parse_type()?;
                let lit_type = LiteralType::Map(span, Box::new(key), Box::new(value));
                let lit_val = self.parse_literal_value()?;
                Ok(Expr::CompositeLit(span, lit_type, lit_val))
            }
            _ => Err(SyntaxError::unexpected_expected(
                self.file,
                span,
                &self.curr.literal,
                "expression",
            )),
        }
    }

    // LiteralValue = "{" [ ElementList [ "," ] ] "}"
    fn parse_literal_value(&mut self) -> Result<LiteralValue> {
        let span = self.curr_span();
        self.expect(TokenKind::LBrace)?;
        let mut elements = Vec::new();

        if self.curr_kind() != TokenKind::RBrace {
            elements.push(self.parse_keyed_element()?);
            while self.consume(TokenKind::Comma) {
                if self.curr_kind() == TokenKind::RBrace {
                    break;
                }
                elements.push(self.parse_keyed_element()?);
            }
        }

        self.expect(TokenKind::RBrace)?;
        Ok(LiteralValue { span, elements })
    }

    // KeyedElement = [ Key ":" ] Element
    fn parse_keyed_element(&mut self) -> Result<KeyedElement> {
        if self.curr_kind() == TokenKind::Ident && self.peek_kind() == TokenKind::Colon {
            let span = self.curr_span();
            let name = self.expect(TokenKind::Ident)?.literal;
            self.expect(TokenKind::Colon)?;
            let value = self.parse_element()?;
            Ok(KeyedElement {
                key: Some(Key::Field(span, name)),
                value,
            })
        } else if self.curr_kind() == TokenKind::LBrace {
            let value = Element::Lit(self.parse_literal_value()?);
            Ok(KeyedElement { key: None, value })
        } else {
            let expr = self.parse_expression()?;
            if self.consume(TokenKind::Colon) {
                let key = Some(Key::Expr(expr));
                let value = self.parse_element()?;
                Ok(KeyedElement { key, value })
            } else {
                Ok(KeyedElement {
                    key: None,
                    value: Element::Expr(expr),
                })
            }
        }
    }

    // Element = Expression | LiteralValue
    fn parse_element(&mut self) -> Result<Element> {
        if self.curr_kind() == TokenKind::LBrace {
            Ok(Element::Lit(self.parse_literal_value()?))
        } else {
            Ok(Element::Expr(self.parse_expression()?))
        }
    }
}
