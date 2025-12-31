use crate::token::{Span, Token, TokenKind};

pub struct Lexer<'a> {
    source: &'a str,
    bytes: &'a [u8],
    pos: usize,
    line: usize,
    col: usize,
    insert_semi: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            bytes: source.as_bytes(),
            pos: 0,
            line: 1,
            col: 1,
            insert_semi: false,
        }
    }

    fn peek(&self) -> Option<u8> {
        self.bytes.get(self.pos).copied()
    }

    fn peek_ahead(&self, n: usize) -> Option<u8> {
        self.bytes.get(self.pos + n).copied()
    }

    fn advance(&mut self) -> Option<u8> {
        let ch = self.peek()?;
        self.pos += 1;
        if ch == b'\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(ch)
    }

    fn skip_whitespace_and_comments(&mut self) -> bool {
        let mut saw_newline = false;
        loop {
            match self.peek() {
                Some(b' ') | Some(b'\t') | Some(b'\r') => {
                    self.advance();
                }
                Some(b'\n') => {
                    saw_newline = true;
                    self.advance();
                }
                Some(b'/') => {
                    if self.peek_ahead(1) == Some(b'/') {
                        while let Some(ch) = self.peek() {
                            if ch == b'\n' {
                                break;
                            }
                            self.advance();
                        }
                    } else if self.peek_ahead(1) == Some(b'*') {
                        self.advance();
                        self.advance();
                        while let Some(ch) = self.advance() {
                            if ch == b'*' && self.peek() == Some(b'/') {
                                self.advance();
                                break;
                            }
                            if ch == b'\n' {
                                saw_newline = true;
                            }
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
        saw_newline
    }

    fn scan_identifier(&mut self, start_pos: usize) -> Token {
        let span = Span::new(self.line, self.col);
        self.advance();
        while let Some(ch) = self.peek() {
            if ch.is_ascii_alphanumeric() || ch == b'_' {
                self.advance();
            } else {
                break;
            }
        }
        let literal = &self.source[start_pos..self.pos];
        let kind = TokenKind::from_keyword(literal).unwrap_or(TokenKind::Ident);
        Token::new(kind, span, literal.to_string())
    }

    fn scan_number(&mut self, start_pos: usize) -> Token {
        let span = Span::new(self.line, self.col);
        let mut is_float = false;

        if self.peek() == Some(b'0') {
            self.advance();
            match self.peek() {
                Some(b'x') | Some(b'X') => {
                    self.advance();
                    while let Some(ch) = self.peek() {
                        if ch.is_ascii_hexdigit() || ch == b'_' {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
                Some(b'o') | Some(b'O') => {
                    self.advance();
                    while let Some(ch) = self.peek() {
                        if (b'0'..=b'7').contains(&ch) || ch == b'_' {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
                Some(b'b') | Some(b'B') => {
                    self.advance();
                    while let Some(ch) = self.peek() {
                        if ch == b'0' || ch == b'1' || ch == b'_' {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
                Some(b'.') => {
                    is_float = true;
                    self.advance();
                    while let Some(ch) = self.peek() {
                        if ch.is_ascii_digit() || ch == b'_' {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    self.scan_exponent();
                }
                Some(b'e') | Some(b'E') => {
                    is_float = true;
                    self.scan_exponent();
                }
                Some(ch) if ch.is_ascii_digit() => {
                    while let Some(ch) = self.peek() {
                        if ch.is_ascii_digit() || ch == b'_' {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
                _ => {}
            }
        } else {
            while let Some(ch) = self.peek() {
                if ch.is_ascii_digit() || ch == b'_' {
                    self.advance();
                } else {
                    break;
                }
            }
            if self.peek() == Some(b'.') && self.peek_ahead(1).is_some_and(|c| c.is_ascii_digit()) {
                is_float = true;
                self.advance();
                while let Some(ch) = self.peek() {
                    if ch.is_ascii_digit() || ch == b'_' {
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
            if matches!(self.peek(), Some(b'e') | Some(b'E')) {
                is_float = true;
                self.scan_exponent();
            }
        }

        let literal = &self.source[start_pos..self.pos];
        let kind = if is_float {
            TokenKind::FloatLit
        } else {
            TokenKind::IntLit
        };
        Token::new(kind, span, literal.to_string())
    }

    fn scan_exponent(&mut self) {
        if matches!(self.peek(), Some(b'e') | Some(b'E')) {
            self.advance();
            if matches!(self.peek(), Some(b'+') | Some(b'-')) {
                self.advance();
            }
            while let Some(ch) = self.peek() {
                if ch.is_ascii_digit() || ch == b'_' {
                    self.advance();
                } else {
                    break;
                }
            }
        }
    }

    fn scan_string(&mut self) -> Token {
        let span = Span::new(self.line, self.col);
        let start_pos = self.pos;
        let quote = self.advance().unwrap();

        if quote == b'"' {
            while let Some(ch) = self.advance() {
                if ch == b'"' {
                    break;
                }
                if ch == b'\\' {
                    self.advance();
                }
            }
        } else {
            while let Some(ch) = self.advance() {
                if ch == b'`' {
                    break;
                }
            }
        }

        let literal = &self.source[start_pos..self.pos];
        Token::new(TokenKind::StringLit, span, literal.to_string())
    }

    fn make_token(&self, kind: TokenKind, span: Span, literal: &str) -> Token {
        Token::new(kind, span, literal.to_string())
    }

    pub fn next_token(&mut self) -> Token {
        let saw_newline = self.skip_whitespace_and_comments();

        if saw_newline && self.insert_semi {
            self.insert_semi = false;
            return self.make_token(TokenKind::Semi, Span::new(self.line, self.col), ";");
        }

        let span = Span::new(self.line, self.col);
        let start_pos = self.pos;

        let Some(ch) = self.peek() else {
            if self.insert_semi {
                self.insert_semi = false;
                return self.make_token(TokenKind::Semi, span, ";");
            }
            return self.make_token(TokenKind::Eof, span, "");
        };

        let token = match ch {
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.scan_identifier(start_pos),
            b'0'..=b'9' => self.scan_number(start_pos),
            b'"' | b'`' => self.scan_string(),
            b'+' => {
                self.advance();
                match self.peek() {
                    Some(b'+') => {
                        self.advance();
                        self.make_token(TokenKind::Inc, span, "++")
                    }
                    Some(b'=') => {
                        self.advance();
                        self.make_token(TokenKind::PlusAssign, span, "+=")
                    }
                    _ => self.make_token(TokenKind::Plus, span, "+"),
                }
            }
            b'-' => {
                self.advance();
                match self.peek() {
                    Some(b'-') => {
                        self.advance();
                        self.make_token(TokenKind::Dec, span, "--")
                    }
                    Some(b'=') => {
                        self.advance();
                        self.make_token(TokenKind::MinusAssign, span, "-=")
                    }
                    _ => self.make_token(TokenKind::Minus, span, "-"),
                }
            }
            b'*' => {
                self.advance();
                if self.peek() == Some(b'=') {
                    self.advance();
                    self.make_token(TokenKind::StarAssign, span, "*=")
                } else {
                    self.make_token(TokenKind::Star, span, "*")
                }
            }
            b'/' => {
                self.advance();
                if self.peek() == Some(b'=') {
                    self.advance();
                    self.make_token(TokenKind::SlashAssign, span, "/=")
                } else {
                    self.make_token(TokenKind::Slash, span, "/")
                }
            }
            b'%' => {
                self.advance();
                if self.peek() == Some(b'=') {
                    self.advance();
                    self.make_token(TokenKind::PercentAssign, span, "%=")
                } else {
                    self.make_token(TokenKind::Percent, span, "%")
                }
            }
            b'&' => {
                self.advance();
                match self.peek() {
                    Some(b'&') => {
                        self.advance();
                        self.make_token(TokenKind::AmpAmp, span, "&&")
                    }
                    Some(b'=') => {
                        self.advance();
                        self.make_token(TokenKind::AmpAssign, span, "&=")
                    }
                    Some(b'^') => {
                        self.advance();
                        if self.peek() == Some(b'=') {
                            self.advance();
                            self.make_token(TokenKind::AmpCaretAssign, span, "&^=")
                        } else {
                            self.make_token(TokenKind::AmpCaret, span, "&^")
                        }
                    }
                    _ => self.make_token(TokenKind::Amp, span, "&"),
                }
            }
            b'|' => {
                self.advance();
                match self.peek() {
                    Some(b'|') => {
                        self.advance();
                        self.make_token(TokenKind::PipePipe, span, "||")
                    }
                    Some(b'=') => {
                        self.advance();
                        self.make_token(TokenKind::PipeAssign, span, "|=")
                    }
                    _ => self.make_token(TokenKind::Pipe, span, "|"),
                }
            }
            b'^' => {
                self.advance();
                if self.peek() == Some(b'=') {
                    self.advance();
                    self.make_token(TokenKind::CaretAssign, span, "^=")
                } else {
                    self.make_token(TokenKind::Caret, span, "^")
                }
            }
            b'<' => {
                self.advance();
                match self.peek() {
                    Some(b'<') => {
                        self.advance();
                        if self.peek() == Some(b'=') {
                            self.advance();
                            self.make_token(TokenKind::ShlAssign, span, "<<=")
                        } else {
                            self.make_token(TokenKind::Shl, span, "<<")
                        }
                    }
                    Some(b'=') => {
                        self.advance();
                        self.make_token(TokenKind::LtEq, span, "<=")
                    }
                    Some(b'-') => {
                        self.advance();
                        self.make_token(TokenKind::Arrow, span, "<-")
                    }
                    _ => self.make_token(TokenKind::Lt, span, "<"),
                }
            }
            b'>' => {
                self.advance();
                match self.peek() {
                    Some(b'>') => {
                        self.advance();
                        if self.peek() == Some(b'=') {
                            self.advance();
                            self.make_token(TokenKind::ShrAssign, span, ">>=")
                        } else {
                            self.make_token(TokenKind::Shr, span, ">>")
                        }
                    }
                    Some(b'=') => {
                        self.advance();
                        self.make_token(TokenKind::GtEq, span, ">=")
                    }
                    _ => self.make_token(TokenKind::Gt, span, ">"),
                }
            }
            b'=' => {
                self.advance();
                if self.peek() == Some(b'=') {
                    self.advance();
                    self.make_token(TokenKind::EqEq, span, "==")
                } else {
                    self.make_token(TokenKind::Eq, span, "=")
                }
            }
            b'!' => {
                self.advance();
                if self.peek() == Some(b'=') {
                    self.advance();
                    self.make_token(TokenKind::NotEq, span, "!=")
                } else {
                    self.make_token(TokenKind::Bang, span, "!")
                }
            }
            b':' => {
                self.advance();
                if self.peek() == Some(b'=') {
                    self.advance();
                    self.make_token(TokenKind::ColonEq, span, ":=")
                } else {
                    self.make_token(TokenKind::Colon, span, ":")
                }
            }
            b'.' => {
                self.advance();
                if self.peek() == Some(b'.') && self.peek_ahead(1) == Some(b'.') {
                    self.advance();
                    self.advance();
                    self.make_token(TokenKind::Ellipsis, span, "...")
                } else {
                    self.make_token(TokenKind::Dot, span, ".")
                }
            }
            b'(' => {
                self.advance();
                self.make_token(TokenKind::LParen, span, "(")
            }
            b')' => {
                self.advance();
                self.make_token(TokenKind::RParen, span, ")")
            }
            b'[' => {
                self.advance();
                self.make_token(TokenKind::LBrack, span, "[")
            }
            b']' => {
                self.advance();
                self.make_token(TokenKind::RBrack, span, "]")
            }
            b'{' => {
                self.advance();
                self.make_token(TokenKind::LBrace, span, "{")
            }
            b'}' => {
                self.advance();
                self.make_token(TokenKind::RBrace, span, "}")
            }
            b',' => {
                self.advance();
                self.make_token(TokenKind::Comma, span, ",")
            }
            b';' => {
                self.advance();
                self.make_token(TokenKind::Semi, span, ";")
            }
            _ => {
                self.advance();
                self.make_token(TokenKind::Invalid, span, &self.source[start_pos..self.pos])
            }
        };

        self.insert_semi = token.kind.triggers_semi_insertion();
        token
    }
}
