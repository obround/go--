use crate::token::Span;
use std::borrow::Cow;
use std::path::Path;
use std::{error, fmt, result};

pub struct SyntaxError {
    pub file: String,
    pub span: Span,
    pub message: String,
}

impl SyntaxError {
    pub fn new(file: &str, span: Span, message: String) -> Self {
        Self {
            file: file.to_string(),
            span,
            message,
        }
    }

    pub fn expected(file: &str, span: Span, expected: &str, found: &str) -> Self {
        Self::new(
            file,
            span,
            format!("expected '{}', found {}", expected, found),
        )
    }

    pub fn unexpected_expected(file: &str, span: Span, found: &str, expected: &str) -> Self {
        Self::new(
            file,
            span,
            format!("syntax error: unexpected {}, expected {}", found, expected),
        )
    }

    pub fn non_decl_outside_func(file: &str, span: Span) -> Self {
        Self::new(
            file,
            span,
            "syntax error: non-declaration statement outside function body".to_string(),
        )
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let path = Path::new(&self.file);
        let display_path: Cow<str> = if path.is_absolute() {
            format!(
                "./{}",
                path.file_name().unwrap_or_default().to_string_lossy()
            )
            .into()
        } else {
            (&self.file).into()
        };
        write!(
            f,
            "{}:{}:{}: {}",
            display_path, self.span.line, self.span.col, self.message
        )
    }
}

impl fmt::Debug for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl error::Error for SyntaxError {}

pub type Result<T> = result::Result<T, SyntaxError>;
