//! Error reporting infrastructure for semantic analysis

use crate::token::Span;
use std::error;
use std::fmt;
use std::mem;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorCode {
    // System errors
    TooManyErrors,

    // Declaration errors
    DuplicateDecl,
    InvalidMainDecl,
    UndeclaredName,
    UndeclaredImportedName,
    UnusedVar,

    // Type definition errors
    InvalidTypeCycle,
    NotAType,
    InvalidArrayLen,
    IncomparableMapKey,
    DuplicateField,
    DuplicateParam,

    // Method errors
    InvalidRecv,
    InvalidRecvBaseType,
    DuplicateMethod,
    DuplicateFieldAndMethod,

    // Expression errors
    NotAnExpr,
    InvalidBlank,
    UncalledBuiltin,
    UndefinedOp,
    MismatchedTypes,
    NonNumericIncDec,
    UnaddressableOperand,
    InvalidIndirection,

    // Constant errors
    InvalidConstInit,
    InvalidConstType,
    UntypedNilUse,
    NumericOverflow,
    DivByZero,
    InvalidConstExpr,

    // Indexing and slicing errors
    NonIndexableOperand,
    InvalidIndex,
    InvalidIndexNeg,
    InvalidIndexBounds,
    NonSliceableOperand,
    SwappedSliceIndices,
    InvalidMapKey,

    // Field/method access errors
    MissingFieldOrMethod,

    // Call errors
    InvalidCall,
    InvalidConversion,
    WrongArgCount,
    IncompatibleArg,
    InvalidMakeArg,

    // Literal errors
    InvalidLit,
    UntypedLit,
    DuplicateLitKey,
    MissingLitKey,
    InvalidLitIndex,
    InvalidLitIndexNeg,
    InvalidLitIndexBounds,
    MixedStructLit,
    InvalidStructLit,
    MissingLitField,
    DuplicateLitField,
    InvalidLitField,

    // Assignment errors
    IncompatibleAssign,
    UnassignableOperand,

    // Control flow errors
    InvalidCond,
    MisplacedBreak,
    MisplacedContinue,
    InvalidPostDecl,
    InvalidRangeExpr,
    DuplicateCase,
    DuplicateDefault,
    InvalidCaseType,
    MissingReturn,
    WrongResultCount,
}

/// Related info for an error (e.g., "other declaration here")
#[derive(Debug, Clone)]
pub struct RelatedInfo {
    pub span: Span,
    pub message: String,
}

/// A semantic analysis error with location and message
#[derive(Debug, Clone)]
pub struct SemanticError {
    // Error codes are just good for categorization; not used for anything else
    #[allow(dead_code)]
    pub code: ErrorCode,
    pub span: Span,
    pub message: String,
    pub related: Option<RelatedInfo>,
    pub file: String,
}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}:{}: {}",
            self.file, self.span.line, self.span.col, self.message
        )
    }
}

impl error::Error for SemanticError {}

pub struct ErrorReporter {
    errors: Vec<SemanticError>,
    file: String,
    max_errors: usize,
    stopped: bool,
}

impl ErrorReporter {
    pub fn new(file: &str) -> Self {
        Self {
            errors: Vec::new(),
            file: file.to_string(),
            max_errors: 10,
            stopped: false,
        }
    }

    pub fn file_name(&self) -> &str {
        &self.file
    }

    pub fn error(&mut self, code: ErrorCode, span: Span, message: impl Into<String>) {
        if self.stopped {
            return;
        }
        self.errors.push(SemanticError {
            code,
            span,
            message: message.into(),
            related: None,
            file: self.file.clone(),
        });
        self.maybe_stop(span);
    }

    pub fn error_with_related(
        &mut self,
        code: ErrorCode,
        span: Span,
        message: impl Into<String>,
        related_span: Span,
        related_message: impl Into<String>,
    ) {
        if self.stopped {
            return;
        }
        self.errors.push(SemanticError {
            code,
            span,
            message: message.into(),
            related: Some(RelatedInfo {
                span: related_span,
                message: related_message.into(),
            }),
            file: self.file.clone(),
        });
        self.maybe_stop(span);
    }

    fn maybe_stop(&mut self, span: Span) {
        if !self.stopped && self.errors.len() == self.max_errors {
            self.errors.push(SemanticError {
                code: ErrorCode::TooManyErrors,
                span,
                message: "too many errors".to_string(),
                related: None,
                file: self.file.clone(),
            });
            self.stopped = true;
        }
    }

    pub fn take_errors(&mut self) -> Vec<SemanticError> {
        mem::take(&mut self.errors)
    }

    pub fn file(&self) -> &str {
        &self.file
    }
}
