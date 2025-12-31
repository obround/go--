#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Ident,
    IntLit,
    FloatLit,
    StringLit,

    Break,
    Case,
    Const,
    Continue,
    Default,
    Else,
    For,
    Func,
    If,
    Import,
    Map,
    Package,
    Return,
    Struct,
    Switch,
    Type,
    Var,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Amp,
    Pipe,
    Caret,
    Shl,
    Shr,
    AmpCaret,

    PlusAssign,
    MinusAssign,
    StarAssign,
    SlashAssign,
    PercentAssign,
    AmpAssign,
    PipeAssign,
    CaretAssign,
    ShlAssign,
    ShrAssign,
    AmpCaretAssign,

    AmpAmp,
    PipePipe,
    Arrow,
    Inc,
    Dec,
    EqEq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,

    Eq,
    ColonEq,
    Ellipsis,

    LParen,
    RParen,
    LBrack,
    RBrack,
    LBrace,
    RBrace,
    Comma,
    Dot,
    Semi,
    Colon,

    Bang,

    Eof,
    Invalid,
}

impl TokenKind {
    pub fn from_keyword(s: &str) -> Option<TokenKind> {
        match s {
            "break" => Some(TokenKind::Break),
            "case" => Some(TokenKind::Case),
            "const" => Some(TokenKind::Const),
            "continue" => Some(TokenKind::Continue),
            "default" => Some(TokenKind::Default),
            "else" => Some(TokenKind::Else),
            "for" => Some(TokenKind::For),
            "func" => Some(TokenKind::Func),
            "if" => Some(TokenKind::If),
            "import" => Some(TokenKind::Import),
            "map" => Some(TokenKind::Map),
            "package" => Some(TokenKind::Package),
            "return" => Some(TokenKind::Return),
            "struct" => Some(TokenKind::Struct),
            "switch" => Some(TokenKind::Switch),
            "type" => Some(TokenKind::Type),
            "var" => Some(TokenKind::Var),
            "range" => Some(TokenKind::Ident),
            _ => None,
        }
    }

    pub fn triggers_semi_insertion(self) -> bool {
        matches!(
            self,
            TokenKind::Ident
                | TokenKind::IntLit
                | TokenKind::FloatLit
                | TokenKind::StringLit
                | TokenKind::Break
                | TokenKind::Continue
                | TokenKind::Return
                | TokenKind::Inc
                | TokenKind::Dec
                | TokenKind::RParen
                | TokenKind::RBrack
                | TokenKind::RBrace
        )
    }

    pub fn description(self) -> &'static str {
        match self {
            TokenKind::Ident => "identifier",
            TokenKind::IntLit => "integer literal",
            TokenKind::FloatLit => "float literal",
            TokenKind::StringLit => "string literal",
            TokenKind::Break => "break",
            TokenKind::Case => "case",
            TokenKind::Const => "const",
            TokenKind::Continue => "continue",
            TokenKind::Default => "default",
            TokenKind::Else => "else",
            TokenKind::For => "for",
            TokenKind::Func => "func",
            TokenKind::If => "if",
            TokenKind::Import => "import",
            TokenKind::Map => "map",
            TokenKind::Package => "package",
            TokenKind::Return => "return",
            TokenKind::Struct => "struct",
            TokenKind::Switch => "switch",
            TokenKind::Type => "type",
            TokenKind::Var => "var",
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Star => "*",
            TokenKind::Slash => "/",
            TokenKind::Percent => "%",
            TokenKind::Amp => "&",
            TokenKind::Pipe => "|",
            TokenKind::Caret => "^",
            TokenKind::Shl => "<<",
            TokenKind::Shr => ">>",
            TokenKind::AmpCaret => "&^",
            TokenKind::PlusAssign => "+=",
            TokenKind::MinusAssign => "-=",
            TokenKind::StarAssign => "*=",
            TokenKind::SlashAssign => "/=",
            TokenKind::PercentAssign => "%=",
            TokenKind::AmpAssign => "&=",
            TokenKind::PipeAssign => "|=",
            TokenKind::CaretAssign => "^=",
            TokenKind::ShlAssign => "<<=",
            TokenKind::ShrAssign => ">>=",
            TokenKind::AmpCaretAssign => "&^=",
            TokenKind::AmpAmp => "&&",
            TokenKind::PipePipe => "||",
            TokenKind::Arrow => "<-",
            TokenKind::Inc => "++",
            TokenKind::Dec => "--",
            TokenKind::EqEq => "==",
            TokenKind::NotEq => "!=",
            TokenKind::Lt => "<",
            TokenKind::LtEq => "<=",
            TokenKind::Gt => ">",
            TokenKind::GtEq => ">=",
            TokenKind::Eq => "=",
            TokenKind::ColonEq => ":=",
            TokenKind::Ellipsis => "...",
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LBrack => "[",
            TokenKind::RBrack => "]",
            TokenKind::LBrace => "{",
            TokenKind::RBrace => "}",
            TokenKind::Comma => ",",
            TokenKind::Dot => ".",
            TokenKind::Semi => ";",
            TokenKind::Colon => ":",
            TokenKind::Bang => "!",
            TokenKind::Eof => "EOF",
            TokenKind::Invalid => "invalid token",
        }
    }

    pub fn is_assign_op(self) -> bool {
        matches!(
            self,
            TokenKind::Eq
                | TokenKind::PlusAssign
                | TokenKind::MinusAssign
                | TokenKind::StarAssign
                | TokenKind::SlashAssign
                | TokenKind::PercentAssign
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub struct Span {
    pub line: usize,
    pub col: usize,
}

impl Span {
    pub fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub literal: String,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span, literal: String) -> Self {
        Self {
            kind,
            span,
            literal,
        }
    }
}
