use crate::token::Span;

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub imports: Vec<ImportDecl>,
    pub decls: Vec<TopLevelDecl>,
}

#[derive(Debug, Clone)]
pub struct ImportDecl {
    #[allow(dead_code)]
    pub span: Span,
    pub path: String,
}

#[derive(Debug, Clone)]
pub enum TopLevelDecl {
    Const(ConstDecl),
    Type(TypeDecl),
    Var(VarDecl),
    Function(FunctionDecl),
    Method(MethodDecl),
}

#[derive(Debug, Clone)]
pub struct ConstDecl {
    pub span: Span,
    pub name: String,
    pub typ: Option<Type>,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub span: Span,
    pub name: String,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub span: Span,
    pub name: String,
    pub typ: Option<Type>,
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub span: Span,
    pub name: String,
    pub signature: Signature,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct MethodDecl {
    #[allow(dead_code)]
    pub span: Span,
    pub receiver: Receiver,
    pub name: String,
    pub name_span: Span,
    pub signature: Signature,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct Receiver {
    pub span: Span,
    pub name: String,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct Signature {
    pub params: Vec<ParameterDecl>,
    pub result: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct ParameterDecl {
    pub span: Span,
    pub name: Option<String>,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub enum Type {
    Name(Span, String),
    Qualified(Span, String, String),
    Array(Span, Box<Expr>, Box<Type>),
    Slice(Span, Box<Type>),
    Struct(Span, Vec<FieldDecl>),
    Pointer(Span, Box<Type>),
    Function(Span, Box<Signature>),
    Map(Span, Box<Type>, Box<Type>),
    Paren(Span, Box<Type>),
}

#[derive(Debug, Clone)]
pub struct FieldDecl {
    pub span: Span,
    pub names: Vec<String>,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct Block {
    #[allow(dead_code)]
    pub span: Span,
    pub end: Span,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Decl(Declaration),
    Simple(SimpleStmt),
    Return(Span, Option<Expr>),
    Break(Span),
    Continue(Span),
    Block(Block),
    If(IfStmt),
    Switch(SwitchStmt),
    For(ForStmt),
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Const(ConstDecl),
    Type(TypeDecl),
    Var(VarDecl),
}

#[derive(Debug, Clone)]
pub enum SimpleStmt {
    #[allow(dead_code)]
    Empty(Span),
    Expr(Expr),
    IncDec(Span, Expr, IncDecOp),
    Assign(Span, Expr, AssignOp, Expr),
    /// Short variable declaration: name := value
    ShortVarDecl(Span, String, Expr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IncDecOp {
    Inc,
    Dec,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignOp {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    #[allow(dead_code)]
    pub span: Span,
    pub init: Option<Box<SimpleStmt>>,
    pub cond: Expr,
    pub body: Block,
    pub else_branch: Option<Else>,
}

#[derive(Debug, Clone)]
pub enum Else {
    If(Box<IfStmt>),
    Block(Block),
}

#[derive(Debug, Clone)]
pub struct SwitchStmt {
    #[allow(dead_code)]
    pub span: Span,
    pub init: Option<Box<SimpleStmt>>,
    pub tag: Option<Expr>,
    pub cases: Vec<CaseClause>,
}

#[derive(Debug, Clone)]
pub struct CaseClause {
    pub span: Span,
    pub case: SwitchCase,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum SwitchCase {
    Expr(Expr),
    Default,
}

#[derive(Debug, Clone)]
pub struct ForStmt {
    #[allow(dead_code)]
    pub span: Span,
    pub clause: ForClause,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub enum ForClause {
    Infinite,
    Condition(Expr),
    Classic(
        Option<Box<SimpleStmt>>,
        Option<Expr>,
        Option<Box<SimpleStmt>>,
    ),
    Range(RangeClause, Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct RangeClause {
    pub span: Span,
    pub key: Option<String>,
    pub value: Option<String>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(Span, Box<Expr>, BinaryOp, Box<Expr>),
    Unary(Span, UnaryOp, Box<Expr>),
    Ident(Span, String),
    IntLit(Span, String),
    FloatLit(Span, String),
    StringLit(Span, String),
    CompositeLit(Span, LiteralType, LiteralValue),
    TypeVal(Span, Type),
    Paren(Span, Box<Expr>),
    Selector(Span, Box<Expr>, String),
    Index(Span, Box<Expr>, Box<Expr>),
    Slice(Span, Box<Expr>, Option<Box<Expr>>, Option<Box<Expr>>),
    Call(Span, Box<Expr>, Vec<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Or,
    And,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Pos,
    Neg,
    Not,
    Deref,
    Addr,
}

#[derive(Debug, Clone)]
pub enum LiteralType {
    Array(Span, Box<Expr>, Box<Type>),
    #[allow(dead_code)]
    Slice(Span, Box<Type>),
    #[allow(dead_code)]
    Struct(Span, Vec<FieldDecl>),
    #[allow(dead_code)]
    Map(Span, Box<Type>, Box<Type>),
    Name(Span, String),
    // TODO: Implement qualified types
    #[allow(dead_code)]
    Qualified(Span, String, String),
}

#[derive(Debug, Clone)]
pub struct LiteralValue {
    pub span: Span,
    pub elements: Vec<KeyedElement>,
}

#[derive(Debug, Clone)]
pub struct KeyedElement {
    pub key: Option<Key>,
    pub value: Element,
}

#[derive(Debug, Clone)]
pub enum Key {
    Field(Span, String),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Element {
    Expr(Expr),
    Lit(LiteralValue),
}

impl Type {
    pub fn span(&self) -> Span {
        match self {
            Type::Name(s, _) => *s,
            Type::Qualified(s, _, _) => *s,
            Type::Array(s, _, _) => *s,
            Type::Slice(s, _) => *s,
            Type::Struct(s, _) => *s,
            Type::Pointer(s, _) => *s,
            Type::Function(s, _) => *s,
            Type::Map(s, _, _) => *s,
            Type::Paren(s, _) => *s,
        }
    }
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Binary(s, _, _, _) => *s,
            Expr::Unary(s, _, _) => *s,
            Expr::Ident(s, _) => *s,
            Expr::IntLit(s, _) => *s,
            Expr::FloatLit(s, _) => *s,
            Expr::StringLit(s, _) => *s,
            Expr::CompositeLit(s, _, _) => *s,
            Expr::TypeVal(s, _) => *s,
            Expr::Paren(s, _) => *s,
            Expr::Selector(s, _, _) => *s,
            Expr::Index(s, _, _) => *s,
            Expr::Slice(s, _, _, _) => *s,
            Expr::Call(s, _, _) => *s,
        }
    }

    pub fn as_ident(&self) -> Option<&str> {
        match self {
            Expr::Ident(_, name) => Some(name),
            _ => None,
        }
    }
}

impl BinaryOp {
    pub fn name(&self) -> &'static str {
        match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::Eq => "==",
            BinaryOp::Ne => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Le => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::Ge => ">=",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
        }
    }
}

impl UnaryOp {
    pub fn name(&self) -> &'static str {
        match self {
            UnaryOp::Pos => "+",
            UnaryOp::Neg => "-",
            UnaryOp::Not => "!",
            UnaryOp::Deref => "*",
            UnaryOp::Addr => "&",
        }
    }
}
