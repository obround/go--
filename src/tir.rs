//! Typed Intermediate Representation (TIR) for the Go compiler

use crate::analysis::{SymbolId, TypeId};

#[derive(Debug, Clone)]
pub enum TExpr {
    // Literals
    Int {
        value: i64,
        type_id: TypeId,
    },
    Uint {
        value: u64,
        type_id: TypeId,
    },
    Float {
        value: f64,
        type_id: TypeId,
    },
    String {
        value: String,
        type_id: TypeId,
    },

    Nil {
        type_id: TypeId,
    },

    // Variable/const reference
    Var {
        symbol_id: SymbolId,
        type_id: TypeId,
    },
    Const {
        value: Box<ConstValue>,
        type_id: TypeId,
    },

    // Binary operation
    Binary {
        op: TBinaryOp,
        left: Box<TExpr>,
        right: Box<TExpr>,
        type_id: TypeId,
    },

    // Unary operation
    Unary {
        op: TUnaryOp,
        operand: Box<TExpr>,
        type_id: TypeId,
    },

    // Function/method call
    Call {
        kind: TCallKind,
        args: Vec<TExpr>,
        type_id: TypeId,
    },

    // Type conversion
    Convert {
        kind: TConversionKind,
        expr: Box<TExpr>,
        type_id: TypeId,
    },

    // Field access
    Field {
        base: Box<TExpr>,
        struct_type: TypeId,
        field_index: u32,
        auto_deref: u32,
        type_id: TypeId,
    },

    // Index operation
    Index {
        kind: TIndexKind,
        base: Box<TExpr>,
        index: Box<TExpr>,
        type_id: TypeId,
    },

    // Slice operation
    Slice {
        kind: TSliceKind,
        base: Box<TExpr>,
        low: Option<Box<TExpr>>,
        high: Option<Box<TExpr>>,
        type_id: TypeId,
    },

    // Composite literal
    Composite {
        kind: TCompositeKind,
        type_id: TypeId,
    },

    // Address-of
    AddrOf {
        operand: Box<TExpr>,
        type_id: TypeId,
    },

    // Dereference
    Deref {
        operand: Box<TExpr>,
        type_id: TypeId,
    },
}

impl TExpr {
    pub fn type_id(&self) -> TypeId {
        match self {
            TExpr::Int { type_id, .. } => *type_id,
            TExpr::Uint { type_id, .. } => *type_id,
            TExpr::Float { type_id, .. } => *type_id,
            TExpr::String { type_id, .. } => *type_id,
            TExpr::Nil { type_id } => *type_id,
            TExpr::Var { type_id, .. } => *type_id,
            TExpr::Const { type_id, .. } => *type_id,
            TExpr::Binary { type_id, .. } => *type_id,
            TExpr::Unary { type_id, .. } => *type_id,
            TExpr::Call { type_id, .. } => *type_id,
            TExpr::Convert { type_id, .. } => *type_id,
            TExpr::Field { type_id, .. } => *type_id,
            TExpr::Index { type_id, .. } => *type_id,
            TExpr::Slice { type_id, .. } => *type_id,
            TExpr::Composite { type_id, .. } => *type_id,
            TExpr::AddrOf { type_id, .. } => *type_id,
            TExpr::Deref { type_id, .. } => *type_id,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TBinaryOp {
    // Integer arithmetic
    IntAdd,
    IntSub,
    IntMul,
    IntDiv { signed: bool },
    IntMod { signed: bool },

    // Float arithmetic
    FloatAdd,
    FloatSub,
    FloatMul,
    FloatDiv,

    // String concatenation
    StringConcat,

    // Integer comparisons
    IntLt { signed: bool },
    IntLe { signed: bool },
    IntGt { signed: bool },
    IntGe { signed: bool },
    IntEq,
    IntNe,

    // Float comparisons
    FloatLt,
    FloatLe,
    FloatGt,
    FloatGe,
    FloatEq,
    FloatNe,

    // String comparisons
    StringLt,
    StringLe,
    StringGt,
    StringGe,
    StringEq,
    StringNe,

    // Bool comparisons
    BoolEq,
    BoolNe,

    // Pointer comparisons
    PointerEq,
    PointerNe,

    // Struct/array equality
    StructEq { equality: TEqualityInfo },
    StructNe { equality: TEqualityInfo },
    ArrayEq { equality: TEqualityInfo },
    ArrayNe { equality: TEqualityInfo },

    // Nil comparisons
    SliceNilEq { slice_on_left: bool },
    SliceNilNe { slice_on_left: bool },
    MapNilEq { map_on_left: bool },
    MapNilNe { map_on_left: bool },
    PointerNilEq { pointer_on_left: bool },
    PointerNilNe { pointer_on_left: bool },
    FuncNilEq { func_on_left: bool },
    FuncNilNe { func_on_left: bool },

    // Logical
    And,
    Or,
}

#[derive(Debug, Clone, Copy)]
pub enum TUnaryOp {
    IntNeg,
    FloatNeg,
    BoolNot,
}

#[derive(Debug, Clone)]
pub enum TCallKind {
    Function {
        symbol_id: SymbolId,
    },
    Method {
        receiver: Box<TExpr>,
        mangled_name: String,
        receiver_is_pointer: bool,
    },
    Indirect {
        callee: Box<TExpr>,
    },

    // Builtins
    Len(TLenTarget),
    Cap(TCapTarget),
    New {
        elem_type: TypeId,
    },
    Make(TMakeInfo),
    Append {
        slice: Box<TExpr>,
        elems: Vec<TExpr>,
        elem_type: TypeId,
    },
    Copy {
        dst: Box<TExpr>,
        src: Box<TExpr>,
        elem_type: TypeId,
    },
    Delete {
        map: Box<TExpr>,
        key: Box<TExpr>,
        key_type: TypeId,
    },
    Print {
        newline: bool,
        args: Vec<(TExpr, TPrintKind)>,
    },
    Panic {
        msg: Box<TExpr>,
    },
}

#[derive(Debug, Clone)]
pub enum TLenTarget {
    Array { len: u64 },
    Slice { expr: Box<TExpr> },
    String { expr: Box<TExpr> },
    Map { expr: Box<TExpr> },
}

#[derive(Debug, Clone)]
pub enum TCapTarget {
    Array { len: u64 },
    Slice { expr: Box<TExpr> },
}

#[derive(Debug, Clone)]
pub struct TMakeInfo {
    pub kind: TMakeKind,
}

#[derive(Debug, Clone)]
pub enum TMakeKind {
    Slice {
        elem_type: TypeId,
        len: Box<TExpr>,
        cap: Option<Box<TExpr>>,
    },
    Map {
        key_type: TypeId,
        value_type: TypeId,
        key_is_string: bool,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum TPrintKind {
    Bool,
    SignedInt,
    UnsignedInt,
    Float,
    String,
    Pointer,
}

#[derive(Debug, Clone)]
pub enum TIndexKind {
    Array {
        len: u64,
    },
    Slice {
        elem_type: TypeId,
    },
    String,
    Map {
        key_type: TypeId,
        value_type: TypeId,
    },
}

#[derive(Debug, Clone)]
pub enum TSliceKind {
    Array { len: u64, elem_type: TypeId },
    Slice { elem_type: TypeId },
    String,
}

#[derive(Debug, Clone)]
pub enum TCompositeKind {
    Struct {
        fields: Vec<(u32, TExpr)>,
    },
    Array {
        elem_type: TypeId,
        elements: Vec<TExpr>,
    },
    Slice {
        elem_type: TypeId,
        elements: Vec<TExpr>,
    },
    Map {
        key_type: TypeId,
        value_type: TypeId,
        entries: Vec<(TExpr, TExpr)>,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum TConversionKind {
    IntToInt { src_signed: bool },
    IntToFloat { src_signed: bool },
    FloatToInt { dst_signed: bool },
    FloatToFloat,
    IntToString,
    StringToBytes,
    BytesToString,
    Identity,
}

#[derive(Debug, Clone)]
pub enum TEqualityInfo {
    Primitive(TPrimitiveEq),
    Struct { fields: Vec<(u32, TEqualityInfo)> },
    Array { len: u64, elem: Box<TEqualityInfo> },
}

#[derive(Debug, Clone, Copy)]
pub enum TPrimitiveEq {
    Int,
    Float,
    Bool,
    String,
    Pointer,
}

#[derive(Debug, Clone)]
pub enum TLValue {
    Var {
        symbol_id: SymbolId,
        type_id: TypeId,
    },
    Field {
        base: Box<TExpr>,
        struct_type: TypeId,
        field_index: u32,
        auto_deref: u32,
        type_id: TypeId,
    },
    Index {
        kind: TIndexKind,
        base: Box<TExpr>,
        index: Box<TExpr>,
        type_id: TypeId,
    },
    Deref {
        operand: Box<TExpr>,
        type_id: TypeId,
    },
    Blank,
}

impl TLValue {
    pub fn type_id(&self) -> TypeId {
        match self {
            TLValue::Var { type_id, .. } => *type_id,
            TLValue::Field { type_id, .. } => *type_id,
            TLValue::Index { type_id, .. } => *type_id,
            TLValue::Deref { type_id, .. } => *type_id,
            TLValue::Blank => TypeId::INVALID,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TStmt {
    VarDecl {
        symbol_id: SymbolId,
        type_id: TypeId,
        init: Option<TExpr>,
    },
    Assign {
        lhs: TLValue,
        rhs: TExpr,
    },
    CompoundAssign {
        kind: TCompoundKind,
        lhs: TLValue,
        rhs: TExpr,
    },
    IncDec {
        kind: TIncDecKind,
        operand: TLValue,
    },
    Expr(TExpr),
    Return(Option<TExpr>),
    Block(Vec<TStmt>),
    If {
        cond: TExpr,
        then_block: Vec<TStmt>,
        else_block: Option<Vec<TStmt>>,
    },
    For {
        kind: TForKind,
        body: Vec<TStmt>,
    },
    Switch {
        tag: Option<TExpr>,
        tag_kind: TSwitchKind,
        cases: Vec<TCase>,
    },
    Break,
    Continue,
}

#[derive(Debug, Clone, Copy)]
pub enum TCompoundKind {
    IntAdd,
    IntSub,
    IntMul,
    IntDiv { signed: bool },
    IntMod { signed: bool },
    FloatAdd,
    FloatSub,
    FloatMul,
    FloatDiv,
    StringConcat,
}

#[derive(Debug, Clone, Copy)]
pub enum TIncDecKind {
    IncSigned,
    IncUnsigned,
    IncFloat,
    DecSigned,
    DecUnsigned,
    DecFloat,
}

#[derive(Debug, Clone)]
pub enum TForKind {
    Infinite,
    Condition {
        cond: TExpr,
    },
    Classic {
        init: Option<Box<TStmt>>,
        cond: Option<TExpr>,
        post: Option<Box<TStmt>>,
    },
    RangeArray {
        len: u64,
        elem_type: TypeId,
        expr: TExpr,
        key_sym: Option<SymbolId>,
        val_sym: Option<SymbolId>,
    },
    RangeSlice {
        elem_type: TypeId,
        expr: TExpr,
        key_sym: Option<SymbolId>,
        val_sym: Option<SymbolId>,
    },
    RangeString {
        expr: TExpr,
        key_sym: Option<SymbolId>,
        val_sym: Option<SymbolId>,
    },
    RangeMap {
        key_type: TypeId,
        value_type: TypeId,
        expr: TExpr,
        key_sym: Option<SymbolId>,
        val_sym: Option<SymbolId>,
    },
}

#[derive(Debug, Clone)]
pub enum TSwitchKind {
    Bool,
    Int,
    Float,
    String,
    Pointer,
}

#[derive(Debug, Clone)]
pub struct TCase {
    pub values: Vec<TExpr>,
    pub body: Vec<TStmt>,
}

#[derive(Debug, Clone)]
pub struct TFunction {
    pub symbol_id: SymbolId,
    pub name: String,
    pub params: Vec<(SymbolId, TypeId)>,
    pub result: Option<TypeId>,
    pub body: Vec<TStmt>,
}

#[derive(Debug, Clone)]
pub struct TMethod {
    pub receiver_sym: SymbolId,
    pub receiver_type: TypeId,
    pub receiver_is_pointer: bool,
    #[allow(dead_code)]
    pub name: String,
    pub mangled_name: String,
    pub params: Vec<(SymbolId, TypeId)>,
    pub result: Option<TypeId>,
    pub body: Vec<TStmt>,
}

#[derive(Debug, Clone)]
pub struct TGlobal {
    pub symbol_id: SymbolId,
    pub name: String,
    pub type_id: TypeId,
    pub init: Option<TExpr>,
}

#[derive(Debug, Clone)]
pub struct TProgram {
    pub functions: Vec<TFunction>,
    pub methods: Vec<TMethod>,
    pub globals: Vec<TGlobal>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    Int(i128),
    Uint(u128),
    Float(f64),
    String(String),
    Bool(bool),
    Nil,
}
