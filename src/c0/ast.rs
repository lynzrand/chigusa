/*
    This file is loosely inspired by Rust's own AST implementation:
    https://github.com/rust-lang/rust/blob/master/src/libsyntax/ast.rs
*/

use super::infra::*;
use indexmap::IndexMap;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::iter::Iterator;
use std::ops::{Deref, DerefMut};
use std::rc::{Rc, Weak};

pub type TypeIdent = u64;

pub struct Program {
    pub all_vars: Vec<u64>,
    pub all_types: Vec<u64>,
}

pub enum TypeDef {
    Primitive(PrimitiveType),
    Struct(StructType),
    Function(FunctionType),
    Ref(RefType),
    Array(ArrayType),
}

pub enum PrimitiveTypeVar {
    SignedInt,
    UnsignedInt,
    Float,
}

pub struct PrimitiveType {
    pub occupy_bytes: usize,
    pub var: PrimitiveTypeVar,
}

pub struct StructType {
    /// Fields of this struct, described as universal identifiers
    pub field_types: Vec<TypeIdent>,
    pub field_offsets: Vec<usize>,
    pub occupy_bytes: usize,
}

pub struct FunctionType {
    pub params: Vec<TypeIdent>,
    pub return_type: TypeIdent,
}

pub struct RefType {
    pub target: TypeIdent,
}

pub struct ArrayType {
    pub target: TypeIdent,
    pub length: usize,
}

pub struct Stmt {
    pub var: StmtVariant,
    pub span: Span,
}

pub enum StmtVariant {
    Expr(Expr),
    Return(Expr),
    Break(Expr),
    Empty,
}

pub struct Expr {
    pub var: ExprVariant,
    pub typ: TypeDef,
    pub span: Span,
}

pub enum ExprVariant {
    Literal(Literal),
    TypeConversion(TypeConversion),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    FunctionCall(FunctionCall),
    StructChild(StructChild),
    ArrayChild(ArrayChild),

    /// If conditional.
    ///
    /// `if` `(` Expression `)` (Expression | Statement)
    /// (`else` (Expression | Statement))?
    IfConditional(IfConditional),

    /// While conditional. Takes the value on break or the last iteration as its
    /// return value.
    ///
    /// `while` `(` Expression `)` BlockExpression
    WhileConditional(WhileConditional),

    /// Block expression. Similar to that in Rust.
    ///
    /// `{` Statement* Expression? `}`
    Block(Block),
}

pub enum Literal {
    Integer { val: ramp::int::Int },
    Float { val: f64 },
    Struct { typ: u64, fields: Vec<Expr> },
    Boolean { val: bool },
    String { val: String },
}

pub struct TypeConversion {
    pub from: TypeIdent,
    pub expr: Ptr<Expr>,
}

pub struct IfConditional {
    pub cond: Ptr<Expr>,
    pub if_block: Ptr<Expr>,
    pub else_block: Option<Ptr<Expr>>,
}

pub struct WhileConditional {
    pub cond: Ptr<Expr>,
    pub block: Ptr<Block>,
}

pub struct Block {
    pub vars: Vec<u64>,
    pub stmts: Vec<Stmt>,
    pub return_type: TypeIdent,
}

pub struct BinaryOp {
    pub lhs: Ptr<Expr>,
    pub rhs: Ptr<Expr>,
    pub op: OpVar,
}

pub struct UnaryOp {
    pub tgt: Ptr<Expr>,
    pub op: OpVar,
}

pub struct FunctionCall {
    pub func: u64,
    pub params: Vec<Expr>,
}

pub struct StructChild {
    pub idx: u64,
}

pub struct ArrayChild {
    pub idx: Ptr<Expr>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum OpVar {
    // Binary
    /// `+`, Addition
    Add,
    /// `-`, Subtraction
    Sub,
    /// `*`, Multiplication
    Mul,
    /// `/`, Division
    Div,
    /// `&&`  And
    And,
    /// `||`, Or
    Or,
    /// `^`, Xor
    Xor,
    /// `&`, Binary And
    Ban,
    /// `|`, Binary Or
    Bor,
    /// `>`, Greater than
    Gt,
    /// `<`, Less than
    Lt,
    /// `==`, Equal to
    Eq,
    /// `>=`, Greater than or equal to
    Gte,
    /// `<=`, Less than or equal to
    Lte,
    /// `!=`, Not equal to
    Neq,

    // Unary
    /// `-`, Negate
    Neg,
    /// `!`, Boolean Inverse
    Inv,
    /// `~`, Bit Inverse
    Bin,
    /// `&`, Reference
    Ref,
    /// `*`, Deref
    Der,
    /// `x++`, Increase After
    Ina,
    /// `++x`, Increase Before
    Inb,
    /// `x--`, Decrease After
    Dea,
    /// `--x`, Decrease Before
    Deb,

    // Code uses
    /// Left parenthesis, should only appear in parser expression stack
    _Lpr,
    /// Right parenthesis
    _Rpr,
    /// Comma
    _Com,
    /// Assignment
    _Asn,
    /// Constant assignment.
    ///
    /// Acts like a conventional assignment with both variables and constants,
    /// but only generated when parsing declarations. This eliminates the problem
    /// of re-assigning constants.
    _Csn,
    /// Dummy value
    _Dum,
}

impl OpVar {
    /// Is this operator a binary operator?
    pub fn is_binary(&self) -> bool {
        use self::OpVar::*;
        match self {
            Add | Sub | Mul | Div | Gt | Lt | Eq | Gte | Lte | Neq | _Asn => true,
            _ => false,
        }
    }

    /// Is this operator a unary operator?
    pub fn is_unary(&self) -> bool {
        use self::OpVar::*;
        match self {
            Neg | Inv | Bin | Ref | Der | Ina | Inb | Dea | Deb => true,
            _ => false,
        }
    }
}
