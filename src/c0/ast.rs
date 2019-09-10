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

pub type TypeDisc = u64;

pub struct Program {
    pub all_vars: Vec<u64>,
}

pub enum TypeDef {
    Primitive(PrimitiveType),
    Struct(StructType),
    Function(FunctionType),
    Ref,
    Array,
}

pub struct PrimitiveType {
    pub occupy_bytes: usize,
    pub is_signed: bool,
    pub is_float: bool,
}

pub struct StructType {
    /// Fields of this struct, described as universal identifiers
    pub field_types: Vec<TypeDisc>,
    pub field_offsets: Vec<usize>,
    pub occupy_bytes: usize,
}

pub struct FunctionType {
    pub params: Vec<TypeDisc>,
    pub return_type: TypeDisc,
}

pub struct RefType {
    pub target: TypeDisc,
}

pub struct ArrayType {
    pub target: TypeDisc,
    pub length: usize,
}

pub struct Stmt {}

pub enum StmtVariant {
    Expr,
    Return,
    Break,
    Empty,
}

pub struct Expr {
    pub var: ExprVariant,
    pub typ: TypeDef,
}

pub enum ExprVariant {
    Literal,
    TypeConversion,
    UnaryOp,
    BinaryOp,
    FunctionCall,
    GetChild,

    /// If conditional.
    ///
    /// `if` `(` Expression `)` (Expression | Statement)
    /// (`else` (Expression | Statement))?
    IfConditional,

    /// While conditional. Takes the value on break or the last iteration as its
    /// return value.
    ///
    /// `while` `(` Expression `)` BlockExpression
    WhileConditional,

    /// Block expression. Similar to that in Rust.
    ///
    /// `{` Statement* Expression? `}`
    Block,
}

pub struct Block {
    pub vars: Vec<u64>,
    pub stmts: Vec<Stmt>,
    pub return_type: TypeDisc,
}

pub struct BinaryOp {}

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
