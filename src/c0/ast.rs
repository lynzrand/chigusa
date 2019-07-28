/*
    This file is loosely inspired by Rust's own AST implementation:
    https://github.com/rust-lang/rust/blob/master/src/libsyntax/ast.rs
*/

use std::cell::RefCell;
use std::iter::Iterator;
use std::rc::Rc;

type Ptr<T> = Rc<RefCell<T>>;

pub struct Program {
    pub decl: Vec<VarDecalaration>,
    pub fns: Vec<FnDeclaration>,
}

pub struct FnDeclaration {
    pub ident: Identifier,
    pub return_typ: Identifier,
    pub params: Vec<VarDecalaration>,
    pub body: Block,
}

pub struct Block {
    pub decl: Vec<VarDecalaration>,
    pub stmt: Vec<Statement>,
}

pub enum Statement {
    If(IfStatement),
    While(WhileStatement),
    Expr(Expr),
    Block(Block),
}

pub struct IfStatement {
    pub check: Ptr<Expr>,
    pub if_body: Ptr<Statement>,
    pub else_body: Option<Ptr<Statement>>,
}

pub struct WhileStatement {
    pub check: Ptr<Expr>,
    pub body: Ptr<Statement>,
}

pub struct VarDecalaration {
    pub ident: Identifier,
    pub typ: Identifier,
    pub is_const: bool,
    pub val: Option<Ptr<Expr>>,
}

pub struct Assignment {
    pub ident: Identifier,
    pub expr: Ptr<Expr>,
}

pub enum Expr {
    Int(IntegerLiteral),
    Str(StringLiteral),
    BinOp(BinaryOp),
    UnaOp(UnaryOp),
    Ident(Identifier),
    Assign(Assignment),
}

pub struct Identifier {}

pub struct FuncCall {
    pub fn_name: Identifier,
    pub params: Vec<Ptr<Expr>>,
}

/// An integer literal
pub struct IntegerLiteral {
    pub val: i64,
}

/// A String Literal
pub struct StringLiteral {
    pub val: String,
}

/// A binary operator
pub struct BinaryOp {
    pub var: BinaryOpVar,
    pub lhs: Ptr<Expr>,
    pub rhs: Ptr<Expr>,
}

/// An unary operator
pub struct UnaryOp {
    pub var: UnaryOpVar,
    pub val: Ptr<Expr>,
}

pub enum BinaryOpVar {
    /// `+`, Addition
    Add,
    /// `-`, Subtraction
    Sub,
    /// `*`, Multiplication
    Mul,
    /// `/`, Division
    Div,
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
}

pub enum UnaryOpVar {
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
}
