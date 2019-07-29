/*
    This file is loosely inspired by Rust's own AST implementation:
    https://github.com/rust-lang/rust/blob/master/src/libsyntax/ast.rs
*/

use std::cell::RefCell;
use std::collections::HashMap;
use std::iter::Iterator;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

pub struct Ptr<T>(Rc<RefCell<T>>);

impl<T> Ptr<T> {
    pub fn new(val: T) -> Ptr<T> {
        Ptr(Rc::new(RefCell::new(val)))
    }
}

impl<T> Clone for Ptr<T> {
    fn clone(&self) -> Self {
        Ptr(self.0.clone())
    }
}

pub struct Program {
    pub scope: Ptr<Scope>,
    pub decl: Vec<Ptr<VarDecalaration>>,
    pub fns: Vec<Ptr<FnDeclaration>>,
}

pub struct FnDeclaration {
    pub ident: Identifier,
    pub return_typ: Identifier,
    pub params: Vec<Ptr<VarDecalaration>>,
    pub body: Block,
}

pub struct Block {
    pub scope: Ptr<Scope>,
    pub decl: Vec<Ptr<VarDecalaration>>,
    pub stmt: Vec<Statement>,
}

pub enum TokenEntry {
    Variable {
        is_const: bool,
        var_type: String,
        decl: Ptr<VarDecalaration>,
    },
    Type {
        is_primitive: bool,
        occupy_bytes: usize,
    },
    Function {
        return_type: String,
        params_type: Vec<String>,
        decl: Ptr<FnDeclaration>,
    },
}

pub struct Scope {
    pub parent: Option<Ptr<Scope>>,
    pub token_table: HashMap<String, TokenEntry>,
}

impl Scope {
    pub fn new(parent: Option<Ptr<Scope>>) -> Scope {
        Scope {
            parent,
            token_table: HashMap::new(),
        }
    }
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
