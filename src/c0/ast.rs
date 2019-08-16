/*
    This file is loosely inspired by Rust's own AST implementation:
    https://github.com/rust-lang/rust/blob/master/src/libsyntax/ast.rs
*/

use indexmap::IndexMap;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::iter::Iterator;
use std::ops::{Deref, DerefMut};
use std::rc::{Rc, Weak};
use super::infra::*;


// ==============

pub struct Program {
    pub scope: Ptr<Scope>,
}

#[derive(Eq, PartialEq)]
pub struct FnDeclaration {
    pub span: Span,
    pub ident: Identifier,
    pub return_typ: Identifier,
    pub params: Vec<Ptr<VarDecalaration>>,
    pub body: Block,
}

#[derive(Eq, PartialEq)]
pub struct Block {
    pub scope: Ptr<Scope>,
    pub span: Span,
    pub stmt: Vec<Statement>,
}

#[derive(Eq, PartialEq)]
pub enum TokenEntry {
    Variable {
        is_const: bool,
        var_type: Ptr<TokenEntry>,
    },
    Type {
        is_primitive: bool,
        occupy_bytes: usize,
    },
    Function {
        returns_type: Ptr<TokenEntry>,
        params: Vec<Ptr<TokenEntry>>,
        decl: FnDeclaration,
    },
}

pub struct Scope {
    pub parent: Option<Weak<RefCell<Scope>>>,
    pub token_table: IndexMap<String, Ptr<TokenEntry>>,
}

impl Scope {
    pub fn new(parent: Option<Ptr<Scope>>) -> Scope {
        let weak_parent = parent.map(|x| x.downgrade());
        Scope {
            parent: weak_parent,
            token_table: IndexMap::new(),
        }
    }

    pub fn parent(&self) -> Option<Ptr<Scope>> {
        self.parent
            .as_ref()
            .and_then(|weak_ptr| weak_ptr.upgrade().map(|rc| rc.into_ptr()))
    }

    pub fn try_insert(&mut self, token: &str, entry: Ptr<TokenEntry>) -> bool {
        match self.token_table.entry(token.to_owned()) {
            indexmap::map::Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(entry);
                true
            }
            _ => false,
        }
    }

    pub fn try_insert_or_replace_same(&mut self, token: &str, entry: Ptr<TokenEntry>) -> bool {
        match self.token_table.entry(token.to_owned()) {
            indexmap::map::Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(entry);
                true
            }
            indexmap::map::Entry::Occupied(mut occupied_entry) => {
                let val = occupied_entry.get_mut();
                // TODO: check if entries are same and replace when needed
                false
            }
        }
    }

    pub fn find_definition_self(&self, token: &str) -> Option<Ptr<TokenEntry>> {
        self.token_table.get(token).map(|x| x.clone())
    }

    pub fn find_definition(&self, token: &str) -> Option<Ptr<TokenEntry>> {
        self.find_definition_self(token).or_else(|| {
            self.parent()
                .as_ref()
                .and_then(|p| p.borrow().find_definition(token))
        })
    }

    pub fn find_definition_skip(&self, token: &str, skip: usize) -> Option<Ptr<TokenEntry>> {
        if skip <= 0 {
            self.find_definition(token)
        } else {
            self.parent()
                .and_then(|p| p.borrow().find_definition_skip(token, skip - 1))
        }
    }
}

impl PartialEq for Scope {
    fn eq(&self, other: &Self) -> bool {
        self.token_table.eq(&other.token_table)
    }
}

impl Eq for Scope {}

#[derive(Eq, PartialEq)]
pub enum Statement {
    If(IfStatement),
    While(WhileStatement),
    Return(Expr),
    Expr(Expr),
    Block(Block),
    Empty,
}

#[derive(Eq, PartialEq)]
pub struct ReturnStatement {
    pub return_val: Option<Ptr<TokenEntry>>,
}

#[derive(Eq, PartialEq)]
pub struct IfStatement {
    pub check: Ptr<Expr>,
    pub if_body: Ptr<Statement>,
    pub else_body: Option<Ptr<Statement>>,
}

#[derive(Eq, PartialEq)]
pub struct WhileStatement {
    pub check: Ptr<Expr>,
    pub body: Ptr<Statement>,
}

#[derive(Eq, PartialEq)]
pub struct VarDecalaration {
    pub is_const: bool,
    pub symbol: Ptr<TokenEntry>,
    pub val: Option<Ptr<Expr>>,
}

#[derive(Eq, PartialEq)]
pub struct Assignment {
    pub ident: Identifier,
    pub expr: Ptr<Expr>,
}

#[derive(Eq, PartialEq)]
pub enum Expr {
    Int(IntegerLiteral),
    Str(StringLiteral),
    BinOp(BinaryOp),
    UnaOp(UnaryOp),
    Var(Identifier),
    FnCall(FuncCall),
    Empty,
}

#[derive(Eq, PartialEq)]
pub struct Identifier(pub Ptr<TokenEntry>);

#[derive(Eq, PartialEq)]
pub struct FuncCall {
    pub fn_name: Identifier,
    pub params: Vec<Ptr<Expr>>,
}

/// An integer literal
#[derive(Eq, PartialEq)]
pub struct IntegerLiteral(pub i64);

/// A String Literal
#[derive(Eq, PartialEq)]
pub struct StringLiteral(pub String);

/// A binary operator
#[derive(Eq, PartialEq)]
pub struct BinaryOp {
    pub var: OpVar,
    pub lhs: Ptr<Expr>,
    pub rhs: Ptr<Expr>,
}

/// An unary operator
#[derive(Eq, PartialEq)]
pub struct UnaryOp {
    pub var: OpVar,
    pub val: Ptr<Expr>,
}

#[derive(Clone, Copy, Eq, PartialEq)]
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
    /// Dummy value
    _Dum,
}

impl OpVar {
    /// Is this operator a binary operator?
    pub fn is_binary(&self) -> bool {
        use self::OpVar::*;
        match self {
            Add | Sub | Mul | Div | Gt | Lt | Eq | Gte | Lte | Neq => true,
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
