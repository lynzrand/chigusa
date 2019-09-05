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

// ==============

#[derive(Debug, Eq, PartialEq)]
pub struct Program {
    pub scope: Ptr<Scope>,
    pub span: Span,
    pub stmts: Vec<Statement>,
}

impl AstNode for Program {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct FnDeclaration {
    // pub return_type: Identifier,
    // pub params: Vec<Ptr<VarDecalaration>>,
    pub body: Block,
    pub span: Span,
}

impl AstNode for FnDeclaration {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Block {
    pub scope: Ptr<Scope>,
    pub stmt: Vec<Statement>,
    pub span: Span,
}

impl AstNode for Block {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum TokenEntry {
    Variable(VarScopeDecl),
    Type(TypeScopeDecl),
    Function(FnScopeDecl),
}

#[derive(Debug, Eq, PartialEq)]
pub struct VarScopeDecl {
    pub is_const: bool,
    pub var_type: String,
}

#[derive(Debug, Eq, PartialEq)]
pub struct TypeScopeDecl {
    pub is_primitive: bool,
    pub occupy_bytes: usize,
}

#[derive(Debug, Eq, PartialEq)]
pub struct FnScopeDecl {
    pub returns_type: String,
    pub params: Vec<String>,
    pub is_ffi: bool,
    pub decl: Option<FnDeclaration>,
}

impl TokenEntry {
    pub fn is_var(&self) -> bool {
        match self {
            TokenEntry::Variable(_) => true,
            _ => false,
        }
    }

    pub fn is_fn(&self) -> bool {
        match self {
            TokenEntry::Function(_) => true,
            _ => false,
        }
    }

    pub fn is_type(&self) -> bool {
        match self {
            TokenEntry::Type(_) => true,
            _ => false,
        }
    }

    pub fn find_var<'a, F: FnOnce() -> ParseError<'a>>(
        &self,
        get_err: F,
    ) -> ParseResult<'a, &VarScopeDecl> {
        match self {
            TokenEntry::Variable(v) => Ok(&v),
            _ => Err(get_err()),
        }
    }

    pub fn find_fn<'a, F: FnOnce() -> ParseError<'a>>(
        &self,
        get_err: F,
    ) -> ParseResult<'a, &FnScopeDecl> {
        match self {
            TokenEntry::Function(v) => Ok(&v),
            _ => Err(get_err()),
        }
    }

    pub fn find_type<'a, F: FnOnce() -> ParseError<'a>>(
        &self,
        get_err: F,
    ) -> ParseResult<'a, &TypeScopeDecl> {
        match self {
            TokenEntry::Type(v) => Ok(&v),
            _ => Err(get_err()),
        }
    }

    pub fn find_var_mut<'a, F: FnOnce() -> ParseError<'a>>(
        &mut self,
        get_err: F,
    ) -> ParseResult<'a, &mut VarScopeDecl> {
        match self {
            TokenEntry::Variable(v) => Ok(v),
            _ => Err(get_err()),
        }
    }

    pub fn find_fn_mut<'a, F: FnOnce() -> ParseError<'a>>(
        &mut self,
        get_err: F,
    ) -> ParseResult<'a, &mut FnScopeDecl> {
        match self {
            TokenEntry::Function(v) => Ok(v),
            _ => Err(get_err()),
        }
    }

    pub fn find_type_mut<'a, F: FnOnce() -> ParseError<'a>>(
        &mut self,
        get_err: F,
    ) -> ParseResult<'a, &mut TypeScopeDecl> {
        match self {
            TokenEntry::Type(v) => Ok(v),
            _ => Err(get_err()),
        }
    }
}

#[derive(Debug)]
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

    // pub fn token_exists(&self, )

    pub fn find_definition_skip(&self, token: &str, skip: usize) -> Option<Ptr<TokenEntry>> {
        if skip <= 0 {
            self.find_definition(token)
        } else {
            self.parent()
                .and_then(|p| p.borrow().find_definition_skip(token, skip - 1))
        }
    }

    pub fn get_fn_params_length(&self, token: &str) -> Option<usize> {
        self.find_definition(token)
            .and_then(|def| match &*def.borrow() {
                TokenEntry::Function(FnScopeDecl { params, .. }) => Some(params.len()),
                _ => None,
            })
    }
}

impl PartialEq for Scope {
    fn eq(&self, other: &Self) -> bool {
        self.token_table.eq(&other.token_table)
    }
}

impl Eq for Scope {}

#[derive(Debug, Eq, PartialEq)]
pub enum Statement {
    If(IfStatement),
    While(WhileStatement),
    Return(ReturnStatement),
    Expr(Expr),
    Block(Block),
    Empty(Span),
}

impl AstNode for Statement {
    fn span(&self) -> Span {
        use Statement::*;
        match self {
            If(i) => i.span(),
            While(w) => w.span(),
            Return(r) => r.span(),
            Expr(e) => e.span(),
            Block(b) => b.span(),
            Empty(span) => *span,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ReturnStatement {
    pub return_val: Option<Expr>,
    pub span: Span,
}

impl AstNode for ReturnStatement {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct IfStatement {
    pub check: Ptr<Expr>,
    pub if_body: Ptr<Statement>,
    pub else_body: Option<Ptr<Statement>>,
    pub span: Span,
}

impl AstNode for IfStatement {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct WhileStatement {
    pub check: Ptr<Expr>,
    pub body: Ptr<Statement>,
    pub span: Span,
}

impl AstNode for WhileStatement {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct VarDecalaration {
    pub is_const: bool,
    pub symbol: Ptr<TokenEntry>,
    pub val: Option<Ptr<Expr>>,
    pub span: Span,
}

impl AstNode for VarDecalaration {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
    Int(IntegerLiteral),
    Str(StringLiteral),
    BinOp(BinaryOp),
    UnaOp(UnaryOp),
    Var(Identifier),
    FnCall(FuncCall),
    Empty(Span),
}

impl AstNode for Expr {
    fn span(&self) -> Span {
        use Expr::*;
        match self {
            Int(i) => i.span(),
            Str(s) => s.span(),
            BinOp(b) => b.span(),
            UnaOp(u) => u.span(),
            Var(i) => i.span(),
            FnCall(f) => f.span(),
            Empty(span) => *span,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Identifier(pub String, pub Span);

impl AstNode for Identifier {
    fn span(&self) -> Span {
        self.1
    }
}
#[derive(Debug, Eq, PartialEq)]
pub struct FuncCall {
    pub fn_name: Identifier,
    pub params: Vec<Ptr<Expr>>,
    pub span: Span,
}

impl AstNode for FuncCall {
    fn span(&self) -> Span {
        self.span
    }
}
/// An integer literal
#[derive(Debug, Eq, PartialEq)]
pub struct IntegerLiteral(pub i64, pub Span);

impl AstNode for IntegerLiteral {
    fn span(&self) -> Span {
        self.1
    }
}

/// A String Literal
#[derive(Debug, Eq, PartialEq)]
pub struct StringLiteral(pub String, pub Span);

impl AstNode for StringLiteral {
    fn span(&self) -> Span {
        self.1
    }
}
/// A binary operator
#[derive(Debug, Eq, PartialEq)]
pub struct BinaryOp {
    pub var: OpVar,
    pub lhs: Ptr<Expr>,
    pub rhs: Ptr<Expr>,
    pub span: Span,
}

impl AstNode for BinaryOp {
    fn span(&self) -> Span {
        self.span
    }
}

/// An unary operator
#[derive(Debug, Eq, PartialEq)]
pub struct UnaryOp {
    pub var: OpVar,
    pub val: Ptr<Expr>,
    pub span: Span,
}

impl AstNode for UnaryOp {
    fn span(&self) -> Span {
        self.span
    }
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
