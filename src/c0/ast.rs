/*
    This file is loosely inspired by Rust's own AST implementation:
    https://github.com/rust-lang/rust/blob/master/src/libsyntax/ast.rs
*/

use super::err::*;
use crate::prelude::*;
use failure::Fail;
use indexmap::IndexMap;
use once_cell::{self, sync::*};
use regex;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::iter::Iterator;
use std::ops::{Deref, DerefMut};
use std::rc::{Rc, Weak};
pub type TypeIdent = u64;

#[derive(Debug, Eq, PartialEq)]
pub struct Program {
    pub scope: Ptr<Scope>,
    pub vars: Vec<VarDef>,
    pub types: Vec<TypeDef>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum SymbolDef {
    Typ {
        def: Ptr<TypeDef>,
    },
    Var {
        typ: Ptr<TypeDef>,
        is_const: bool,
        is_callable: bool,
    },
}

#[derive(Debug, Eq, PartialEq)]
pub struct Scope {
    pub last: Option<Ptr<Scope>>,
    pub defs: IndexMap<String, Ptr<SymbolDef>>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            last: None,
            defs: IndexMap::new(),
        }
    }

    pub fn new_with_parent(parent: Ptr<Scope>) -> Scope {
        Scope {
            last: Some(parent),
            defs: IndexMap::new(),
        }
    }

    pub fn find_def(&self, name: &str) -> Option<Ptr<SymbolDef>> {
        self.defs.get(name).map(|def| def.clone()).or_else(|| {
            self.last
                .as_ref()
                .and_then(|last| last.borrow().find_def(name))
        })
    }

    pub fn find_def_self(&self, name: &str) -> Option<Ptr<SymbolDef>> {
        self.defs.get(name).map(|def| def.clone())
    }

    pub fn insert_def(&mut self, name: &str, def: SymbolDef) -> ParseResult<()> {
        if self.defs.contains_key(name) {
            Err(parse_err_z(ParseErrVariant::DuplicateDeclaration(
                name.into(),
            )))
        } else {
            if ident_regex.is_match(name) {
                self.defs.insert(name.to_owned(), Ptr::new(def));
                Ok(())
            } else {
                Err(parse_err_z(ParseErrVariant::BadIdentifier(name.into())))
            }
        }
    }
}
static ident_regex: Lazy<regex::Regex> =
    Lazy::new(|| regex::Regex::new("^[_a-zA-Z][_a-zA-Z0-9]*$").unwrap());

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeDef {
    /// A primitive type. Either an integer or a IEEE-754 floating point
    ///
    /// Only integers of length 1 (bool), 8, 16, 32 and 64, and floats of length
    /// 32, 64 are supported.
    Primitive(PrimitiveType),
    /// A struct type. Contains multiple fields at different offsets.
    Struct(StructType),
    /// A function type. Contains a vector of input parameters and one return
    /// value.
    Function(FunctionType),
    /// A reference type. This is the "pointer" to type called in other languages.
    Ref(RefType),
    /// An array of items. Optionally contains a length parameter.
    Array(ArrayType),

    /// Unit type. Also called "void" if you like that name.
    Unit,
    /// This type is unknown. It should be resolved according to other information
    Unknown,

    /// This type is explicitly named but not resolved. Feeling cute, might resolve later
    NamedType(String),

    /// Crap. We've found a type error.
    TypeErr,
}

impl TypeDef {
    // Nope. No implicit conversion here!
    // pub fn can_implicit_conv_to(&self, other: &TypeDef) -> bool {
    //     use TypeDef::*;
    //     match other {
    //         Unit | Unknown | TypeErr => true,
    //         _ => match self {
    //             Primitive(p) => {
    //                 if let Primitive(o) = other {
    //                     if o.occupy_bytes >= p.occupy_bytes && o.var == p.var {
    //                         true
    //                     } else {
    //                         false
    //                     }
    //                 } else {
    //                     false
    //                 }
    //             }
    //             Ref(r) => {
    //                 // TODO: r.target == Unit
    //                 false
    //             }
    //             _ => false,
    //         },
    //     }
    // }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PrimitiveTypeVar {
    SignedInt,
    UnsignedInt,
    Float,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PrimitiveType {
    pub occupy_bytes: usize,
    pub var: PrimitiveTypeVar,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StructType {
    /// Fields of this struct, described as universal identifiers
    pub field_types: Vec<Ptr<TypeDef>>,
    pub field_offsets: Vec<usize>,
    pub occupy_bytes: usize,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionType {
    pub params: Vec<Ptr<TypeDef>>,
    pub return_type: Ptr<TypeDef>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RefType {
    pub target: Ptr<TypeDef>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ArrayType {
    pub target: Ptr<TypeDef>,
    pub length: Option<usize>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VarDef {
    pub typ: Ptr<TypeDef>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Stmt {
    pub var: StmtVariant,
    pub span: Span,
}

impl AstNode for Stmt {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StmtVariant {
    If(IfConditional),
    While(WhileConditional),
    Block(Block),
    Expr(Ptr<Expr>),
    // TODO: Workaround for declaration and similar statements that results
    // in multiple expressions
    ManyExpr(Vec<Ptr<Expr>>),
    Return(Option<Ptr<Expr>>),
    Break,
    Empty,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Expr {
    pub var: ExprVariant,
    pub span: Span,
}

impl AstNode for Expr {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ExprVariant {
    Ident(Identifier),
    Literal(Literal),
    TypeConversion(TypeConversion),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    FunctionCall(FunctionCall),
    StructChild(StructChild),
    ArrayChild(ArrayChild),
    // /// If conditional.
    // ///
    // /// `if` `(` Expression `)` (Expression | Statement)
    // /// (`else` (Expression | Statement))?
    // IfConditional(IfConditional),

    // /// While conditional. Takes the value on break or the last iteration as its
    // /// return value.
    // ///
    // /// `while` `(` Expression `)` BlockExpression
    // WhileConditional(WhileConditional),

    // /// Block expression. Similar to that in Rust.
    // ///
    // /// `{` Statement* Expression? `}`
    // Block(Block),
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Literal {
    Number { val: ramp::rational::Rational },
    Struct { typ: TypeDef, fields: Vec<Expr> },
    Boolean { val: bool },
    String { val: String },
}

impl From<super::lexer::Literal> for Literal {
    fn from(lit: super::lexer::Literal) -> Self {
        use super::lexer::Literal::*;
        match lit {
            Number(i) => Literal::Number { val: i },
            String(s) => Literal::String { val: s },
            Boolean(b) => Literal::Boolean { val: b },
            Char(c) => Literal::Number {
                val: (c as u32).into(),
            },
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypeConversion {
    pub from: TypeIdent,
    pub expr: Ptr<Expr>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct IfConditional {
    pub cond: Ptr<Expr>,
    pub if_block: Ptr<Expr>,
    pub else_block: Option<Ptr<Expr>>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct WhileConditional {
    pub cond: Ptr<Expr>,
    pub block: Ptr<Block>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Block {
    pub scope: Ptr<Scope>,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BinaryOp {
    pub lhs: Ptr<Expr>,
    pub rhs: Ptr<Expr>,
    pub op: OpVar,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct UnaryOp {
    pub val: Ptr<Expr>,
    pub op: OpVar,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionCall {
    // TODO: Subject to change
    pub func: String,
    pub params: Vec<Ptr<Expr>>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StructChild {
    pub idx: usize,
}

#[derive(Debug, Clone, Eq, PartialEq)]
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
