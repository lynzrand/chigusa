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
use std::fmt::{self, Display, Formatter};
use std::iter::Iterator;
use std::ops::{Deref, DerefMut};
use std::rc::{Rc, Weak};

pub type TypeIdent = u64;

#[derive(Eq, PartialEq)]
pub struct Program {
    pub blk: Block,
    // pub vars: Vec<VarDef>,
    // pub types: Vec<TypeDef>
}

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Program").field("blk", &self.blk).finish()
    }
}

#[derive(Eq, PartialEq)]
pub enum SymbolDef {
    Typ {
        def: Ptr<TypeDef>,
    },
    Var {
        typ: Ptr<TypeDef>,
        is_const: bool,
        decl_span: Span,
    },
}

impl fmt::Debug for SymbolDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            SymbolDef::Typ { def } => f.debug_tuple("Type").field(&*def.borrow()).finish(),
            SymbolDef::Var { typ, is_const, .. } => f
                .debug_tuple("Var")
                .field(&*typ.borrow())
                .field(is_const)
                .finish(),
        }
    }
}

impl SymbolDef {
    /// Return the type variant of self
    pub fn get_typ(&self) -> Option<Ptr<TypeDef>> {
        match self {
            SymbolDef::Typ { def } => Some(def.cp()),
            _ => None,
        }
    }

    /// Return the symbol variant of self
    pub fn get_sym(&self) -> Option<(Ptr<TypeDef>, bool)> {
        match self {
            SymbolDef::Var { typ, is_const, .. } => Some((typ.cp(), *is_const)),
            _ => None,
        }
    }
}

#[derive(Eq, PartialEq)]
pub struct Scope {
    pub last: Option<Ptr<Scope>>,
    pub defs: IndexMap<String, Ptr<SymbolDef>>,
    pub id: usize,
}

static mut scope_id: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0usize);

impl Scope {
    pub fn reset_id() {
        unsafe {
            scope_id.store(0, std::sync::atomic::Ordering::SeqCst);
        }
    }

    pub fn new() -> Scope {
        let id = unsafe { scope_id.fetch_add(1, std::sync::atomic::Ordering::SeqCst) };
        Scope {
            last: None,
            defs: IndexMap::new(),
            id,
        }
    }

    pub fn new_with_parent(parent: Ptr<Scope>) -> Scope {
        let id = unsafe { scope_id.fetch_add(1, std::sync::atomic::Ordering::SeqCst) };
        Scope {
            last: Some(parent),
            defs: IndexMap::new(),
            id,
        }
    }

    pub fn find_def(&self, name: &str) -> Option<Ptr<SymbolDef>> {
        self.defs.get(name).map(|def| def.cp()).or_else(|| {
            self.last
                .as_ref()
                .and_then(|last| last.borrow().find_def(name))
        })
    }

    pub fn find_def_depth(&self, name: &str) -> Option<(Ptr<SymbolDef>, usize)> {
        self.defs
            .get(name)
            .map(|def| (def.cp(), self.id))
            .or_else(|| {
                self.last
                    .as_ref()
                    .and_then(|last| last.borrow().find_def_depth(name))
            })
    }

    pub fn find_def_self(&self, name: &str) -> Option<Ptr<SymbolDef>> {
        self.defs.get(name).map(|def| def.cp())
    }

    pub fn insert_def(&mut self, name: &str, def: SymbolDef) -> ParseResult<()> {
        if self.defs.contains_key(name) {
            let orig = self.defs.get(name).unwrap().borrow();

            // * Compare function declarations. Only allow duplicate declration of function types.
            if let SymbolDef::Var { typ, .. } = &*orig {
                let orig = typ.borrow();
                if let SymbolDef::Var { typ, .. } = &def {
                    let other = typ.borrow();
                    if orig.compare_fns(&*other) {
                        Ok(())
                    } else {
                        Err(parse_err_z(ParseErrVariant::ConflictingDeclaration(
                            name.into(),
                        )))
                    }
                } else {
                    Err(parse_err_z(ParseErrVariant::ConflictingDeclaration(
                        name.into(),
                    )))
                }
            } else {
                Err(parse_err_z(ParseErrVariant::DuplicateDeclaration(
                    name.into(),
                )))
            }
        } else {
            if ident_regex.is_match(name) {
                Ok(())
            } else {
                Err(parse_err_z(ParseErrVariant::BadIdentifier(name.into())))
            }
        }?;

        self.defs.insert(name.into(), Ptr::new(def));
        Ok(())
    }
}

impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Scope")
            .field("id", &self.id)
            .field(
                "parent",
                &match self.last {
                    Some(_) => "Some",
                    None => "None",
                },
            )
            .field("defs", &self.defs)
            .finish()
    }
}

static ident_regex: Lazy<regex::Regex> =
    Lazy::new(|| regex::Regex::new("^[_a-zA-Z][_a-zA-Z0-9]*$").unwrap());

#[derive(Clone, Eq, PartialEq)]
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

    /// Arguments of a variadic function. Must be the last argument.
    VariableArgs(Option<Ptr<TypeDef>>),

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
    pub fn compare_fns(&self, other: &TypeDef) -> bool {
        match self {
            TypeDef::Function(fn_self) => match other {
                TypeDef::Function(fn_other) => {
                    fn_other.params == fn_self.params && fn_other.return_type == fn_self.return_type
                }
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_unit(&self) -> bool {
        match self {
            TypeDef::Unit => true,
            _ => false,
        }
    }

    pub fn is_fn(&self) -> bool {
        match self {
            TypeDef::Function(..) => true,
            _ => false,
        }
    }

    pub fn is_primitive(&self) -> bool {
        match self {
            TypeDef::Primitive(..) => true,
            _ => false,
        }
    }
}

impl fmt::Debug for TypeDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TypeDef::Primitive(p) => {
                if f.alternate() {
                    write!(f, "{:#?}", p)
                } else {
                    write!(f, "{:?}", p)
                }
            }
            TypeDef::Array(a) => {
                if f.alternate() {
                    write!(f, "{:#?}", a)
                } else {
                    write!(f, "{:?}", a)
                }
            }
            TypeDef::Function(p) => {
                if f.alternate() {
                    write!(f, "{:#?}", p)
                } else {
                    write!(f, "{:?}", p)
                }
            }
            TypeDef::NamedType(p) => write!(f, "Type({})", p),
            TypeDef::Ref(p) => {
                if f.alternate() {
                    write!(f, "{:#?}", p)
                } else {
                    write!(f, "{:?}", p)
                }
            }
            TypeDef::Struct(p) => {
                if f.alternate() {
                    write!(f, "{:#?}", p)
                } else {
                    write!(f, "{:?}", p)
                }
            }
            TypeDef::TypeErr => write!(f, "Error type"),
            TypeDef::Unit => write!(f, "Void"),
            TypeDef::Unknown => write!(f, "Unknown"),
            TypeDef::VariableArgs(_p) => write!(f, "VA_ARGS"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Copy)]
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
    pub body: Option<Block>,
    pub is_extern: bool,
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

#[derive(Clone, Eq, PartialEq)]
pub struct Stmt {
    pub var: StmtVariant,
    pub span: Span,
}

impl AstNode for Stmt {
    fn span(&self) -> Span {
        self.span
    }
}

impl fmt::Debug for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:#?}", self.var)
        } else {
            write!(f, "{:?}", self.var)
        }
    }
}

#[derive(Eq, PartialEq, Clone)]
pub enum StmtVariant {
    If(IfConditional),
    While(WhileConditional),
    Block(Block),
    Expr(Ptr<Expr>),
    Print(Vec<Ptr<Expr>>),
    Scan(Identifier),
    // TODO: Workaround for declaration and similar statements that results
    // in multiple expressions
    ManyExpr(Vec<Ptr<Expr>>),
    Return(Option<Ptr<Expr>>),
    Break,
    Empty,
}

impl fmt::Debug for StmtVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            match self {
                StmtVariant::If(x) => write!(f, "{:#?}", x),
                StmtVariant::While(x) => write!(f, "{:#?}", x),
                StmtVariant::Block(x) => write!(f, "{:#?}", x),
                StmtVariant::Print(x) => {
                    write!(f, "Print(")?;
                    f.debug_list().entries(x).finish()?;
                    write!(f, ")")
                }
                StmtVariant::Scan(x) => write!(f, "Scan({})", x),
                StmtVariant::Expr(x) => write!(f, "{:#?}", &*x.borrow()),
                StmtVariant::ManyExpr(x) => write!(f, "{:#?}", x),
                StmtVariant::Return(x) => write!(f, "{:#?}", x),
                StmtVariant::Break => write!(f, "Break"),
                StmtVariant::Empty => write!(f, "Empty"),
            }
        } else {
            match self {
                StmtVariant::If(x) => write!(f, "{:?}", x),
                StmtVariant::While(x) => write!(f, "{:?}", x),
                StmtVariant::Block(x) => write!(f, "{:?}", x),
                StmtVariant::Print(x) => {
                    write!(f, "Print(")?;
                    f.debug_list().entries(x).finish()?;
                    write!(f, ")")
                }
                StmtVariant::Scan(x) => write!(f, "Scan({})", x),
                StmtVariant::Expr(x) => write!(f, "{:?}", &*x.borrow()),
                StmtVariant::ManyExpr(x) => write!(f, "{:?}", x),
                StmtVariant::Return(x) => write!(f, "{:?}", x),
                StmtVariant::Break => write!(f, "Break"),
                StmtVariant::Empty => write!(f, "Empty"),
            }
        }
    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct Expr {
    pub var: ExprVariant,
    pub span: Span,
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:#?}", self.var)
        } else {
            write!(f, "{:?}", self.var)
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.var)
    }
}

impl AstNode for Expr {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Eq, PartialEq)]
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

impl fmt::Display for ExprVariant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprVariant::Ident(i) => write!(f, "{}", i),
            ExprVariant::Literal(i) => write!(f, "{}", i),
            ExprVariant::TypeConversion(i) => write!(f, "{}", i),
            ExprVariant::UnaryOp(i) => write!(f, "{}", i),
            ExprVariant::BinaryOp(i) => write!(f, "{}", i),
            ExprVariant::FunctionCall(i) => write!(f, "{}", i),
            ExprVariant::StructChild(i) => write!(f, "{}", i),
            ExprVariant::ArrayChild(i) => write!(f, "{}", i),
        }
    }
}

impl fmt::Debug for ExprVariant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprVariant::Ident(i) => write!(f, "{}", i),
            ExprVariant::Literal(i) => write!(f, "{}", i),
            ExprVariant::TypeConversion(i) => write!(f, "{}", i),
            ExprVariant::UnaryOp(i) => write!(f, "{}", i),
            ExprVariant::BinaryOp(i) => write!(f, "{}", i),
            ExprVariant::FunctionCall(i) => write!(f, "{}", i),
            ExprVariant::StructChild(i) => write!(f, "{}", i),
            ExprVariant::ArrayChild(i) => write!(f, "{}", i),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Identifier {
    pub name: String,
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Identifier({})", self.name)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Literal {
    Char { val: char },
    Integer { val: ramp::Int },
    Float { val: ramp::rational::Rational },
    Struct { typ: TypeDef, fields: Vec<Expr> },
    Boolean { val: bool },
    String { val: String },
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Char { val } => write!(f, "'{}'", val),
            Literal::Integer { val } => write!(f, "{}", val),
            Literal::Float { val } => write!(f, "{}", val),
            Literal::Struct { typ, fields } => write!(f, "{:?}{{{:?}}}", typ, fields),
            Literal::Boolean { val } => write!(f, "{}", val),
            Literal::String { val } => write!(f, "\"{}\"", val),
        }
    }
}

impl From<super::lexer::Literal> for Literal {
    fn from(lit: super::lexer::Literal) -> Self {
        use super::lexer::Literal::*;
        match lit {
            Integer(i) => Literal::Integer { val: i },
            Float(i) => Literal::Float { val: i },
            String(s) => Literal::String { val: s },
            Boolean(b) => Literal::Boolean { val: b },
            Char(c) => Literal::Char { val: c },
            _Dummy => panic!("Dummy literal cannot be used!"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypeConversion {
    pub to: Ptr<TypeDef>,
    pub expr: Ptr<Expr>,
}

impl fmt::Display for TypeConversion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} as {:?})", self.expr, self.to)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct IfConditional {
    pub cond: Ptr<Expr>,
    pub if_block: Ptr<Stmt>,
    pub else_block: Option<Ptr<Stmt>>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct WhileConditional {
    pub cond: Ptr<Expr>,
    pub block: Ptr<Stmt>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Block {
    pub scope: Ptr<Scope>,
    pub stmts: Vec<Stmt>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BinaryOp {
    pub lhs: Ptr<Expr>,
    pub rhs: Ptr<Expr>,
    pub op: OpVar,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.op, self.lhs, self.rhs)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct UnaryOp {
    pub val: Ptr<Expr>,
    pub op: OpVar,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {})", self.op, self.val)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionCall {
    // TODO: Subject to change
    pub func: String,
    pub params: Vec<Ptr<Expr>>,
}

impl fmt::Display for FunctionCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {:?})", self.func, self.params)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StructChild {
    pub val: Ptr<Expr>,
    pub idx: usize,
}

impl fmt::Display for StructChild {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}.{})", self.val, self.idx)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ArrayChild {
    pub val: Ptr<Expr>,
    pub idx: Ptr<Expr>,
}

impl fmt::Display for ArrayChild {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}.[]{})", self.val, self.idx)
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
    /// `+`, Positive (no operation)
    Pos,
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
    /// but only generated when parsing declarations. This hopefully eliminates
    /// the problem of re-assigning constants.
    _Csn,
    /// Dummy operation, or noop
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

impl fmt::Display for OpVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
