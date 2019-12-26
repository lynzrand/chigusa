use crate::c0::lexer::TokenType;
use crate::prelude::*;
use std::str::{Chars, FromStr};
use std::{
    cell::{Ref, RefCell, RefMut},
    cmp::PartialOrd,
    fmt,
    fmt::Display,
    fmt::Formatter,
    hash::Hash,
    rc::{Rc, Weak},
    string::String,
};

use failure::*;

pub type ParseResult<T> = Result<T, ParseError>;

pub fn parse_err(var: ParseErrVariant, span: Span) -> ParseError {
    ParseError {
        var,
        span,
        backtrace: Backtrace::new(),
    }
}

pub fn parse_err_z(var: ParseErrVariant) -> ParseError {
    ParseError {
        var,
        span: Span::zero(),
        backtrace: Backtrace::new(),
    }
}

/// An error present in parsing process.
///
/// > When span is not avaliable, use Span::zero().
#[derive(Debug, Fail)]
pub struct ParseError {
    pub var: ParseErrVariant,
    pub span: Span,
    pub backtrace: Backtrace,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {}", self.var, self.span)
    }
}

pub type LexResult<T> = Result<T, LexError>;

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum LexError {
    BadEscaping,
    UnexpectedCharacter(char),
    BadInteger,
    MalformedString,
    UnexpectedEOL,
    UnexpectedEOF,
    ReservedWord(String),
}

#[derive(Debug)]
pub enum ParseErrVariant {
    InvalidToken(String),
    BadEscaping { cause: Box<dyn Fail> },

    ExpectToken(TokenType, TokenType),
    ExpectTokenOneOf(Vec<TokenType>, TokenType),
    UnexpectedToken(TokenType),
    UnexpectedTokenMsg { typ: TokenType, msg: &'static str },
    NoConstFns,
    ConstTypeNeedExplicitInitialization,

    CannotFindIdent(String),
    CannotFindType(String),
    CannotFindVar(String),
    CannotFindFn(String),

    ExpectToBeType(String),
    ExpectToBeVar(String),
    ExpectToBeFn(String),

    UnsupportedToken(TokenType),

    DuplicateDeclaration(String),
    BadIdentifier(String),
    ConflictingDeclaration(String),
    EarlyEof,

    MissingOperandUnary,
    MissingOperandL,
    MissingOperandR,

    NotMatchFnArguments(usize, usize),
    LexerErr(LexError),
    CustomErr(String),
    InternalErr(String),
}

impl ParseErrVariant {
    pub fn get_err_desc(&self) -> String {
        use self::ParseErrVariant::*;
        match self {
            InvalidToken(tok) => format!("{} is an invalid token", tok),
            BadEscaping { cause } => format!("Bad escaping sequence: {}", cause),

            ExpectToken(expected, found) => format!("Expected {}, found {}", expected, found),
            ExpectTokenOneOf(expected, found) => {
                format!("Expected to be one of {:?}, found {}", expected, found)
            }
            UnexpectedToken(found) => format!("Unexpected token {}", found),
            UnexpectedTokenMsg { typ, msg } => format!("Unexpected token {}: {}", typ, msg),
            NoConstFns => format!("Functions cannot be constant"),
            ConstTypeNeedExplicitInitialization => {
                format!("Constant values need explicit initialization")
            }

            CannotFindIdent(ident) => format!("Unable to find identifier: {}", ident),
            CannotFindType(ty) => format!("Unable to find type: {}", ty),
            CannotFindVar(var) => format!("Unable to find variable: {}", var),
            CannotFindFn(func) => format!("Unable to find function: {}", func),

            ExpectToBeType(ident) => format!("Expected identifier '{}' to be a type", ident),
            ExpectToBeVar(ident) => format!("Expected identifier '{}' to be a variable", ident),
            ExpectToBeFn(ident) => format!("Expected identifier '{}' to be a function", ident),

            UnsupportedToken(typ) => format!(
                "Token type '{}' is not supported in this version of compiler",
                typ
            ),

            DuplicateDeclaration(ident) => format!("Identifier '{}' is declared before", ident),
            BadIdentifier(ident) => format!("Identifier '{}' is invalid", ident),
            ConflictingDeclaration(ident) => {
                format!("Identifier '{}' has conflicting declarations", ident)
            }
            EarlyEof => format!("The file unexpectedly ends"),

            MissingOperandUnary => format!("Unary operator is missing its operand"),
            MissingOperandL => format!("Binary operator is missing its left operand"),
            MissingOperandR => format!("Binary operator is missing its right operand"),

            NotMatchFnArguments(expected, found) => format!(
                "Function arguments mismatch. Expected: {}, found: {}",
                expected, found
            ),
            LexerErr(l) => format!("{:?}", l),
            CustomErr(err) => format!("{}", err),
            InternalErr(internal) => format!("Internal error inside compiler: {}", internal),
            _ => "Unknown Error".to_string(),
        }
    }
}

impl Display for ParseErrVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_err_desc())
    }
}

pub trait WithSpan {
    fn with_span(self, span: Span) -> Self;
}

impl<T> WithSpan for Result<T, ParseError> {
    fn with_span(self, span: Span) -> Result<T, ParseError> {
        self.map_err(|mut e| {
            e.span = span;
            e
        })
    }
}

pub fn str_span(s: &str, span: Span) -> &str {
    &s[span.start.index..span.end.index]
}
