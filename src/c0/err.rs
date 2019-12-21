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
        write!(f, "{:#?} at {}", self.var, self.span)
    }
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

    CustomErr(String),
    InternalErr(String),
}

impl ParseErrVariant {
    pub fn get_err_code(&self) -> usize {
        use self::ParseErrVariant::*;
        match self {
            ExpectToken(..) => 1,
            NoConstFns => 2,
            InternalErr(_) => 1023,
            _ => 1024,
        }
    }

    pub fn get_err_desc(&self) -> String {
        use self::ParseErrVariant::*;
        match self {
            ExpectToken(token, found) => format!("Expected {}, found {}", token, found),
            NoConstFns => "Functions cannot be marked as constant".to_string(),
            InternalErr(msg) => format!("The compiler encountered an internal error: {}", msg),
            _ => "Unknown Error".to_string(),
        }
    }
}

impl Display for ParseErrVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "E{:4}: {}", self.get_err_code(), self.get_err_desc())
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
