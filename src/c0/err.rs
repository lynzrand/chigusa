use crate::c0::lexer::TokenVariant;
use crate::prelude::*;
use std::str::{Chars, FromStr};
use std::{
    cell::{Ref, RefCell, RefMut},
    cmp::PartialOrd,
    fmt,
    fmt::Display,
    fmt::Formatter,
    hash::Hash,
    ops::Try,
    rc::{Rc, Weak},
    string::String,
};

pub type ParseResult<T> = Result<T, ParseError>;

pub fn parse_err(var: ParseErrVariant, span: Span) -> ParseError {
    ParseError { var, span }
}

pub fn parse_err_z(var: ParseErrVariant) -> ParseError {
    ParseError {
        var,
        span: Span::zero(),
    }
}

/// An error present in parsing process.
///
/// > When span is not avaliable, use Span::zero().
#[derive(Debug)]
pub struct ParseError {
    pub var: ParseErrVariant,
    pub span: Span,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?} at {}", self.var, self.span)
    }
}

impl std::error::Error for ParseError {
    fn description(&self) -> &str {
        "description() is deprecated; use Display"
    }
    fn cause(&self) -> Option<&dyn std::error::Error> {
        self.source()
    }
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

#[derive(Debug)]
pub enum ParseErrVariant {
    ExpectToken(TokenVariant),
    UnexpectedToken(TokenVariant),
    UnexpectedTokenMsg(TokenVariant, &'static str),
    NoConstFns,
    CannotFindIdent(String),
    CannotFindType(String),
    CannotFindVar(String),
    CannotFindFn(String),
    ExpectToBeType(String),
    ExpectToBeVar(String),
    ExpectToBeFn(String),
    UnsupportedToken(TokenVariant),
    TokenExists(String),
    FnDeclarationConflict(String),
    EarlyEof,
    UnbalancedParenthesisExpectL,
    UnbalancedParenthesisExpectR,
    MissingOperandUnary,
    MissingOperandL,
    MissingOperandR,
    NotMatchFnArguments(usize, usize),
    InternalErr,
}

impl ParseErrVariant {
    pub fn get_err_code(&self) -> usize {
        use self::ParseErrVariant::*;
        match self {
            ExpectToken(_) => 1,
            NoConstFns => 2,
            InternalErr => 1023,
            _ => 1024,
        }
    }

    pub fn get_err_desc(&self) -> String {
        use self::ParseErrVariant::*;
        match self {
            ExpectToken(token) => format!("Expected {}", token),
            NoConstFns => "Functions cannot be marked as constant".to_string(),
            InternalErr => "Something went wrong inside the compiler".to_string(),
            _ => "Unknown Error".to_string(),
        }
    }
}

impl Display for ParseErrVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "E{:4}: {}", self.get_err_code(), self.get_err_desc())
    }
}

pub fn str_span(s: &str, span: Span) -> &str {
    &s[span.start.index..span.end.index]
}
