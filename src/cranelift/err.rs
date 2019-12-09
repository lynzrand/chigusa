use failure::*;
use std::fmt;

#[derive(Fail, Debug, Eq, PartialEq)]
pub enum CompileError {
    Unknown,
    Error(String),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub type CompileResult<T> = Result<T, CompileError>;
