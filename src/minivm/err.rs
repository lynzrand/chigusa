use failure::*;
use std::fmt;

#[derive(Fail, Debug)]
pub enum CompileError {
    Unknown,
    AssignVoid,
    UnsupportedType,
    Error(String),
    InternalError(String),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub type CompileResult<T> = Result<T, CompileError>;
