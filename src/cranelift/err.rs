
use failure::*;
use std::fmt;

#[derive(Fail, Debug)]
pub enum CompileError {
    Unknown,
    AssignVoid,
    UnsupportedType,
    Error(String),
    ModuleError(cranelift_module::ModuleError),
    InternalError(String),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl From<cranelift_module::ModuleError> for CompileError {
    fn from(err: cranelift_module::ModuleError) -> Self {
        CompileError::ModuleError(err)
    }
}

pub type CompileResult<T> = Result<T, CompileError>;
