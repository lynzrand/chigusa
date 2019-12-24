use crate::c0::ast;
use crate::prelude::*;
use failure::*;
use std::fmt;

#[derive(Fail, Debug)]
pub enum CompileError {
    Unknown,
    AssignVoid,
    UnsupportedType,
    UnsupportedOp,
    NoExternFunction,

    ErrorType,
    MakeRefFromPrimitive,
    MakePrimitiveFromRef,
    RequireSized(String),
    RequirePrintable(String),
    RequireScannable(String),

    IntOverflow,
    ParamLengthMismatch,
    ReturnTypeMismatch(String),
    NonExistFunc(String),
    NonExistVar(String),

    ControlReachesEndOfNonVoidFunction,
    NoTargetToBreak,
    FunctionMissingBody,
    NestedFunctions,

    NotLValue(String),
    NotImplemented(String),

    Error(String),
    InternalError(String),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub type CompileResult<T> = Result<T, CompileError>;
