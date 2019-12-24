use crate::c0::ast;
use crate::prelude::*;
use failure::*;
use std::fmt;

#[derive(Fail, Debug)]
pub struct CompileError {
    pub var: CompileErrorVar,
    pub span: Option<Span>,
    pub backtrace: Backtrace,
}

pub fn compile_err_n(v: CompileErrorVar) -> CompileError {
    CompileError {
        var: v,
        span: None,
        backtrace: Backtrace::new(),
    }
}
pub fn compile_err(v: CompileErrorVar, span: Option<Span>) -> CompileError {
    CompileError {
        var: v,
        span,
        backtrace: Backtrace::new(),
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl From<CompileErrorVar> for CompileError {
    fn from(e: CompileErrorVar) -> CompileError {
        CompileError {
            var: e,
            span: None,
            backtrace: Backtrace::new(),
        }
    }
}

pub trait WithSpan<E> {
    fn with_span(self, span: Span) -> E;
}

impl WithSpan<Self> for CompileError {
    fn with_span(mut self, span: Span) -> Self {
        if self.span.is_none() {
            self.span = Some(span);
        }
        self
    }
}

impl<T> WithSpan<Self> for CompileResult<T> {
    fn with_span(self, span: Span) -> Self {
        self.map_err(|e| e.with_span(span))
    }
}

impl<T> WithSpan<CompileResult<T>> for Result<T, CompileErrorVar> {
    fn with_span(self, span: Span) -> CompileResult<T> {
        self.map_err(|e| compile_err(e, Some(span)))
    }
}

#[derive(Debug)]
pub enum CompileErrorVar {
    Unknown,
    AssignVoid,
    AssignConst,
    VoidVariable(String),
    UnsupportedType,
    UnsupportedOp,
    NoExternFunction(String),

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
    FunctionMissingBody(String),
    NestedFunctions(String),

    NotLValue(String),
    NotImplemented(String),

    Error(String),
    InternalError(String),
}

impl fmt::Display for CompileErrorVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub type CompileResult<T> = Result<T, CompileError>;
