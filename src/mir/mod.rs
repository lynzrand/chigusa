//! Mir crate stands for mid-level IR.
//!
//! Mir handles the conversion from AST to machine code, pretty much
//! like LLVM-IR.

use crate::prelude::*;
use std::collections::{HashMap, HashSet};

mod codegen;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum VarTy {
    Global,
    Local,
}

pub type TyRef = usize;
pub type BBId = usize;
pub type VarId = usize;

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct VarRef(VarTy, VarId);
// #[derive(Debug, Eq, PartialEq, Copy, Clone)]

#[derive(Debug, Clone, PartialEq)]
pub enum ValueKind {
    IntImm(u64),
    FloatImm(f64),
    Var(usize),
    Reg(u8),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Value {
    pub ty: Ty,
    pub kind: ValueKind,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Ins {
    /// Type Conversion
    TyCon(Value),
    /// Assign
    Asn(Value),
    /// Binary
    Bin(BinOp, Value, Value),
    /// Unary
    Una(UnaOp, Value),
    /// Function Call
    Call(VarRef, Vec<Value>),
    /// Conditional Assign
    Phi(Vec<(BBId, Value)>),
    /// Read va_arg
    RestRead(usize),
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Lt,
    Gt,
    Eq,
    Neq,
    Lte,
    Gte,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum UnaOp {
    Neg,
}

#[derive(Debug, Clone)]
pub struct MirCode {
    ins: Ins,
    ty: TyRef,
    tgt: VarRef,
}

#[derive(Debug, Clone, PartialEq)]
pub enum JumpInst {
    Jump(BBId),
    Conditional(BBId, BBId),
    Return(Option<VarRef>),
    Unreachable,
    Unknown,
}

impl Default for JumpInst {
    fn default() -> Self {
        JumpInst::Unknown
    }
}

#[derive(Debug, Clone)]
pub struct BasicBlk {
    /// Basic blocks that jump into this block.
    pub jump_in: HashSet<BBId>,
    pub uses_var: HashSet<VarId>,
    // pub id: BBId,
    pub inst: Vec<MirCode>,
    /// How this basic block terminates. Defaults to Unknown.
    pub end: JumpInst,
}

impl BasicBlk {
    pub fn new<'s, I>(jump_in: I) -> BasicBlk
    where
        I: IntoIterator<Item = &'s usize>,
    {
        let mut jump_in_set = HashSet::new();
        for item in jump_in {
            jump_in_set.insert(*item);
        }

        BasicBlk {
            jump_in: jump_in_set,
            uses_var: HashSet::new(),
            inst: Vec::new(),
            end: Default::default(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BasicTy {
    I32,
    F64,
    B32,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Ty {
    Void,
    Basic(BasicTy),
    Ptr(Ptr<Ty>),
    Array(Ptr<Ty>),
    Fn(Ptr<Ty>, Vec<Ty>),
    RestParams,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum VarKind {
    Imm,
    Param,
    Ret,
    Local,
    Temp,
    Dummy,
}

#[derive(Debug, Clone)]
pub struct Var {
    pub ty: TyRef,
    pub kind: VarKind,
}

#[derive(Debug, Clone)]
pub struct Func {
    // this: VarRef,
    pub ty: TyRef,

    /// Variable Table
    pub var_table: HashMap<usize, Var>,
    pub bb: Vec<BasicBlk>,
}

#[derive(Debug, Clone)]
pub struct MirPackage {
    pub global_var_table: HashMap<usize, TyRef>,
    pub ty_table: HashMap<TyRef, Ty>,
    pub func_table: HashMap<usize, Func>,
}
