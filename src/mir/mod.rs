//! Mir crate stands for mid-level IR.
//!
//! Mir handles the conversion from AST to machine code, pretty much
//! like LLVM-IR.

use crate::prelude::*;
use std::collections::HashMap;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum VarTy {
    Global,
    Local,
}
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct VarRef(VarTy, usize);
// #[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub type TyRef = usize;
pub type BBId = usize;

pub enum Value {
    Imm(u64),
    Var(VarRef),
    Reg(u8),
}

#[derive(Clone, Eq, PartialEq, Debug)]
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

pub enum JumpInst {
    Jump(BBId),
    Conditional(BBId, BBId),
    Return(Option<VarRef>),
    Dead,
}

pub struct BasicBlk {
    id: BBId,
    inst: Vec<MirCode>,
    end: JumpInst,
}

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

pub struct Func {
    // this: VarRef,
    ty: TyRef,
    /// Variable Table
    ///
    /// %0 is return value;  
    /// %1..=ty is parameter.
    var_table: HashMap<usize, TyRef>,
    bb: Vec<BasicBlk>,
}

pub struct MirPackage {
    global_var_table: HashMap<usize, TyRef>,
    ty_table: HashMap<TyRef, Ty>,
    func_table: HashMap<usize, Func>,
}
