//! Mir crate stands for mid-level IR.
//!
//! Mir handles the conversion from AST to machine code, pretty much
//! like LLVM-IR.

use crate::prelude::*;
use std::{
    cell::Ref,
    collections::{HashMap, HashSet},
};

pub mod codegen;

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
pub enum Value {
    IntImm(i32),
    FloatImm(f64),
    Var(VarRef),
    Reg(u8),
    Void,
}

impl Value {
    pub fn is_assignable(&self) -> bool {
        matches!(self, Value::Void)
    }
}

// #[derive(Debug, Clone, PartialEq)]
// pub struct Value {
//     // pub ty: Ty,
//     pub kind: ValueKind,
// }

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

impl From<crate::c0::ast::OpVar> for BinOp {
    fn from(op: crate::c0::ast::OpVar) -> Self {
        match op {
            crate::c0::ast::OpVar::Add => BinOp::Add,
            crate::c0::ast::OpVar::Sub => BinOp::Sub,
            crate::c0::ast::OpVar::Mul => BinOp::Mul,
            crate::c0::ast::OpVar::Div => BinOp::Div,
            crate::c0::ast::OpVar::And => BinOp::And,
            crate::c0::ast::OpVar::Or => BinOp::Or,
            crate::c0::ast::OpVar::Xor => unimplemented!("Unsupported operator {}", op),
            crate::c0::ast::OpVar::Ban => unimplemented!("Unsupported operator {}", op),
            crate::c0::ast::OpVar::Bor => unimplemented!("Unsupported operator {}", op),
            crate::c0::ast::OpVar::Gt => BinOp::Gt,
            crate::c0::ast::OpVar::Lt => BinOp::Lt,
            crate::c0::ast::OpVar::Eq => BinOp::Eq,
            crate::c0::ast::OpVar::Gte => BinOp::Gte,
            crate::c0::ast::OpVar::Lte => BinOp::Lte,
            crate::c0::ast::OpVar::Neq => BinOp::Neq,
            _ => panic!("Not a binary operator: {}!", op),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum UnaOp {
    Pos,
    Neg,
}

impl From<crate::c0::ast::OpVar> for UnaOp {
    fn from(op: crate::c0::ast::OpVar) -> Self {
        match op {
            crate::c0::ast::OpVar::Neg => UnaOp::Neg,
            crate::c0::ast::OpVar::Pos => UnaOp::Pos,
            crate::c0::ast::OpVar::Inv => unimplemented!("Unsupported operator {}", op),
            crate::c0::ast::OpVar::Bin => unimplemented!("Unsupported operator {}", op),
            crate::c0::ast::OpVar::Ref => unimplemented!("Unsupported operator {}", op),
            crate::c0::ast::OpVar::Der => unimplemented!("Unsupported operator {}", op),
            crate::c0::ast::OpVar::Ina => unimplemented!("Unsupported operator {}", op),
            crate::c0::ast::OpVar::Inb => unimplemented!("Unsupported operator {}", op),
            crate::c0::ast::OpVar::Dea => unimplemented!("Unsupported operator {}", op),
            crate::c0::ast::OpVar::Deb => unimplemented!("Unsupported operator {}", op),
            _ => panic!("Not an unary operator: {}!", op),
        }
    }
}

#[derive(Debug, Clone)]
pub struct MirCode {
    ins: Ins,
    // ty: Ty,
    tgt: VarRef,
}

#[derive(Debug, Clone, PartialEq)]
pub enum JumpInst {
    Jump(BBId),
    Conditional(Value, BBId, BBId),
    Return(Option<Value>),
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
pub enum PrimitiveTy {
    Int,
    Float,
    Bool,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Ty {
    Void,
    Primitive(PrimitiveTy, u8),
    Ptr(Ptr<Ty>),
    Array(Ptr<Ty>, Option<usize>),
    Fn(Ptr<Ty>, Vec<Ty>, bool),
    RestParams,
}

impl Ty {
    pub fn byte() -> Ty {
        Ty::Primitive(PrimitiveTy::Int, 1)
    }
    pub fn int() -> Ty {
        Ty::Primitive(PrimitiveTy::Int, 4)
    }
    pub fn double() -> Ty {
        Ty::Primitive(PrimitiveTy::Float, 8)
    }
    pub fn bool() -> Ty {
        Ty::Primitive(PrimitiveTy::Bool, 4)
    }
    pub fn ptr_of(ty: Ty) -> Ty {
        Ty::Ptr(Ptr::new(ty))
    }
    pub fn array_of(ty: Ty, size: Option<usize>) -> Ty {
        Ty::Array(Ptr::new(ty), size)
    }
    pub fn function_of<V: Into<Vec<Ty>>>(ret: Ty, params: V, is_extern: bool) -> Ty {
        Ty::Fn(Ptr::new(ret), params.into(), is_extern)
    }
    pub fn is_assignable(&self) -> bool {
        !matches!(self, Ty::Void)
    }
    pub fn is_int(&self) -> bool {
        matches!(self, Ty::Primitive(PrimitiveTy::Int, 4))
    }
    pub fn is_double(&self) -> bool {
        matches!(self, Ty::Primitive(PrimitiveTy::Float, 8))
    }
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            Ty::Primitive(PrimitiveTy::Int,_) | Ty::Primitive(PrimitiveTy::Float,_)
        )
    }
    pub fn is_bool(&self) -> bool {
        matches!(self, Ty::Primitive(PrimitiveTy::Bool, _))
    }
    pub fn is_ptr(&self) -> bool {
        matches!(self, Ty::Ptr(_))
    }
    pub fn is_ptr_of(&self, ty: &Ty) -> bool {
        if let Ty::Ptr(t) = self {
            &*t.borrow() == ty
        } else {
            false
        }
    }
    pub fn get_ptr_of(&self) -> Option<Ref<Ty>> {
        if let Ty::Ptr(t) = self {
            Some(t.borrow())
        } else {
            None
        }
    }
    pub fn is_array(&self) -> bool {
        matches!(self, Ty::Array(..))
    }
    pub fn is_array_of(&self, ty: &Ty) -> bool {
        if let Ty::Array(t, _) = self {
            &*t.borrow() == ty
        } else {
            false
        }
    }
    pub fn get_array_of(&self) -> Option<(Ref<Ty>, Option<usize>)> {
        if let Ty::Array(t, size) = self {
            Some((t.borrow(), size.to_owned()))
        } else {
            None
        }
    }
    pub fn get_fn_params(&self) -> Option<&Vec<Ty>> {
        if let Ty::Fn(_, param, _) = self {
            Some(param)
        } else {
            None
        }
    }
    pub fn get_fn_ret(&self) -> Option<Ptr<Ty>> {
        if let Ty::Fn(ret, _, _) = self {
            Some(ret.clone())
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum VarKind {
    Param,
    Ret,
    Local,
    Temp,
    Dummy,
}

#[derive(Debug, Clone)]
pub struct Var {
    pub ty: Ty,
    pub kind: VarKind,
}

#[derive(Debug, Clone)]
pub struct Func {
    // this: VarRef,
    pub ty: Ty,
    pub name: String,

    /// Variable Table
    pub var_table: HashMap<usize, Var>,
    pub bb: HashMap<usize, BasicBlk>,
}

#[derive(Debug, Clone)]
pub struct GlobalVar {
    pub ty: Ty,
    pub name: Option<String>,
}

#[derive(Debug, Clone)]
pub struct MirPackage {
    pub entry_point: usize,
    pub global_var_table: HashMap<usize, GlobalVar>,
    // pub ty_table: HashMap<TyRef, Ty>,
    pub func_table: HashMap<usize, Func>,
}
