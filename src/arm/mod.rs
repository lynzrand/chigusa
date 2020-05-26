pub mod codegen;
mod util;
use crate::mir;
use indexmap::IndexSet;
use once_cell::sync::Lazy;
use std::collections::HashSet;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Reg(u8);

pub type LazyRegSet = Lazy<IndexSet<Reg>>;

pub static VARIABLE_REGISTERS: LazyRegSet = Lazy::new(|| {
    (&[4, 5, 6, 7, 8, 10, 11])
        .into_iter()
        .map(|&v| Reg(v))
        .collect()
});
pub static PARAM_REGISTERS: LazyRegSet =
    Lazy::new(|| (&[0, 1, 2, 3]).into_iter().map(|&v| Reg(v)).collect());
pub static RESULT_REGISTERS: LazyRegSet =
    Lazy::new(|| (&[0, 1]).into_iter().map(|&v| Reg(v)).collect());
pub static SCRATCH_REGISTERS: LazyRegSet =
    Lazy::new(|| (&[0, 1, 2, 3, 12]).into_iter().map(|&v| Reg(v)).collect());
pub static SCRATCH_VARIABLE_ALLOWED_REGISTERS: LazyRegSet = Lazy::new(|| {
    (&[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12])
        .into_iter()
        .map(|&v| Reg(v))
        .collect()
});
pub static SP_REGISTER: Reg = Reg(13);
pub static LINK_REGISTER: Reg = Reg(14);

pub static DOUBLE_OFFSET: u8 = 16;
pub static DOUBLE_REGISTERS: LazyRegSet = Lazy::new(|| {
    (0u8..16u8)
        .into_iter()
        .map(|v| Reg(v + DOUBLE_OFFSET))
        .collect()
});

impl mir::Ty {
    pub(self) fn require_double_registers(&self) -> bool {
        match self {
            mir::Ty::Primitive(mir::PrimitiveTy::Float, 8) => true,
            _ => false,
        }
    }
    pub(self) fn register_count(&self) -> usize {
        match self {
            mir::Ty::Primitive(mir::PrimitiveTy::Float, _) => 1,
            mir::Ty::Primitive(_, x) => ((*x + 3) / 4) as usize,
            mir::Ty::Fn(..) | mir::Ty::Array(..) | mir::Ty::Ptr(..) => 1,
            mir::Ty::Void => 0,
            mir::Ty::RestParams => todo!("Not supported"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Label {
    Name(String),
    LocalNum(usize),
}

/// Second operand for ARM instruction
#[derive(Debug, Clone)]
pub enum ArmOperand {
    Reg(Reg),
    Imm(i32),
}

#[derive(Debug, Clone)]
pub struct NumericOperand {
    pub dest: Reg,
    pub lhs: Reg,
    pub rhs: ArmOperand,
}

#[derive(Debug, Clone)]
pub enum MemoryAccess {
    Register(Reg, isize),
    Label(Label),
}

#[derive(Debug, Clone, Copy)]
pub enum Conditional {
    /// Equal
    Eq,
    /// Not equal
    Ne,
    /// Carry
    Cs,
    /// No Carry
    Cc,
    /// Minus, Negative
    Mi,
    /// Plus, Positive or zero
    Pl,
    /// Overflow
    Vs,
    /// No overflow
    Vc,
    /// Unsigned Greater
    Hi,
    /// Unsigned Less
    Ls,
    /// Greater or equal
    Ge,
    /// Less than
    Lt,
    /// Greater than
    Gt,
    /// Less or equal
    Le,
    /// Always or None
    Al,
}

#[derive(Debug, Clone)]
pub enum ArmCode {
    // Branching
    /// Branch
    B(Conditional, Label),
    /// Call
    Bl(Label),

    // Data Processing
    Add(NumericOperand),
    Sub(NumericOperand),
    Mul(NumericOperand),
    Div(NumericOperand),
    /// Reverse Subtract
    Rsb(NumericOperand),
    And(NumericOperand),
    Orr(NumericOperand),
    /// Xor
    Eor(NumericOperand),
    Mov(Reg, ArmOperand),
    /// Conditional move. The same instruction as Mov, but adds a condition
    /// since it's useful
    CMov(Conditional, Reg, ArmOperand),

    // Floating Point Data Processing
    VAdd(NumericOperand),
    VSub(NumericOperand),
    VMul(NumericOperand),
    VDiv(NumericOperand),
    VMov(Reg, ArmOperand),

    // Comparative
    /// Compare
    Cmp(Reg, ArmOperand),
    /// Compare Negative
    Cmn(Reg, ArmOperand),
    /// Test AND
    Tst(Reg, ArmOperand),
    /// Test
    Teq(Reg, ArmOperand),

    // Load and Store
    LdR(Reg, MemoryAccess),
    StR(Reg, MemoryAccess),
    LdRD(Reg, MemoryAccess),
    StRD(Reg, MemoryAccess),

    Push(Reg),
    Pop(Reg),

    VLdR(Reg, MemoryAccess),
    VStR(Reg, MemoryAccess),
}

pub enum StaticData {
    Byte(Vec<u8>),
    Word(Vec<u32>),
    AsciiZ(String),
}
