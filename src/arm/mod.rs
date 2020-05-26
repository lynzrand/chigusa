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

pub struct ArmCode {
    op: ArmOp,
}

pub enum ArmOp {
    // Branching
    /// Branch
    B,
    /// Conditional Branch if Non-Zero
    Cbnz,
    /// Conditional Branch if Zero
    Cbz,
    /// Call
    Bl,

    // Data Processing
    Add,
    Sub,
    Mul,
    Div,
    /// Reverse Subtract
    Rsb,
    And,
    Orr,
    /// Xor
    Eor,
    Mov,

    // Floating Point Data Processing
    VAdd,
    VSub,
    VMul,
    VDiv,
    VMov,

    // Comparative
    /// Compare
    Cmp,
    /// Compare Negative
    Cmn,
    /// Test
    Tst,
    /// Test
    Teq,

    // Load and Store
    LdR,
    StR,
    LdRD,
    StRD,

    Push,
    Pop,

    VLdR,
    VStR,
}
