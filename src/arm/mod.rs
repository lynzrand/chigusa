pub mod codegen;
mod util;
use crate::mir;
use indexmap::IndexSet;
use once_cell::sync::Lazy;
use std::collections::HashSet;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Reg(u8);

pub type LazyRegSet = Lazy<IndexSet<Reg>>;

pub static VARIABLE_REGISTERS: LazyRegSet = Lazy::new(|| {
    (&[4, 5, 6, 7, 8, 10])
        .into_iter()
        .map(|&v| Reg(v))
        .collect()
});
pub static PARAM_REGISTERS: LazyRegSet =
    Lazy::new(|| (&[0, 1, 2, 3]).into_iter().map(|&v| Reg(v)).collect());
pub static RESULT_REGISTERS: LazyRegSet =
    Lazy::new(|| (&[0, 1]).into_iter().map(|&v| Reg(v)).collect());
pub static SCRATCH_REGISTERS: LazyRegSet =
    Lazy::new(|| (&[0, 1, 2, 3]).into_iter().map(|&v| Reg(v)).collect());
pub static SCRATCH_VARIABLE_ALLOWED_REGISTERS: LazyRegSet = Lazy::new(|| {
    (&[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
        .into_iter()
        .map(|&v| Reg(v))
        .collect()
});
pub static FP_REGISTER: Reg = Reg(11);
pub static SP_REGISTER: Reg = Reg(13);
pub static LINK_REGISTER: Reg = Reg(14);
pub static PC_REGISTER: Reg = Reg(15);

pub static DOUBLE_OFFSET: u8 = 16;
pub static DOUBLE_REGISTERS: LazyRegSet = Lazy::new(|| {
    (0u8..16u8)
        .into_iter()
        .map(|v| Reg(v + DOUBLE_OFFSET))
        .collect()
});

impl std::fmt::Debug for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            0..=11 => write!(f, "r{}", self.0),
            12 => write!(f, "ip"),
            13 => write!(f, "sp"),
            14 => write!(f, "lr"),
            15 => write!(f, "pc"),
            16..=31 => write!(f, "d{}", self.0 - 16),
            _ => panic!("Unknown register r{}", self.0),
        }
    }
}

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

// #[derive(Debug, Clone)]
pub type Label = String;

/// Second operand for ARM instruction
#[derive(Clone)]
pub enum ArmOperand {
    Reg(Reg),
    Imm(i32),
}

impl std::fmt::Debug for ArmOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArmOperand::Reg(r) => write!(f, "{:?}", r),
            ArmOperand::Imm(i) => write!(f, "#{}", i),
        }
    }
}

#[derive(Clone)]
pub struct NumericOperand {
    pub dest: Reg,
    pub lhs: Reg,
    pub rhs: ArmOperand,
}

impl std::fmt::Debug for NumericOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?},\t{:?},\t{:?}", self.dest, self.lhs, self.rhs)
    }
}

#[derive(Clone)]
pub enum MemoryAccess {
    Register(Reg, isize),
    Label(Label),
}

impl std::fmt::Debug for MemoryAccess {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MemoryAccess::Register(reg, i) => write!(f, "[{:?}, {}]", reg, i),
            MemoryAccess::Label(l) => write!(f, "{}", l),
        }
    }
}

#[derive(Clone, Copy)]
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

impl Conditional {
    pub fn inv(self) -> Self {
        match self {
            Conditional::Eq => Conditional::Ne,
            Conditional::Ne => Conditional::Eq,
            Conditional::Cs => Conditional::Cc,
            Conditional::Cc => Conditional::Cs,
            Conditional::Mi => Conditional::Pl,
            Conditional::Pl => Conditional::Mi,
            Conditional::Vs => Conditional::Vc,
            Conditional::Vc => Conditional::Vs,
            Conditional::Hi => Conditional::Ls,
            Conditional::Ls => Conditional::Hi,
            Conditional::Ge => Conditional::Lt,
            Conditional::Lt => Conditional::Ge,
            Conditional::Gt => Conditional::Le,
            Conditional::Le => Conditional::Gt,
            Conditional::Al => panic!("AL has no inverse"),
        }
    }
}

impl std::fmt::Debug for Conditional {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Conditional::Eq => write!(f, "eq"),
            Conditional::Ne => write!(f, "ne"),
            Conditional::Cs => write!(f, "cs"),
            Conditional::Cc => write!(f, "cc"),
            Conditional::Mi => write!(f, "mi"),
            Conditional::Pl => write!(f, "pl"),
            Conditional::Vs => write!(f, "vs"),
            Conditional::Vc => write!(f, "vc"),
            Conditional::Hi => write!(f, "hi"),
            Conditional::Ls => write!(f, "ls"),
            Conditional::Ge => write!(f, "ge"),
            Conditional::Lt => write!(f, "lt"),
            Conditional::Gt => write!(f, "gt"),
            Conditional::Le => write!(f, "le"),
            Conditional::Al => write!(f, ""),
        }
    }
}

#[derive(Clone)]
pub enum ArmCode {
    // Branching
    /// Branch
    B(Conditional, Label),
    /// Call
    Bl(Label),
    /// Return to
    Bx(Reg),

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
    // LdRD(Reg, MemoryAccess),
    // StRD(Reg, MemoryAccess),
    /// Store multiple (bool: write back)
    StM(Reg, bool, Vec<Reg>),
    /// Load multiple (bool: write back)
    LdM(Reg, bool, Vec<Reg>),

    Push(Vec<Reg>),
    Pop(Vec<Reg>),

    VLdR(Reg, MemoryAccess),
    VStR(Reg, MemoryAccess),

    /// Represents a label inside code; is pseudo-instruction
    _Label(String),
}

impl std::fmt::Debug for ArmCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArmCode::B(c, t) => write!(f, "\tb{:?}\t{}", c, t),
            ArmCode::Bl(t) => write!(f, "\tbl\t{}", t),
            ArmCode::Bx(r) => write!(f, "\tbx\t{:?}", r),
            ArmCode::Add(op) => write!(f, "\tadd\t{:?}", op),
            ArmCode::Sub(op) => write!(f, "\tsub\t{:?}", op),
            ArmCode::Mul(op) => write!(f, "\tmul\t{:?}", op),
            ArmCode::Div(op) => write!(f, "\tdiv\t{:?}", op),
            ArmCode::Rsb(op) => write!(f, "\trsb\t{:?}", op),
            ArmCode::And(op) => write!(f, "\tand\t{:?}", op),
            ArmCode::Orr(op) => write!(f, "\torr\t{:?}", op),
            ArmCode::Eor(op) => write!(f, "\teor\t{:?}", op),
            ArmCode::Mov(t, s) => write!(f, "\tmov\t{:?},\t{:?}", t, s),
            ArmCode::CMov(c, t, s) => write!(f, "\tmov{:?}\t{:?},\t{:?}", c, t, s),
            ArmCode::VAdd(op) => write!(f, "\tvadd\t{:?}", op),
            ArmCode::VSub(op) => write!(f, "\tvsub\t{:?}", op),
            ArmCode::VMul(op) => write!(f, "\tvmul\t{:?}", op),
            ArmCode::VDiv(op) => write!(f, "\tvdiv\t{:?}", op),
            ArmCode::VMov(t, s) => write!(f, "\tvmov\t{:?},\t{:?}", t, s),
            ArmCode::Cmp(x, y) => write!(f, "\tcmp\t{:?},\t{:?}", x, y),
            ArmCode::Cmn(x, y) => write!(f, "\tcmn\t{:?},\t{:?}", x, y),
            ArmCode::Tst(x, y) => write!(f, "\ttst\t{:?},\t{:?}", x, y),
            ArmCode::Teq(x, y) => write!(f, "\tteq\t{:?},\t{:?}", x, y),
            ArmCode::LdR(d, s) => write!(f, "\tldr\t{:?},\t{:?}", d, s),
            ArmCode::StR(d, s) => write!(f, "\tstr\t{:?},\t{:?}", d, s),
            // ArmCode::LdRD(d, s) => write!(f, "\t"),
            // ArmCode::StRD(d, s) => write!(f, "\t"),
            ArmCode::StM(b, x, m) => {
                write!(f, "\tstm\t{:?}{},\t{{", b, if *x { "!" } else { "" })?;
                for (idx, reg) in m.iter().enumerate() {
                    if idx != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", reg)?;
                }
                write!(f, "}}")
            }
            ArmCode::LdM(b, x, m) => {
                write!(f, "\tldm\t{:?}{},\t{{", b, if *x { "!" } else { "" })?;
                for (idx, reg) in m.iter().enumerate() {
                    if idx != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", reg)?;
                }
                write!(f, "}}")
            }
            ArmCode::Push(r) => {
                write!(f, "\tpush\t{{")?;
                for (idx, reg) in r.iter().enumerate() {
                    if idx != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", reg)?;
                }
                write!(f, "}}")
            }
            ArmCode::Pop(r) => {
                write!(f, "\tpop\t{{")?;
                for (idx, reg) in r.iter().enumerate() {
                    if idx != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", reg)?;
                }
                write!(f, "}}")
            }
            ArmCode::VLdR(r, s) => write!(f, "\tvldr\t{:?},\t{:?}", r, s),
            ArmCode::VStR(r, s) => write!(f, "\tvstr\t{:?},\t{:?}", r, s),
            ArmCode::_Label(l) => write!(f, "{}:", l),
        }
    }
}

#[derive(Debug)]
pub enum StaticData {
    Byte(Vec<u8>),
    Word(Vec<u32>),
    AsciiZ(String),
}

#[derive(Debug)]
pub struct ArmFunction {
    name: String,
    code: Vec<ArmCode>,
}
