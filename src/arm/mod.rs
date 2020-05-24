pub mod codegen;
mod util;

pub type Reg = u8;

pub static VARIABLE_REGISTERS: &'static [u8] = &[4, 5, 6, 7, 8, 10, 11];
pub static PARAM_REGISTERS: &'static [u8] = &[1, 2, 3, 4];
pub static RESULT_REGISTERS: &'static [u8] = &[1, 2];
pub static SCRATCH_REGISTERS: &'static [u8] = &[1, 2, 3, 4, 12];
pub static SP_REGISTER: u8 = 13;
pub static LINK_REGISTER: u8 = 14;

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
