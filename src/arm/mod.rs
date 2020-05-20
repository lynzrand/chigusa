pub type Reg = u8;
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
