pub enum Op {
    // Data
    /// Load Immediate
    Li(i64),
    /// Load Address
    La,
    /// Load stack
    Ls,
    /// Load stack offset
    Lso,
    /// Pop to address
    Pa,
    /// Pop to stack
    Ps,
    /// Duplicate stack top
    Dup,
    /// Pop stack top
    Pop,
    // Test
    /// Test less or equal
    Tle,
    /// Test less
    Tl,
    /// Test equal
    Teq,
    /// Test not equal
    Tne,
    /// Test greater
    Tg,
    /// Test greater or equal
    Tge,
    /// Test true
    Ttr,

    // Jump
    /// Jump if true
    Jt,
    /// Jump unconditional
    Jmp,
    /// Call
    Call(usize),
    /// Return
    Ret,

    // Arithmic
    /// Integer addition
    IAdd,
    /// Integer subtraction
    ISub,
    /// Integer multiplication
    IMul,
    /// Integer division
    IDiv,
    /// Integer negate
    INeg,
    /// Binary invert
    BInv,
    /// Boolean not
    Not,
    /// Binary and
    And,
    /// Binary or
    Or,
    /// Binary xor
    Xor,
}
