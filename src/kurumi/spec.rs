use std::collections::HashMap;

/// Representation of a whole Kurumi Program
pub struct Program{
    data: Vec<u64>,
    text: Vec<Op>,
    
}

/// Instructions available inside Kurumi VM.
/// 
/// These instructions are used to manipulate the register stack, stack memory
/// and heap memory. 
/// 
/// # Note on instructions
/// 
/// A description of an instruction contains 3 parts: its name and brief 
/// introduction, the value it pushes and pops from register stack, and its 
/// side effects. Below is an example of it.
/// 
/// ```markdown
/// Alloc
/// 
/// Allocates a continuous region of memory.
/// ```
/// 
/// This is the name and brief introduction of the instruction. It describes 
/// what this instruction will do. If the name is clear enough, the instruction
/// might be absent.
/// 
/// ```markdown
/// # Pops 1 item
/// 
/// - The number of bytes to be allocated
/// 
/// # Pushes 1 item
/// 
/// - The address of the allocated bytes
/// ```
/// 
/// This part describes how it will manipulate the register stack. Values are
/// popped and pushed from top to bottom inside the description.
/// 
/// ```markdown
/// # Side effects
/// 
/// Memory will be allocated as specified.
/// ```
pub enum Op {
    // Data
    /// Load Immediate
    /// 
    /// # Data
    /// 
    /// - The value to be pushed
    /// 
    /// # Pushes 1 item
    /// 
    /// - The immediate value provided inside instruction
    LImm(i64),

    /// Load local variable
    /// 
    /// # Pushes 1 item
    /// 
    /// - The local variable numbered inside instruction
    LLoc(u64),

    /// Load Address
    /// 
    /// # Pops 1 item
    /// 
    /// - The address pointing to the variable
    /// 
    /// # Pushes 1 item
    /// 
    /// - The variable the address is pointing to
    La,

    /// Load stack
    /// 
    /// # Pops 1 item
    /// 
    /// - The number indicating how many item down the stack should be loaded
    /// 
    /// # Pushes 1 item
    /// 
    /// - The item in that stack address, cloned
    Ls,

    /// Load base pointer value
    /// 
    /// # Pushes 1 item
    /// 
    /// - The current value of base poiner
    Lbp,

    /// Store to address
    /// 
    /// # Pops 2 items
    /// 
    /// - The address to memory
    /// - The value to store
    Sa,

    /// Store to address, sized
    /// 
    /// # Data
    /// 
    /// - The bytes of data to be saved
    /// 
    /// # Pops 2 items
    /// 
    /// - The address to memory
    /// - The value to store
    Sas(u64),

    /// Store to stack
    /// 
    /// # Pops 2 items
    /// 
    /// - The stack offset to the value
    /// - The value to store
    Ss,

    /// Store to stack, sized
    /// 
    /// # Data
    /// 
    /// The size of bytes to be saved
    /// 
    /// # Pops 2 items
    /// 
    /// - The stack offset to the value
    /// - The value to store
    Sss(u8),

    /// Duplicate stack top
    /// 
    /// # Pushes 1 item
    /// 
    /// - The value on stack top
    Dup,

    /// Pop stack top
    /// 
    /// # Pops 1 item
    /// 
    /// - Current stack top, to nowhere.
    Pop,

    // Memory
    /// Allocate memory space
    /// 
    /// # Pops 1 item
    /// 
    /// - The number of bytes to allocate
    /// 
    /// # Pushes 1 item
    /// 
    /// - The address of that memory space
    /// 
    /// # Side effect
    /// 
    /// The specified memory will be allocated.
    Alloc,

    /// Free the allocated memory
    /// 
    /// # Pops 1 item
    /// 
    /// - The address to the start of the allocated memory
    /// 
    /// # Side effect
    /// 
    /// The specified memory will be freed. Future uses of that address will 
    /// cause the program to panic.
    Free,

    // Testing
    
    /// Test less or equal
    /// 
    /// # Pops 2 items
    /// 
    /// - `Lhs`
    /// - `Rhs`
    /// 
    /// # Pushes 1 item
    /// 
    /// - 1 if `Lhs <= Rhs`, else 0
    Tle,

    /// Test less
    /// 
    /// # Pops 2 items
    /// 
    /// - `Lhs`
    /// - `Rhs`
    /// 
    /// # Pushes 1 item
    /// 
    /// - 1 if `Lhs < Rhs`, else 0
    Tl,

    /// Test equal
    /// 
    /// # Pops 2 items
    /// 
    /// - `Lhs`
    /// - `Rhs`
    /// 
    /// # Pushes 1 item
    /// 
    /// - 1 if `Lhs == Rhs`, else 0
    Teq,

    /// Test not equal
    /// 
    /// # Pops 2 items
    /// 
    /// - `Lhs`
    /// - `Rhs`
    /// 
    /// # Pushes 1 item
    /// 
    /// - 1 if `Lhs != Rhs`, else 0
    Tne,

    /// Test greater
    /// 
    /// # Pops 2 items
    /// 
    /// - `Lhs`
    /// - `Rhs`
    /// 
    /// # Pushes 1 item
    /// 
    /// - 1 if `Lhs >= Rhs`, else 0
    Tg,

    /// Test greater or equal
    /// 
    /// # Pops 2 items
    /// 
    /// - `Lhs`
    /// - `Rhs`
    /// 
    /// # Pushes 1 item
    /// 
    /// - 1 if `Lhs > Rhs`, else 0
    Tge,

    /// Test truthy
    /// 
    /// # Pops 2 items
    /// 
    /// - `Lhs`
    /// 
    /// # Pushes 1 item
    /// 
    /// - 1 if `Lhs` is not equal to 0, else 0
    Ttr,

    // Jump
    /// Jump if true
    /// 
    /// # Data
    /// 
    /// The program location offset to jump to.
    /// 
    /// # Pops 1 item
    /// 
    /// - The value to be tested
    /// 
    /// # Side effect
    /// 
    /// Will jump to the corresponding program location offset if the value is 
    /// true. Else it will just pop the value and nothing will happen.
    Jt(i64),

    /// Jump unconditional
    /// 
    /// # Side effect
    /// 
    /// Will jump to the corresponding program offset unconditionally.
    Jmp(i64),

    /// Jump absolute unconditional
    /// 
    /// # Side effect
    /// 
    /// Will jump to the corresponding absolute program position unconditionally.
    JmpAbs(u64),

    /// Call
    /// 
    /// # Data
    /// 
    /// The relative position of the function being called
    /// 
    /// # Pushes 1 value
    /// 
    /// - The current instruction pointer value
    /// 
    /// # Side effect
    /// 
    /// Jumps to the start of the specified address
    Call(i64),

    /// Call Absolute
    /// 
    /// # Data
    /// 
    /// The absolute position of the function being called
    /// 
    /// # Pushes 1 value
    /// 
    /// - The current instruction pointer value
    /// 
    /// # Side effect
    /// 
    /// Jumps to the start of the specified address
    CallAbs(u64),

    /// Return
    /// 
    /// # Pops 2 values
    /// 
    /// - The return value (i64; for larger return values use RetS instead;
    ///     for void returns provide anything (conventionally `0`))
    /// - The original instruction pointer
    /// 
    /// # Pushes 1 value
    /// 
    /// - The return value (restored at stack top)
    /// 
    /// # Side effect
    /// 
    /// Pops the stack and sets instruction pointer to whatever stored inside
    /// it.
    Ret,

    /// Return Multiple
    /// 
    /// # Data
    /// 
    /// The size of return value
    /// 
    /// # Pops multiple values
    /// 
    /// - The return value, sized as supplied
    /// - The original instruction pointer
    /// 
    /// # Pushes multiple values
    /// 
    /// - The return value, sized as supplied
    RetM(u64),

    /// Reserve stack
    /// 
    /// # Data
    /// 
    /// The size to reserve, exact
    /// 
    /// # Pushes multiple values
    /// 
    /// - The reserved stack, sized as supplied
    Rs(u64),

    /// Clean stack
    /// 
    /// # Data
    /// 
    /// The size to clean, exact
    /// 
    /// # Pops multiple values
    /// 
    /// - The reserved stack, sized as supplied
    Cs(u64),

    /// Clean stack with stack top reserved
    /// 
    /// # Data
    /// 
    /// The size to reserve at stack top, exact
    /// 
    /// # Pops multiple values
    /// 
    /// - The size to clean, exact
    /// - The reserved stack, sized as supplied
    Csz(u64),

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
    /// Float addition
    FAdd,
    /// Float subtraction
    FSub,
    /// Float multiplication
    FMul,
    /// Float division
    FDiv,
    /// Float negate
    FNeg,
    /// Invert
    Inv,
    /// Shift right
    Shr,
    /// SHift left
    Shl,
    /// Boolean not
    Not,
    /// Binary and
    And,
    /// Binary or
    Or,
    /// Binary xor
    Xor,

    /// Convert Unsigned
    /// 
    /// # Data
    /// 
    /// - The number of bits of the original number
    /// - The number of bits of the converted number
    ConU(u8, u8),

    /// Convert Signed
    /// 
    /// # Data
    /// 
    /// - The number of bits of the original number
    /// - The number of bits of the converted number
    ConS(u8, u8)
}
