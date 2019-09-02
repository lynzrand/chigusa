
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

    /// Store to address
    /// 
    /// # Pops 2 items
    /// 
    /// - The address to memory
    /// - The value to store
    Sa,

    /// Store to stack
    /// 
    /// # Pops 2 items
    /// 
    /// - The stack offset to the value
    /// - The value to store
    Ss,

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

    // Test
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
    Jt(isize),

    /// Jump unconditional
    /// 
    /// # Side effect
    /// 
    /// Will jump to the corresponding program offset unconditionally.
    Jmp(isize),

    /// Jump absolute unconditional
    /// 
    /// # Side effect
    /// 
    /// Will jump to the corresponding absolute program position unconditionally.
    JmpAbs(usize),

    /// Call
    /// 
    /// # Data
    /// 
    /// The absolute position of the function being called
    /// 
    /// # Side effect
    /// 
    /// Will
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
