pub use crate::c0::ast::OpVar;
use indexmap::IndexMap;

/// Program in MIR.
pub struct MirProg {
    /// Functions
    pub fns: IndexMap<String, MirFn>,

    /// Constants
    pub const_area: IndexMap<String, Vec<u8>>,

    /// Global vars
    pub global_area: IndexMap<String, Vec<u8>>,

    /// BSS seg for uninitialized variables(?)
    pub bss: IndexMap<String, Vec<u8>>,
}

/// A function represented in MIR
pub struct MirFn {
    /// Function signature.
    ///
    /// TODO: Finalize type representation
    pub sig: Vec<usize>,

    /// Return type
    pub ret: usize,

    /// Function body
    pub body: Vec<Mir>,
}

pub enum Mir {
    /// Immediate
    Imm { typ: usize, val: Vec<u8> },

    /// Declare variable
    Var {
        /// Local id
        loc: usize,
        /// Type
        typ: usize,
        /// is const
        is_const: bool,
    },

    /// Apply binary operator
    BinOp {
        /// Operator
        op: OpVar,
        /// Result var
        res: usize,
        /// Left hand side
        lhs: usize,
        /// Right hand side
        rhs: usize,
    },

    /// Apply unary operator
    UnaOp {
        /// Operator
        op: OpVar,
        /// Result var
        res: usize,
        /// Used local variable
        lhs: usize,
    },

    /// Call function
    FnCall {
        /// Result var
        res: usize,
        /// Function name
        f: String,
        /// Args
        args: Vec<usize>,
    },

    /// Branch if
    BrIf {
        /// Condition
        cond: usize,
        /// Branch block if zero
        zero: usize,
        /// Branch block otherwise
        other: usize,
    },

    /// Branch unconditional
    Br {
        /// Target block
        tgt: usize,
    },

    /// Allocate in heap
    HeapAlloc {
        /// Allocate size
        size: usize,
        /// Result var
        res: usize,
    },
}
