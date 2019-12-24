use std::io::{Read, Write};
pub mod out;

trait Readable {
    fn read_from(&self, w: &mut impl Read) -> std::io::Result<()>;
}

trait Writable {
    fn write_to(&self, w: &mut impl Write) -> std::io::Result<()>;
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Inst {
    /// No-op
    Nop,

    /// () -> char
    CPush(u8),
    /// () -> u32
    IPush(i32),

    /// u32 -> ()
    Pop1,
    /// u64 -> ()
    Pop2,
    /// u32[(n)] -> ()
    PopN(u32),

    /// u32 -> u32, u32
    Dup,
    /// u64 -> u64, u64
    Dup2,

    /// (idx) -> val
    ///
    /// val:
    /// - const[(idx)]: u32 => u32
    /// - const[(idx)]: f64 => f64
    /// - const[(idx)]: str | T[] => usize
    LoadC(u16),
    /// (lvl, off) -> u32
    LoadA(u16, i32),

    /// u32 -> usize
    New,
    /// (n) -> u32[n]
    SNew(u32),

    /// Load(address) -> u32
    ILoad,
    /// Load(address) -> u64
    DLoad,
    /// Load(address) -> usize
    ALoad,

    /// Load(address + offset) -> u32
    IALoad,
    /// Load(address + offset) -> u64
    DALoad,
    /// Load(address + offset) -> usize
    AALoad,

    /// Store(address, u32) -> ()
    IStore,
    /// Store(address, u64) -> ()
    DStore,
    /// Store(address, address) -> ()
    AStore,

    /// Load(address + offset, u32) -> ()
    IAStore,
    /// Load(address + offset, u32) -> ()
    DAStore,
    /// Load(address + offset, u32) -> ()
    AAStore,

    /// i32 + i32 -> i32
    IAdd,
    /// f64 + f64 -> f64
    DAdd,
    /// i32 - i32 -> i32
    ISub,
    /// f64 - f64 -> f64
    DSub,
    /// i32 * i32 -> i32
    IMul,
    /// f64 * f64 -> f64
    DMul,
    /// i32 / i32 -> i32
    IDiv,
    /// f64 / f64 -> f64
    DDiv,
    /// -i32 -> i32
    INeg,
    /// -f64 -> f64
    DNeg,
    /// i32 - i32 -> i32 [+1, 0, -1]
    ICmp,
    /// f64 - f64 -> i32 [+1, 0, -1]
    DCmp,
    /// i32 -> f64
    I2D,
    /// f64 -> f64
    D2I,
    /// 0xff & u32 -> u32 (u8)
    I2C,

    /// () -> jmp (offset)
    Jmp(u16),
    /// u32 -> if == 0 then jmp (offset)
    JE(u16),
    /// u32 -> if != 0 then jmp (offset)
    JNe(u16),
    /// u32 -> if < 0 then jmp (offset)
    JL(u16),
    /// u32 -> if >= 0 then jmp (offset)
    JGe(u16),
    /// u32 -> if > 0 then jmp (offset)
    JG(u16),
    /// u32 -> if <= 0 then jmp (offset)
    JLe(u16),

    /// params -> call(function[(idx)])
    Call(u16),
    /// () -> ret
    Ret,
    /// u32 -> ret u32
    IRet,
    /// u64 -> ret u64
    DRet,
    /// usize -> ret usize
    ARet,

    /// i32 -> Print()
    IPrint,
    /// f64 -> Print()
    DPrint,
    /// u8 -> Print()
    CPrint,
    /// usize -> Print(str)
    SPrint,
    /// () -> Print(\n)
    PrintLn,
    /// () -> Scan u32
    IScan,
    /// () -> Scan f64
    DScan,
    /// () -> Scan u8
    CScan,
    //
    //==== Compiler-use instructions
    _Gt,
    _Lt,
    _Eq,
    _Gte,
    _Lte,
    _Neq,
}

impl Inst {
    pub fn opcode(&self) -> u8 {
        use Inst::*;
        match self {
            Nop => 0x00,
            CPush(..) => 0x01,
            IPush(..) => 0x02,
            Pop1 => 0x04,
            Pop2 => 0x05,
            PopN(..) => 0x06,
            Dup => 0x07,
            Dup2 => 0x08,
            LoadC(..) => 0x09,
            LoadA(..) => 0x0a,
            New => 0x0b,
            SNew(..) => 0x0c,
            ILoad => 0x10,
            DLoad => 0x11,
            ALoad => 0x12,
            IALoad => 0x18,
            DALoad => 0x19,
            AALoad => 0x1a,
            IStore => 0x20,
            DStore => 0x21,
            AStore => 0x22,
            IAStore => 0x28,
            DAStore => 0x29,
            AAStore => 0x2a,
            IAdd => 0x30,
            DAdd => 0x31,
            ISub => 0x34,
            DSub => 0x35,
            IMul => 0x38,
            DMul => 0x39,
            IDiv => 0x3c,
            DDiv => 0x3d,
            INeg => 0x40,
            DNeg => 0x41,
            ICmp => 0x44,
            DCmp => 0x45,
            I2D => 0x60,
            D2I => 0x61,
            I2C => 0x62,
            Jmp(..) => 0x70,
            JE(..) => 0x71,
            JNe(..) => 0x72,
            JL(..) => 0x73,
            JGe(..) => 0x74,
            JG(..) => 0x75,
            JLe(..) => 0x76,
            Call(..) => 0x80,
            Ret => 0x88,
            IRet => 0x89,
            DRet => 0x8a,
            ARet => 0x8b,
            IPrint => 0xa0,
            DPrint => 0xa1,
            CPrint => 0xa2,
            SPrint => 0xa3,
            PrintLn => 0xaf,
            IScan => 0xb0,
            DScan => 0xb1,
            CScan => 0xb2,
            _ => panic!("Compiler-used instructions should not appear in binary files"),
        }
    }
}

impl Writable for Inst {
    fn write_to(&self, w: &mut impl Write) -> std::io::Result<()> {
        w.write_all(&self.opcode().to_be_bytes())?;
        use Inst::*;
        match self {
            CPush(c) => c.write_to(w),
            IPush(i) => i.write_to(w),
            PopN(n) => n.write_to(w),

            SNew(s) => s.write_to(w),
            LoadC(c) => c.write_to(w),
            LoadA(a, i) => {
                a.write_to(w)?;
                i.write_to(w)
            }

            Jmp(c) => c.write_to(w),
            JE(c) => c.write_to(w),
            JNe(c) => c.write_to(w),
            JL(c) => c.write_to(w),
            JGe(c) => c.write_to(w),
            JG(c) => c.write_to(w),
            JLe(c) => c.write_to(w),
            Call(c) => c.write_to(w),
            _ => Ok(()),
        }
    }
}

const MAGIC: u32 = 0x43303A29;

#[derive(Debug, Clone)]
pub struct FnInfo {
    pub name_idx: u16,
    pub param_siz: u16,
    pub lvl: u16,
    pub ins: Vec<Inst>,
}

impl Writable for FnInfo {
    fn write_to(&self, w: &mut impl Write) -> std::io::Result<()> {
        self.name_idx.write_to(w)?;
        self.param_siz.write_to(w)?;
        self.lvl.write_to(w)?;
        self.ins.write_to(w)
    }
}

#[derive(Debug, Clone)]
pub struct StartCodeInfo {
    pub ins: Vec<Inst>,
}

impl Writable for StartCodeInfo {
    fn write_to(&self, w: &mut impl Write) -> std::io::Result<()> {
        self.ins.write_to(w)
    }
}
#[derive(Debug, Clone)]
pub enum Constant {
    Number(u32),
    Float(f64),
    String(Vec<u8>),
}

impl Writable for Constant {
    fn write_to(&self, w: &mut impl Write) -> std::io::Result<()> {
        match self {
            Constant::Number(n) => {
                0x01u8.write_to(w)?;
                n.write_to(w)
            }

            Constant::Float(f) => {
                0x02u8.write_to(w)?;
                f.write_to(w)
            }

            Constant::String(s) => {
                0x00u8.write_to(w)?;
                s.write_to(w)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct O0 {
    pub version: u32,
    pub constants: Vec<Constant>,
    pub start_code: StartCodeInfo,
    pub functions: Vec<FnInfo>,
}

impl Writable for O0 {
    fn write_to(&self, w: &mut impl Write) -> std::io::Result<()> {
        MAGIC.write_to(w)?;
        w.write_all(&self.version.to_be_bytes())?;
        self.constants.write_to(w)?;
        self.start_code.write_to(w)?;
        self.functions.write_to(w)
    }
}

impl O0 {
    pub fn write_binary(&self, w: &mut impl Write) -> std::io::Result<()> {
        self.write_to(w)
    }
}

impl Writable for u8 {
    #[inline(always)]
    fn write_to(&self, w: &mut impl Write) -> std::result::Result<(), std::io::Error> {
        w.write_all(&[*self])
    }
}

impl Writable for u16 {
    #[inline(always)]
    fn write_to(&self, w: &mut impl Write) -> std::result::Result<(), std::io::Error> {
        w.write_all(&self.to_be_bytes())
    }
}

impl Writable for u32 {
    #[inline(always)]
    fn write_to(&self, w: &mut impl Write) -> std::result::Result<(), std::io::Error> {
        w.write_all(&self.to_be_bytes())
    }
}

impl Writable for i32 {
    #[inline(always)]
    fn write_to(&self, w: &mut impl Write) -> std::result::Result<(), std::io::Error> {
        w.write_all(&self.to_be_bytes())
    }
}

impl Writable for u64 {
    #[inline(always)]
    fn write_to(&self, w: &mut impl Write) -> std::result::Result<(), std::io::Error> {
        w.write_all(&self.to_be_bytes())
    }
}

impl Writable for f64 {
    #[inline(always)]
    fn write_to(&self, w: &mut impl Write) -> std::result::Result<(), std::io::Error> {
        w.write_all(&self.to_be_bytes())
    }
}

impl<T> Writable for Vec<T>
where
    T: Writable,
{
    fn write_to(&self, w: &mut impl Write) -> std::io::Result<()> {
        w.write_all(&(self.len() as u16).to_be_bytes())?;
        for i in self {
            i.write_to(w)?;
        }
        Ok(())
    }
}
