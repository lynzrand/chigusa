pub mod codegen;

use std::io::{Read, Write};

trait Readable {
    fn read_from(&self, w: &mut impl Read) -> std::io::Result<()>;
}

trait Writable {
    fn write_to(&self, w: &mut impl Write) -> std::io::Result<()>;
}

pub enum Ins {
    Nop,
    CPush(u8),
    IPush(u32),
    Pop1,
    Pop2,
    PopN(u32),
    Dup,
    Dup2,
    LoadC(u16),
    LoadA(u16, u32),
    New,
    SNew,
    ILoad,
    DLoad,
    ALoad,
    IALoad,
    DALoad,
    AALoad,
    IStore,
    DStore,
    AStore,
    IAStore,
    DAStore,
    AAStore,
    IAdd,
    DAdd,
    ISub,
    DSub,
    IMul,
    DMul,
    IDiv,
    DDiv,
    INeg,
    DNeg,
    ICmp,
    DCmp,
    I2D,
    D2I,
    I2C,
    Jmp(u16),
    JE(u16),
    JNe(u16),
    JL(u16),
    JGe(u16),
    JG(u16),
    JLe(u16),
    Call(u16),
    Ret,
    IRet,
    DRet,
    ARet,
    IPrint,
    DPrint,
    CPrint,
    SPrint,
    PrintLn,
    IScan,
    DScan,
    CScan,
}

impl Ins {
    fn opcode(&self) -> u8 {
        use Ins::*;
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
            SNew => 0x0c,
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
            JLe(..) => 0x77,
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
        }
    }
}

impl Writable for Ins {
    fn write_to(&self, w: &mut impl Write) -> std::io::Result<()> {
        w.write_all(&self.opcode().to_be_bytes())?;
        use Ins::*;
        match self {
            CPush(c) => c.write_to(w),
            IPush(i) => i.write_to(w),
            PopN(n) => n.write_to(w),

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

pub struct FnInfo {
    name_idx: u16,
    param_siz: u16,
    lvl: u16,
    ins: Vec<Ins>,
}

impl Writable for FnInfo {
    fn write_to(&self, w: &mut impl Write) -> std::io::Result<()> {
        self.name_idx.write_to(w)?;
        self.param_siz.write_to(w)?;
        self.lvl.write_to(w)?;
        self.ins.write_to(w)
    }
}

pub struct StartCodeInfo {
    ins: Vec<Ins>,
}

impl Writable for StartCodeInfo {
    fn write_to(&self, w: &mut impl Write) -> std::io::Result<()> {
        self.ins.write_to(w)
    }
}

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

pub struct O0 {
    version: u32,
    constants: Vec<Constant>,
    start_code: StartCodeInfo,
    functions: Vec<FnInfo>,
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
