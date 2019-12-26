use super::*;
use std::fmt::{self, *};

fn fmt_insts(f: &mut Formatter<'_>, inst: &[Inst]) -> Result {
    let iter = inst.iter().zip(0..);
    for code in iter {
        writeln!(f, "{} {}", code.1, code.0)?;
    }
    Ok(())
}

impl Display for O0 {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        {
            writeln!(f, ".constants:")?;
            let iter = self.constants.iter().zip(0..);
            for c in iter {
                writeln!(f, "{} {}", c.1, c.0)?;
            }
        }
        {
            writeln!(f, ".start:")?;
            write!(f, "{}", self.start_code)?;
        }
        {
            writeln!(f, ".functions:")?;
            let iter = self.functions.iter().zip(0..);
            for c in iter {
                writeln!(f, "{} {} {} {}", c.1, c.0.name_idx, c.0.param_siz, c.0.lvl)?;
            }
            let iter = self.functions.iter().zip(0..);
            for c in iter {
                writeln!(f, ".F{}:", c.1)?;
                fmt_insts(f, &c.0.ins[..])?;
            }
        }
        Ok(())
    }
}

impl Display for FnInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        fmt_insts(f, &self.ins[..])
    }
}

impl Display for StartCodeInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        fmt_insts(f, &self.ins[..])
    }
}

impl Display for Inst {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Inst::Nop => write!(f, "nop"),
            Inst::CPush(a) => write!(f, "bipush {}", a),
            Inst::IPush(a) => write!(f, "ipush {}", a),
            Inst::Pop1 => write!(f, "pop"),
            Inst::Pop2 => write!(f, "pop2"),
            Inst::PopN(a) => write!(f, "popn {}", a),
            Inst::Dup => write!(f, "dup"),
            Inst::Dup2 => write!(f, "dup2"),
            Inst::LoadC(a) => write!(f, "loadc {}", a),
            Inst::LoadA(a, b) => write!(f, "loada {}, {}", a, b),
            Inst::New => write!(f, "new"),
            Inst::SNew(a) => write!(f, "snew {}", a),
            Inst::ILoad => write!(f, "iload"),
            Inst::DLoad => write!(f, "dload"),
            Inst::ALoad => write!(f, "aload"),
            Inst::IALoad => write!(f, "iaload"),
            Inst::DALoad => write!(f, "daload"),
            Inst::AALoad => write!(f, "aaload"),
            Inst::IStore => write!(f, "istore"),
            Inst::DStore => write!(f, "dstore"),
            Inst::AStore => write!(f, "astore"),
            Inst::IAStore => write!(f, "iastore"),
            Inst::DAStore => write!(f, "dastore"),
            Inst::AAStore => write!(f, "aastore"),
            Inst::IAdd => write!(f, "iadd"),
            Inst::DAdd => write!(f, "dadd"),
            Inst::ISub => write!(f, "isub"),
            Inst::DSub => write!(f, "dsub"),
            Inst::IMul => write!(f, "imul"),
            Inst::DMul => write!(f, "dmul"),
            Inst::IDiv => write!(f, "idiv"),
            Inst::DDiv => write!(f, "ddiv"),
            Inst::INeg => write!(f, "ineg"),
            Inst::DNeg => write!(f, "dneg"),
            Inst::ICmp => write!(f, "icmp"),
            Inst::DCmp => write!(f, "dcmp"),
            Inst::I2D => write!(f, "i2d"),
            Inst::D2I => write!(f, "d2i"),
            Inst::I2C => write!(f, "i2c"),
            Inst::Jmp(a) => write!(f, "jmp {}", a),
            Inst::JE(a) => write!(f, "je {}", a),
            Inst::JNe(a) => write!(f, "jne {}", a),
            Inst::JL(a) => write!(f, "jl {}", a),
            Inst::JGe(a) => write!(f, "jge {}", a),
            Inst::JG(a) => write!(f, "jg {}", a),
            Inst::JLe(a) => write!(f, "jle {}", a),
            Inst::Call(a) => write!(f, "call {}", a),
            Inst::Ret => write!(f, "ret"),
            Inst::IRet => write!(f, "iret"),
            Inst::DRet => write!(f, "dret"),
            Inst::ARet => write!(f, "aret"),
            Inst::IPrint => write!(f, "iprint"),
            Inst::DPrint => write!(f, "dprint"),
            Inst::CPrint => write!(f, "cprint"),
            Inst::SPrint => write!(f, "sprint"),
            Inst::PrintLn => write!(f, "printl"),
            Inst::IScan => write!(f, "iscan"),
            Inst::DScan => write!(f, "dscan"),
            Inst::CScan => write!(f, "cscan"),
            _ => Ok(()),
        }
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Constant::Float(i) => write!(f, "D {}", i),
            Constant::String(s) => {
                let s = String::from_utf8_lossy(&s);
                let s = s.escape_default();
                write!(f, "S \"{}\"", s)
            }
            Constant::Number(n) => write!(f, "I {}", n),
        }
    }
}

/*
    # 常量表，记录int、double、字符串常量的信息
    .constants:
        # 下标  常量的类型 常量的值
        {index} {type}    {value}
        ...
    # 启动代码，负责执行全局变量的初始化
    .start:
        #  下标  指令名   操作数
        {index} {opcode} {operands}
        ...
    # 函数表，记录函数的基本信息
    .functions:
        # 下标   函数名在.constants中的下标 参数占用的slot数 函数嵌套的层级
        {index} {name_index}              {params_size}   {level}
        ...
    # 函数体
    .F0:
        # 下标   指令名   操作数
        {index} {opcode} {operands}
        ...
    .F1:
        {index} {opcode} {operands}
        ...
    ...
    .F{functions_count-1}:
        {index} {opcode} {operands}
        ...
*/
