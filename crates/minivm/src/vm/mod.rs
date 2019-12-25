//! Should be the virtual machine for c0 code.
//!
//! This thing is mostly unimplemented and I dont want to deal with it for now.

#![allow(unreachable_patterns)]
#![allow(unused_variables)]
use crate::*;

pub struct CallStack<'a> {
    pub stack: Vec<u32>,
    pub ip: u16,
    pub f: &'a Vec<Inst>,
}

impl<'a> CallStack<'a> {
    pub fn of(f: &'a Vec<Inst>) -> CallStack<'a> {
        CallStack {
            f,
            ip: 0,
            stack: Vec::new(),
        }
    }
}

pub struct MiniVM<'a> {
    pub prog: &'a O0,
    pub call_stack: Vec<CallStack<'a>>,
}

impl<'a> MiniVM<'a> {
    pub fn of(prog: &'a O0) -> MiniVM<'a> {
        MiniVM {
            prog,
            call_stack: Vec::new(),
        }
    }

    /*
        Address space:
            Stack address:
                00[stack_index:16][ptr_offset:14]
            Constant address:
                01[constant_index:16][ptr_offset:14]
            Heap address:
                1[heap_index:16][ptr_offset:15]
    */

    pub fn run(&mut self) {
        self.call_stack
            .push(CallStack::of(&self.prog.start_code.ins))
    }

    fn run_f(&mut self) {
        let cur_f = self.call_stack.last_mut().unwrap();
        loop {
            let inst = *cur_f
                .f
                .get(cur_f.ip as usize)
                .expect("Instruction pointer overflow!");
            cur_f.ip += 1;
            match inst {
                Inst::Nop => {}
                Inst::CPush(a) => cur_f.stack.push(a as u32),
                Inst::IPush(a) => cur_f.stack.push(a as u32),
                Inst::Pop1 => {
                    cur_f.stack.pop();
                }
                Inst::Pop2 => cur_f.stack.truncate(cur_f.stack.len() - 2),
                Inst::PopN(a) => cur_f.stack.truncate(cur_f.stack.len() - a as usize),
                Inst::Dup => cur_f
                    .stack
                    .push(*cur_f.stack.last().expect("Stack is empty")),
                Inst::Dup2 => {
                    let last = *cur_f.stack.last().expect("Stack is empty");
                    let second_last = *cur_f
                        .stack
                        .get(cur_f.stack.len() - 2)
                        .expect("Stack is empty");
                    cur_f.stack.push(second_last);
                    cur_f.stack.push(last);
                }
                Inst::LoadC(a) => {
                    let const_entry = self
                        .prog
                        .constants
                        .get(a as usize)
                        .expect("Bad constant entry");
                    match const_entry {
                        Constant::Float(f) => {
                            let f = f.to_bits();
                            cur_f.stack.push((f & 0xfffffff) as u32);
                            cur_f.stack.push(((f & (0xfffffff << 8)) >> 8) as u32);
                        }
                        Constant::Number(n) => cur_f.stack.push(*n),
                        Constant::String(s) => todo!(),
                    }
                }
                _ => todo!(),
                Inst::LoadA(a, b) => {}
                Inst::New => {}
                Inst::SNew(a) => {}
                Inst::ILoad => {}
                Inst::DLoad => {}
                Inst::ALoad => {}
                Inst::IALoad => {}
                Inst::DALoad => {}
                Inst::AALoad => {}
                Inst::IStore => {}
                Inst::DStore => {}
                Inst::AStore => {}
                Inst::IAStore => {}
                Inst::DAStore => {}
                Inst::AAStore => {}
                Inst::IAdd => {}
                Inst::DAdd => {}
                Inst::ISub => {}
                Inst::DSub => {}
                Inst::IMul => {}
                Inst::DMul => {}
                Inst::IDiv => {}
                Inst::DDiv => {}
                Inst::INeg => {}
                Inst::DNeg => {}
                Inst::ICmp => {}
                Inst::DCmp => {}
                Inst::I2D => {}
                Inst::D2I => {}
                Inst::I2C => {}
                Inst::Jmp(a) => {}
                Inst::JE(a) => {}
                Inst::JNe(a) => {}
                Inst::JL(a) => {}
                Inst::JGe(a) => {}
                Inst::JG(a) => {}
                Inst::JLe(a) => {}
                Inst::Call(a) => {}
                Inst::Ret => {}
                Inst::IRet => {}
                Inst::DRet => {}
                Inst::ARet => {}
                Inst::IPrint => {}
                Inst::DPrint => {}
                Inst::CPrint => {}
                Inst::SPrint => {}
                Inst::PrintLn => {}
                Inst::IScan => {}
                Inst::DScan => {}
                Inst::CScan => {}
                _ => {}
            }
        }
    }
}
