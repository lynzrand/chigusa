use crate::mir;
use std::collections::{HashMap, HashSet, VecDeque};

pub struct Codegen<'src> {
    src: &'src mir::MirPackage,
}

impl<'src> Codegen<'src> {}

pub struct FnCodegen<'src> {
    func: &'src mir::Func,
}

impl<'src> FnCodegen<'src> {
    /// Generate a basic block arrange that is good enough for a structured
    /// program. _We don't have `goto`-s anyway!_
    fn arrange_basic_blocks(&self) -> Vec<mir::BBId> {
        let mut cycle_solver = super::util::CycleSolver::new(&self.func.bb);
        cycle_solver.solve();

        let mut input_count = HashMap::<usize, usize>::new();
        let cycle_count = cycle_solver.counter;

        // starting block
        input_count.insert(0, 1);
        for (_, blk) in &self.func.bb {
            for input_blk in &blk.jump_in {
                input_count
                    .entry(*input_blk)
                    .and_modify(|count| *count = *count + 1)
                    .or_insert(1);
            }
        }

        let mut bb_arrangement = Vec::new();
        let mut bfs_q = VecDeque::new();
        bfs_q.push_back(0);

        while bfs_q.len() > 0 {
            let item = bfs_q.pop_front().unwrap();
            let entry = input_count.get_mut(&item).unwrap();
            *entry -= 1;
            if *entry > cycle_count.get(&item).cloned().unwrap_or(0) {
                continue;
            }

            bb_arrangement.push(*entry);

            let blk = self.func.bb.get(entry).unwrap();
            match &blk.end {
                mir::JumpInst::Jump(id) => {
                    bfs_q.push_back(*id);
                }
                mir::JumpInst::Conditional(_, t, f) => {
                    bfs_q.push_back(*t);
                    bfs_q.push_back(*f);
                }
                mir::JumpInst::Return(_) | mir::JumpInst::Unreachable | mir::JumpInst::Unknown => {
                    // Noop
                }
            }
        }

        bb_arrangement
    }

    pub fn scan_intervals(&mut self) {}

    pub fn assign_registers(&mut self) {}

    pub fn gen_assembly(&mut self) {}
}

pub struct BasicBlkIntervals<'src> {
    bb: &'src mir::BasicBlk,
    bb_prev_vars: &'src HashSet<mir::VarId>,
    bb_next_vars: &'src HashSet<mir::VarId>,
}

impl<'src> BasicBlkIntervals<'src> {
    pub fn scan_intervals(&mut self) {}
    pub fn assign_registers(&mut self) {}
}
