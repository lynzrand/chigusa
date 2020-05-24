use crate::mir;
use indexmap::IndexMap;
use std::{
    cmp::{max, min},
    collections::{HashMap, HashSet, VecDeque},
};
pub struct Codegen<'src> {
    src: &'src mir::MirPackage,
}

impl<'src> Codegen<'src> {}

pub struct FnCodegen<'src> {
    func: &'src mir::Func,
    bb_arrangement: Vec<mir::BBId>,
    bb_start_pos: HashMap<usize, usize>,
    live_intervals: IndexMap<usize, Interval>,
    var_collapse: HashMap<usize, usize>,
}

impl<'src> FnCodegen<'src> {
    /// Generate a basic block arrangement that is good enough for a structured
    /// program. _We don't have `goto`-s anyway!_
    fn arrange_basic_blocks(&mut self) {
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

        let mut bfs_q = VecDeque::new();
        bfs_q.push_back(0);

        while !bfs_q.is_empty() {
            let item = bfs_q.pop_front().unwrap();
            let entry = input_count.get_mut(&item).unwrap();
            *entry -= 1;
            if *entry > cycle_count.get(&item).cloned().unwrap_or(0) {
                continue;
            }

            self.bb_arrangement.push(*entry);

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
    }

    fn calc_bb_starting_points(&mut self) {
        let mut acc = 0;
        for id in self.bb_arrangement.iter().cloned() {
            self.bb_start_pos.insert(id, acc);
            acc += self.func.bb.get(&id).unwrap().inst.len() + 1;
        }
    }

    pub fn scan_intervals(&mut self) {
        self.arrange_basic_blocks();
        self.calc_bb_starting_points();
    }

    pub fn assign_registers(&mut self) {}

    pub fn gen_assembly(&mut self) {}
}

pub struct BasicBlkIntervals<'src> {
    offset: usize,
    bb: &'src mir::BasicBlk,
    bb_prev_vars: &'src HashSet<mir::VarId>,
    bb_next_vars: &'src HashSet<mir::VarId>,
    intervals: &'src mut IndexMap<usize, Interval>,
    var_collapse: &'src mut HashMap<usize, usize>,
}

impl<'src> BasicBlkIntervals<'src> {
    fn get_collapsed_var_optional(&mut self, var: usize) -> Option<usize> {
        if let Some(&v) = self.var_collapse.get(&var) {
            let res = self.get_collapsed_var_optional(v);
            if let Some(res) = res {
                // 并查集行为
                self.var_collapse.insert(var, res);
            }
            res
        } else {
            None
        }
    }

    fn get_collapsed_var(&mut self, var: usize) -> usize {
        self.get_collapsed_var_optional(var).unwrap_or(var)
    }

    fn collapse_var<I>(&mut self, var: usize, targets: I)
    where
        I: Iterator<Item = usize>,
    {
        let var = self.get_collapsed_var(var);
        for target in targets {
            assert!(target >= var);
            if target == var {
                continue;
            }
            // if target has already collapsed into another var, also collapse that
            let target = self.get_collapsed_var(target);
            let res = self.var_collapse.insert(target, var);
            assert!(res.is_none());
        }
    }

    fn collapse_intervals<I>(&mut self, vars: I)
    where
        I: Iterator<Item = usize>,
    {
        let mut v = vars.collect::<Vec<_>>();
        v.sort_unstable();
        for &var in &v {}
        // self.collapse_var(var, targets)
    }

    fn interval_start(&mut self, var: usize, pos: usize) {
        self.intervals
            .entry(var)
            .and_modify(|entry| entry.update_starting_pos(pos))
            .or_insert_with(|| Interval::point(pos));
    }

    fn interval_end(&mut self, var: usize, pos: usize) {
        self.intervals
            .entry(var)
            .and_modify(|entry| entry.update_ending_pos(pos))
            .or_insert_with(|| Interval::point(pos));
    }

    fn var(&mut self, val: &mir::VarRef, pos: usize) {
        match &val.0 {
            mir::VarTy::Global => todo!(),
            mir::VarTy::Local => self.interval_start(val.1, pos),
        }
    }
    fn value_interval_end(&mut self, val: &mir::Value, pos: usize) {
        match val {
            mir::Value::Var(v) => match &v.0 {
                mir::VarTy::Global => todo!(),
                mir::VarTy::Local => self.interval_end(v.1, pos),
            },
            _ => {}
        }
    }

    pub fn scan_intervals(&mut self) {
        let self_offset = self.offset;

        for var in self.bb_prev_vars.iter().cloned() {
            self.interval_start(var, self_offset);
        }

        for (pos, inst) in self
            .bb
            .inst
            .iter()
            .enumerate()
            .map(|(idx, val)| (idx + self_offset, val))
        {
            match &inst.ins {
                mir::Ins::TyCon(val) => self.value_interval_end(val, pos),
                mir::Ins::Asn(val) => self.value_interval_end(val, pos),
                mir::Ins::Bin(_, l, r) => {
                    self.value_interval_end(l, pos);
                    self.value_interval_end(r, pos)
                }
                mir::Ins::Una(_, val) => self.value_interval_end(val, pos),
                mir::Ins::Call(_, params) => {
                    for val in params {
                        self.value_interval_end(val, pos)
                    }
                }
                mir::Ins::Phi(vals) => todo!("Collapse intervals"),
                mir::Ins::RestRead(_) => todo!("Unsupported"),
            }
        }

        let self_end = self_offset + self.bb.inst.len() + 1;
        for var in self.bb_next_vars.iter().cloned() {
            self.interval_end(var, self_end);
        }
    }
}

/// Represents an half-open half-close interval over MIR instructions.
///
/// `self.0` represents the place where the variable is first assigned,
/// and `self.1` represents the place where the variable is last used.
struct Interval(pub usize, pub usize);

impl Interval {
    pub fn point(pos: usize) -> Self {
        Interval(pos, pos)
    }

    pub fn update_ending_pos(&mut self, pos: usize) {
        self.1 = max(pos, self.1);
    }

    pub fn update_starting_pos(&mut self, pos: usize) {
        self.0 = min(self.0, pos);
    }

    pub fn start(&self) -> usize {
        self.0
    }

    pub fn end(&self) -> usize {
        self.1
    }

    pub fn union(x: Self, y: Self) {}
}
