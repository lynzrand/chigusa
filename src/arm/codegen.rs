use crate::mir;
use indexmap::IndexMap;
use std::{
    cmp::{max, min},
    collections::{HashMap, HashSet, VecDeque},
};

pub struct Codegen<'src> {
    src: &'src mir::MirPackage,
}

impl<'src> Codegen<'src> {
    pub fn new(src: &'src mir::MirPackage) -> Self {
        Codegen { src }
    }
    pub fn gen(&mut self) {
        for (id, f) in &self.src.func_table {
            let mut fc = FnCodegen::new(f);
            fc.gen();
        }
    }
}

#[derive(Debug)]
pub struct FnCodegen<'src> {
    src: &'src mir::Func,
    bb_arrangement: Vec<mir::BBId>,
    bb_start_pos: IndexMap<usize, usize>,
    live_intervals: IndexMap<usize, Interval>,
    var_collapse: IndexMap<usize, usize>,
}

impl<'src> FnCodegen<'src> {
    pub fn new(src: &'src mir::Func) -> Self {
        FnCodegen {
            src,
            bb_arrangement: Vec::new(),
            bb_start_pos: IndexMap::new(),
            live_intervals: IndexMap::new(),
            var_collapse: IndexMap::new(),
        }
    }

    /// Generate a basic block arrangement that is good enough for a structured
    /// program. _We don't have `goto`-s anyway!_
    fn arrange_basic_blocks(&mut self) {
        let mut cycle_solver = super::util::CycleSolver::new(&self.src.bb);
        cycle_solver.solve();

        let mut input_count = HashMap::<usize, isize>::new();
        let cycle_count = cycle_solver.counter;

        // starting block
        input_count.insert(0, 1);
        for (id, blk) in &self.src.bb {
            for input_blk in &blk.jump_in {
                input_count
                    .entry(*id)
                    .and_modify(|count| *count = *count + 1)
                    .or_insert(1);
            }
        }

        let mut bfs_q = VecDeque::new();
        bfs_q.push_back(0);
        let mut vis = HashSet::new();

        while !bfs_q.is_empty() {
            let bb_id = bfs_q.pop_front().unwrap();
            let count = input_count.get_mut(&bb_id).unwrap();

            *count -= 1;
            if *count > cycle_count.get(&bb_id).cloned().unwrap_or(0) {
                continue;
            }
            if vis.contains(&bb_id) {
                continue;
            } else {
                vis.insert(bb_id);
            }

            self.bb_arrangement.push(bb_id);

            let blk = self.src.bb.get(&bb_id).unwrap();
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
            acc += self.src.bb.get(&id).unwrap().inst.len() + 1;
        }
    }

    pub fn scan_intervals(&mut self) {
        self.arrange_basic_blocks();
        self.calc_bb_starting_points();
        for bb_id in self.bb_arrangement.iter().cloned() {
            let bb = self.src.bb.get(&bb_id).unwrap();
            let offset = *self.bb_start_pos.get(&bb_id).unwrap();

            let mut bb_next_vars = HashSet::new();
            for next_id in &bb.end.next_ids()[..] {
                bb_next_vars.extend(self.src.bb.get(next_id).unwrap().uses_var.iter().cloned());
            }

            let mut bb_interval_scanner = BasicBlkIntervals::new(
                offset,
                bb,
                &bb_next_vars,
                &mut self.live_intervals,
                &mut self.var_collapse,
            );

            bb_interval_scanner.scan_intervals();
        }
        log::debug!("{:#?}", self);
    }

    pub fn assign_registers(&mut self) {}

    pub fn gen_assembly(&mut self) {}

    pub fn gen(&mut self) {
        self.scan_intervals();
    }
}

pub struct BasicBlkIntervals<'src> {
    offset: usize,
    bb: &'src mir::BasicBlk,
    // bb_prev_vars: &'src HashSet<mir::VarId>,
    bb_next_vars: &'src HashSet<mir::VarId>,
    intervals: &'src mut IndexMap<usize, Interval>,
    var_collapse: &'src mut IndexMap<usize, usize>,
}

impl<'src> BasicBlkIntervals<'src> {
    pub(super) fn new(
        offset: usize,
        bb: &'src mir::BasicBlk,
        // bb_prev_vars: &'src HashSet<mir::VarId>,
        bb_next_vars: &'src HashSet<mir::VarId>,
        intervals: &'src mut IndexMap<usize, Interval>,
        var_collapse: &'src mut IndexMap<usize, usize>,
    ) -> Self {
        BasicBlkIntervals {
            offset,
            bb,
            // bb_prev_vars,
            bb_next_vars,
            intervals,
            var_collapse,
        }
    }

    fn get_collapsed_var_optional(&mut self, var: usize) -> Option<usize> {
        if let Some(&v) = self.var_collapse.get(&var) {
            let res = self.get_collapsed_var_optional(v);
            if let Some(res) = res {
                // 并查集行为
                self.var_collapse.insert(var, res);
                Some(res)
            } else {
                Some(v)
            }
        } else {
            None
        }
    }

    fn get_collapsed_var(&mut self, var: usize) -> usize {
        let res = self.get_collapsed_var_optional(var).unwrap_or(var);
        res
    }

    /// Collapse variable as aliases of a single variable. The target variable
    /// id **must** be the **smallest** of them all.
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

    fn collapse_intervals<I>(&mut self, vars: I, default_pos: usize)
    where
        I: Iterator<Item = usize>,
    {
        let mut v = vars.collect::<Vec<_>>();
        v.sort();

        let collapse_tgt = self.get_collapsed_var(v[0]);
        let orig_interval = self
            .intervals
            .entry(collapse_tgt)
            .or_insert_with(|| Interval::point(default_pos))
            .clone();

        let new_interval = v.iter().skip(1).fold(orig_interval, |interval, next_k| {
            let var_interval = self
                .intervals
                .remove(next_k)
                .unwrap_or_else(|| Interval::point(default_pos));
            Interval::union(interval, var_interval)
        });

        self.intervals.insert(collapse_tgt, new_interval);

        self.collapse_var(collapse_tgt, v.iter().skip(1).cloned());
    }

    fn interval_start(&mut self, var: usize, pos: usize) {
        let var = self.get_collapsed_var(var);
        self.intervals
            .entry(var)
            .and_modify(|entry| entry.update_starting_pos(pos))
            .or_insert_with(|| Interval::point(pos));
    }

    fn interval_end(&mut self, var: usize, pos: usize) {
        let var = self.get_collapsed_var(var);
        self.intervals
            .entry(var)
            .and_modify(|entry| entry.update_ending_pos(pos))
            .or_insert_with(|| Interval::point(pos));
    }

    fn var_interval_start(&mut self, val: &mir::VarRef, pos: usize) {
        match &val.0 {
            mir::VarTy::Global => {
                // Global value is directly dereferenced into variable
            }
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

        for var in self.bb.uses_var.iter().cloned() {
            self.interval_start(var, self_offset);
        }

        for (pos, inst) in self
            .bb
            .inst
            .iter()
            .enumerate()
            .map(|(idx, inst)| (idx + self_offset, inst))
        {
            self.var_interval_start(&inst.tgt, pos);
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
                mir::Ins::Phi(vals) => self.collapse_intervals(
                    std::iter::once(inst.tgt.get_local_id())
                        .chain(vals.iter().map(|k| k.1.get_local_id()))
                        .filter(Option::is_some)
                        .map(|x| x.unwrap()),
                    pos,
                ),
                mir::Ins::RestRead(_) => todo!("Unsupported"),
            }
        }

        let self_end = self_offset + self.bb.inst.len();
        match &self.bb.end {
            mir::JumpInst::Conditional(v, ..) => self.value_interval_end(v, self_end),
            mir::JumpInst::Return(v) => {
                if let Some(v) = v {
                    self.value_interval_end(v, self_end)
                }
            }
            _ => {}
        };

        for var in self.bb_next_vars.iter().cloned() {
            self.interval_end(var, self_end + 1);
        }
    }
}

/// Represents an half-open half-close interval over MIR instructions.
///
/// `self.0` represents the place where the variable is first assigned,
/// and `self.1` represents the place where the variable is last used.
#[derive(Clone, Copy)]
pub(super) struct Interval(pub usize, pub usize);

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

    pub fn union(mut x: Self, y: Self) -> Self {
        x.update_ending_pos(y.1);
        x.update_starting_pos(y.0);
        x
    }
}

impl std::fmt::Debug for Interval {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}, {})", self.0, self.1)
    }
}
