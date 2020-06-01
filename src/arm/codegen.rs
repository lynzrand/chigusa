use super::{
    util::{CycleSolver, Interval},
    *,
};
use crate::mir;
use bimap::BiMap;
use indexmap::IndexMap;
use once_cell::sync::Lazy;
use std::{
    cmp::{max, min},
    collections::{HashMap, HashSet, VecDeque},
    fmt::Debug,
};
use vec1::{vec1, Vec1};

macro_rules! get_interval {
    ($self_:expr, $idx:expr) => {
        *$self_
            .reg_alloc
            .live_intervals
            .get(&$self_.get_collapsed_var($idx))
            .unwrap()
    };
}

macro_rules! pre_alloc_reg {
    ($self_:expr, $pos:expr, $entry:expr) => {
        match $self_.pre_alloc.entry($pos) {
            std::collections::hash_map::Entry::Occupied(mut x) => x.get_mut().push($entry),
            std::collections::hash_map::Entry::Vacant(x) => {
                x.insert(vec![$entry]);
            }
        }
    };
}

pub struct Codegen<'src> {
    src: &'src mir::MirPackage,
    dst: ArmPackage,
}

impl<'src> Codegen<'src> {
    pub fn new(src: &'src mir::MirPackage) -> Self {
        Codegen {
            src,
            dst: ArmPackage { functions: vec![] },
        }
    }
    pub fn gen(mut self) -> ArmPackage {
        for (id, f) in &self.src.func_table {
            if f.is_extern {
                continue;
            }
            let fc = FnCodegen::new(&self.src, f);
            let code = fc.gen();
            self.dst.functions.push(code);
            // log::debug!("{:#?}", code);
        }

        self.dst
    }
}

fn format_bb_label(func_name: &str, bb_id: usize) -> String {
    format!("{}$bb{}", func_name, bb_id)
}

fn format_function_end(func_name: &str) -> String {
    format!("{}$end", func_name,)
}

#[derive(Debug)]
pub struct FnCodegen<'src> {
    pkg: &'src mir::MirPackage,
    src: &'src mir::Func,
    bb_arrangement: Vec<mir::BBId>,
    bb_start_pos: IndexMap<usize, usize>,

    reg_alloc: RegStackAlloc<'src>,
    pre_alloc: HashMap<usize, Vec<PreAllocEntry>>,

    code: Vec<ArmCode>,
    static_values: HashMap<String, StaticData>,
}

impl<'src> FnCodegen<'src> {
    pub fn new(pkg: &'src mir::MirPackage, src: &'src mir::Func) -> Self {
        log::debug!("{:#?}", src);
        FnCodegen {
            pkg,
            src,
            bb_arrangement: Vec::new(),
            bb_start_pos: IndexMap::new(),
            // live_intervals: IndexMap::new(),
            // var_collapse: IndexMap::new(),
            reg_alloc: RegStackAlloc::new(src),
            pre_alloc: HashMap::new(),
            code: vec![],
            static_values: HashMap::new(),
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
                mir::JumpInst::Return(_) => {
                    // noop
                }
                mir::JumpInst::Unreachable | mir::JumpInst::Unknown => {
                    if !self.is_start() && !self.is_void() {
                        panic!("Reaches unreachable or uninitialized block!")
                    }
                }
            }
        }
    }

    fn is_start(&self) -> bool {
        self.src.name == "main"
    }

    fn is_void(&self) -> bool {
        !self.src.ty.get_fn_ret().unwrap().borrow().is_assignable()
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

        log::debug!("BB arrangement is {:?}", self.bb_arrangement);

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
                &mut self.reg_alloc.live_intervals,
                &mut self.reg_alloc.var_collapse,
            );

            bb_interval_scanner.scan_intervals();
        }
        log::debug!("live intervals: {:#?}", self.reg_alloc.live_intervals);
    }

    pub fn get_collapsed_var_varref(&mut self, var: &mir::VarRef) -> mir::VarRef {
        match var.0 {
            mir::VarTy::Global => *var,
            mir::VarTy::Local => {
                let res = self.get_collapsed_var(var.1);
                mir::VarRef(mir::VarTy::Local, res)
            }
        }
    }
    fn get_collapsed_var_optional(&self, var: usize) -> Option<usize> {
        if let Some(&v) = self.reg_alloc.var_collapse.get(&var) {
            let res = self.get_collapsed_var_optional(v);
            if let Some(res) = res {
                // 并查集行为
                Some(res)
            } else {
                Some(v)
            }
        } else {
            None
        }
    }

    fn get_collapsed_var(&self, var: usize) -> usize {
        let res = self.get_collapsed_var_optional(var).unwrap_or(var);
        res
    }

    fn set_param_and_ret_registers(&mut self) {
        let mut param_register_size = 0;
        for (&id, var) in &self.src.var_table {
            if var.kind == mir::VarKind::Param {
                // * we ARE iterating variables in the same way they are declared
                let var_reg_size = var.ty.register_count();
                if var.ty.require_double_registers() {
                    todo!("Support doubles")
                }
                if param_register_size + var_reg_size <= RESULT_REGISTERS.len() {
                    // Allocate register
                    assert!(var_reg_size == 1, "only int-s are supported");
                    self.reg_alloc.allocate_register(
                        id,
                        PARAM_REGISTERS
                            .get_index(param_register_size)
                            .cloned()
                            .unwrap(),
                        0,
                        get_interval!(self, id),
                        false,
                    );
                    param_register_size += 1;
                } else {
                    // spill param onto stack
                    self.reg_alloc
                        .alloc_param(id, (param_register_size - 4) * 4);
                }
            } else if var.kind == mir::VarKind::Ret {
                let var_reg_size = var.ty.register_count();
                if var.ty.require_double_registers() {
                    todo!("Support doubles")
                }

                assert!(var_reg_size == 1, "only int-s are supported");
                log::trace!("set return value {}", id);
                let interval = get_interval!(self, id);
                pre_alloc_reg!(
                    self,
                    interval.start(),
                    PreAllocEntry {
                        id: id,
                        reg: RESULT_REGISTERS.get_index(0).cloned().unwrap(),
                        interval,
                        volatile: false,
                    }
                );
            }
        }
    }

    fn alloc_fixed_vars(&mut self) {
        for bb in self.bb_arrangement.iter().cloned() {
            let &bb_offset = self.bb_start_pos.get(&bb).unwrap();
            let bb = self.src.bb.get(&bb).unwrap();

            for (pos, inst) in bb
                .inst
                .iter()
                .enumerate()
                .map(|(pos, inst)| (pos + bb_offset, inst))
            {
                match &inst.ins {
                    mir::Ins::Call(_, params) => {
                        let param_size = params.len();
                        for (idx, param) in params.iter().enumerate() {
                            if let mir::Value::Var(x) = param {
                                assert!(
                                    x.0 == mir::VarTy::Local,
                                    "Function param is not a local variable!"
                                );
                                let var_id = self.get_collapsed_var(x.1);
                                if idx < 4 {
                                    let interval = get_interval!(self, var_id);
                                    pre_alloc_reg!(
                                        self,
                                        interval.start(),
                                        PreAllocEntry {
                                            id: var_id,
                                            reg: *PARAM_REGISTERS.get_index(idx).unwrap(),
                                            interval,
                                            volatile: false,
                                        }
                                    );
                                } else {
                                    // Allocate to stack
                                    self.reg_alloc.alloc_caller(var_id, (param_size - idx) * 4);
                                }
                            } else {
                                panic!("Function param is not a variable!")
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    fn gen_body_assembly(&mut self) {
        for (&bb_id, &next_bb) in self.bb_arrangement.iter().zip(
            self.bb_arrangement
                .iter()
                .skip(1)
                .chain(std::iter::once(&usize::max_value())),
        ) {
            let &bb_offset = self.bb_start_pos.get(&bb_id).unwrap();
            let bb = self.src.bb.get(&bb_id).unwrap();

            log::debug!("Generating code for bb{}: {:#?}", bb_id, bb);

            // generate label
            self.code
                .push(ArmCode::_Label(format_bb_label(&self.src.name, bb_id)));

            for &_var in &bb.uses_var {
                // TODO: pre-allocate?
                // let _reg = self.reg_alloc.request_write_allocation(
                //     var,
                //     bb_offset,
                //     self.reg_alloc.live_intervals.get(&var).unwrap().clone(),
                // );
                // * We assume that variables are always assigned before first
                // * use, so that they are already allocated before loops
            }

            for (pos, inst) in bb
                .inst
                .iter()
                .enumerate()
                .map(|(pos, inst)| (pos + bb_offset, inst))
            {
                log::trace!("{} : {}", pos, inst);

                let mut inst_gen = InstructionGen {
                    alloc: &mut self.reg_alloc,
                    codes: &mut self.code,
                    pre_alloc: &mut self.pre_alloc,
                    static_values: &mut self.static_values,
                    func: &self.src,
                    pkg: &self.pkg,
                    pos,
                };

                inst_gen.gen_inst(inst);
            }

            match bb.end {
                mir::JumpInst::Jump(x) => {
                    if x != next_bb {
                        self.code.push(ArmCode::B(
                            Conditional::Al,
                            format_bb_label(&self.src.name, x),
                        ));
                    }
                }
                mir::JumpInst::Conditional(val, t, f) => {
                    let mut rewrite_cmp = None;
                    if self.code.len() >= 3 {
                        if let [.., ArmCode::Cmp(_, _), ArmCode::Mov(rx, ArmOperand::Imm(0)), ArmCode::CMov(cond, ry, ArmOperand::Imm(1))] =
                            &self.code[..]
                        {
                            if rx == ry {
                                rewrite_cmp = Some(*cond);
                            }
                        }
                    }

                    if let Some(cond) = rewrite_cmp {
                        // rewrite condition
                        self.code.pop();
                        self.code.pop();

                        if t == next_bb {
                            self.code
                                .push(ArmCode::B(cond.inv(), format_bb_label(&self.src.name, f)));
                        } else if f == next_bb {
                            self.code
                                .push(ArmCode::B(cond, format_bb_label(&self.src.name, t)));
                        } else {
                            self.code
                                .push(ArmCode::B(cond, format_bb_label(&self.src.name, t)));
                            self.code.push(ArmCode::B(
                                Conditional::Al,
                                format_bb_label(&self.src.name, f),
                            ));
                        }
                    } else {
                        let mut inst_gen = InstructionGen {
                            alloc: &mut self.reg_alloc,
                            codes: &mut self.code,
                            pre_alloc: &mut self.pre_alloc,
                            static_values: &mut self.static_values,
                            func: &self.src,
                            pkg: &self.pkg,
                            pos: bb_offset + bb.inst.len(),
                        };

                        let val_reg = inst_gen.gen_value_reg(val);
                        self.code.push(ArmCode::Cmp(val_reg, ArmOperand::Imm(0)));
                        self.code.push(ArmCode::B(
                            Conditional::Ne,
                            format_bb_label(&self.src.name, t),
                        ));
                        self.code.push(ArmCode::B(
                            Conditional::Al,
                            format_bb_label(&self.src.name, f),
                        ));
                    }
                }
                mir::JumpInst::Return(_x) => {
                    // _x is already at %r0
                    self.code.push(ArmCode::B(
                        Conditional::Al,
                        format_function_end(&self.src.name),
                    ));
                }
                mir::JumpInst::Unreachable => panic!("Reaches unreachable basic block!"),
                mir::JumpInst::Unknown => {
                    if self.is_start() {
                    } else if self.is_void() {
                        self.code.push(ArmCode::B(
                            Conditional::Al,
                            format_function_end(&self.src.name),
                        ));
                    } else {
                        panic!("Reaches uninitialized basic block!")
                    }
                }
            }
        }
    }

    pub fn gen_assembly(&mut self) {
        self.set_param_and_ret_registers();
        self.alloc_fixed_vars();
        self.gen_body_assembly();

        let self_param_count = self.src.ty.get_fn_params().unwrap().len();
        let mut save_reg: Vec<_> = self
            .reg_alloc
            .all_used_reg
            .iter()
            .filter(|reg| (reg.0 as usize) < std::cmp::min(4, self_param_count))
            .cloned()
            .collect();
        save_reg.append(&mut vec![FP_REGISTER, LINK_REGISTER]);
        save_reg.sort();

        let self_stack_size = self
            .reg_alloc
            .spilled
            .iter()
            .filter_map(|(_, &(_, pos))| {
                if pos.0 == StackBase::SP && pos.1 >= 0 {
                    Some(pos.1)
                } else {
                    None
                }
            })
            .max()
            .map(|x| x + 4)
            .unwrap_or(0) as usize;

        let mut final_code = Vec::new();
        final_code.push(ArmCode::Push(save_reg.clone()));
        final_code.push(ArmCode::Mov(FP_REGISTER, ArmOperand::Reg(SP_REGISTER)));
        final_code.push(ArmCode::Sub(NumericOperand {
            dest: SP_REGISTER,
            lhs: SP_REGISTER,
            rhs: ArmOperand::Imm(self_stack_size as i32),
        }));

        final_code.append(&mut self.code);

        if self.is_start() {
            final_code.push(ArmCode::Bl("__chigusa_main".into()));
        }

        final_code.push(ArmCode::_Label(format_function_end(&self.src.name)));
        final_code.push(ArmCode::Mov(SP_REGISTER, ArmOperand::Reg(FP_REGISTER)));
        save_reg.pop();
        save_reg.push(PC_REGISTER);
        final_code.push(ArmCode::Pop(save_reg));

        self.code = final_code;

        log::debug!("{:#?}", self.code);
    }

    pub fn gen(mut self) -> ArmFunction {
        self.scan_intervals();
        self.gen_assembly();

        ArmFunction {
            name: self.src.name.clone(),
            code: self.code,
            static_values: self.static_values,
        }
    }
}

struct BasicBlkIntervals<'src> {
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

        // Sort the intervals by their starting point
        self.intervals.sort_by(|_, v1, _, v2| v1.0.cmp(&v2.0))
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum StackBase {
    /// The variable position is relative to frame pointer, e.g. params
    FP,
    /// The variable position is relative to stack pointer,
    /// e.g. local variables, function params of to-be-called functions
    SP,
}

#[derive(Copy, Clone, Eq, PartialEq)]
struct StackLoc(pub StackBase, pub i32);

impl Debug for StackLoc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{:?}{:+}", self.0, self.1)
    }
}

impl Into<MemoryAccess> for StackLoc {
    fn into(self) -> MemoryAccess {
        let register = match self.0 {
            StackBase::FP => FP_REGISTER,
            StackBase::SP => SP_REGISTER,
        };
        MemoryAccess::Register(register, self.1 as isize)
    }
}

#[derive(Debug)]
struct PreAllocEntry {
    pub id: mir::VarId,
    pub reg: Reg,
    pub interval: Interval,
    pub volatile: bool,
}

/// This struct uses a simplified version of Second-chance binpacking register
/// allocation algorithm.
///
/// The SCB algorithm is described in <https://www.researchgate.net/publication/221302629>
#[derive(Debug)]
struct RegStackAlloc<'src> {
    src: &'src mir::Func,
    // === Register Allocation State ===
    pub assignment: IndexMap<mir::VarId, Vec1<(Interval, Reg)>>,
    pub active: BiMap<mir::VarId, Reg>,
    /// Tracking memory usage in stack. Assigning every variable to a separate
    /// stack location. Value is relative to normal %sp, function params uses
    /// negative values, permanent variables use positive values.
    pub spilled: IndexMap<mir::VarId, (Vec1<Interval>, StackLoc)>,
    // pub pre_allocated: HashSet<mir::VarId>,
    pub all_used_reg: HashSet<Reg>,

    pub live_intervals: IndexMap<usize, Interval>,
    pub var_collapse: IndexMap<usize, usize>,

    scratch_register_counter: usize,
    pub stack_alloc_counter: i32,

    // === Temporary data ===
    pub just_spilled: VecDeque<(mir::VarId, Reg, StackLoc)>,
    pub just_revived: VecDeque<(mir::VarId, Reg, StackLoc)>,
}

impl<'src> RegStackAlloc<'src> {
    pub fn new(src: &'src mir::Func) -> Self {
        RegStackAlloc {
            src,
            assignment: IndexMap::new(),
            active: BiMap::new(),
            spilled: IndexMap::new(),
            all_used_reg: HashSet::new(),
            // pre_allocated: HashSet::new(),
            live_intervals: IndexMap::new(),
            var_collapse: IndexMap::new(),
            scratch_register_counter: usize::max_value(),
            stack_alloc_counter: 0,
            just_spilled: VecDeque::new(),
            just_revived: VecDeque::new(),
        }
    }

    pub fn get_collapsed_var_varref(&mut self, var: &mir::VarRef) -> mir::VarRef {
        match var.0 {
            mir::VarTy::Global => *var,
            mir::VarTy::Local => {
                let res = self.get_collapsed_var(var.1);
                mir::VarRef(mir::VarTy::Local, res)
            }
        }
    }

    pub fn get_collapsed_var_optional(&self, var: usize) -> Option<usize> {
        if let Some(&v) = self.var_collapse.get(&var) {
            let res = self.get_collapsed_var_optional(v);
            if let Some(res) = res {
                Some(res)
            } else {
                Some(v)
            }
        } else {
            None
        }
    }

    pub fn get_collapsed_var(&self, var: usize) -> usize {
        let res = self.get_collapsed_var_optional(var).unwrap_or(var);
        res
    }

    pub fn force_allocate_register(
        &mut self,
        var_id: mir::VarId,
        reg: Reg,
        pos: usize,
        val_interval: Interval,
        volatile: bool,
    ) {
        log::trace!("force-allocating ${} to {:?} at {}", var_id, reg, pos);
        if self.active.get_by_right(&reg).is_some() {
            self.force_free_register(reg, pos);
        }
        self.allocate_register(var_id, reg, pos, val_interval, volatile);
    }

    pub fn allocate_register(
        &mut self,
        var_id: mir::VarId,
        reg: Reg,
        pos: usize,
        val_interval: Interval,
        volatile: bool,
    ) {
        log::trace!("allocating ${} to {:?} at {}", var_id, reg, pos);
        let entry = self.assignment.entry(var_id);
        match entry {
            indexmap::map::Entry::Occupied(mut e) => {
                // if a variable has an entry and needs to allocate again
                // then it must be spilled somewhere else
                let v = e.get_mut();
                if v.last().0.alive_for_writing(pos) && v.last().1 == reg {
                    // The allocated register is the same as the current register
                    return;
                }
                assert!(
                    v.iter()
                        .all(|(interval, r)| !interval.alive_for_writing(pos)),
                    "No duplicate allocations"
                );
                let (spilled, loc) = self.spilled.get_mut(&var_id).unwrap();
                if !volatile {
                    let new_interval = spilled.last_mut().split(pos);

                    // ! This is a revive outside Self::revive() function.
                    self.just_revived.push_back((var_id, reg, *loc));

                    v.push((new_interval, reg));
                } else {
                    // this is a transient read
                    v.push((Interval::point(pos), reg));
                }
            }
            indexmap::map::Entry::Vacant(e) => {
                let spilled = self.spilled.get_mut(&var_id);
                let interval = if let Some((intervals, loc)) = spilled {
                    // this variable is located in the stack from the beginning
                    // ! This is a revive outside Self::revive() function.
                    self.just_revived.push_back((var_id, reg, *loc));

                    intervals.last_mut().split(pos)
                } else {
                    val_interval
                };
                e.insert(vec1![(interval, reg)]);
            }
        };
        self.active.insert(var_id, reg);
    }

    fn spill_reg(&mut self, reg: Reg, pos: usize) {
        log::trace!("spilling {:?} at {}", reg, pos);
        let var_id = self.active.get_by_right(&reg);
        if let Some(&var_id) = var_id {
            self.spill_var(var_id, pos);
        }
    }

    fn spill_var(&mut self, var_id: mir::VarId, pos: usize) {
        log::trace!("spilling ${} at {}", var_id, pos);

        let entry = self.assignment.entry(var_id);
        match entry {
            indexmap::map::Entry::Occupied(mut entry) => {
                // Spill a variable from its last assignment
                let val = entry.get_mut();

                let new_interval = val.last_mut().0.split(pos);

                // allocate memory slot for variable
                let pos = self.stack_alloc_counter;
                self.stack_alloc_counter += 4;
                let stack_pos = StackLoc(StackBase::SP, pos);

                self.spilled
                    .entry(var_id)
                    .and_modify(|(intervals, _)| intervals.push(new_interval))
                    .or_insert_with(|| (vec1![new_interval], stack_pos));

                let result = self.active.remove_by_left(&var_id).unwrap();
                log::trace!("${} assignments: {:?}", var_id, val);
                self.just_spilled.push_back((result.0, result.1, stack_pos));
            }
            indexmap::map::Entry::Vacant(_) => panic!("The variable is not allocated!"),
        }
    }

    fn scan_and_deactivate_read(&mut self, pos: usize) {
        log::trace!(
            "Scan for active variables (read) @{}: {:?}",
            pos,
            self.active
        );
        let active_vars = self.active.left_values().cloned().collect::<Vec<_>>();
        for variable in active_vars {
            let is_active = self
                .live_intervals
                .get(&variable)
                .map_or(false, |interval| interval.alive_for_either(pos));
            if !is_active {
                self.active.remove_by_left(&variable);
                log::trace!(
                    "deactivating variable {}: {} vs {:?}",
                    variable,
                    pos,
                    self.live_intervals.get(&variable),
                );
            } else {
            }
        }
    }

    fn scan_and_deactivate_write(&mut self, pos: usize) {
        log::trace!(
            "Scan for active variables (write) @{}: {:?}",
            pos,
            self.active
        );
        let active_vars = self.active.left_values().cloned().collect::<Vec<_>>();
        for variable in active_vars {
            let is_active = self
                .live_intervals
                .get(&variable)
                .map_or(false, |interval| interval.alive_for_writing(pos));
            if !is_active {
                self.active.remove_by_left(&variable);
                log::trace!(
                    "deactivating variable {}: {} vs {:?}",
                    variable,
                    pos,
                    self.live_intervals.get(&variable),
                );
            } else {
            }
        }
    }

    fn is_volatile(&self, var_id: mir::VarId) -> bool {
        self.src.var_table.get(&var_id).unwrap().kind == mir::VarKind::FixedTemp
    }

    pub fn is_spilled(&self, var_id: mir::VarId, pos: usize) -> bool {
        self.spilled.get(&var_id).map_or(false, |intervals| {
            intervals
                .0
                .iter()
                .any(|interval| interval.alive_for_writing(pos))
        })
    }

    pub fn active_intersects(&self, allowed_regs: &HashSet<Reg>) -> HashSet<Reg> {
        self.active
            .right_values()
            .filter_map(|val| {
                if allowed_regs.contains(val) {
                    Some(*val)
                } else {
                    None
                }
            })
            .collect()
    }

    /// Choose one register to spill. longer-lived registers have a higher precedence.
    pub fn choose_spill_register(&self, allowed_regs: &IndexSet<Reg>) -> Option<Reg> {
        let mut regs: Vec<_> = (&self.active)
            .iter()
            // filter all registers that cannot be spilled
            .filter(|&(v, _r)| {
                !matches!(
                    self.src.var_table.get(v).unwrap().kind,
                    mir::VarKind::FixedTemp | mir::VarKind::Ret
                )
            })
            // filter out all allowed registers
            .filter(|&(_v, r)| allowed_regs.contains(r))
            .map(|(&v, &r)| (v, r))
            .collect();

        regs.sort_by_cached_key(|(v, _r)| {
            self.live_intervals.get(v).map(|int| int.len()).unwrap_or(0)
        });

        regs.last().map(|(_v, r)| r).cloned()
    }

    /// Find the register occupied by the current variable, or spill a register and
    /// allocate the current variable to satisfy the need. This method assumes
    /// that handled variables are already removed from active set.
    pub fn find_allocate_or_spill(
        &mut self,
        var_id: mir::VarId,
        allowed_regs: &IndexSet<Reg>,
        interval: Interval,
        pos: usize,
    ) -> Reg {
        if let Some(&reg) = self.active.get_by_left(&var_id) {
            reg
        } else {
            let mut avail_regs = allowed_regs
                .iter()
                // filter all registers that hasn't been occupied
                .filter(|reg| !self.active.contains_right(reg));

            // get the first register available
            if let Some(&reg) = avail_regs.next() {
                // There's an empty register
                self.allocate_register(var_id, reg, pos, interval, false);
                reg
            } else {
                // No empty registers, spill one from active.
                let spilled = self.choose_spill_register(allowed_regs);
                if let Some(reg) = spilled {
                    self.spill_reg(reg, pos);
                    self.allocate_register(var_id, reg, pos, interval, false);
                    reg
                } else {
                    panic!("No register to spill! This is an internal error");
                }
            }
        }
    }

    fn revive(&mut self, var_id: mir::VarId, pos: usize) -> (Interval, StackLoc) {
        log::trace!("reviving ${} at {}", var_id, pos);

        let (spill_intervals, loc) = self.spilled.get_mut(&var_id).expect(&format!(
            "The variable {} is not spilled at {}",
            var_id, pos
        ));

        let last_spill = spill_intervals.last_mut();
        assert!(
            last_spill.alive_for_reading(pos),
            "Reviving dead variable ${}: total {:?}, expected {:?}, got pos {}",
            var_id,
            self.live_intervals.get(&var_id),
            last_spill,
            pos
        );

        let new_interval = last_spill.split(pos);
        (new_interval, *loc)
    }

    /// Request to allocate a register for reading the variable, or return the
    /// register already allocated for it
    pub fn alloc_read(&mut self, var_id: mir::VarId, pos: usize) -> Reg {
        self.scan_and_deactivate_read(pos);

        if let Some(&x) = self.active.get_by_left(&var_id) {
            return x;
        }

        let last_allocation = self.assignment.get(&var_id);
        log::debug!("Allocate for reading: ${}@{}", var_id, pos);

        if last_allocation.is_some()
            && last_allocation
                .as_ref()
                .unwrap()
                .last()
                .0
                .alive_for_reading(pos)
        {
            panic!("Inconsistent variable live period: variable not in active set but has live interval {:?}",last_allocation);
            let reg = last_allocation.unwrap().last().1;
            log::debug!("Allocate for reading: ${}@{} -> {:?}", var_id, pos, reg);
            reg
        } else {
            // The value might be spilled
            let is_volatile = self.is_volatile(var_id);

            // Revived
            let (new_interval, stack_loc) = self.revive(var_id, pos);
            let reg = self.find_allocate_or_spill(var_id, &*VARIABLE_REGISTERS, new_interval, pos);
            self.just_revived.push_back((var_id, reg, stack_loc));

            self.allocate_register(var_id, reg, pos, new_interval, is_volatile);
            log::debug!("Allocated: ${}@{} for {:?}", var_id, pos, new_interval);

            reg
        }
    }

    // var_kind: mir::VarKind,
    /// Request to allocate a register, returns the register and whether needs
    /// to write back
    pub fn alloc_write(&mut self, var_id: mir::VarId, pos: usize) -> (Reg, bool) {
        self.scan_and_deactivate_write(pos);

        let volatile = self.is_volatile(var_id);

        if let Some(&x) = self.active.get_by_left(&var_id) {
            return (x, volatile);
        }

        let last_allocation = self.assignment.entry(var_id);

        log::debug!(
            "Allocate for writing: ${}@{}, alloc: {:?}",
            var_id,
            pos,
            &last_allocation
        );

        let reg = match last_allocation {
            indexmap::map::Entry::Occupied(e) => {
                let alloc = e.get();
                let last_allocation = alloc.last();
                if last_allocation.0.alive_for_writing(pos) {
                    last_allocation.1
                } else {
                    // variable is spilled
                    let (interval, stack_loc) = self.revive(var_id, pos);
                    let reg =
                        self.find_allocate_or_spill(var_id, &*VARIABLE_REGISTERS, interval, pos);

                    // Revived
                    self.just_revived.push_back((var_id, reg, stack_loc));

                    reg
                }
            }
            indexmap::map::Entry::Vacant(_v) => {
                // variable is not yet allocated
                let &interval = self.live_intervals.get(&var_id).unwrap();
                let reg = self.find_allocate_or_spill(
                    var_id,
                    &*SCRATCH_VARIABLE_ALLOWED_REGISTERS,
                    interval,
                    pos,
                );
                reg
            }
        };

        log::debug!("Current active: {:?}", self.active);

        (reg, volatile)
    }

    /// Request to allocate a register that is only alive inside current MIR
    /// instruction.
    ///
    /// The number of scratch registers must be less than the total number of
    /// registers; If no register could be allocated, the method panics.
    pub fn alloc_scratch(&mut self, pos: usize) -> Reg {
        self.scan_and_deactivate_read(pos);

        let var_id = self.scratch_register_counter;
        self.scratch_register_counter -= 1;

        self.find_allocate_or_spill(
            var_id,
            &SCRATCH_VARIABLE_ALLOWED_REGISTERS,
            Interval::point(pos),
            pos,
        )
    }

    ///
    pub fn request_allocate_memory(&mut self, var_id: mir::VarId, pos: usize) {
        self.spill_var(var_id, pos)
    }

    ///
    pub fn force_free_register(&mut self, reg: Reg, pos: usize) {
        self.scan_and_deactivate_write(pos);
        self.spill_reg(reg, pos)
    }

    /// Try to allocate the variable to register or memory without using it
    pub fn try_alloc_local(&mut self, var_id: mir::VarId, pos: usize) {
        self.scan_and_deactivate_read(pos);

        if let Some(_) = self.active.get_by_left(&var_id) {
            //  noop
        } else {
            let mut avail_regs = SCRATCH_VARIABLE_ALLOWED_REGISTERS
                .iter()
                // filter all registers that hasn't been occupied
                .filter(|reg| !self.active.contains_right(reg));
            let &val_interval = self.live_intervals.get(&var_id).unwrap();
            // get the first register available
            if let Some(&reg) = avail_regs.next() {
                // There's an empty register
                self.allocate_register(var_id, reg, pos, val_interval, false);
            } else {
                if self.spilled.get(&var_id).map_or(false, |intervals| {
                    intervals
                        .0
                        .iter()
                        .any(|interval| interval.alive_for_writing(pos))
                }) {
                    // noop
                } else {
                    // self.alloc_local_stack(var_id);
                    let pos = self.stack_alloc_counter;
                    self.stack_alloc_counter += 4;
                    self.spilled
                        .insert(var_id, (vec1![val_interval], StackLoc(StackBase::SP, pos)));
                }
            }
        }
    }

    /// Get or set the allocation of a local variable, position not defined
    pub fn get_stack_pos(&mut self, var_id: mir::VarId) -> Option<StackLoc> {
        match self.spilled.entry(var_id) {
            indexmap::map::Entry::Occupied(entry) => Some(entry.get().1),
            indexmap::map::Entry::Vacant(entry) => None,
        }
    }

    /// Allocate a variable as this function's param, positioned at `%fp - Offset`
    pub fn alloc_param(&mut self, var_id: mir::VarId, offset: usize) -> StackLoc {
        match self.spilled.entry(var_id) {
            indexmap::map::Entry::Occupied(_entry) => panic!(
                "Param entries must only be allocated once, already allocated at {:?}",
                _entry
            ),
            indexmap::map::Entry::Vacant(entry) => {
                let &param_interval = self.live_intervals.get(&var_id).unwrap();
                let pos = offset as i32;
                entry.insert((vec1![param_interval], StackLoc(StackBase::FP, pos)));
                StackLoc(StackBase::FP, pos)
            }
        }
    }

    /// Allocate a variable as the param of a function to be called, positioned
    /// at `%sp + offset` (relative to current `%sp`, the real `%sp` will
    /// be increased)
    pub fn alloc_caller(&mut self, var_id: mir::VarId, offset: usize) -> StackLoc {
        match self.spilled.entry(var_id) {
            indexmap::map::Entry::Occupied(_entry) => panic!(
                "Param entries must only be allocated once, already allocated at {:?}",
                _entry
            ),
            indexmap::map::Entry::Vacant(entry) => {
                let &param_interval = self.live_intervals.get(&var_id).unwrap();
                let pos = -(offset as i32);
                entry.insert((vec1![param_interval], StackLoc(StackBase::SP, pos)));
                StackLoc(StackBase::SP, pos)
            }
        }
    }
}

struct InstructionGen<'src, 'b> {
    alloc: &'src mut RegStackAlloc<'b>,
    codes: &'src mut Vec<ArmCode>,
    pre_alloc: &'src mut HashMap<usize, Vec<PreAllocEntry>>,
    static_values: &'src mut HashMap<String, StaticData>,
    func: &'src mir::Func,
    pkg: &'src mir::MirPackage,
    pos: usize,
}

impl<'src, 'b> InstructionGen<'src, 'b> {
    fn get_ty(&self, var: mir::VarId) -> Option<&mir::Ty> {
        self.func.var_table.get(&var).map(|var| &var.ty)
    }

    fn get_global_ty(&self, var: usize) -> Option<&mir::Ty> {
        self.pkg.global_var_table.get(&var).map(|var| &var.ty)
    }

    fn val_ty(&self, val: &mir::Value) -> Option<mir::Ty> {
        match val {
            mir::Value::IntImm(_) => Some(mir::Ty::int()),
            mir::Value::FloatImm(_) => Some(mir::Ty::double()),
            mir::Value::Var(v) => match v.0 {
                mir::VarTy::Local => self.get_ty(v.1),
                mir::VarTy::Global => self.get_global_ty(v.1),
            }
            .map(|ty| ty.clone()),
            // TODO: Support register values?
            // mir::Value::Reg(_) => None,
            mir::Value::Void => Some(mir::Ty::Void),
        }
    }

    fn get_dest(&mut self, dest: mir::VarRef) -> (Reg, bool) {
        if let Some(entries) = self.pre_alloc.get_mut(&self.pos) {
            for entry in entries.drain(..) {
                self.alloc.force_allocate_register(
                    entry.id,
                    entry.reg,
                    self.pos,
                    entry.interval,
                    entry.volatile,
                );
            }
        }
        match dest.0 {
            mir::VarTy::Global => {
                let reg = self.alloc.alloc_scratch(self.pos);
                todo!("Global variable");
                (reg, true)
            }
            mir::VarTy::Local => {
                let var_id = self.alloc.get_collapsed_var(dest.1);
                let var_ty = self.get_ty(var_id).unwrap();
                if var_ty.is_assignable() {
                    self.alloc.alloc_write(var_id, self.pos)
                } else {
                    (Reg(255), false)
                }
            }
        }
    }

    fn write_back(&mut self, volatile: bool, reg: Reg, dest: mir::VarRef) {
        if !volatile {
            return;
        }
        log::trace!("Write back variable {:?} from {:?}", dest, reg);

        // Write the volatile value back
        if dest.0 == mir::VarTy::Local {
            let mem = self.alloc.get_stack_pos(dest.1);
            if let Some(mem) = mem {
                self.codes.push(ArmCode::StR(reg, mem.into()));
            }
        } else {
            // let mem = self
            todo!("Support global values")
        }
    }

    pub fn gen_inst(&mut self, inst: &mir::MirCode) {
        // if matches!(inst.ins, mir::Ins::Phi(_)) {
        //     return;
        // }
        let tgt_val = inst.tgt;

        match &inst.ins {
            mir::Ins::TyCon(src_val) => self.gen_ty_con(tgt_val, tgt_val, *src_val),
            mir::Ins::Asn(val) => self.gen_assign(tgt_val, *val),
            mir::Ins::Bin(op, l, r) => self.gen_binary(tgt_val, *op, *l, *r),
            mir::Ins::Una(op, v) => self.gen_unary(tgt_val, *op, *v),
            mir::Ins::Call(f, params) => self.gen_call(tgt_val, *f, params),
            mir::Ins::RestRead(..) => panic!("Not implemented"),
            mir::Ins::Phi(..) => {
                assert!(tgt_val.0 == mir::VarTy::Local);
                self.alloc
                    .alloc_read(self.alloc.get_collapsed_var(tgt_val.1), self.pos);
            }
        }
        self.spill_and_revive();
    }

    fn gen_ty_con(&mut self, dest: mir::VarRef, dest_val: mir::VarRef, src_val: mir::Value) {
        todo!("Type conversion")
    }

    /// Assign immediate to a scratch register
    fn scratch_int_imm(&mut self, num: i32) -> Reg {
        let label_name = format!("VAL__{}${}", &self.func.name, self.pos);
        self.static_values
            .insert(label_name.clone(), StaticData::Word(vec![num as u32]));
        let reg = self.alloc.alloc_scratch(self.pos);

        self.spill_and_revive();

        self.codes
            .push(ArmCode::LdR(reg, MemoryAccess::Label(label_name)));
        reg
    }

    fn scratch_int_imm_or_direct(&mut self, num: i32) -> ArmOperand {
        if num > (1 << 10 - 1) || num < (-1 << 10) {
            ArmOperand::Reg(self.scratch_int_imm(num))
        } else {
            ArmOperand::Imm(num)
        }
    }

    fn scratch_global_read(&mut self, var_id: mir::VarId) -> Reg {
        todo!("Read global value")
    }

    fn gen_value_reg(&mut self, val: mir::Value) -> Reg {
        let reg = match val {
            mir::Value::IntImm(i) => self.scratch_int_imm(i),
            mir::Value::FloatImm(_) => todo!("Support floats"),
            mir::Value::Var(v) => match v.0 {
                mir::VarTy::Global => self.scratch_global_read(v.1),
                mir::VarTy::Local => self
                    .alloc
                    .alloc_read(self.alloc.get_collapsed_var(v.1), self.pos),
            },
            mir::Value::Void => panic!("Requesting register for void value"),
        };
        self.spill_and_revive();
        reg
    }

    fn gen_value_operand(&mut self, val: mir::Value) -> ArmOperand {
        let operand = match val {
            mir::Value::IntImm(i) => self.scratch_int_imm_or_direct(i),
            mir::Value::FloatImm(_) => todo!("Support floats"),
            mir::Value::Var(v) => match v.0 {
                mir::VarTy::Global => ArmOperand::Reg(self.scratch_global_read(v.1)),
                mir::VarTy::Local => ArmOperand::Reg(
                    self.alloc
                        .alloc_read(self.alloc.get_collapsed_var(v.1), self.pos),
                ),
            },
            mir::Value::Void => panic!("Requesting register for void value"),
        };
        self.spill_and_revive();
        operand
    }

    fn spill_and_revive(&mut self) {
        for (_id, reg, loc) in self.alloc.just_spilled.drain(..) {
            self.codes.push(ArmCode::StR(reg, loc.into()))
        }
        for (_id, reg, loc) in self.alloc.just_revived.drain(..) {
            self.codes.push(ArmCode::LdR(reg, loc.into()))
        }
    }

    fn gen_assign(&mut self, dest_val: mir::VarRef, src_val: mir::Value) {
        match src_val {
            mir::Value::Void => {
                return;
            }
            src_val @ _ => {
                let operand = self.gen_value_operand(src_val);
                let (dest, volatile) = self.get_dest(dest_val);
                if let ArmOperand::Reg(r) = operand {
                    if r == dest {
                        return;
                    }
                }
                self.codes.push(ArmCode::Mov(dest, operand));
                self.write_back(volatile, dest, dest_val);
            }
        }
    }

    fn gen_cmp(&mut self, dest: Reg, op: mir::BinOp, lhs: Reg, rhs: ArmOperand) {
        self.codes.push(ArmCode::Cmp(lhs, rhs));
        self.codes.push(ArmCode::Mov(dest, ArmOperand::Imm(0)));
        self.codes.push(ArmCode::CMov(
            match op {
                mir::BinOp::Lt => Conditional::Lt,
                mir::BinOp::Gt => Conditional::Gt,
                mir::BinOp::Eq => Conditional::Eq,
                mir::BinOp::Neq => Conditional::Ne,
                mir::BinOp::Lte => Conditional::Le,
                mir::BinOp::Gte => Conditional::Ge,
                _ => panic!("Not a comparasion"),
            },
            dest,
            ArmOperand::Imm(1),
        ));
    }

    fn gen_binary(
        &mut self,
        dest_val: mir::VarRef,
        op: mir::BinOp,
        mut lhs: mir::Value,
        mut rhs: mir::Value,
    ) {
        let val_ty = self.val_ty(&lhs).expect("No type information");
        if val_ty.is_int() {
            if op == mir::BinOp::Sub {
                // Reverse subtraction stuff
                let mut rev_sub = false;
                if lhs.is_int() && !rhs.is_int() {
                    std::mem::swap(&mut lhs, &mut rhs);
                    rev_sub = true;
                }
                let lhs = self.gen_value_reg(lhs);
                let rhs = self.gen_value_operand(rhs);
                let (dest, volatile) = self.get_dest(dest_val);
                if !rev_sub {
                    self.codes
                        .push(ArmCode::Sub(NumericOperand { dest, lhs, rhs }))
                } else {
                    self.codes
                        .push(ArmCode::Rsb(NumericOperand { dest, lhs, rhs }))
                }
                self.write_back(volatile, dest, dest_val);
            } else {
                let lhs = self.gen_value_reg(lhs);
                let rhs = if matches!(&op, mir::BinOp::Mul | mir::BinOp::Div) {
                    ArmOperand::Reg(self.gen_value_reg(rhs))
                } else {
                    self.gen_value_operand(rhs)
                };
                let (dest, volatile) = self.get_dest(dest_val);

                let arm_code = match op {
                    mir::BinOp::Add => ArmCode::Add(NumericOperand { dest, lhs, rhs }),
                    // mir::BinOp::Sub => {ArmCode::Sub(NumericOperand{dest,lhs,rhs})}
                    mir::BinOp::Mul => ArmCode::Mul(NumericOperand { dest, lhs, rhs }),
                    mir::BinOp::Div => ArmCode::SDiv(NumericOperand { dest, lhs, rhs }),
                    mir::BinOp::And => ArmCode::And(NumericOperand { dest, lhs, rhs }),
                    mir::BinOp::Or => ArmCode::Orr(NumericOperand { dest, lhs, rhs }),
                    mir::BinOp::Lt
                    | mir::BinOp::Gt
                    | mir::BinOp::Eq
                    | mir::BinOp::Neq
                    | mir::BinOp::Lte
                    | mir::BinOp::Gte => {
                        self.gen_cmp(dest, op, lhs, rhs);
                        self.write_back(volatile, dest, dest_val);
                        return;
                    }
                    _ => unreachable!(),
                };
                self.codes.push(arm_code);
                self.write_back(volatile, dest, dest_val);
            }
        } else {
            unimplemented!("Binary operation of other types ")
        }
    }

    fn gen_unary(&mut self, dest_val: mir::VarRef, op: mir::UnaOp, val: mir::Value) {
        let val_ty = self.val_ty(&val).expect("No type information");
        if val_ty.is_int() {
            match op {
                mir::UnaOp::Pos => {
                    let operand = self.gen_value_operand(val);
                    let (dest, volatile) = self.get_dest(dest_val);
                    if let ArmOperand::Reg(r) = operand {
                        if r == dest {
                            return;
                        }
                    }
                    self.codes.push(ArmCode::Mov(dest, operand));
                    self.write_back(volatile, dest, dest_val);
                }
                mir::UnaOp::Neg => {
                    let operand = self.gen_value_reg(val);
                    let (dest, volatile) = self.get_dest(dest_val);
                    self.codes.push(ArmCode::Rsb(NumericOperand {
                        dest,
                        lhs: operand,
                        rhs: ArmOperand::Imm(0),
                    }));
                    self.write_back(volatile, dest, dest_val);
                }
            };
        } else {
            unimplemented!("Binary operation of other types ")
        }
    }

    fn gen_call(&mut self, dest_val: mir::VarRef, func: mir::VarRef, params: &Vec<mir::Value>) {
        // parameters are already set
        assert!(func.0 == mir::VarTy::Global);
        assert!(params
            .iter()
            .all(|param| matches!(param, mir::Value::Var(mir::VarRef(mir::VarTy::Local, _)))));

        let func_info = self.pkg.func_table.get(&func.1).unwrap();
        let func_name = func_info.name.clone();
        let has_return_value = func_info.ty.get_fn_ret().unwrap().borrow().is_assignable();

        let param_count = func_info.ty.get_fn_params().unwrap().len();
        if param_count < 4 {
            for i in (param_count)..4 {
                self.alloc.force_free_register(Reg((i) as u8), self.pos);
            }
        } else {
            // ramp up stack size; we have used the red zone to store params
            self.codes.push(ArmCode::Add(NumericOperand {
                dest: SP_REGISTER,
                lhs: SP_REGISTER,
                rhs: ArmOperand::Imm((param_count - 4) as i32),
            }));
        }
        self.spill_and_revive();

        if has_return_value {
            self.alloc.force_free_register(Reg(0), self.pos);
        }

        self.codes.push(ArmCode::Bl(func_name));

        if param_count > 4 {
            // crush down stack size
            self.codes.push(ArmCode::Add(NumericOperand {
                dest: SP_REGISTER,
                lhs: SP_REGISTER,
                rhs: ArmOperand::Imm(-((param_count - 4) as i32)),
            }));
        }
        let (dest, volatile) = self.get_dest(dest_val);

        if has_return_value && dest != Reg(0) {
            self.codes.push(ArmCode::Mov(dest, ArmOperand::Reg(Reg(0))));
        }
        self.write_back(volatile, dest, dest_val);
    }

    fn gen_rest_read(&mut self, dest: Reg, dest_val: mir::VarRef) {
        todo!("Variadic function is not supported")
    }
}

fn val_both_int_immediate(lhs: &mir::Value, rhs: &mir::Value) -> Option<(i32, i32)> {
    if let mir::Value::IntImm(l) = lhs {
        if let mir::Value::IntImm(r) = rhs {
            return Some((*l, *r));
        }
    }
    None
}
