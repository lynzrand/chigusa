use crate::{arm, mir::*};
use indexmap::IndexMap;
use std::{
    cmp::{max, min},
    collections::{HashMap, HashSet},
};
/// Represents an half-open half-close interval over MIR instructions.
///
/// `self.0` represents the place where the variable is first assigned,
/// and `self.1` represents the place where the variable is last used. The
/// variable is valid for reading inside `(self.0, self.1]` and valid for writing
/// inside `[self.0, self.1)`.
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

    pub fn is_inside_write(&self, pos: usize) -> bool {
        pos >= self.0 && pos < self.1
    }
    pub fn alive_for_reading(&self, pos: usize) -> bool {
        pos > self.0 && pos <= self.1
    }

    pub fn union(mut x: Self, y: Self) -> Self {
        x.update_ending_pos(y.1);
        x.update_starting_pos(y.0);
        x
    }

    pub fn split(&mut self, pos: usize) -> Self {
        assert!(pos >= self.0 && pos <= self.1);
        let old_end = self.1;
        self.1 = pos;
        Interval(pos, old_end)
    }

    pub fn intersects_with(&self, other: &Interval) -> bool {
        self.0 < other.1 || self.1 > other.0
    }

    pub fn len(&self) -> usize {
        self.1 - self.0
    }
}

impl std::fmt::Debug for Interval {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}, {})", self.0, self.1)
    }
}

/// Solve the cycles inside basic blocks
pub struct CycleSolver<'src> {
    bb: &'src IndexMap<usize, BasicBlk>,
    pub counter: HashMap<usize, isize>,
    path: HashSet<usize>,
    pub visited: HashSet<usize>,
}

impl<'src> CycleSolver<'src> {
    pub fn new(bb: &'src IndexMap<usize, BasicBlk>) -> Self {
        CycleSolver {
            bb,
            counter: HashMap::new(),
            path: HashSet::new(),
            visited: HashSet::new(),
        }
    }

    pub fn solve(&mut self) {
        self.dfs(0)
    }

    fn dfs(&mut self, id: usize) {
        if self.visited.contains(&id) {
            return;
        } else if self.path.contains(&id) {
            self.counter
                .entry(id)
                .and_modify(|count| *count += 1)
                .or_insert(1);
        } else {
            self.visited.insert(id);
            match &self.bb.get(&id).unwrap().end {
                JumpInst::Jump(id) => self.dfs(*id),
                JumpInst::Conditional(_, t, f) => {
                    self.dfs(*t);
                    self.dfs(*f);
                }
                JumpInst::Return(_) | JumpInst::Unreachable | JumpInst::Unknown => {}
            }
        }
    }
}
