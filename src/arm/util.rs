use crate::mir::*;
use std::collections::{HashMap, HashSet};

/// Solve the cycles inside basic blocks
pub struct CycleSolver<'src> {
    bb: &'src HashMap<usize, BasicBlk>,
    pub counter: HashMap<usize, usize>,
    path: HashSet<usize>,
    pub visited: HashSet<usize>,
}

impl<'src> CycleSolver<'src> {
    pub fn new(bb: &'src HashMap<usize, BasicBlk>) -> Self {
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
