use super::spec::*;
use crate::c0::ast::*;
use crate::c0::infra::*;
use std::collections::HashMap;

pub struct Compiler {
    functions: Vec<FuncFrame>
    
}

struct FuncFrame {
    pub args_offset: HashMap<String, isize>,
    pub loc_offset: HashMap<String, isize>,
    pub ops: Vec<Op>,
}

impl FuncFrame {
    pub fn build(func: &FnScopeDecl) {
        let frame = Self::init();
    }

    fn init() -> FuncFrame {
        FuncFrame {
            args_offset: HashMap::new(),
            loc_offset: HashMap::new(),
            ops: Vec::new(),
        }
    }

    fn build_expr(&mut self, expr: Expr, scope: Ptr<Scope>){

    }
}
