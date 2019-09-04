use super::spec::*;
use crate::c0::ast::*;
use crate::c0::infra::*;
use std::collections::HashMap;
use std::iter::Iterator;

pub struct Compiler {
    functions: Vec<FuncFrame>,
}

struct FuncFrame {
    pub params_offset: HashMap<String, isize>,
    pub loc_offset: HashMap<String, isize>,
    pub ops: Vec<Op>,
}

impl FuncFrame {
    pub fn build(func: &FnScopeDecl) {
        let frame = Self::init();
    }

    fn init() -> FuncFrame {
        FuncFrame {
            params_offset: HashMap::new(),
            loc_offset: HashMap::new(),
            ops: Vec::new(),
        }
    }

    fn init_loc(&mut self, func: &FnScopeDecl, fn_name: &str) -> CompileResult<()> {
        let params = func.params.iter();
        let loc = func
            .decl.as_ref()
            .ok_or(CompileError::NoDeclaration(fn_name.to_owned()))?;
        let mut param_top = 0i64;
        params.for_each(|param: &Ptr<TokenEntry>|{
            let param = param.borrow();
            // let 
        });
        Ok(())
    }

    fn build_expr(&mut self, expr: Expr, scope: Ptr<Scope>) {}
}

pub enum CompileError {
    NoDeclaration(String),
}

// pub struct CompileError(CompileErrorVariant, Span);

pub type CompileResult<T> = Result<T, CompileError>;
