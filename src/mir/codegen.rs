use crate::c0::ast;
use crate::minivm::{compile_err, compile_err_n, CompileError, CompileErrorVar, CompileResult};
use crate::mir;
use crate::prelude::*;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Codegen<'src> {
    src: &'src ast::Program,
    pkg: mir::MirPackage,
}

impl<'src> Codegen<'src> {
    fn gen_fn(&mut self, func: &ast::FunctionType) -> CompileResult<mir::Func> {
        let fn_codegen: FnCodegen = todo!("Initialize function code-generator");
        // fn_codegen.
    }
}

#[derive(Debug)]
pub struct FnCodegen<'src> {
    src: &'src ast::FunctionType,
    pkg: &'src mut mir::MirPackage,

    // basic blocks
    bb_counter: usize,
    bbs: HashMap<mir::BBId, mir::BasicBlk>,
}

impl<'src> FnCodegen<'src> {
    fn new(src: &'src ast::FunctionType, pkg: &'src mut mir::MirPackage) -> FnCodegen<'src> {
        let init_bb_id = 0;
        let init_bb = mir::BasicBlk {
            id: init_bb_id,
            inst: vec![],
            end: mir::JumpInst::Unknown,
        };
        let mut bbs = HashMap::new();
        bbs.insert(init_bb_id, init_bb);

        FnCodegen {
            src,
            bb_counter: 0,
            bbs,
            pkg,
        }
    }

    fn gen(&mut self) -> CompileResult<mir::Func> {
        todo!("Generate code for function")
    }

    /// Get a new basic block
    fn new_bb(&mut self) -> CompileResult<mir::BBId> {
        self.bb_counter += 1;
        let bbid = self.bb_counter;
        let bb = mir::BasicBlk {
            id: bbid,
            inst: vec![],
            end: mir::JumpInst::Unknown,
        };
        self.bbs.insert(bbid, bb);
        Ok(bbid)
    }

    /// Generate MIR for statement
    fn gen_stmt(
        &mut self,
        stmt: &ast::Stmt,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::BBId> {
        todo!()
    }

    fn gen_expr(
        &mut self,
        expr: &ast::Expr,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<()> {
        todo!()
    }
}
