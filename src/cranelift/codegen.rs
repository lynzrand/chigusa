use super::*;
use crate::c0::ast;
use cranelift_codegen::ir;
use cranelift_codegen::*;
pub struct Codegen<'a> {
    ctx: Context,
    prog: &'a ast::Program,
}

impl<'a> Codegen<'a> {
    pub fn new(prog: &'a ast::Program) -> Codegen<'a> {
        Codegen {
            ctx: Context::new(),
            prog,
        }
    }

    pub fn compile(&mut self) {}

    pub fn compile_fn(&mut self, func: &ast::FunctionType) -> CompileResult<()> {
        Ok(())
    }
}
