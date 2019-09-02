use crate::c0::ast;
use inkwell::basic_block::*;
use inkwell::builder::*;
use inkwell::context::*;
use inkwell::types::*;
use inkwell::AddressSpace;

pub struct Compiler {
    ast: ast::Program,
    ctx: Context,
    module: Module,
    builder: Builder,
}

impl Compiler {
    pub fn new(ast: ast::Program) {
        let ctx = Context::create();
        let module = ctx.create_module("chigusa_main");
        let builder = ctx.create_builder();
        Compiler {
            ast,
            ctx,
            module,
            builder,
        }
    }
    pub fn compile(&mut self) -> Context {
        ctx
    }

    fn add_consts(&mut self) {
        let global_module = ctx.create_module();
    }

    fn complile_fn(&mut self, function: ast::FnScopeDecl) {}
}
