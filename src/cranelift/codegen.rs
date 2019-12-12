use super::*;
use crate::c0::ast;
use crate::prelude::*;
use cranelift::codegen::ir;
use cranelift::codegen::ir::types::*;
use cranelift::codegen::*;
use cranelift::prelude::*;
use cranelift::*;
use cranelift_module::{self, Backend, DataContext, Module};
use frontend::FunctionBuilderContext;
use frontend::*;
use indexmap::IndexMap;
pub struct Codegen<'a, T>
where
    T: cranelift_module::Backend,
{
    module: cranelift_module::Module<T>,
    ctx: Context,
    data_ctx: DataContext,
    fn_builder_ctx: FunctionBuilderContext,
    prog: &'a ast::Program,
}

impl<'a, T> Codegen<'a, T>
where
    T: cranelift_module::Backend,
{
    pub fn new(prog: &'a ast::Program, builder: T::Builder) -> Codegen<'a, T> {
        let module = Module::new(builder);
        Codegen {
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            prog,
            module,
            fn_builder_ctx: FunctionBuilderContext::new(),
        }
    }

    pub fn compile(&mut self) {}

    pub fn compile_fn<'t>(&mut self, func: &ast::FunctionType) -> CompileResult<()> {
        // TODO: Add signature extractor
        // let sig = FnCodegen::extract_sig(func, self);
        let fnc = FnCodegen::new(func, self);

        Ok(())
    }
}

fn extract_sig<T>(
    f: &ast::FunctionType,
    f_parent_scope: Ptr<ast::Scope>,
    f_scope: Ptr<ast::Scope>,
    module: &cranelift_module::Module<T>,
) -> (Vec<AbiParam>, Vec<AbiParam>)
where
    T: cranelift_module::Backend,
{
    // let return_type = Vec::new();

    let f_args = &f.params;
    let f_ret = &f.return_type;

    let args_type: Vec<_> = f_args
        .iter()
        .map(|arg: &Ptr<ast::TypeDef>| {
            AbiParam::new(extract_ty(arg, f_parent_scope.clone(), module))
        })
        .collect();

    let ret_type = AbiParam::new(extract_ty(f_ret, f_parent_scope.clone(), module));
    let ret_type = vec![ret_type];

    (args_type, ret_type)
}

fn extract_ty<T>(
    ty: &Ptr<ast::TypeDef>,
    scope: Ptr<ast::Scope>,
    module: &cranelift_module::Module<T>,
) -> Type
where
    T: cranelift_module::Backend,
{
    let ty = ty.borrow();

    match &*ty {
        ast::TypeDef::NamedType(n) => {
            let scope_c = scope.clone();
            let scope_b = scope_c.borrow();
            let sty = scope_b.find_def(n).expect("Unknown type inside AST");
            let sty = sty.borrow();

            match &*sty {
                ast::SymbolDef::Typ { def } => extract_ty(def, scope, module),
                _ => panic!("Type pointing at value in AST"),
            }
        }

        // Unit types are interpreted as i1 for now, as no other uses i1
        ast::TypeDef::Unit => Type::int(1).unwrap(),

        ast::TypeDef::Primitive(prim) => match prim.var {
            ast::PrimitiveTypeVar::Float => match prim.occupy_bytes {
                32 => types::F32,
                64 => types::F64,
                n @ _ => panic!("Bad float representation: {} bytes", n),
            },
            ast::PrimitiveTypeVar::SignedInt | ast::PrimitiveTypeVar::UnsignedInt => {
                Type::int(prim.occupy_bytes as u16).expect(&format!(
                    "Bad integer representation: {} bytes",
                    prim.occupy_bytes
                ))
            }
        },

        ast::TypeDef::Ref(r) => {
            let ty = r.target.clone();

            // Type information is lost here.
            // TODO: Check pointer compatibility beforehand
            module.target_config().pointer_type()
        }

        _ => todo!("Types are not implemented"),
    }
}

struct FnCodegen<'b, T>
where
    T: cranelift_module::Backend,
{
    f: &'b ast::FunctionType,
    // ctx: &'b mut Codegen<'a, T>,
    ebb_cnt: u32,
    loc_cnt: u32,
    loc: IndexMap<String, Variable>,
    builder: frontend::FunctionBuilder<'b>,
    module: &'b mut cranelift_module::Module<T>,
}

impl<'b, T> FnCodegen<'b, T>
where
    T: cranelift_module::Backend,
{
    pub fn new<'a>(f: &'b ast::FunctionType, ctx: &'b mut Codegen<'a, T>) -> FnCodegen<'b, T> {
        FnCodegen {
            builder: frontend::FunctionBuilder::new(&mut ctx.ctx.func, &mut ctx.fn_builder_ctx),
            f,
            // ctx,
            ebb_cnt: 0,
            loc_cnt: 0,
            loc: IndexMap::new(),
            module: &mut ctx.module,
        }
    }

    pub fn gen(&mut self) {
        if let Some(b) = &self.f.body {
            let scope = b.scope.borrow();
            let stmts = &b.stmts;
            for stmt in stmts {
                self.gen_stmt(stmt);
            }
        } else {
            if self.f.is_extern {
                return;
            } else {
                panic!("No body in function")
            }
        }
    }

    fn add_local(
        &mut self,
        name: &str,
        var: &ast::SymbolDef,
        depth: usize,
        scope: Ptr<ast::Scope>,
    ) {
        match var {
            ast::SymbolDef::Typ { .. } => (),
            ast::SymbolDef::Var { typ, is_const } => {
                if depth != 0 {
                    // Who cares about constants?
                    let var_name = format!("{}`{}", name, depth);
                    let var_loc = self.loc_cnt;
                    self.loc_cnt = self.loc_cnt + 1;
                    let var = Variable::with_u32(var_loc);
                    let typ = extract_ty(typ, scope, self.module);
                    self.builder.declare_var(var, typ);
                } else {
                    todo!("Add global values");
                    // self.builder.create_global_value();
                }
            }
        }
    }

    fn gen_stmt(&mut self, stmt: &ast::Stmt) {
        match &stmt.var {
            ast::StmtVariant::Expr(e) => {
                self.gen_expr(e.clone());
            }
            ast::StmtVariant::ManyExpr(e) => {
                for e in e {
                    self.gen_expr(e.clone());
                }
            }
            ast::StmtVariant::Return(e) => todo!("Generate code for return"),
            ast::StmtVariant::Block(e) => todo!("Generate code for block"),
            ast::StmtVariant::Break => todo!("Generate code for return"),
            ast::StmtVariant::If(e) => todo!("Generate code for return`"),
            ast::StmtVariant::While(e) => todo!("Generate code for ret`urn"),
            ast::StmtVariant::Empty => (),
        }
    }

    fn gen_expr(&mut self, expr: Ptr<ast::Expr>) -> Value {
        let expr = expr.borrow();
        let expr = &*expr;
        match &expr.var {
            ast::ExprVariant::BinaryOp(b) => {
                let lhs = self.gen_expr(b.lhs.clone());
                let rhs = self.gen_expr(b.rhs.clone());
                b.op.build_inst_bin(self.builder.ins(), lhs, rhs)
            }
            _ => todo!("Implement other expression variants"),
        }
    }
}

impl ast::OpVar {
    fn build_inst_bin<'a>(
        &self,
        inst_builder: impl InstBuilder<'a>,
        lhs: Value,
        rhs: Value,
    ) -> Value
where {
        match self {
            ast::OpVar::Add => inst_builder.iadd(lhs, rhs),
            _ => todo!(),
        }
    }
}
