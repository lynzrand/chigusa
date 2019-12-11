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
    ctx: &mut Codegen<'_, T>,
) -> (Vec<AbiParam>, Vec<AbiParam>)
where
    T: cranelift_module::Backend,
{
    // let return_type = Vec::new();

    let f_args = &f.params;
    let f_ret = &f.return_type;

    let args_type: Vec<_> = f_args
        .iter()
        .map(|arg: &Ptr<ast::TypeDef>| AbiParam::new(extract_ty(arg, f_parent_scope.clone(), ctx)))
        .collect();

    let ret_type = AbiParam::new(extract_ty(f_ret, f_parent_scope.clone(), ctx));
    let ret_type = vec![ret_type];

    (args_type, ret_type)
}

fn extract_ty<T>(ty: &Ptr<ast::TypeDef>, scope: Ptr<ast::Scope>, ctx: &mut Codegen<'_, T>) -> Type
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
                ast::SymbolDef::Typ { def } => extract_ty(def, scope, ctx),
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
                Type::int(prim.occupy_bytes as u16).expect(format!(
                    "Bad integer representation: {} bytes",
                    prim.occupy_bytes
                ))
            }
        },

        ast::TypeDef::Ref(r) => {
            let ty = r.target;

            // Type information is lost here.
            // TODO: Check pointer compatibility beforehand
            ctx.module.target_config().pointer_type()
        }

        _ => todo!("Types are not implemented"),
    }
}

struct FnCodegen<'b> {
    f: &'b ast::FunctionType,
    // ctx: &'b mut Codegen<'a, T>,
    ebb_cnt: usize,
    loc: usize,
    builder: frontend::FunctionBuilder<'b>,
}

impl<'b> FnCodegen<'b> {
    pub fn new<'a, T>(f: &'b ast::FunctionType, ctx: &'b mut Codegen<'a, T>) -> FnCodegen<'b>
    where
        T: cranelift_module::Backend,
    {
        FnCodegen {
            builder: frontend::FunctionBuilder::new(&mut ctx.ctx.func, &mut ctx.fn_builder_ctx),
            f,
            // ctx,
            ebb_cnt: 0,
            loc: 0,
        }
    }

    pub fn gen(&mut self) {}
}
