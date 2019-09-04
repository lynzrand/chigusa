use super::spec::*;
use crate::c0::ast::*;
use crate::c0::infra::*;
use indexmap::IndexMap;
use std::collections::HashMap;
use std::iter::Iterator;
use std::ops::Deref;

pub struct Compiler {
    functions: Vec<FuncFrame>,
}

struct FuncFrame {
    pub params_offset: IndexMap<String, isize>,
    pub loc_offset: IndexMap<String, isize>,
    pub ops: Vec<Op>,
}

impl FuncFrame {
    pub fn build(func: &FnScopeDecl, fn_name: &str) -> CompileResult<()> {
        let mut frame = Self::init();

        let decl = func.decl.as_ref().unwrap();
        let scope = decl.body.scope.borrow();
        frame.init_param_offsets(func.params.iter(), scope)?;

        Ok(())
    }

    fn init() -> FuncFrame {
        FuncFrame {
            params_offset: IndexMap::new(),
            loc_offset: IndexMap::new(),
            ops: Vec::new(),
        }
    }

    fn init_param_offsets<'s>(
        &mut self,
        params: impl Iterator<Item = &'s String>,
        scope: impl Deref<Target = Scope>,
    ) -> CompileResult<()> {
        // params are one offset under base pointer, so -8 bits it is
        let mut param_top = -8;

        params.for_each(|param: &String| {
            let param_def = scope.find_definition(param).unwrap();

            // Get the type name of param
            let param_type = param_def.borrow();
            let param_type = param_type.find_var(|| unreachable!()).unwrap();
            let param_type: &str = &param_type.var_type;

            // get the occupy size of param
            let params_type_size = scope
                .find_definition(&param_type)
                .unwrap()
                .borrow()
                .find_type(|| unreachable!())
                .unwrap()
                .occupy_bytes as isize;

            self.params_offset.insert(param_type.to_owned(), param_top);
            param_top -= params_type_size;
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
