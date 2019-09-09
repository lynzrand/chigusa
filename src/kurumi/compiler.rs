use super::spec::{self, *};
use crate::c0::ast::{self, *};
use crate::c0::infra::{self, *};
use indexmap::IndexMap;
use std::collections::HashMap;
use std::iter::Iterator;
use std::ops::Deref;

pub struct Compiler<'a> {
    program: &'a ast::Program,
    functions: Vec<FuncFrame>,
}

impl<'a> Compiler<'a> {
    pub fn compile(&mut self) {
        let scope = self.program.scope.borrow();
        let scope_iter = scope.token_table.iter();
        scope_iter.try_fold((), |(), (k, v)| {
            let entry = v.borrow();
            match *entry {
                TokenEntry::Function(ref func) => {
                    let new_frame = FuncFrame::build(func, &k)?;
                    self.functions.push(new_frame);
                }
                _ => unreachable!(),
            }
            Ok(())
        });
    }
}

struct FuncFrame {
    pub params_offset: IndexMap<String, isize>,
    pub loc_offset: IndexMap<String, isize>,
    pub ops: Vec<Op>,
}

impl FuncFrame {
    pub fn build(func: &FnScopeDecl, fn_name: &str) -> CompileResult<FuncFrame> {
        let mut frame = Self::init();

        let decl = func.decl.as_ref().unwrap();
        let scope = decl.body.scope.borrow();
        frame.init_param_offsets(func.params.iter(), scope)?;

        Ok(frame)
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
