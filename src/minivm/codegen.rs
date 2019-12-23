use super::err::*;
use super::instgen::*;
use super::*;
use crate::c0::ast::{self, *};
use crate::prelude::*;
use either::Either;
use indexmap::{map::Entry, IndexMap};
use std::convert::TryInto;
use std::iter::Iterator;
const bytes_per_slot: u16 = 4;

#[derive(Debug, Clone)]
struct Data {
    typ: Ptr<ast::TypeDef>,

    /// Either its value or the bytes it occupy
    init_val: Either<Constant, u16>,

    is_const: bool,
}

/// A sink of global data
#[derive(Debug, Clone)]
struct DataSink {
    map: IndexMap<String, Data>,
}

impl DataSink {
    pub fn new() -> DataSink {
        DataSink {
            map: IndexMap::new(),
            // max_offset:0
        }
    }

    pub fn put_data(&mut self, name: &str, val: Data) -> Option<u16> {
        if self.map.len() < u16::max_value() as usize {
            if self.map.contains_key(name) {
                None
            } else {
                let idx = self.map.insert_full(name.into(), val).0 as u16;
                Some(idx)
            }
        } else {
            None
        }
    }

    fn put_str(&mut self, name: &str, val: String, is_const: bool) -> Option<u16> {
        let str_val: Vec<_> = val.as_bytes().iter().map(|x| *x).collect();
        // let str_val = std::ffi::CString::new(str_val).unwrap();
        // let str_val = str_val.into_bytes_with_nul();

        let typ = Ptr::new(ast::TypeDef::Ref(ast::RefType {
            target: Ptr::new(ast::TypeDef::Primitive(ast::PrimitiveType {
                var: ast::PrimitiveTypeVar::UnsignedInt,
                occupy_bytes: 1,
            })),
        }));

        let val = Data {
            typ,
            init_val: Either::Left(Constant::String(str_val)),
            is_const: true,
        };

        self.put_data(name, val)
    }

    pub fn get_offset(&self, name: &str) -> Option<u16> {
        self.map.get_full(name).map(|x| x.0 as u16)
    }

    pub fn get_data(&self, name: &str) -> Option<&Data> {
        self.map.get(name)
    }

    pub fn unwrap(mut self) -> Vec<Data> {
        self.map.into_iter().map(|(s, d)| d).collect()
    }
}

#[derive(Debug, Clone)]
pub(super) struct FunctionType {
    pub params: Vec<Ptr<TypeDef>>,
    pub return_type: Ptr<TypeDef>,
    pub body: Option<InstSink>,
    pub param_siz: u32,
    pub is_extern: bool,
    pub name_idx: u16,
}

impl Into<FnInfo> for FunctionType {
    fn into(self) -> FnInfo {
        FnInfo {
            name_idx: self.name_idx,
            ins: self.body.unwrap().unwrap(),
            lvl: 1,
            // TODO
            param_siz: self.param_siz as u16,
        }
    }
}

#[derive(Debug, Clone)]
struct GlobalData {
    pub vars: DataSink,
    pub consts: DataSink,
    pub fns: IndexMap<String, FunctionType>,
}

impl GlobalData {
    pub fn new() -> GlobalData {
        GlobalData {
            vars: DataSink::new(),
            consts: DataSink::new(),
            fns: IndexMap::new(),
        }
    }
}

pub type Type = Ptr<ast::TypeDef>;

/// An opaque sink of instructions
#[derive(Debug, Clone)]
pub(super) struct InstSink(Vec<Inst>);

impl InstSink {
    pub fn new() -> InstSink {
        InstSink(Vec::new())
    }

    pub fn inner(&self) -> &Vec<Inst> {
        &self.0
    }

    pub fn unwrap(self) -> Vec<Inst> {
        self.0
    }

    /// Append all instruction from the other InstSink
    pub fn append_all(&mut self, other: &mut InstSink) {
        self.0.append(&mut other.0);
    }

    pub fn push(&mut self, inst: Inst) {
        self.0.push(inst)
    }

    pub fn push_many(&mut self, inst: &[Inst]) {
        for i in inst {
            self.0.push(*i)
        }
    }

    pub fn prepend(&mut self, inst: Inst) {
        self.0.insert(0, inst);
    }

    pub fn pop(&mut self) -> Option<Inst> {
        self.0.pop()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn reset(&mut self) {
        self.0.clear()
    }

    pub fn reset_self(mut self) -> Self {
        self.reset();
        self
    }
}

#[derive(Debug)]
pub struct Codegen<'a> {
    prog: &'a ast::Program,
    glob: GlobalData,
}

impl<'a> Codegen<'a> {
    pub fn new(prog: &'a ast::Program) -> Codegen<'a> {
        Codegen {
            prog,
            glob: GlobalData::new(),
        }
    }

    pub fn compile(mut self) -> CompileResult<O0> {
        let start_stmts = &self.prog.stmts;

        let decls = &self.prog.scope;
        let decls = &*decls.borrow();

        for item in decls.defs.iter() {
            let name = item.0;
            let def = item.1.borrow();
            if let ast::SymbolDef::Var { typ, .. } = &*def {
                let typ = typ.borrow();
                if let ast::TypeDef::Function(f) = &*typ {
                    self.add_fn(f, name)?;
                } else {
                    todo!("Implement global variables")
                    // self.add_var(t)
                }
            }
        }

        // TODO: Make _start function

        let start_code = InstSink::new();

        for item in decls.defs.iter() {
            let name = item.0;
            let def = item.1.borrow();
            if let ast::SymbolDef::Var { typ, .. } = &*def {
                let typ = typ.borrow();
                if let ast::TypeDef::Function(f) = &*typ {
                    self.compile_fn(f, name)?;
                }
            }
        }

        Ok(O0 {
            version: 1,
            constants: self
                .glob
                .consts
                .unwrap()
                .into_iter()
                .map(|data: Data| {
                    data.init_val
                        .either(|c| c, |len| Constant::String(vec![0; len as usize]))
                })
                .collect(),
            start_code: StartCodeInfo {
                ins: start_code.unwrap(),
            },
            functions: self.glob.fns.into_iter().map(|f| f.1.into()).collect(),
        })
    }

    /// Add the signature of a function to `self.glob`, but does not compile it.
    fn add_fn(&mut self, func: &ast::FunctionType, name: &str) -> CompileResult<()> {
        if !func.is_extern {
            let fn_name = format!("`function_name`{}", name);
            // ** The `fn_name` variable is only for identifying the string name!
            let name_idx = self
                .glob
                .consts
                .put_str(&fn_name, name.into(), true)
                .unwrap();

            let ret = Ptr::new(resolve_ty(
                &*func.return_type.borrow(),
                self.prog.scope.cp(),
            ));

            let params: Vec<_> = func
                .params
                .iter()
                .map(|i| Ptr::new(resolve_ty(&*i.borrow(), self.prog.scope.cp())))
                .collect();

            let param_siz = params.iter().try_fold(0, |sum, item| {
                Ok(item
                    .borrow()
                    .occupy_slots()
                    .ok_or(CompileError::RequireSized("".into()))?
                    + sum)
            })?;

            let func = FunctionType {
                name_idx,
                param_siz,
                params,
                return_type: ret,
                body: None,
                is_extern: false,
            };

            // ** We insert the original name to global function registry
            self.glob.fns.insert(name.into(), func);

            Ok(())
        } else {
            Err(CompileError::NoExternFunction)
        }
    }

    /// Add a global variable to `self.glob`
    fn add_var(&mut self, func: &ast::FunctionType, name: &str) -> CompileResult<()> {
        todo!("Add global variables")
    }

    /// Compile and repair the function declaration in `self.glob`
    fn compile_fn(&mut self, func: &ast::FunctionType, name: &str) -> CompileResult<()> {
        // Get the function. Things can't go wrong here right?
        let fn_ref = self.glob.fns.get(name).unwrap();

        let ret = fn_ref.return_type.cp();
        let params = fn_ref.params.iter().map(|x| x.cp()).collect();
        // * Return fn_ref so that we can borrow self for function compilation

        let mut fnc = FnCodegen::new(func, name, self, ret, params);

        fnc.gen()?;
        let inst = fnc.finish();

        // * We're done here. Add the instructions
        let fn_ref = self.glob.fns.get_mut(name).unwrap();

        fn_ref.body = Some(inst);

        Ok(())
    }
}

/// Resolve all named types into their definitions, and strip function types' bodies
fn resolve_ty(ty: &ast::TypeDef, scope: Ptr<ast::Scope>) -> ast::TypeDef {
    match ty {
        ast::TypeDef::NamedType(n) => {
            let scope_c = scope.cp();
            let scope_b = scope_c.borrow();
            let sty = scope_b.find_def(n).expect("Unknown type inside AST");
            let sty = sty.borrow().get_typ().unwrap();
            let sty = sty.borrow();

            resolve_ty(&*sty, scope.cp())
        }
        prim @ ast::TypeDef::Primitive(..) => prim.clone(),
        ast::TypeDef::Ref(r) => {
            let src = r.target.borrow();
            let res = Ptr::new(resolve_ty(&*src, scope.cp()));
            ast::TypeDef::Ref(ast::RefType { target: res })
        }
        ast::TypeDef::Function(f) => {
            let params = f
                .params
                .iter()
                .map(|a| {
                    let a = a.borrow();
                    Ptr::new(resolve_ty(&*a, scope.cp()))
                })
                .collect();
            let ret = Ptr::new(resolve_ty(&*f.return_type.borrow(), scope.cp()));
            ast::TypeDef::Function(ast::FunctionType {
                params,
                return_type: ret,
                body: None,
                is_extern: f.is_extern,
            })
        }
        ast::TypeDef::Unit => ast::TypeDef::Unit,
        _ => todo!("Type resolve not implemented"),
    }
}

/// Calculate the bits needed for a type to contain a value
fn type_bits(len: u32) -> Option<u16> {
    if len > 128 {
        None
    } else if len > 64 {
        Some(128)
    } else if len > 32 {
        Some(64)
    } else if len > 16 {
        Some(32)
    } else if len > 8 {
        Some(16)
    } else {
        Some(8)
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub(super) struct LocalVar {
    size: u32,
    offset: u32,
    is_const: bool,
    typ: Type,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub(super) struct LocalVars {
    def_map: IndexMap<String, LocalVar>,
    size_stack: Vec<u32>,
    max_stack_size: u32,
}

impl LocalVars {
    pub fn new() -> LocalVars {
        LocalVars {
            def_map: IndexMap::new(),
            size_stack: Vec::new(),
            max_stack_size: 0,
        }
    }

    pub fn add_var(
        &mut self,
        name: &str,
        size: u32,
        is_const: bool,
        typ: Type,
    ) -> CompileResult<()> {
        let cur_stack_size = self.size_stack.iter().sum();
        let loc = LocalVar {
            offset: cur_stack_size,
            size,
            is_const,
            typ: typ.cp(),
        };
        log::trace!(
            "Inserting local variable: {}, size {}, offset {}",
            name,
            size,
            cur_stack_size
        );
        self.def_map.insert(name.into(), loc).map_or_else(
            || Ok(()),
            |_| {
                Err(CompileError::InternalError(
                    "Name conflict on local variable declaration".into(),
                ))
            },
        )?;
        {
            let last = self.size_stack.last_mut().unwrap();
            *last = *last + size;
        }
        if cur_stack_size + size > self.max_stack_size {
            self.max_stack_size = cur_stack_size + size;
        }
        Ok(())
    }

    pub fn get_var(&self, name: &str) -> Option<&LocalVar> {
        self.def_map.get(name)
    }

    pub fn dive_into_scope(&mut self) {
        self.size_stack.push(0);
    }

    pub fn pop_scope(&mut self) {
        self.size_stack.pop();
    }

    pub fn max_stack_size(&self) -> u32 {
        self.max_stack_size
    }
}

pub struct DeqPool<'a, T> {
    pool: std::collections::VecDeque<T>,
    new_item: &'a dyn Fn() -> T,
    reset_item: Option<&'a dyn Fn(&mut T)>,
}

impl<'a, T> DeqPool<'a, T> {
    pub fn new(new_item: &'a dyn Fn() -> T) -> DeqPool<'a, T> {
        Self {
            pool: std::collections::VecDeque::new(),
            new_item,
            reset_item: None,
        }
    }

    pub fn new_with_reset(
        new_item: &'a dyn Fn() -> T,
        reset_item: &'a dyn Fn(&mut T),
    ) -> DeqPool<'a, T> {
        Self {
            pool: std::collections::VecDeque::new(),
            new_item,
            reset_item: Some(reset_item),
        }
    }

    pub fn get(&mut self) -> T {
        match self.pool.pop_front() {
            Some(t) => t,
            None => (self.new_item)(),
        }
    }

    pub fn put(&mut self, mut t: T) {
        if let Some(f) = self.reset_item {
            f(&mut t);
        }
        self.pool.push_back(t)
    }
}

impl<'a, T> std::fmt::Debug for DeqPool<'a, T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DeqPool").field("pool", &self.pool).finish()
    }
}

/// A function code generator. Responsible for generating
#[derive(Debug)]
pub(super) struct FnCodegen<'a, 'b> {
    f: &'b ast::FunctionType,
    ret_type: Type,
    params: Vec<Type>,
    param_siz: u32,

    name: &'b str,
    // ctx: &'b mut Codegen<'a, T>,
    branch_cnt: u32,
    branch_returned: u32,

    /// How many instructions are yet to be inserted?
    ///
    /// This field is for usage in conditionals, so not-yet-inserted instructions
    /// in expressions will not be count.
    pending_inst_cnt: u16,

    /// Data count, only for naming usage
    data_cnt: u32,
    data: &'b mut GlobalData,
    loc: LocalVars,

    /// Instruction sinks; The one at the top of the stack is the one currently used
    inst: Vec<InstSink>,
    sink_pool: DeqPool<'a, InstSink>,
}

/// Implementation for larger function, statement and expression structures
impl<'a, 'b> FnCodegen<'a, 'b> {
    pub fn new<'c>(
        f: &'b ast::FunctionType,
        name: &'b str,
        ctx: &'b mut Codegen<'c>,
        ret_type: Type,
        params: Vec<Type>,
    ) -> FnCodegen<'a, 'b> {
        FnCodegen {
            f,
            name,
            ret_type,
            params,
            param_siz: 0,
            branch_cnt: 1,
            branch_returned: 0,
            data_cnt: 0,
            pending_inst_cnt: 0,
            data: &mut ctx.glob,
            loc: LocalVars::new(),
            // module: &mut ctx.module,,
            inst: vec![InstSink::new()],
            sink_pool: DeqPool::new_with_reset(&InstSink::new, &InstSink::reset),
        }
    }

    pub(super) fn inst_sink(&mut self) -> &mut InstSink {
        self.inst
            .last_mut()
            .expect("A function generator must have at least one instruction sink")
    }

    pub fn cur_pos(&self) -> u16 {
        self.inst
            .iter()
            .fold(0u16, |sum, vec| sum + vec.len() as u16)
            + self.pending_inst_cnt
    }

    pub fn gen(&mut self) -> CompileResult<()> {
        if let Some(b) = &self.f.body {
            self.param_siz = self.params.iter().try_fold(0, |sum, item| {
                Ok(item
                    .borrow()
                    .occupy_slots()
                    .ok_or(CompileError::RequireSized("".into()))?
                    + sum)
            })?;

            // * One instruction for that stack allocation
            self.pending_inst_cnt += 1;

            self.gen_scope(b, b.scope.cp())?;

            // Calculate local variable size
            {
                let stack_size = self.loc.max_stack_size();

                log::info!(
                    "The function has max stack size of {} slots, of which {} are params.",
                    stack_size,
                    self.param_siz
                );
                let param_siz = self.param_siz;
                self.inst_sink().prepend(Inst::SNew(stack_size - param_siz));

                self.pending_inst_cnt -= 1;
            }

            // Check return type
            {
                let is_void = {
                    let ret = self.ret_type.borrow();
                    ast::TypeDef::Unit == *ret
                };
                // If there are branches not returned, add a return statement
                if is_void && self.branch_returned != self.branch_cnt {
                    self.inst_sink().push(Inst::Ret);
                }
            }

            Ok(())
        } else {
            if self.f.is_extern {
                Ok(())
            } else {
                panic!("No body in function")
            }
        }
    }

    pub fn finish(mut self) -> InstSink {
        assert!(self.inst.len() == 1);
        self.inst.pop().expect("The one and only instruction sink")
    }

    pub(super) fn add_local(
        &mut self,
        name: &str,
        var: &ast::SymbolDef,
        // if id == 0 then it's global variable
        id: usize,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<()> {
        match var {
            // We don't care about type decls
            ast::SymbolDef::Typ { .. } => (),

            // Variable decl
            ast::SymbolDef::Var { typ, is_const } => {
                if id != 0 {
                    // Who cares about constants?
                    let var_name = format!("{}`{}", name, id);

                    let typ = resolve_ty(&*typ.borrow(), scope);
                    let occupy_slots = typ
                        .occupy_slots()
                        .ok_or(CompileError::RequireSized(format!("{:?}", typ)))?;

                    self.loc
                        .add_var(&var_name, occupy_slots, *is_const, Ptr::new(typ))?;
                } else {
                    todo!("Add global values");
                    // self.builder.create_global_value();
                }
            }
        }
        Ok(())
    }

    fn gen_stmt(&mut self, stmt: &ast::Stmt, scope: Ptr<ast::Scope>) -> CompileResult<()> {
        match &stmt.var {
            ast::StmtVariant::Expr(e) => {
                let typ = self.gen_expr(e.cp(), scope.cp())?;
                if !typ.borrow().is_unit() {
                    pop(typ.cp(), self.inst_sink())?;
                }
                Ok(())
            }
            ast::StmtVariant::ManyExpr(e) => {
                for e in e {
                    let typ = self.gen_expr(e.cp(), scope.cp())?;
                    if !typ.borrow().is_unit() {
                        pop(typ.cp(), self.inst_sink())?;
                    }
                }
                Ok(())
            }
            ast::StmtVariant::Return(e) => self.gen_return(e, scope),
            ast::StmtVariant::Block(e) => self.gen_scope(e, scope),
            ast::StmtVariant::Print(e) => self.gen_print(e, scope),
            ast::StmtVariant::Scan(e) => self.gen_scan(e, scope),
            ast::StmtVariant::Break => self.gen_break(scope),
            ast::StmtVariant::If(e) => self.gen_if(e, scope),
            ast::StmtVariant::While(e) => self.gen_while(e, scope),
            ast::StmtVariant::Empty => Ok(()),
        }
    }

    fn gen_expr(&mut self, expr: Ptr<ast::Expr>, scope: Ptr<ast::Scope>) -> CompileResult<Type> {
        let expr = expr.borrow();
        let expr = &*expr;
        match &expr.var {
            ast::ExprVariant::BinaryOp(b) => self.gen_bin_op(b, scope),
            ast::ExprVariant::UnaryOp(u) => self.gen_una_op(u, scope),
            ast::ExprVariant::Ident(i) => self.gen_ident_expr(i, scope),
            ast::ExprVariant::FunctionCall(f) => self.gen_func_call(f, scope),
            ast::ExprVariant::Literal(lit) => self.gen_literal(lit, scope),
            ast::ExprVariant::TypeConversion(ty) => self.gen_ty_conversion(ty, scope),
            _ => Err(CompileError::NotImplemented(
                "Implement other expression variants".into(),
            )),
        }
    }

    fn gen_scope(&mut self, block: &ast::Block, scope: Ptr<ast::Scope>) -> CompileResult<()> {
        self.loc.dive_into_scope();

        let scope = block.scope.cp();
        let defs = scope.borrow();
        for local in &defs.defs {
            self.add_local(&local.0, &*local.1.borrow(), defs.id, scope.cp())?;
        }

        let stmts = &block.stmts;
        for stmt in stmts {
            self.gen_stmt(stmt, scope.cp())?;
        }

        self.loc.pop_scope();
        Ok(())
    }

    fn gen_ident_address(
        &mut self,
        i: &ast::Identifier,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<Type> {
        let def = scope.borrow().find_def_depth(&i.name).unwrap();
        if def.1 != 0 {
            // Local variable
            let loc =
                self.loc
                    .get_var(&format!("{}`{}", i.name, def.1))
                    .ok_or(CompileError::Error(format!(
                        "Unable to find  identifier {}",
                        i.name
                    )))?;
            let typ = loc.typ.cp();
            let offset = loc.offset as i32;
            self.inst_sink().push(Inst::LoadA(0, offset));
            Ok(typ)
        } else {
            // Global variable
            todo!("Global variable")
        }
    }

    fn gen_l_value_address(
        &mut self,
        expr: Ptr<ast::Expr>,
        is_const_storage: bool,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<Type> {
        let expr = expr.borrow();
        let expr = &*expr;

        match &expr.var {
            ast::ExprVariant::Ident(i) => self.gen_ident_address(i, scope),
            _ => Err(CompileError::NotLValue(format!("{}", expr))),
        }
    }

    fn gen_bin_op(&mut self, b: &ast::BinaryOp, scope: Ptr<ast::Scope>) -> CompileResult<Type> {
        if b.op == ast::OpVar::_Asn || b.op == ast::OpVar::_Csn {
            // * This generates address for lhs.
            let lhs = self.gen_l_value_address(b.lhs.cp(), b.op == ast::OpVar::_Csn, scope.cp())?;

            let rhs = self.gen_expr(b.rhs.cp(), scope.cp())?;

            conv(rhs, lhs.cp(), self.inst_sink())?;

            // store lhs
            store(lhs, self.inst_sink())?;

            // * Assignment evaluates as unit type!
            Ok(Ptr::new(ast::TypeDef::Unit))
        } else {
            // Normal expressions
            self.inst.push(self.sink_pool.get());
            let lhs = self.gen_expr(b.lhs.cp(), scope.cp())?;
            let mut lhs_op = self.inst.pop().unwrap();

            self.inst.push(self.sink_pool.get());
            let rhs = self.gen_expr(b.rhs.cp(), scope.cp())?;
            let mut rhs_op = self.inst.pop().unwrap();

            let typ = flatten_ty(lhs, &mut lhs_op, rhs, &mut rhs_op)?;

            self.inst_sink().append_all(&mut lhs_op);
            self.inst_sink().append_all(&mut rhs_op);

            b.op.inst(self.inst_sink(), typ.cp())?;

            self.sink_pool.put(lhs_op);
            self.sink_pool.put(rhs_op);

            Ok(typ)
        }
    }

    fn gen_una_op(&mut self, u: &ast::UnaryOp, scope: Ptr<ast::Scope>) -> CompileResult<Type> {
        // Calculate expression body
        // self.inst.push(self.sink_pool.get());
        let lhs = self.gen_expr(u.val.cp(), scope.cp())?;
        // let mut lhs_op = self.inst.pop().unwrap();

        u.op.inst(&mut self.inst_sink(), lhs.cp())?;

        Ok(lhs)
    }

    fn gen_ident_expr(
        &mut self,
        i: &ast::Identifier,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<Type> {
        let def = scope.borrow().find_def_depth(&i.name).unwrap();
        let loc = self
            .loc
            .get_var(&format!("{}`{}", i.name, def.1))
            .ok_or(CompileError::Error(format!(
                "Unable to find  identifier {}",
                i.name
            )))?;
        let typ = loc.typ.cp();
        let offset = loc.offset as i32;
        self.inst_sink().push(Inst::LoadA(0, offset));
        load(typ.cp(), self.inst_sink())?;
        Ok(typ)
    }

    fn gen_func_call(
        &mut self,
        f: &ast::FunctionCall,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<Type> {
        let func = &f.func;
        let func_entry = self
            .data
            .fns
            .get_full(func)
            .ok_or_else(|| CompileError::NonExistFunc("Function does not exist".into()))?;

        let params = &func_entry.2.params;

        if f.params.len() != params.len() {
            return Err(CompileError::ParamLengthMismatch);
        }
        let f_idx = func_entry.0 as u16;
        let f_ret_typ = func_entry.2.return_type.cp();

        // Push each param into stack
        // * Rust complains about lifetimes here, so we'll just move everything into
        // * a vector for now. A little waste of memory, but hey it works.
        let params_pair_iter: Vec<_> = f
            .params
            .iter()
            .zip(params.iter().map(|param| param.cp()))
            .collect();

        for param in params_pair_iter {
            let res = self.gen_expr(param.0.cp(), scope.cp())?;
            conv(res, param.1.cp(), self.inst_sink())?;
        }

        self.inst_sink().push(Inst::Call(f_idx));

        Ok(f_ret_typ)
    }

    fn gen_literal(&mut self, lit: &ast::Literal, scope: Ptr<ast::Scope>) -> CompileResult<Type> {
        match lit {
            ast::Literal::Boolean { val } => {
                self.inst_sink().push(Inst::IPush(*val as i32));
                let typ = Ptr::new(ast::TypeDef::Primitive(ast::PrimitiveType {
                    var: ast::PrimitiveTypeVar::UnsignedInt,
                    occupy_bytes: 1,
                }));
                Ok(typ)
            }

            ast::Literal::Integer { val } => {
                let val: i32 = val.try_into().map_err(|_| CompileError::IntOverflow)?;
                self.inst_sink().push(Inst::IPush(val));

                let typ = Ptr::new(ast::TypeDef::Primitive(ast::PrimitiveType {
                    var: ast::PrimitiveTypeVar::UnsignedInt,
                    occupy_bytes: 4,
                }));
                Ok(typ)
            }

            ast::Literal::Float { val } => {
                let typ = Ptr::new(ast::TypeDef::Primitive(ast::PrimitiveType {
                    var: ast::PrimitiveTypeVar::Float,
                    occupy_bytes: 8,
                }));

                let val: f64 = val.to_f64();
                let idx = self
                    .data
                    .consts
                    .put_data(
                        &format!("`{}``str{}", self.name, self.data_cnt),
                        Data {
                            typ: typ.cp(),
                            init_val: Either::Left(Constant::Float(val)),
                            is_const: true,
                        },
                    )
                    .expect("Unable to add double data");
                self.inst_sink().push(Inst::LoadC(idx));
                self.data_cnt += 1;

                // let val = self.builder.ins().f64const(val.to_f64());
                Ok(typ)
            }

            ast::Literal::String { val } => {
                let offset = self
                    .data
                    .consts
                    .put_str(
                        &format!("`{}``str{}", self.name, self.data_cnt),
                        val.into(),
                        true,
                    )
                    .unwrap();
                self.data_cnt += 1;
                self.inst_sink().push(Inst::LoadC(offset));
                let typ = Ptr::new(ast::TypeDef::Ref(ast::RefType {
                    target: Ptr::new(ast::TypeDef::Primitive(ast::PrimitiveType {
                        var: ast::PrimitiveTypeVar::UnsignedInt,
                        occupy_bytes: 1,
                    })),
                }));
                Ok(typ)
            }

            ast::Literal::Struct { .. } => Err(CompileError::InternalError(
                "Structs are not yet supported!".into(),
            )),
        }
    }

    fn gen_ty_conversion(
        &mut self,
        i: &ast::TypeConversion,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<Type> {
        let expr = i.expr.cp();
        let ty = i.to.cp();

        let expr_ty = self.gen_expr(expr, scope)?;

        conv(expr_ty, ty, self.inst_sink())
    }

    fn gen_if(&mut self, i: &ast::IfConditional, scope: Ptr<ast::Scope>) -> CompileResult<()> {
        let cond = i.cond.cp();
        todo!()
    }

    fn gen_while(
        &mut self,
        i: &ast::WhileConditional,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<()> {
        todo!()
    }

    fn gen_break(&mut self, scope: Ptr<ast::Scope>) -> CompileResult<()> {
        todo!()
    }

    fn gen_scan(&mut self, scan: &ast::Identifier, scope: Ptr<ast::Scope>) -> CompileResult<()> {
        let typ = self.gen_ident_address(scan, scope.cp())?;
        let typ_borrow = typ.borrow();
        match &*typ_borrow {
            ast::TypeDef::Primitive(p) => {
                match p.var {
                    ast::PrimitiveTypeVar::Float => {
                        self.inst_sink().push_many(&[Inst::DScan, Inst::DStore])
                    }
                    ast::PrimitiveTypeVar::UnsignedInt => {
                        if p.occupy_bytes == 1 {
                            self.inst_sink().push_many(&[Inst::CScan, Inst::IStore])
                        } else {
                            self.inst_sink().push_many(&[Inst::IScan, Inst::IStore])
                        }
                    }
                    ast::PrimitiveTypeVar::SignedInt => {
                        self.inst_sink().push_many(&[Inst::IScan, Inst::IStore])
                    }
                }
                Ok(())
            }
            _ => Err(CompileError::RequireScannable(format!(
                "{:?}",
                &*typ.borrow()
            ))),
        }
    }

    fn gen_print(&mut self, print: &Vec<Ptr<Expr>>, scope: Ptr<ast::Scope>) -> CompileResult<()> {
        let mut is_first = true;
        for val in print {
            if is_first {
                is_first = false;
            } else {
                // Print spaces
                self.inst_sink().push(Inst::IPush(b' ' as i32));
                self.inst_sink().push(Inst::CPrint);
            }
            let typ = self.gen_expr(val.cp(), scope.cp())?;
            let typ_borrow = typ.borrow();
            match &*typ_borrow {
                ast::TypeDef::Primitive(p) => match p.var {
                    ast::PrimitiveTypeVar::Float => self.inst_sink().push(Inst::DPrint),
                    ast::PrimitiveTypeVar::UnsignedInt => {
                        if p.occupy_bytes == 1 {
                            // Char
                            self.inst_sink().push(Inst::CPrint)
                        } else {
                            self.inst_sink().push(Inst::IPrint)
                        }
                    }
                    ast::PrimitiveTypeVar::SignedInt => self.inst_sink().push(Inst::IPrint),
                },
                ast::TypeDef::Ref(..) => {
                    // ! For now we assume all ref types are strings. To be changed. Maybe.
                    self.inst_sink().push(Inst::SPrint)
                }
                _ => Err(CompileError::RequirePrintable(format!("{:?}", typ)))?,
            }
        }

        self.inst_sink().push(Inst::PrintLn);
        Ok(())
    }

    fn gen_return(
        &mut self,
        ret_expr: &Option<Ptr<ast::Expr>>,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<()> {
        if let Some(e) = ret_expr {
            // TODO: Check if every branch returns
            if self.ret_type.borrow().is_unit() {
                return Err(CompileError::ReturnTypeMismatch(format!(
                    "{:?}",
                    self.ret_type.borrow()
                )));
            }
            // * Non-void return:

            let expr_typ = self.gen_expr(e.cp(), scope.cp())?;
            let typ = conv(expr_typ, self.ret_type.cp(), self.inst_sink())?;
            ret(typ, self.inst_sink())?;
            Ok(())
        } else {
            // * void return
            if !self.ret_type.borrow().is_unit() {
                return Err(CompileError::ReturnTypeMismatch(format!(
                    "{:?}",
                    self.ret_type.borrow()
                )));
            }
            self.inst_sink().push(Inst::Ret);
            Ok(())
        }
    }
}

impl ast::OpVar {
    pub(super) fn inst(&self, sink: &mut InstSink, typ: Type) -> CompileResult<()> {
        use ast::OpVar::*;
        use Inst::*;

        let emit_double_inst = match &*typ.borrow() {
            ast::TypeDef::Primitive(p) => match p.var {
                ast::PrimitiveTypeVar::Float => true,
                _ => false,
            },
            _ => false,
        };

        if !emit_double_inst {
            // Integer instructions
            match self {
                // Binary
                Add => sink.push(IAdd),
                Sub => sink.push(ISub),
                Mul => sink.push(IMul),
                Div => sink.push(IDiv),

                /*
                 * Workaround instructions for comparison ops:
                 *
                 * Eq: Cmp, Imm 2, ISub
                 * NEq: Cmp
                 * Gt: Cmp, Imm 1, ISub, Imm 0, Cmp, Imm 2, ISub
                 * NGt: Cmp, Imm 1, ISub
                 * Lt: Cmp, Imm 1, IAdd, Imm 0, Cmp, Imm 2, ISub
                 * NLt: Cmp, Imm 1, IAdd
                 *
                 * Should be recognized and replaced in conditionals
                 */
                Eq => sink.push_many(&[ICmp, IPush(2), ISub]),
                Neq => sink.push_many(&[ICmp]),
                Gt => sink.push_many(&[ICmp, IPush(1), ISub, IPush(0), ICmp, IPush(2), ISub]),
                Lt => sink.push_many(&[ICmp, IPush(1), IAdd, IPush(0), ICmp, IPush(2), ISub]),
                Gte => sink.push_many(&[ICmp, IPush(1), IAdd]),
                Lte => sink.push_many(&[ICmp, IPush(1), ISub]),

                Neg => sink.push(INeg),
                Pos => (),

                Inv | Bin | Ref | Der | And | Or | Xor | Ban | Bor => {
                    Err(CompileError::UnsupportedOp)?
                }
                _Asn | _Csn => Err(CompileError::InternalError(
                    "Assign operators should be spotted early".into(),
                ))?,

                Ina | Inb | Dea | Deb | _ => Err(CompileError::UnsupportedOp)?,
            }
        } else {
            // Double instructions
            match self {
                // Binary
                Add => sink.push(DAdd),
                Sub => sink.push(DSub),
                Mul => sink.push(DMul),
                Div => sink.push(DDiv),

                Eq => sink.push_many(&[DCmp, IPush(2), ISub]),
                Neq => sink.push_many(&[DCmp]),
                Gt => sink.push_many(&[DCmp, IPush(1), ISub, IPush(0), ICmp, IPush(2), ISub]),
                Lt => sink.push_many(&[DCmp, IPush(1), IAdd, IPush(0), ICmp, IPush(2), ISub]),
                Gte => sink.push_many(&[DCmp, IPush(1), IAdd]),
                Lte => sink.push_many(&[DCmp, IPush(1), ISub]),

                Neg => sink.push(DNeg),
                Pos => (),

                Inv | Bin | Ref | Der | And | Or | Xor | Ban | Bor => {
                    Err(CompileError::UnsupportedOp)?
                }
                _Asn | _Csn => Err(CompileError::InternalError(
                    "Assign operators should be spotted early".into(),
                ))?,

                Ina | Inb | Dea | Deb | _ => Err(CompileError::UnsupportedOp)?,
            }
        }
        Ok(())
    }
}

impl ast::TypeDef {
    /// Calculate the bytes one type occupy
    ///
    /// We don't have Sized trait, but we can still calculate the bytes types occupy
    pub fn occupy_slots(&self) -> Option<u32> {
        match self {
            ast::TypeDef::Unit => Some(0),
            ast::TypeDef::Ref(..) => Some(1),
            ast::TypeDef::Array(a) => a.length.and_then(|l| {
                (a.target
                    .borrow()
                    .occupy_slots()
                    .map(|s| (s * l as u32) as u32))
            }),
            ast::TypeDef::Function(..) => None,
            ast::TypeDef::NamedType(..) => None,
            ast::TypeDef::Primitive(p) => Some(((p.occupy_bytes + 3) / 4) as u32),
            _ => None,
        }
    }
}
