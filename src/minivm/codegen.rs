use super::err::*;
use super::instgen::*;
use super::*;
use crate::c0::ast::{self, *};
use crate::prelude::*;
use either::Either;
use indexmap::{map::Entry, IndexMap, IndexSet};
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

type BB = Ptr<BasicBlock>;

#[derive(Debug, Clone)]
pub(super) struct BasicBlock {
    pub id: usize,
    pub inst: InstSink,
    pub end: BlockEndJump,
}

impl BasicBlock {
    pub fn end_len(&self) -> Option<u16> {
        match self.end {
            // Jmp(..)
            BlockEndJump::Unconditional(..) => Some(1),
            // Jnz(..), Jmp(..)
            BlockEndJump::Conditional { .. } => Some(2),
            // We assume the return instruction is already inserted
            BlockEndJump::Return => Some(0),
            BlockEndJump::Unknown => None,
        }
    }

    pub fn len(&self) -> usize {
        self.inst.len()
    }
}

#[derive(Debug, Clone)]
pub(super) enum BlockEndJump {
    Unknown,
    // We assume the return instruction is already inserted
    Return,
    Unconditional(usize),
    Conditional { z: usize, nz: usize },
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
    pub vars: LocalVars,
    pub consts: DataSink,
    pub fns: IndexMap<String, FunctionType>,
}

impl GlobalData {
    pub fn new() -> GlobalData {
        GlobalData {
            vars: LocalVars::new(),
            consts: DataSink::new(),
            fns: IndexMap::new(),
        }
    }
}

pub type Type = Ptr<ast::TypeDef>;

/// An opaque sink of instructions
#[derive(Debug, Clone)]
pub(super) struct InstSink(pub Vec<Inst>);

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
        let decls = &self.prog.blk.scope;
        let decls = &*decls.borrow();

        for item in decls.defs.iter() {
            let name = item.0;
            let def = item.1.borrow();
            if let ast::SymbolDef::Var { typ, .. } = &*def {
                let typ = typ.borrow();
                if let ast::TypeDef::Function(f) = &*typ {
                    self.add_fn(f, name)?;
                } else {
                    // ...
                }
            }
        }

        let start_code = self.make_start()?;

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

    fn make_start(&mut self) -> CompileResult<InstSink> {
        let prog = &self.prog.blk;
        let name = "_start";
        let ret = Ptr::new(ast::TypeDef::Unit);
        let params = Vec::new();
        let mut fnc = FnCodegen::new(prog, name, self, ret, params);

        fnc.gen()?;
        let (mut start_code, loc) = fnc.finish_with_loc()?;
        self.glob.vars = loc;
        start_code.pop();
        Ok(start_code)
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
                self.prog.blk.scope.cp(),
            ));

            let params: Vec<_> = func
                .params
                .iter()
                .map(|i| Ptr::new(resolve_ty(&*i.borrow(), self.prog.blk.scope.cp())))
                .collect();

            let param_siz =
                params
                    .iter()
                    .try_fold::<u32, _, CompileResult<u32>>(0, |sum, item| {
                        let item_size = item
                            .borrow()
                            .occupy_slots()
                            .ok_or(compile_err_n(CompileErrorVar::RequireSized("".into())))?;
                        Ok(item_size + sum)
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
            Err(CompileErrorVar::NoExternFunction(name.into()).into())
        }
    }

    /// Compile and repair the function declaration in `self.glob`
    fn compile_fn(&mut self, func: &ast::FunctionType, name: &str) -> CompileResult<()> {
        // Get the function. Things can't go wrong here right?
        let fn_ref = self.glob.fns.get(name).unwrap();

        let ret = fn_ref.return_type.cp();
        let params = fn_ref.params.iter().map(|x| x.cp()).collect();
        // * Return fn_ref so that we can borrow self for function compilation

        if let Some(b) = &func.body {
            let mut fnc = FnCodegen::new(b, name, self, ret, params);

            fnc.gen()?;
            let inst = fnc.finish()?;

            // * We're done here. Add the instructions
            let fn_ref = self.glob.fns.get_mut(name).unwrap();

            fn_ref.body = Some(inst);

            Ok(())
        } else {
            Err(CompileErrorVar::FunctionMissingBody(name.into()).into())
        }
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
                Err(CompileErrorVar::InternalError(
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
    f: &'b ast::Block,
    ret_type: Type,
    params: Vec<Type>,
    param_siz: u32,

    name: &'b str,

    break_tgt: Vec<usize>,

    /// Data count, only for naming usage
    data_cnt: u32,
    data: &'b mut GlobalData,
    loc: LocalVars,

    inst: Option<&'a mut InstSink>,
    sink_pool: DeqPool<'a, InstSink>,

    start_bb: BB,
    bbs: Vec<BB>,
}

/// Implementation for larger function, statement and expression structures
impl<'a, 'b> FnCodegen<'a, 'b> {
    pub fn new<'c>(
        f: &'b ast::Block,
        name: &'b str,
        ctx: &'b mut Codegen<'c>,
        ret_type: Type,
        params: Vec<Type>,
    ) -> FnCodegen<'a, 'b> {
        let start_bb = Ptr::new(BasicBlock {
            id: 0,
            inst: InstSink::new(),
            end: BlockEndJump::Unknown,
        });

        FnCodegen {
            f,
            name,
            ret_type,
            params,
            param_siz: 0,
            data_cnt: 0,
            break_tgt: vec![],
            data: &mut ctx.glob,
            loc: LocalVars::new(),
            // module: &mut ctx.module,,
            inst: None,
            sink_pool: DeqPool::new_with_reset(&InstSink::new, &InstSink::reset),
            start_bb: start_bb.cp(),
            bbs: vec![start_bb],
        }
    }

    pub(super) fn inst_sink(&mut self) -> &mut InstSink {
        self.inst.as_mut().unwrap()
    }

    pub fn gen(&mut self) -> CompileResult<()> {
        let b = self.f;
        self.param_siz =
            self.params
                .iter()
                .try_fold::<u32, _, CompileResult<u32>>(0, |sum, item| {
                    Ok(item
                        .borrow()
                        .occupy_slots()
                        .ok_or(CompileErrorVar::RequireSized("".into()))?
                        + sum)
                })?;

        self.gen_scope(b, self.start_bb.cp(), b.scope.cp())?;

        // Calculate local variable size
        {
            let stack_size = self.loc.max_stack_size();

            log::info!(
                "The function has max stack size of {} slots, of which {} are params.",
                stack_size,
                self.param_siz
            );
            let param_siz = self.param_siz;
            self.start_bb
                .borrow_mut()
                .inst
                .prepend(Inst::SNew(stack_size - param_siz));
        }

        Ok(())
    }

    pub fn finish_with_loc(mut self) -> CompileResult<(InstSink, LocalVars)> {
        let inst = self.finish().map_err(|e| {
            if let Some(span) = self.f.span {
                e.with_span(span)
            } else {
                e
            }
        })?;
        let loc = self.loc;
        Ok((inst, loc))
    }

    pub fn finish(&mut self) -> CompileResult<InstSink> {
        log::debug!("Finished compiling. function is {:#?}", &self.bbs);

        let mut bb_start: IndexMap<usize, usize> = IndexMap::new();
        let mut bb_length: IndexMap<usize, usize> = IndexMap::new();
        let mut finished_bb: IndexSet<usize> = IndexSet::new();
        let mut inst = InstSink::new();
        let mut pending_bb = std::collections::VecDeque::new();
        pending_bb.push_back(0);

        while pending_bb.len() != 0 {
            let bb_id = pending_bb.pop_back().unwrap();
            let bb = self.bbs.get(bb_id).unwrap();
            let mut bb_mut = bb.borrow_mut();

            log::info!("Parsing BB {}", bb_id);
            if !bb_start.contains_key(&bb_id) {
                log::debug!("BB is not seen before");
                // * Brand new basic block
                bb_start.insert(bb_mut.id, inst.len());
                bb_length.insert(bb_mut.id, bb_mut.len());
                inst.append_all(&mut bb_mut.inst);
                match bb_mut.end {
                    BlockEndJump::Conditional { z, nz } => {
                        log::debug!("BB: Conditional z {} nz {}", z, nz);
                        // * To be replaced with `JNz(nz)`
                        inst.push(Inst::Nop);
                        // * To be replaced with `Jmp(z)`
                        inst.push(Inst::Nop);

                        pending_bb.push_back(bb_id);
                        pending_bb.push_back(z);
                        pending_bb.push_back(nz);
                    }
                    BlockEndJump::Unconditional(z) => {
                        // * To be replaced with `Jmp(z)`
                        log::info!("BB: Unconditional z {}", z);
                        inst.push(Inst::Nop);

                        pending_bb.push_back(bb_id);
                        pending_bb.push_back(z);
                    }
                    BlockEndJump::Return => {
                        // * Already finished because BB does not link to another
                        log::info!("BB: Return",);
                        finished_bb.insert(bb_id);
                    }
                    BlockEndJump::Unknown => {
                        log::info!("BB: Unknown",);
                        if self.ret_type.borrow().is_unit() {
                            // * Unit return type. Manually add `ret` here. Same as above.
                            inst.push(Inst::Ret);
                            finished_bb.insert(bb_id);
                        } else {
                            // * Hey, your favorite error message!
                            return Err(compile_err(
                                CompileErrorVar::ControlReachesEndOfNonVoidFunction,
                                self.f.span,
                            ));
                        }
                    }
                }
            } else if !finished_bb.contains(&bb_id) {
                // * Basic block that has its decendants resolved
                log::debug!("BB has seen before");
                match bb_mut.end {
                    BlockEndJump::Conditional { z, nz } => {
                        let nz_place =
                            *bb_start.get(&bb_id).unwrap() + *bb_length.get(&bb_id).unwrap();

                        let mut not_finished = false;
                        // Replace nop with `JNz(nz)`
                        if bb_start.contains_key(&nz) {
                            let replace_nz = inst.0.get_mut(nz_place).unwrap();
                            *replace_nz = Inst::JNe(*bb_start.get(&nz).unwrap() as u16);
                        } else {
                            // No luck. Try again later!
                            not_finished = not_finished || true;
                            pending_bb.push_front(bb_id);
                            pending_bb.push_back(nz);
                            log::debug!("BB has no nz. Waiting.");
                        }

                        // Replace nop with `Jmp(z)`
                        if bb_start.contains_key(&z) {
                            let replace_z = inst.0.get_mut(nz_place + 1).unwrap();
                            *replace_z = Inst::Jmp(*bb_start.get(&z).unwrap() as u16);
                        } else {
                            not_finished = not_finished || true;
                            pending_bb.push_front(bb_id);
                            pending_bb.push_back(z);
                            log::debug!("BB has no z. Waiting.");
                        }

                        if !not_finished {
                            finished_bb.insert(bb_id);
                        }
                    }
                    BlockEndJump::Unconditional(z) => {
                        let z_place =
                            *bb_start.get(&bb_id).unwrap() + *bb_length.get(&bb_id).unwrap();

                        if bb_start.contains_key(&z) {
                            // Replace nop with `Jmp(nz)`
                            let replace_nz = inst.0.get_mut(z_place).unwrap();
                            *replace_nz = Inst::Jmp(*bb_start.get(&z).unwrap() as u16);
                            finished_bb.insert(bb_id);
                        } else {
                            pending_bb.push_front(bb_id);
                        }
                    }
                    _ => {} // Already finished!
                }
            } else {
                log::debug!("BB is finished");
            }
        }

        Ok(inst)
    }

    pub(super) fn new_bb(&mut self) -> (usize, BB) {
        let bb_id = self.bbs.len();
        let bb = Ptr::new(BasicBlock {
            id: bb_id,
            inst: InstSink::new(),
            end: BlockEndJump::Unknown,
        });
        self.bbs.push(bb.cp());
        (bb_id, bb)
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
            ast::SymbolDef::Typ { .. } => Ok(()),

            // Variable decl
            ast::SymbolDef::Var {
                typ,
                is_const,
                decl_span,
            } => {
                // if id != 0 {
                // Who cares about constants?
                // * This function does not care about where this variable is declared
                let var_name = format!("{}`{}", name, id);

                let typ = resolve_ty(&*typ.borrow(), scope);
                if !typ.is_fn() && !typ.is_unit() {
                    let occupy_slots = typ
                        .occupy_slots()
                        .ok_or(CompileErrorVar::RequireSized(format!("{:?}", typ)))?;

                    self.loc
                        .add_var(&var_name, occupy_slots, *is_const, Ptr::new(typ))?;

                    Ok(())
                } else if typ.is_unit() {
                    Err(compile_err(
                        CompileErrorVar::VoidVariable(name.into()),
                        Some(*decl_span),
                    ))
                } else if typ.is_fn() && self.f.scope.borrow().id != 0 {
                    Err(compile_err(
                        CompileErrorVar::NestedFunctions(name.into()),
                        Some(*decl_span),
                    ))
                } else {
                    Ok(())
                }
                // } else {
                //     s
                // }
            }
        }
    }

    fn gen_stmt(&mut self, stmt: &ast::Stmt, bb: BB, scope: Ptr<ast::Scope>) -> CompileResult<BB> {
        match &stmt.var {
            ast::StmtVariant::Expr(e) => {
                {
                    let inst = &mut bb.borrow_mut().inst;

                    let typ = self.gen_expr(e.cp(), inst, scope.cp())?;
                    if !typ.borrow().is_unit() {
                        pop(typ.cp(), inst)?;
                    }
                }
                Ok(bb)
            }
            ast::StmtVariant::ManyExpr(e) => {
                {
                    let inst = &mut bb.borrow_mut().inst;

                    for e in e {
                        let typ = self.gen_expr(e.cp(), inst, scope.cp())?;
                        if !typ.borrow().is_unit() {
                            pop(typ.cp(), inst)?;
                        }
                    }
                }

                Ok(bb)
            }
            ast::StmtVariant::Return(e) => self.gen_return(e, bb, scope),
            ast::StmtVariant::Block(e) => self.gen_scope(e, bb, scope),
            ast::StmtVariant::Print(e) => self.gen_print(e, bb, scope),
            ast::StmtVariant::Scan(e) => self.gen_scan(e, bb, scope),
            ast::StmtVariant::Break => self.gen_break(bb, scope),
            ast::StmtVariant::If(e) => self.gen_if(e, bb, scope),
            ast::StmtVariant::While(e) => self.gen_while(e, bb, scope),
            ast::StmtVariant::Empty => Ok(bb),
        }
        .with_span(stmt.span)
    }

    fn gen_expr(
        &mut self,
        expr: Ptr<ast::Expr>,
        inst: &mut InstSink,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<Type> {
        let expr = expr.borrow();
        let expr = &*expr;
        match &expr.var {
            ast::ExprVariant::BinaryOp(b) => self.gen_bin_op(b, inst, scope),
            ast::ExprVariant::UnaryOp(u) => self.gen_una_op(u, inst, scope),
            ast::ExprVariant::Ident(i) => self.gen_ident_expr(i, inst, scope),
            ast::ExprVariant::FunctionCall(f) => self.gen_func_call(f, inst, scope),
            ast::ExprVariant::Literal(lit) => self.gen_literal(lit, inst, scope),
            ast::ExprVariant::TypeConversion(ty) => self.gen_ty_conversion(ty, inst, scope),
            _ => Err(
                CompileErrorVar::NotImplemented("Implement other expression variants".into())
                    .into(),
            ),
        }
        .with_span(expr.span)
    }

    fn gen_scope(
        &mut self,
        block: &ast::Block,
        bb: BB,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<BB> {
        self.loc.dive_into_scope();

        let scope = block.scope.cp();
        let defs = scope.borrow();
        for local in &defs.defs {
            self.add_local(&local.0, &*local.1.borrow(), defs.id, scope.cp())?;
        }

        let stmts = &block.stmts;
        let mut bb = bb;
        for stmt in stmts {
            bb = self.gen_stmt(stmt, bb, scope.cp())?;
        }

        self.loc.pop_scope();
        Ok(bb)
    }

    fn gen_ident_address_and_const(
        &mut self,
        i: &ast::Identifier,
        inst: &mut InstSink,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<(Type, bool)> {
        let def = scope.borrow().find_def_depth(&i.name).unwrap();

        // Global var in global scope is also local var
        let global_scope = self.f.scope.borrow().id == 0;
        let is_local_var = def.1 != 0 || global_scope;

        if is_local_var {
            // Local variable
            let loc = self.loc.get_var(&format!("{}`{}", i.name, def.1)).ok_or(
                CompileErrorVar::Error(format!("Unable to find local identifier {}", i.name)),
            )?;
            let typ = loc.typ.cp();
            let offset = loc.offset as i32;
            inst.push(Inst::LoadA(0, offset));
            Ok((typ, loc.is_const))
        } else {
            // Global variable
            let glob = self
                .data
                .vars
                .get_var(&format!("{}`{}", i.name, def.1))
                .ok_or(CompileErrorVar::Error(format!(
                    "Unable to find global identifier {}",
                    i.name
                )))?;
            let typ = glob.typ.cp();
            let offset = glob.offset as i32;
            inst.push(Inst::LoadA(1, offset));
            Ok((typ, glob.is_const))
        }
    }

    fn gen_l_value_address(
        &mut self,
        expr: Ptr<ast::Expr>,
        inst: &mut InstSink,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<Type> {
        let expr = expr.borrow();
        let expr = &*expr;

        match &expr.var {
            ast::ExprVariant::Ident(i) => Ok(self.gen_ident_address_and_const(i, inst, scope)?.0),
            _ => Err(CompileErrorVar::NotLValue(format!("{}", expr))).with_span(expr.span),
        }
    }

    fn gen_l_value_address_and_const(
        &mut self,
        expr: Ptr<ast::Expr>,
        inst: &mut InstSink,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<(Type, bool)> {
        let expr = expr.borrow();
        let expr = &*expr;

        match &expr.var {
            ast::ExprVariant::Ident(i) => self.gen_ident_address_and_const(i, inst, scope),
            _ => Err(CompileErrorVar::NotLValue(format!("{}", expr))).with_span(expr.span),
        }
    }

    fn gen_bin_op(
        &mut self,
        b: &ast::BinaryOp,
        inst: &mut InstSink,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<Type> {
        if b.op == ast::OpVar::_Asn || b.op == ast::OpVar::_Csn {
            // * This generates address for lhs.
            let (lhs, constance) =
                self.gen_l_value_address_and_const(b.lhs.cp(), inst, scope.cp())?;

            if constance && b.op != ast::OpVar::_Csn {
                return Err(compile_err_n(CompileErrorVar::AssignConst));
            }

            let rhs = self.gen_expr(b.rhs.cp(), inst, scope.cp())?;

            conv(rhs, lhs.cp(), inst)?;

            // store lhs
            store(lhs, inst)?;

            // * Assignment evaluates as unit type!
            Ok(Ptr::new(ast::TypeDef::Unit))
        } else {
            // Normal expressions
            let mut lhs_op = self.sink_pool.get();

            let lhs = self.gen_expr(b.lhs.cp(), &mut lhs_op, scope.cp())?;

            let mut rhs_op = self.sink_pool.get();
            let rhs = self.gen_expr(b.rhs.cp(), &mut rhs_op, scope.cp())?;

            let typ = flatten_ty(lhs, &mut lhs_op, rhs, &mut rhs_op)?;

            inst.append_all(&mut lhs_op);
            inst.append_all(&mut rhs_op);

            b.op.inst(inst, typ.cp())?;

            self.sink_pool.put(lhs_op);
            self.sink_pool.put(rhs_op);

            match b.op {
                ast::OpVar::Gt
                | ast::OpVar::Gte
                | ast::OpVar::Lt
                | ast::OpVar::Lte
                | ast::OpVar::Eq
                | ast::OpVar::Neq => Ok(Self::int_type(1)),
                _ => Ok(typ),
            }
        }
    }

    fn gen_una_op(
        &mut self,
        u: &ast::UnaryOp,
        inst: &mut InstSink,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<Type> {
        // Calculate expression body
        // self.inst.push(self.sink_pool.get());
        let lhs = self.gen_expr(u.val.cp(), inst, scope.cp())?;
        // let mut lhs_op = self.inst.pop().unwrap();

        u.op.inst(inst, lhs.cp())?;

        Ok(lhs)
    }

    fn gen_ident_expr(
        &mut self,
        i: &ast::Identifier,
        inst: &mut InstSink,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<Type> {
        let typ = self.gen_ident_address_and_const(i, inst, scope)?.0;
        load(typ.cp(), inst)?;
        Ok(typ)
    }

    fn gen_func_call(
        &mut self,
        f: &ast::FunctionCall,
        inst: &mut InstSink,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<Type> {
        let func = &f.func;
        let func_entry = self
            .data
            .fns
            .get_full(func)
            .ok_or_else(|| CompileErrorVar::NonExistFunc("Function does not exist".into()))?;

        let params = &func_entry.2.params;

        if f.params.len() != params.len() {
            return Err(CompileErrorVar::ParamLengthMismatch.into());
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
            let res = self.gen_expr(param.0.cp(), inst, scope.cp())?;
            conv(res, param.1.cp(), inst)?;
        }

        inst.push(Inst::Call(f_idx));

        Ok(f_ret_typ)
    }

    fn uint_type(bytes: usize) -> Type {
        Ptr::new(ast::TypeDef::Primitive(ast::PrimitiveType {
            var: ast::PrimitiveTypeVar::UnsignedInt,
            occupy_bytes: bytes,
        }))
    }

    fn int_type(bytes: usize) -> Type {
        Ptr::new(ast::TypeDef::Primitive(ast::PrimitiveType {
            var: ast::PrimitiveTypeVar::SignedInt,
            occupy_bytes: bytes,
        }))
    }

    fn float_type(bytes: usize) -> Type {
        Ptr::new(ast::TypeDef::Primitive(ast::PrimitiveType {
            var: ast::PrimitiveTypeVar::Float,
            occupy_bytes: bytes,
        }))
    }

    fn ref_type(typ: Type) -> Type {
        Ptr::new(ast::TypeDef::Ref(ast::RefType { target: typ }))
    }

    fn gen_literal(
        &mut self,
        lit: &ast::Literal,
        inst: &mut InstSink,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<Type> {
        match lit {
            ast::Literal::Boolean { val } => {
                inst.push(Inst::IPush(*val as i32));
                let typ = Self::int_type(1);
                Ok(typ)
            }

            ast::Literal::Integer { val } => {
                let val: i32 = val.try_into().map_err(|_| CompileErrorVar::IntOverflow)?;
                inst.push(Inst::IPush(val));

                let typ = Self::int_type(4);
                Ok(typ)
            }

            ast::Literal::Char { val } => {
                let val: i32 = *val as u32 as i32;
                inst.push(Inst::IPush(val));

                let typ = Self::uint_type(1);
                Ok(typ)
            }

            ast::Literal::Float { val } => {
                let typ = Self::float_type(8);

                let val: f64 = val.to_f64();
                let idx = self
                    .data
                    .consts
                    .put_data(
                        &format!("`{}``double{}", self.name, self.data_cnt),
                        Data {
                            typ: typ.cp(),
                            init_val: Either::Left(Constant::Float(val)),
                            is_const: true,
                        },
                    )
                    .expect("Unable to add double data");
                inst.push(Inst::LoadC(idx));
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
                inst.push(Inst::LoadC(offset));
                let typ = Self::ref_type(Self::uint_type(8));
                Ok(typ)
            }

            ast::Literal::Struct { .. } => {
                Err(CompileErrorVar::InternalError("Structs are not yet supported!".into()).into())
            }
        }
    }

    fn gen_ty_conversion(
        &mut self,
        i: &ast::TypeConversion,
        inst: &mut InstSink,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<Type> {
        let expr = i.expr.cp();
        let ty = i.to.cp();
        let ty = Ptr::new(resolve_ty(&*ty.borrow(), scope.cp()));

        let expr_ty = self.gen_expr(expr, inst, scope)?;

        conv(expr_ty, ty, inst)
    }

    fn gen_if(
        &mut self,
        i: &ast::IfConditional,
        bb: BB,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<BB> {
        {
            // Condition
            let cond = i.cond.cp();
            let inst = &mut bb.borrow_mut().inst;
            let cond_ty = self.gen_expr(cond, inst, scope.cp())?;
            conv(cond_ty, Self::int_type(1), inst)?;
        }
        // * True branch
        let (true_bb_id, true_bb) = self.new_bb();
        let true_bb = self.gen_stmt(&*i.if_block.borrow(), true_bb, scope.cp())?;

        if let Some(else_br) = &i.else_block {
            let (else_bb_id, else_bb) = self.new_bb();
            let else_bb = self.gen_stmt(&*else_br.borrow(), else_bb, scope.cp())?;

            let (final_bb_id, final_bb) = self.new_bb();

            bb.borrow_mut().end = BlockEndJump::Conditional {
                z: else_bb_id,
                nz: true_bb_id,
            };
            true_bb.borrow_mut().end = BlockEndJump::Unconditional(final_bb_id);
            else_bb.borrow_mut().end = BlockEndJump::Unconditional(final_bb_id);

            Ok(final_bb)
        } else {
            let (final_bb_id, final_bb) = self.new_bb();

            bb.borrow_mut().end = BlockEndJump::Conditional {
                z: final_bb_id,
                nz: true_bb_id,
            };
            true_bb.borrow_mut().end = BlockEndJump::Unconditional(final_bb_id);

            Ok(final_bb)
        }
    }

    fn gen_while(
        &mut self,
        i: &ast::WhileConditional,
        bb: BB,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<BB> {
        {
            // Condition
            let cond = i.cond.cp();
            let inst = &mut bb.borrow_mut().inst;
            let cond_ty = self.gen_expr(cond, inst, scope.cp())?;
            conv(cond_ty, Self::int_type(1), inst)?;
        }
        let (while_bb_id, while_bb) = self.new_bb();
        let (final_bb_id, final_bb) = self.new_bb();
        self.break_tgt.push(final_bb_id);
        let while_bb = self.gen_stmt(&*i.block.borrow(), while_bb, scope.cp())?;
        {
            // Condition
            let cond = i.cond.cp();
            let inst = &mut while_bb.borrow_mut().inst;
            let cond_ty = self.gen_expr(cond, inst, scope.cp())?;
            conv(cond_ty, Self::int_type(1), inst)?;
        }
        self.break_tgt.pop();
        {
            bb.borrow_mut().end = BlockEndJump::Conditional {
                z: final_bb_id,
                nz: while_bb_id,
            };
            while_bb.borrow_mut().end = BlockEndJump::Conditional {
                z: final_bb_id,
                nz: while_bb_id,
            };
        }
        Ok(final_bb)
    }

    fn gen_break(&mut self, bb: BB, _: Ptr<ast::Scope>) -> CompileResult<BB> {
        let break_tgt = *self
            .break_tgt
            .last()
            .ok_or(CompileErrorVar::NoTargetToBreak)?;
        let (_, dummy_bb) = self.new_bb();
        bb.borrow_mut().end = BlockEndJump::Unconditional(break_tgt);
        Ok(dummy_bb)
    }

    fn gen_scan(
        &mut self,
        scan: &ast::Identifier,
        bb: BB,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<BB> {
        {
            let inst = &mut bb.borrow_mut().inst;
            let typ = self.gen_ident_address_and_const(scan, inst, scope.cp())?.0;
            let typ_borrow = typ.borrow();
            match &*typ_borrow {
                ast::TypeDef::Primitive(p) => match p.var {
                    ast::PrimitiveTypeVar::Float => inst.push_many(&[Inst::DScan, Inst::DStore]),
                    ast::PrimitiveTypeVar::UnsignedInt => {
                        if p.occupy_bytes == 1 {
                            inst.push_many(&[Inst::CScan, Inst::IStore])
                        } else {
                            inst.push_many(&[Inst::IScan, Inst::IStore])
                        }
                    }
                    ast::PrimitiveTypeVar::SignedInt => {
                        inst.push_many(&[Inst::IScan, Inst::IStore])
                    }
                },
                _ => Err(CompileErrorVar::RequireScannable(format!(
                    "{:?}",
                    &*typ.borrow()
                )))?,
            }
        }
        Ok(bb)
    }

    fn gen_print(
        &mut self,
        print: &Vec<Ptr<Expr>>,
        bb: BB,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<BB> {
        {
            let inst = &mut bb.borrow_mut().inst;
            let mut is_first = true;
            for val in print {
                if is_first {
                    is_first = false;
                } else {
                    // Print spaces
                    inst.push(Inst::IPush(b' ' as i32));
                    inst.push(Inst::CPrint);
                }
                let typ = self.gen_expr(val.cp(), inst, scope.cp())?;
                let typ_borrow = typ.borrow();
                match &*typ_borrow {
                    ast::TypeDef::Primitive(p) => match p.var {
                        ast::PrimitiveTypeVar::Float => inst.push(Inst::DPrint),
                        ast::PrimitiveTypeVar::UnsignedInt => {
                            if p.occupy_bytes == 1 {
                                // Char
                                inst.push(Inst::CPrint)
                            } else {
                                inst.push(Inst::IPrint)
                            }
                        }
                        ast::PrimitiveTypeVar::SignedInt => inst.push(Inst::IPrint),
                    },
                    ast::TypeDef::Ref(..) => {
                        // ! For now we assume all ref types are strings. To be changed. Maybe.
                        inst.push(Inst::SPrint)
                    }
                    _ => Err(CompileErrorVar::RequirePrintable(format!("{:?}", typ)))?,
                }
            }

            inst.push(Inst::PrintLn);
        }
        Ok(bb)
    }

    fn gen_return(
        &mut self,
        ret_expr: &Option<Ptr<ast::Expr>>,
        bb: BB,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<BB> {
        if let Some(e) = ret_expr {
            // TODO: Check if every branch returns
            if self.ret_type.borrow().is_unit() {
                return Err(CompileErrorVar::ReturnTypeMismatch(format!(
                    "{:?}",
                    self.ret_type.borrow()
                ))
                .into());
            }
            // * Non-void return:
            let mut bb = bb.borrow_mut();
            let inst = &mut bb.inst;

            let expr_typ = self.gen_expr(e.cp(), inst, scope.cp())?;
            let typ = conv(expr_typ, self.ret_type.cp(), inst)?;
            ret(typ, inst)?;
            bb.end = BlockEndJump::Return;

            let (_, dummy_bb) = self.new_bb();
            Ok(dummy_bb)
        } else {
            // * void return
            if !self.ret_type.borrow().is_unit() {
                return Err(CompileErrorVar::ReturnTypeMismatch(format!(
                    "{:?}",
                    self.ret_type.borrow()
                ))
                .into());
            }
            let mut bb = bb.borrow_mut();
            bb.inst.push(Inst::Ret);
            bb.end = BlockEndJump::Return;

            let (_, dummy_bb) = self.new_bb();
            Ok(dummy_bb)
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
                Eq => sink.push_many(&[ICmp, Dup, IMul, IPush(1), ICmp]),
                Neq => sink.push_many(&[ICmp]),
                Gt => sink.push_many(&[ICmp, IPush(1), ISub, IPush(0), ICmp, IPush(-1), ICmp]),
                Lt => sink.push_many(&[ICmp, IPush(1), IAdd, IPush(0), ICmp, IPush(1), ICmp]),
                Gte => sink.push_many(&[ICmp, IPush(1), IAdd]),
                Lte => sink.push_many(&[ICmp, IPush(1), ISub]),

                Neg => sink.push(INeg),
                Pos => (),

                Inv | Bin | Ref | Der | And | Or | Xor | Ban | Bor => {
                    Err(CompileErrorVar::UnsupportedOp)?
                }
                _Asn | _Csn => Err(CompileErrorVar::InternalError(
                    "Assign operators should be spotted early".into(),
                ))?,

                Ina | Inb | Dea | Deb | _ => Err(CompileErrorVar::UnsupportedOp)?,
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
                Gt => sink.push_many(&[DCmp, IPush(1), ISub, IPush(0), ICmp, IPush(-1), ICmp]),
                Lt => sink.push_many(&[DCmp, IPush(1), IAdd, IPush(0), ICmp, IPush(1), ICmp]),
                Gte => sink.push_many(&[DCmp, IPush(1), IAdd]),
                Lte => sink.push_many(&[DCmp, IPush(1), ISub]),

                Neg => sink.push(DNeg),
                Pos => (),

                Inv | Bin | Ref | Der | And | Or | Xor | Ban | Bor => {
                    Err(CompileErrorVar::UnsupportedOp)?
                }
                _Asn | _Csn => Err(CompileErrorVar::InternalError(
                    "Assign operators should be spotted early".into(),
                ))?,

                Ina | Inb | Dea | Deb | _ => Err(CompileErrorVar::UnsupportedOp)?,
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
