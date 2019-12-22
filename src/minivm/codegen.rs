use super::err::*;
use super::*;
use crate::c0::ast::{self, *};
use crate::prelude::*;
use either::Either;
use indexmap::{map::Entry, IndexMap};

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

    pub fn get_offset(&self, name: &str) -> Option<u16> {
        self.map.get_full(name).map(|x| x.0 as u16)
    }

    pub fn get_data(&self, name: &str) -> Option<&Data> {
        self.map.get(name)
    }
}

#[derive(Debug, Clone)]
pub(super) struct FunctionType {
    pub params: Vec<Ptr<TypeDef>>,
    pub return_type: Ptr<TypeDef>,
    pub body: Option<InstSink>,
    pub is_extern: bool,
}

impl From<ast::FunctionType> for FunctionType {
    fn from(t: ast::FunctionType) -> Self {
        FunctionType {
            params: t.params,
            return_type: t.return_type,
            body: None,
            is_extern: t.is_extern,
        }
    }
}

#[derive(Debug, Clone)]
struct GlobalData {
    vars: DataSink,
    consts: DataSink,
    fns: IndexMap<String, FunctionType>,
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

    pub fn reset(&mut self) {
        self.0.clear()
    }

    pub fn reset_self(mut self) -> Self {
        self.reset();
        self
    }
}

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

    pub fn compile(&mut self) {}

    fn add_glob(&mut self) {}

    fn compile_fn(&mut self, func: &ast::FunctionType, name: &str) -> CompileResult<()> {
        // TODO: Add signature extractor

        // let sig = FnCodegen::extract_sig(func, self);
        let fnc = FnCodegen::new(func, name, self);

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

pub(super) struct FnCodegen<'a, 'b> {
    f: &'b ast::FunctionType,
    name: &'b str,
    // ctx: &'b mut Codegen<'a, T>,
    ebb_cnt: u32,
    loc_cnt: u16,
    data: &'b mut GlobalData,
    loc: DataSink,

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
    ) -> FnCodegen<'a, 'b> {
        FnCodegen {
            f,
            name,
            ebb_cnt: 0,
            loc_cnt: 0,
            data: &mut ctx.glob,
            loc: DataSink::new(),
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

    pub fn gen(&mut self) {
        if let Some(b) = &self.f.body {
            let scope = b.scope.cp();
            let stmts = &b.stmts;
            for stmt in stmts {
                self.gen_stmt(stmt, scope.cp());
            }
        } else {
            if self.f.is_extern {
                return;
            } else {
                panic!("No body in function")
            }
        }
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
                    let var_loc = self.loc_cnt;
                    self.loc_cnt = self.loc_cnt + 1;

                    let occupy_slots = typ
                        .borrow()
                        .occupy_slots()
                        .ok_or(CompileError::RequireSized(format!("{:?}", typ)))?;

                    let data = Data {
                        typ: typ.cp(),
                        init_val: Either::Right(occupy_slots),
                        is_const: *is_const,
                    };
                    self.loc
                        .put_data(&var_name, data)
                        .ok_or(CompileError::InternalError(
                            "Conflicting name inside AST".into(),
                        ))?;
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
            ast::StmtVariant::Expr(e) => self.gen_expr(e.cp(), scope.cp()).map(|_| ()),
            ast::StmtVariant::ManyExpr(e) => {
                for e in e {
                    self.gen_expr(e.cp(), scope.cp())?;
                }
                Ok(())
            }
            ast::StmtVariant::Return(e) => todo!("Generate code for return"),
            ast::StmtVariant::Block(e) => todo!("Generate code for block"),
            ast::StmtVariant::Print(e) => todo!("Generate code for print"),
            ast::StmtVariant::Scan(e) => todo!("Generate code for scan"),
            ast::StmtVariant::Break => todo!("Generate code for return"),
            ast::StmtVariant::If(e) => todo!("Generate code for return`"),
            ast::StmtVariant::While(e) => todo!("Generate code for ret`urn"),
            ast::StmtVariant::Empty => Ok(()),
        }
    }

    fn gen_l_value_store(
        &mut self,
        expr: Ptr<ast::Expr>,
        is_const_storage: bool,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<Type> {
        let expr = expr.borrow();
        let expr = &*expr;

        match &expr.var {
            ast::ExprVariant::Ident(i) => {}
            _ => Err(CompileError::NotLValue(format!("{}", expr)))?,
        }

        todo!()
    }

    fn gen_expr(&mut self, expr: Ptr<ast::Expr>, scope: Ptr<ast::Scope>) -> CompileResult<Type> {
        let expr = expr.borrow();
        let expr = &*expr;
        match &expr.var {
            ast::ExprVariant::BinaryOp(b) => {
                if b.op == ast::OpVar::_Asn || b.op == ast::OpVar::_Csn {
                    // It's an assignment
                    // * This generates storage procedure for lhs.
                    self.inst.push(self.sink_pool.get());
                    let lhs =
                        self.gen_l_value_store(b.lhs.cp(), b.op == ast::OpVar::_Csn, scope.cp())?;
                    let mut lhs_store = self.inst.pop().unwrap();

                    self.inst.push(self.sink_pool.get());
                    let rhs = self.gen_expr(b.rhs.cp(), scope.cp())?;
                    let mut rhs_op = self.inst.pop().unwrap();

                    let _ = self.conv(rhs, lhs, &mut rhs_op)?;

                    self.inst_sink().append_all(&mut rhs_op);
                    self.inst_sink().append_all(&mut lhs_store);

                    // Put sinks back to pool
                    self.sink_pool.put(lhs_store);
                    self.sink_pool.put(rhs_op);

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

                    let typ = self.flatten_ty(lhs, &mut lhs_op, rhs, &mut rhs_op)?;

                    self.inst_sink().append_all(&mut lhs_op);
                    self.inst_sink().append_all(&mut rhs_op);

                    b.op.inst(self.inst_sink(), typ.cp())?;

                    self.sink_pool.put(lhs_op);
                    self.sink_pool.put(rhs_op);

                    Ok(typ)
                }
            }

            ast::ExprVariant::UnaryOp(u) => {
                // TODO
                todo!("Implement unary operators")
            }

            ast::ExprVariant::Ident(i) => {
                todo!()
                // let Type = self.loc.get(&i.name).unwrap();
                // let val = self.builder.use_var(Type.0);
                // let typ = Type.1.cp();
                // Ok(Type(val, typ))
            }

            ast::ExprVariant::FunctionCall(f) => {
                // TODO
                todo!("Implement function calls")
            }

            ast::ExprVariant::Literal(lit) => match lit {
                ast::Literal::Boolean { val } => {
                    // let val = self.builder.ins().bconst(types::B8, *val);
                    let typ = Ptr::new(ast::TypeDef::Primitive(ast::PrimitiveType {
                        var: ast::PrimitiveTypeVar::UnsignedInt,
                        occupy_bytes: 1,
                    }));
                    Ok(typ)
                }

                ast::Literal::Integer { val } => {
                    let l = type_bits(val.bit_length()).unwrap();
                    // let val = self
                    //     .builder
                    //     .ins()
                    //     .iconst(Type::int(l).unwrap(), Imm64::new(val.into()));
                    let typ = Ptr::new(ast::TypeDef::Primitive(ast::PrimitiveType {
                        var: ast::PrimitiveTypeVar::UnsignedInt,
                        occupy_bytes: (l / 8) as usize,
                    }));
                    Ok(typ)
                }

                ast::Literal::Float { val } => {
                    todo!();
                    // let val = self.builder.ins().f64const(val.to_f64());
                    let typ = Ptr::new(ast::TypeDef::Primitive(ast::PrimitiveType {
                        var: ast::PrimitiveTypeVar::Float,
                        occupy_bytes: 8,
                    }));
                    Ok(typ)
                }

                ast::Literal::String { val } => {
                    // let data_name = format!("data`{}`{}", self.name, self.dat_cnt);
                    // let data = self.module.declare_data(
                    //     &data_name,
                    //     cranelift_module::Linkage::Local,
                    //     false,
                    //     None,
                    // )?;
                    // self.dat_cnt += 1;
                    // let mut data_ctx = DataContext::new();
                    // let str_val: Vec<_> = val.as_bytes().iter().map(|x| *x).collect();
                    // let str_val = std::ffi::CString::new(str_val).unwrap();
                    // let str_val = str_val.into_bytes_with_nul();
                    // let str_val = str_val.into_boxed_slice();
                    // data_ctx.define(str_val);
                    // self.module.define_data(data, &data_ctx)?;
                    // let global = self.module.declare_data_in_func(data, self.builder.func);
                    // let ptr_typ = self.module.isa().pointer_type();

                    // let val = self.builder.ins().global_value(ptr_typ, global);
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
            },
            _ => todo!("Implement other expression variants"),
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
    pub fn occupy_slots(&self) -> Option<u16> {
        match self {
            ast::TypeDef::Unit => Some(0),
            ast::TypeDef::Ref(..) => Some(1),
            ast::TypeDef::Array(a) => a.length.and_then(|l| {
                (a.target
                    .borrow()
                    .occupy_slots()
                    .map(|s| (s * l as u16) as u16))
            }),
            ast::TypeDef::Function(..) => None,
            ast::TypeDef::NamedType(..) => None,
            ast::TypeDef::Primitive(p) => Some(((p.occupy_bytes + 3) / 4) as u16),
            _ => None,
        }
    }
}
