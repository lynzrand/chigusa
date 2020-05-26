use crate::c0::ast;
use crate::minivm::{
    compile_err, compile_err_n, CompileError, CompileErrorVar, CompileResult, WithSpan,
};
use crate::mir;
use crate::prelude::*;
use either::Either;
use indexmap::IndexMap;
use log::*;
use std::collections::{BTreeMap, HashMap, HashSet};

#[derive(Debug)]
pub struct Codegen<'src> {
    src: &'src ast::Program,
    pkg: mir::MirPackage,
    global_var_counter: usize,
    global_var_names: HashMap<String, usize>,
    // ty_counter: usize,
    // ty_table: HashMap<usize, mir::Ty>,
}

impl<'src> Codegen<'src> {
    pub fn new(src: &'src ast::Program) -> Codegen<'src> {
        Codegen {
            src,
            pkg: mir::MirPackage {
                entry_point: usize::max_value(),
                global_var_table: IndexMap::new(),
                func_table: IndexMap::new(),
                static_values: IndexMap::new(),
            },
            global_var_counter: 0,
            global_var_names: HashMap::new(),
        }
    }

    pub fn gen(mut self) -> CompileResult<mir::MirPackage> {
        let decls = self.src.blk.scope.borrow();

        for (name, symbol) in decls.defs.iter() {
            let symbol = symbol.borrow();
            match &*symbol {
                ast::SymbolDef::Var {
                    typ,
                    is_const,
                    decl_span,
                } => {
                    let typ = typ.borrow();
                    self.add_global_var(name, &typ, *is_const, *decl_span)?;
                }
                ast::SymbolDef::Typ { .. } => {}
            }
        }

        for (name, symbol) in decls.defs.iter() {
            let symbol = symbol.borrow();
            match &*symbol {
                ast::SymbolDef::Var { typ, .. } => {
                    let typ = typ.borrow();
                    if let ast::TypeDef::Function(f) = &*typ {
                        self.gen_fn(name, f)?;
                    }
                }
                ast::SymbolDef::Typ { .. } => {}
            }
        }
        self.gen_start_fn(&self.src.blk)?;
        Ok(self.pkg)
    }

    fn add_global_var(
        &mut self,
        name: &str,
        typ: &ast::TypeDef,
        is_const: bool,
        decl_span: Span,
    ) -> CompileResult<()> {
        let var_id = self.global_var_counter;
        self.global_var_counter += 1;

        let var_ty = resolve_ty(typ, &self.src.blk.scope.borrow())?;
        self.global_var_names.insert(name.to_owned(), var_id);
        self.pkg.global_var_table.insert(
            var_id,
            mir::GlobalVar {
                ty: var_ty,
                name: name.to_owned().into(),
                binary_value: None,
            },
        );
        Ok(())
    }

    fn gen_start_fn(&mut self, blk: &ast::Block) -> CompileResult<()> {
        let entry_point_id = self.global_var_counter;
        self.global_var_counter += 1;

        let entry_point_ty = mir::Ty::function_of(mir::Ty::Void, [], false);

        let entry_point_fn = ast::FunctionType {
            params: [].into(),
            return_type: Ptr::new(ast::TypeDef::Unit),
            // TODO: reduce clone
            body: Some(blk.clone()),
            is_extern: false,
        };

        let entry_point = self.gen_non_extern_fn("_start", &entry_point_fn)?;

        self.pkg.global_var_table.insert(
            entry_point_id,
            mir::GlobalVar {
                ty: entry_point_ty,
                name: "_start".to_owned().into(),
                binary_value: None,
            },
        );
        self.pkg.func_table.insert(entry_point_id, entry_point);
        self.pkg.entry_point = entry_point_id;

        Ok(())
    }

    fn gen_fn(&mut self, name: &str, func: &ast::FunctionType) -> CompileResult<()> {
        if func.is_extern {
            assert!(func.body.is_none());
            self.gen_extern_fn(name, func)
        } else {
            assert!(func.body.is_some());
            let f = self.gen_non_extern_fn(name, func)?;
            self.pkg
                .func_table
                .insert(*self.global_var_names.get(name).unwrap(), f);
            Ok(())
        }
    }

    fn gen_extern_fn(&mut self, name: &str, func: &ast::FunctionType) -> CompileResult<()> {
        // TODO: noop?
        Ok(())
    }

    fn gen_non_extern_fn(
        &mut self,
        name: &str,
        func: &ast::FunctionType,
    ) -> CompileResult<mir::Func> {
        let fn_codegen: FnCodegen = FnCodegen::new(
            name,
            func,
            self.src.blk.scope.clone(),
            &mut self.pkg,
            &self.global_var_names,
            &mut self.global_var_counter,
        );
        fn_codegen.gen()
    }
}

fn format_ident(name: &str, depth: usize) -> String {
    format!("{}`{}", name, depth)
}

macro_rules! return_as_is {
    ($func:ident, $param:expr) => {
        if $param.$func() {
            return Some($param.clone());
        } else {
            return None;
        }
    };
}
macro_rules! return_bool {
    ($func:ident, $param:expr) => {{
        use $crate::mir::Ty;
        if $param.$func() {
            return Some(Ty::bool());
        } else {
            return None;
        }
    }};
}

impl ast::OpVar {
    pub(super) fn res_ty(&self, param_ty: &mir::Ty) -> Option<mir::Ty> {
        match self {
            ast::OpVar::Add => return_as_is!(is_numeric, param_ty),
            ast::OpVar::Sub => return_as_is!(is_numeric, param_ty),
            ast::OpVar::Mul => return_as_is!(is_numeric, param_ty),
            ast::OpVar::Div => return_as_is!(is_numeric, param_ty),
            ast::OpVar::And => return_as_is!(is_int, param_ty),
            ast::OpVar::Or => return_as_is!(is_int, param_ty),
            ast::OpVar::Xor => return_as_is!(is_int, param_ty),
            ast::OpVar::Ban => return_as_is!(is_int, param_ty),
            ast::OpVar::Bor => return_as_is!(is_int, param_ty),
            ast::OpVar::Gt => return_bool!(is_assignable, param_ty),
            ast::OpVar::Lt => return_bool!(is_assignable, param_ty),
            ast::OpVar::Eq => return_bool!(is_assignable, param_ty),
            ast::OpVar::Gte => return_bool!(is_assignable, param_ty),
            ast::OpVar::Lte => return_bool!(is_assignable, param_ty),
            ast::OpVar::Neq => return_bool!(is_assignable, param_ty),
            ast::OpVar::Neg => return_as_is!(is_numeric, param_ty),
            ast::OpVar::Pos => return_as_is!(is_numeric, param_ty),
            ast::OpVar::Inv => return_as_is!(is_bool, param_ty),
            ast::OpVar::Bin => return_as_is!(is_int, param_ty),
            ast::OpVar::Ref => unimplemented!("Unsupported operator"),
            ast::OpVar::Der => unimplemented!("Unsupported operator"),
            ast::OpVar::Ina => unimplemented!("Unsupported operator"),
            ast::OpVar::Inb => unimplemented!("Unsupported operator"),
            ast::OpVar::Dea => unimplemented!("Unsupported operator"),
            ast::OpVar::Deb => unimplemented!("Unsupported operator"),
            ast::OpVar::_Lpr => unimplemented!("Unsupported operator"),
            ast::OpVar::_Rpr => unimplemented!("Unsupported operator"),
            ast::OpVar::_Com => unreachable!("Unsupported operator"),
            ast::OpVar::_Asn => Some(mir::Ty::Void),
            ast::OpVar::_Csn => Some(mir::Ty::Void),
            ast::OpVar::_Dum => unreachable!("Unsupported operator"),
        }
    }
}

fn resolve_named_ty(ty_name: &str, scope: &ast::Scope) -> CompileResult<mir::Ty> {
    let def = scope
        .find_def(ty_name)
        .ok_or_else(|| CompileErrorVar::NonExistType(ty_name.to_owned()))?;
    let ty_def = def.borrow();
    match &*ty_def {
        ast::SymbolDef::Typ { def } => resolve_ty(&*def.borrow(), scope),
        ast::SymbolDef::Var { .. } => Err(compile_err_n(CompileErrorVar::NonExistType(
            ty_name.to_owned(),
        ))),
    }
}

/// A simple type resolver to resolve `ast::TypeDef` down to `mir::Ty`.
/// Does not support circular types, or types where the source type name cannot
/// be accessed from target site.
fn resolve_ty(ty: &ast::TypeDef, scope: &ast::Scope) -> CompileResult<mir::Ty> {
    match ty {
        ast::TypeDef::Primitive(p) => {
            if p.var == ast::PrimitiveTypeVar::SignedInt
                || p.var == ast::PrimitiveTypeVar::UnsignedInt
            {
                Ok(mir::Ty::Primitive(
                    mir::PrimitiveTy::Int,
                    p.occupy_bytes as u8,
                ))
            } else if p.var == ast::PrimitiveTypeVar::Float {
                Ok(mir::Ty::Primitive(
                    mir::PrimitiveTy::Float,
                    p.occupy_bytes as u8,
                ))
            } else if p.var == ast::PrimitiveTypeVar::UnsignedInt && p.occupy_bytes == 1 {
                Ok(mir::Ty::bool())
            } else {
                Err(compile_err_n(CompileErrorVar::NonExistType("".into())))
            }
        }
        ast::TypeDef::Struct(_) => panic!("Unsupported: struct type"),
        ast::TypeDef::Function(f) => {
            let ret_ty = resolve_ty(&f.return_type.borrow(), scope.clone())?;
            let mut params_ty = Vec::new();
            for param in f.params.iter() {
                params_ty.push(resolve_ty(&param.borrow(), scope.clone())?);
            }
            Ok(mir::Ty::Fn(Ptr::new(ret_ty), params_ty, f.is_extern))
        }
        ast::TypeDef::Ref(ty) => {
            let ty = &ty.target;
            let ptr_to = resolve_ty(&ty.borrow(), scope)?;
            Ok(mir::Ty::ptr_of(ptr_to))
        }
        ast::TypeDef::Array(arr) => {
            let ty = &arr.target;
            let ptr_to = resolve_ty(&ty.borrow(), scope)?;
            Ok(mir::Ty::Array(Ptr::new(ptr_to), arr.length))
        }
        ast::TypeDef::VariableArgs(va_args) => {
            if let Some(opt) = va_args {
                todo!("Resolve variable args")
            } else {
                Ok(mir::Ty::RestParams)
            }
        }
        ast::TypeDef::Unit => Ok(mir::Ty::Void),
        ast::TypeDef::Unknown => panic!("Unknown type"),
        ast::TypeDef::NamedType(name) => resolve_named_ty(name, scope),
        ast::TypeDef::TypeErr => panic!("Error type"),
    }
}

/// Generates MIR code for a single function.
///
/// # Notes
///
/// Every function code generator contains two passes: one for determining
/// assignments and basic blocks, another for inserting actual code for the
/// expressions inside.
///
/// Functions starting with `scan_` are for the first pass. They determine
/// at which position should one basic block start, and at which position an
/// assignment changes the underlying value a variable binds to. Both are
/// determined by the text position inside code - there's no better way to do
/// that.
///
/// Functions starting with `gen_` are for the second pass. They generate the
/// MIR instructions for expressions, function calls, etc. They use the
/// information provided in the first pass.
#[derive(Debug)]
pub struct FnCodegen<'src> {
    name: &'src str,
    src: &'src ast::FunctionType,
    root_scope: Ptr<ast::Scope>,
    pkg: &'src mut mir::MirPackage,
    global_names: &'src HashMap<String, usize>,
    global_var_counter: &'src mut usize,

    /*
     * Note:
     *
     * Tracking basic-blocks is now possible! Inside every basic block there's
     * a field that tracks which block jumps to this basic block. Just to note
     * that we need more code to ensure the jump code and preceding block are
     * matched.
     *
     * Before filling in the code for each basic block, we need to generate the
     * blocks and references first.
     */
    var_id_counter: usize,

    /// The last-used variable ID in each basic block. Intended to be referenced
    /// from following basic blocks
    var_bb_table: HashMap<String, HashMap<mir::BBId, mir::VarId>>,
    var_pos_table: HashMap<String, BTreeMap<Pos, mir::VarId>>,
    var_table: IndexMap<mir::VarId, mir::Var>,

    /// The start position of each basic block. Code will find its basic block
    /// ID based on this value.
    bb_pos_table: BTreeMap<Pos, mir::BBId>,

    // basic blocks
    /// How many basic block do we have now?
    bb_counter: usize,
    /// ID to block match
    bbs: IndexMap<mir::BBId, mir::BasicBlk>,

    /// Break targets for basic block calculation
    break_target: Vec<mir::BBId>,
}

impl<'src> FnCodegen<'src> {
    fn new(
        name: &'src str,
        src: &'src ast::FunctionType,
        root_scope: Ptr<ast::Scope>,
        pkg: &'src mut mir::MirPackage,
        global_names: &'src HashMap<String, usize>,
        global_var_counter: &'src mut usize,
    ) -> FnCodegen<'src> {
        FnCodegen {
            name,
            src,
            var_id_counter: 0,
            bb_counter: 0,
            var_bb_table: HashMap::new(),
            var_table: IndexMap::new(),
            var_pos_table: HashMap::new(),
            bb_pos_table: BTreeMap::new(),
            break_target: Vec::new(),
            bbs: IndexMap::new(),
            root_scope,
            pkg,
            global_names,
            global_var_counter,
        }
    }

    fn gen(mut self) -> CompileResult<mir::Func> {
        {
            let init_bb = mir::BasicBlk {
                jump_in: HashSet::new(),
                uses_var: HashSet::new(),
                // id: init_bb_id,
                inst: vec![],
                end: mir::JumpInst::Unknown,
            };
            self.add_bb(
                self.src
                    .body
                    .as_ref()
                    .unwrap()
                    .span
                    .unwrap_or(Span::zero())
                    .start,
                init_bb,
            )?;
        }
        //  no return variable; RETURN statements adds a return variable
        let mut params_ty = Vec::new();
        {
            // param variables
            let param_scope = self.src.body.as_ref().unwrap().scope.borrow();
            for (param_ty, (name, _)) in self
                .src
                .params
                .iter()
                .zip(param_scope.defs.iter().take(self.src.params.len()))
            {
                let param_ty = resolve_ty(&param_ty.borrow(), &self.root_scope.borrow())?;
                params_ty.push(param_ty.clone());
                self.add_assign_entry(
                    Some(name),
                    param_ty,
                    mir::VarKind::Param,
                    self.src.body.as_ref().unwrap().span.unwrap().start,
                    0,
                );
            }
        }
        {
            // Scan - first pass
            self.scan_blk(&self.src.body.as_ref().unwrap(), 0, self.root_scope.clone())?;
        }
        {
            // Generate - second pass
            self.gen_blk(&self.src.body.as_ref().unwrap(), 0, self.root_scope.clone())?;
        }
        let self_ty = {
            let ret_ty = resolve_ty(&self.src.return_type.borrow(), &self.root_scope.borrow())?;
            mir::Ty::function_of(ret_ty, params_ty, false)
        };
        // result
        Ok(mir::Func {
            name: self.name.to_owned(),
            ty: self_ty,
            var_table: self.var_table,
            bb: self.bbs,
        })
    }

    /// Get a new basic block
    fn new_bb(&mut self) -> CompileResult<mir::BBId> {
        self.bb_counter += 1;
        let bb_id = self.bb_counter;
        let bb = mir::BasicBlk {
            jump_in: HashSet::new(),
            uses_var: HashSet::new(),
            // id: bb_id,
            inst: vec![],
            end: mir::JumpInst::Unknown,
        };
        self.bbs.insert(bb_id, bb);
        Ok(bb_id)
    }

    /// Scan once to find each basic block and assignment inside each function
    ///
    /// This allows us to track variable changes
    fn scan_for_bb_and_assignment(&mut self) -> CompileResult<()> {
        let root = self
            .src
            .body
            .as_ref()
            .expect("Any function in FnCodegen should have a body");

        self.scan_blk(root, 0, root.scope.clone())?;
        Ok(())
    }

    fn add_assign_entry(
        &mut self,
        assignment: Option<&str>,
        ty: mir::Ty,
        kind: mir::VarKind,
        pos: Pos,
        bb: mir::BBId,
    ) -> mir::VarRef {
        self.var_id_counter += 1;
        let var = self.var_id_counter;

        let assignment = assignment
            .map(|x| x.to_owned())
            .unwrap_or_else(|| format!("${}", var));

        debug!(
            "Add assign entry: {} (${}) {:?}: {:?} @{}, bb{}",
            &assignment, var, kind, &ty, pos, bb
        );

        if !self.var_bb_table.contains_key(&assignment) {
            self.var_bb_table.insert(assignment.clone(), HashMap::new());
        }
        if !self.var_pos_table.contains_key(&assignment) {
            self.var_pos_table
                .insert(assignment.to_owned(), BTreeMap::new());
        }

        // ! The variable ID will replace the one specified in the last assignment,
        // ! this is intended behavior! The hashmap tracks for the **last** occurring
        // ! variable ID inside this basic block.
        self.var_bb_table
            .get_mut(&assignment)
            .expect("The bb entry must be present")
            .insert(bb, var);

        self.var_pos_table
            .get_mut(&assignment)
            .expect("The span entry must be present")
            .insert(pos, var);

        self.var_table.insert(var, mir::Var { ty, kind });

        mir::VarRef(mir::VarTy::Local, var)
    }

    fn add_bb(&mut self, pos: Pos, bb: mir::BasicBlk) -> CompileResult<mir::BBId> {
        let id = self.bb_counter;
        self.bb_counter += 1;

        self.bb_pos_table.insert(pos, id);
        self.bbs.insert(id, bb);

        Ok(id)
    }

    /// Scans a statement, add all assignment and basic block changes in it,
    /// returns the basic block ID to be used in the next statement
    fn scan_stmt(
        &mut self,
        stmt: &ast::Stmt,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::BBId> {
        match &stmt.var {
            ast::StmtVariant::If(i) => self.scan_if_stmt(i, bb, scope),
            ast::StmtVariant::While(w) => self.scan_while_stmt(w, bb, scope),
            ast::StmtVariant::Block(blk) => self.scan_blk(blk, bb, scope),
            ast::StmtVariant::Expr(expr) => self.scan_expr(&*expr.borrow(), bb, scope).map(|_| bb),
            ast::StmtVariant::Print(_) => Ok(bb),
            ast::StmtVariant::Scan(s) => self.scan_scan_stmt(s, stmt.span, bb, scope).map(|_| bb),
            ast::StmtVariant::ManyExpr(exprs) => {
                for expr in exprs {
                    self.scan_expr(&*expr.borrow(), bb, scope.clone())?;
                }
                Ok(bb)
            }
            ast::StmtVariant::Return(_) => self.scan_return(stmt.span, bb, scope),
            ast::StmtVariant::Break => self.scan_break_stmt(stmt.span, bb, scope),
            ast::StmtVariant::Empty => Ok(bb),
        }
    }

    fn scan_if_stmt(
        &mut self,
        if_stmt: &ast::IfConditional,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::BBId> {
        let if_blk = if_stmt.if_block.borrow();
        let if_start = if_blk.span.start;

        let if_bb = mir::BasicBlk::new(&[bb]);
        let if_id = self.add_bb(if_start, if_bb)?;

        let if_end = self.scan_stmt(&*if_blk, if_id, scope.clone())?;

        let next_bb;
        let stmt_end;

        if let Some(else_blk) = &if_stmt.else_block {
            let else_blk = else_blk.borrow();
            let else_start = else_blk.span.start;

            let else_bb = mir::BasicBlk::new(&[bb]);
            let else_id = self.add_bb(else_start, else_bb)?;

            let else_end = self.scan_stmt(&*else_blk, else_id, scope.clone())?;

            // * Add return block

            stmt_end = else_blk.span.end;
            next_bb = mir::BasicBlk::new(&[if_end, else_end]);
        } else {
            stmt_end = if_blk.span.end;
            next_bb = mir::BasicBlk::new(&[if_end, bb]);
        }
        let next_id = self.add_bb(stmt_end, next_bb)?;
        Ok(next_id)
    }

    fn scan_while_stmt(
        &mut self,
        while_stmt: &ast::WhileConditional,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::BBId> {
        let while_blk = while_stmt.block.borrow();
        let while_start = while_blk.span.start;

        let while_bb = mir::BasicBlk::new(&[bb]);
        let while_id = self.add_bb(while_start, while_bb)?;

        let stmt_end = while_blk.span.end;
        let next_bb = mir::BasicBlk::new(&[bb]);
        let next_id = self.add_bb(stmt_end, next_bb)?;

        self.break_target.push(next_id);

        let while_end = self.scan_stmt(&*while_blk, while_id, scope.clone())?;

        {
            let break_tgt = self.break_target.pop();
            assert_eq!(break_tgt, Some(next_id))
        }

        let next_bb = self.bbs.get_mut(&next_id).expect("basic block must exist");
        next_bb.jump_in.insert(while_end);

        Ok(next_id)
    }

    fn scan_blk(
        &mut self,
        blk: &ast::Block,
        bb: mir::BBId,
        _scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::BBId> {
        let mut bb_id = bb;
        for stmt in &blk.stmts {
            bb_id = self.scan_stmt(stmt, bb_id, blk.scope.clone())?;
        }
        Ok(bb_id)
    }

    fn scan_expr(
        &mut self,
        expr: &ast::Expr,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<()> {
        match &expr.var {
            ast::ExprVariant::Ident(_) => {}
            ast::ExprVariant::Literal(_) => {}
            ast::ExprVariant::TypeConversion(_) => {}
            ast::ExprVariant::UnaryOp(info) => {
                self.scan_expr(&*info.val.borrow(), bb, scope.clone())?;
            }
            ast::ExprVariant::BinaryOp(info) => {
                if info.op == ast::OpVar::_Asn {
                    let (name, ty) =
                        self.get_l_value_name_and_ty(&*info.lhs.borrow(), scope.clone())?;

                    self.add_assign_entry(Some(&name), ty, mir::VarKind::Local, expr.span.end, bb);
                    self.scan_expr(&*info.rhs.borrow(), bb, scope.clone())?;
                } else {
                    self.scan_expr(&*info.lhs.borrow(), bb, scope.clone())?;
                    self.scan_expr(&*info.rhs.borrow(), bb, scope.clone())?;
                }
            }
            ast::ExprVariant::FunctionCall(_) => {}
            ast::ExprVariant::ArrayChild(_) => {}
        }
        Ok(())
    }

    fn scan_scan_stmt(
        &mut self,
        scan: &ast::Identifier,
        span: Span,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<()> {
        let name = &scan.name;
        let (name, ty) = self.get_var_name_and_ty(name, scope.clone())?;
        debug!("Scan statement: name {} type {:?}", &name, &ty);
        self.add_assign_entry(Some(&name), ty, mir::VarKind::Local, span.end, bb);

        Ok(())
    }

    fn scan_return(
        &mut self,
        span: Span,
        _bb: mir::BBId,
        _scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::BBId> {
        // * The rest is a dangling basic block, but we have to assume it's
        // * somewhat present for a uniform code style.
        let new_bb = self.add_bb(span.end, mir::BasicBlk::new(&[]))?;
        self.bbs.get_mut(&new_bb).unwrap().end = mir::JumpInst::Unreachable;
        Ok(new_bb)
    }

    fn scan_break_stmt(
        &mut self,
        span: Span,
        bb: mir::BBId,
        _scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::BBId> {
        let break_tgt = *self
            .break_target
            .last()
            .ok_or_else(|| compile_err(CompileErrorVar::NoTargetToBreak, Some(span)))?;

        let cur_bb = self
            .bbs
            .get_mut(&break_tgt)
            .expect("Basic block must exist");

        // ! This is the only place we set the value of `BasicBlk::end` not
        // ! to `Unknown`, because the break target information will not be
        // ! available afterwards.
        cur_bb.end = mir::JumpInst::Jump(break_tgt);

        let tgt_bb = self
            .bbs
            .get_mut(&break_tgt)
            .expect("Basic block must exist");

        tgt_bb.jump_in.insert(bb);

        // * The rest is a dangling basic block, but we have to assume it's
        // * somewhat present for a uniform code style, same as return.
        let new_bb = self.add_bb(span.end, mir::BasicBlk::new(&[]))?;
        self.bbs.get_mut(&new_bb).unwrap().end = mir::JumpInst::Unreachable;
        Ok(new_bb)
    }

    fn get_var_name(&self, name: &str, scope: Ptr<ast::Scope>) -> CompileResult<String> {
        let (_, depth) = scope
            .borrow()
            .find_def_depth(name)
            .ok_or_else(|| compile_err_n(CompileErrorVar::NonExistVar(name.to_owned())))?;
        let name = format_ident(&name, depth);
        Ok(name)
    }

    fn get_var_name_and_ty(
        &self,
        name: &str,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<(String, mir::Ty)> {
        let (def, depth) = scope
            .borrow()
            .find_def_depth(name)
            .ok_or_else(|| compile_err_n(CompileErrorVar::NonExistVar(name.to_owned())))?;

        let name = format_ident(&name, depth);
        let (ty, _) = def
            .borrow()
            .get_sym()
            .ok_or_else(|| CompileErrorVar::NoTypeInformation)?;
        let ty = ty.borrow();
        let ty = resolve_ty(&*ty, &scope.borrow())?;
        Ok((name, ty))
    }

    fn get_l_value_name_and_ty(
        &self,
        expr: &ast::Expr,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<(String, mir::Ty)> {
        match &expr.var {
            ast::ExprVariant::Ident(i) => self
                .get_var_name_and_ty(&i.name, scope)
                .with_span(expr.span),
            _ => Err(CompileErrorVar::NotLValue(format!("{}", expr))).with_span(expr.span),
        }
    }

    /// Get the variable bindded to the specific identifier coming from
    /// different basic blocks. Add variable reference to them.
    ///
    /// # Path
    ///
    /// `path` is the current bb path used in this function. Starts with the bb
    /// that calls this function.
    fn resole_ident_binding_add_usage_by_bb(
        &mut self,
        ident: &str,
        bb: mir::BBId,
        vis: &mut HashSet<mir::BBId>,
        path: &mut Vec<mir::BBId>,
        res: &mut HashSet<(mir::BBId, mir::VarId)>,
    ) {
        // TODO: Do we need break-through assignments?
        // WARN: No. Related code commented.
        if vis.contains(&bb) {
            return;
        }
        vis.insert(bb);

        let jump_in = &self.bbs.get(&bb).unwrap().jump_in;
        let bindings = jump_in
            .iter()
            .filter(|x| !vis.contains(x))
            .map(|bb_id| {
                (
                    *bb_id,
                    self.var_bb_table
                        .get(ident)
                        .unwrap()
                        .get(bb_id)
                        .map(|id| *id),
                )
            })
            .collect::<Vec<_>>();

        for (id, val) in bindings {
            path.push(id);
            if let Some(val) = val {
                // path[1] is the direct successor basic block
                res.insert((path[1], val));
            // TODO: break-through assignment
            // for bb_id in path.iter() {
            //     self.bbs.get_mut(bb_id).unwrap().uses_var.insert(val);
            // }
            } else {
                self.resole_ident_binding_add_usage_by_bb(ident, id, vis, path, res);
            }
            path.pop();
        }
    }

    fn resolve_ident_binding_and_add_usage(
        &mut self,
        ident: &str,
        pos: Pos,
        bb: mir::BBId,
    ) -> Option<Either<mir::VarId, HashSet<(mir::BBId, mir::VarId)>>> {
        // TODO: Do we need break-through assignments?
        // WARN: No. Related code commented.

        let var_pos = self.var_pos_table.get(ident)?;
        let last_def = var_pos.range(..=pos).last();
        match last_def {
            None => {
                let mut res = HashSet::new();
                let mut vis = HashSet::new();
                self.resole_ident_binding_add_usage_by_bb(
                    ident,
                    bb,
                    &mut vis,
                    &mut vec![bb],
                    &mut res,
                );

                if res.len() > 0 {
                    Some(Either::Right(res))
                } else {
                    None
                }
            }
            Some((pos, id)) => {
                let last_bb = self.bb_pos_table.range(..=pos).last();

                let same_bb = last_bb.map_or(false, |(_, last_id)| *last_id == bb);

                if same_bb {
                    Some(either::Left(*id))
                } else {
                    let mut res = HashSet::new();
                    let mut vis = HashSet::new();
                    self.resole_ident_binding_add_usage_by_bb(
                        ident,
                        bb,
                        &mut vis,
                        &mut vec![bb],
                        &mut res,
                    );
                    if res.len() > 0 {
                        Some(Either::Right(res))
                    } else {
                        None
                    }
                }
            }
        }
    }

    fn insert_ins(&mut self, bb: mir::BBId, ins: mir::MirCode) -> CompileResult<()> {
        self.bbs
            .get_mut(&bb)
            .ok_or_else(|| {
                compile_err_n(CompileErrorVar::InternalError("Unknown basic block".into()))
            })?
            .inst
            .push(ins);
        Ok(())
    }

    fn get_ty(&self, var: mir::VarId) -> Option<&mir::Ty> {
        self.var_table.get(&var).map(|var| &var.ty)
    }

    fn get_global_ty(&self, var: usize) -> Option<&mir::Ty> {
        self.pkg.global_var_table.get(&var).map(|var| &var.ty)
    }

    fn val_ty(&self, val: &mir::Value) -> Option<mir::Ty> {
        match val {
            mir::Value::IntImm(_) => Some(mir::Ty::int()),
            mir::Value::FloatImm(_) => Some(mir::Ty::double()),
            mir::Value::Var(v) => match v.0 {
                mir::VarTy::Local => self.get_ty(v.1),
                mir::VarTy::Global => self.get_global_ty(v.1),
            }
            .map(|ty| ty.clone()),
            // TODO: Support register values?
            // mir::Value::Reg(_) => None,
            mir::Value::Void => Some(mir::Ty::Void),
        }
    }

    /// Generate MIR for statement
    fn gen_stmt(
        &mut self,
        stmt: &ast::Stmt,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::BBId> {
        match &stmt.var {
            ast::StmtVariant::If(i) => self.gen_if_stmt(i, bb, scope),
            ast::StmtVariant::While(w) => self.gen_while_stmt(w, bb, scope),
            ast::StmtVariant::Block(blk) => self.gen_blk(blk, bb, scope),
            ast::StmtVariant::Expr(e) => {
                let expr = e.borrow();
                self.gen_expr(&expr, bb, scope)?;
                Ok(bb)
            }
            ast::StmtVariant::Print(p) => self.gen_print(p, stmt.span, bb, scope),
            ast::StmtVariant::Scan(s) => self.gen_scan(&s.name, stmt.span, bb, scope),
            ast::StmtVariant::ManyExpr(e) => {
                for expr in e {
                    let expr = expr.borrow();
                    self.gen_expr(&expr, bb, scope.clone())?;
                }
                Ok(bb)
            }
            ast::StmtVariant::Return(ret_val) => {
                self.gen_return(ret_val.to_owned(), stmt.span, bb, scope)
            }
            ast::StmtVariant::Break => {
                // Break statement is already dealt with in the 1st pass, so nothing
                // to do here. Just dump all code into the dangling block.
                let next_bb = *self.bb_pos_table.get(&stmt.span.end).unwrap();
                Ok(next_bb)
            }
            ast::StmtVariant::Empty => Ok(bb),
        }
    }

    fn gen_expr(
        &mut self,
        expr: &ast::Expr,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::Value> {
        match &expr.var {
            ast::ExprVariant::Ident(ident) => {
                self.gen_rvalue_ident_expr(ident, expr.span, bb, scope)
            }
            ast::ExprVariant::Literal(lit) => self.gen_literal_expr(lit, expr.span, bb, scope),
            ast::ExprVariant::TypeConversion(ty_conv) => {
                self.gen_type_conversion(ty_conv, expr.span, bb, scope)
            }
            ast::ExprVariant::UnaryOp(op) => self.gen_unary_op(op, expr.span, bb, scope),
            ast::ExprVariant::BinaryOp(op) => self.gen_binary_op(op, expr.span, bb, scope),
            ast::ExprVariant::FunctionCall(op) => self.gen_fn_call(op, expr.span, bb, scope),
            ast::ExprVariant::ArrayChild(_) => unimplemented!("Array child is not implemented"),
        }
    }

    fn gen_rvalue_ident_expr(
        &mut self,
        ident: &ast::Identifier,
        span: Span,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::Value> {
        let (def, depth) = scope.borrow().find_def_depth(&ident.name).ok_or_else(|| {
            compile_err(CompileErrorVar::NonExistVar(ident.name.clone()), Some(span))
        })?;
        if depth == 0 {
            // global value
            let global_id = self.global_names.get(&ident.name).unwrap();
            Ok(mir::Value::Var(mir::VarRef(mir::VarTy::Global, *global_id)))
        } else {
            let ident_fmt = format_ident(&ident.name, depth);
            let ident = self.resolve_ident_binding_and_add_usage(&ident_fmt, span.start, bb);
            let ty = resolve_ty(
                &def.borrow().get_sym().expect("is symbol").0.borrow(),
                &scope.borrow(),
            )?;

            match ident {
                None => Err(compile_err(
                    CompileErrorVar::NonExistVar(ident_fmt),
                    Some(span),
                )),
                Some(Either::Left(id)) => Ok(mir::Value::Var(mir::VarRef(mir::VarTy::Local, id))),
                Some(Either::Right(ids)) => {
                    // * Moved to resolve_ident_binding_and_add_usage
                    // {
                    //     let bb = self.bbs.get_mut(&bb).unwrap();
                    //     for (_, var_id) in ids.iter() {
                    //         bb.uses_var.insert(*var_id);
                    //     }
                    // }

                    let new_var =
                        self.add_assign_entry(None, ty.clone(), mir::VarKind::Temp, span.start, bb);
                    self.insert_ins(
                        bb,
                        mir::MirCode {
                            ins: mir::Ins::Phi(
                                ids.into_iter()
                                    .map(|(bb, var)| {
                                        (bb, mir::Value::Var(mir::VarRef(mir::VarTy::Local, var)))
                                    })
                                    .collect(),
                            ),
                            tgt: new_var,
                        },
                    )?;
                    Ok(mir::Value::Var(new_var))
                }
            }
        }
    }

    fn gen_literal_expr(
        &mut self,
        literal: &ast::Literal,
        span: Span,
        _bb: mir::BBId,
        _scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::Value> {
        match literal {
            ast::Literal::Char { val } => Ok(mir::Value::IntImm(*val as i32)),
            ast::Literal::Integer { val } => {
                if val > &i32::max_value() {
                    Err(compile_err(CompileErrorVar::IntOverflow, Some(span)))
                } else {
                    Ok(mir::Value::IntImm(val.into()))
                }
            }
            ast::Literal::Float { val } => Ok(mir::Value::FloatImm(val.to_f64())),
            ast::Literal::Boolean { val } => {
                if *val {
                    Ok(mir::Value::IntImm(1))
                } else {
                    Ok(mir::Value::IntImm(0))
                }
            }
            ast::Literal::String { val } => {
                let s = val.as_bytes().to_vec();
                let var_id = *self.global_var_counter;
                *self.global_var_counter += 1;
                self.pkg.global_var_table.insert(
                    var_id,
                    mir::GlobalVar {
                        ty: mir::Ty::array_of(mir::Ty::byte(), Some(s.len())),
                        name: None,
                        binary_value: Some(var_id),
                    },
                );
                self.pkg.static_values.insert(var_id, s);
                Ok(mir::Value::Var(mir::VarRef(mir::VarTy::Global, var_id)))
            }
        }
    }

    fn gen_type_conversion(
        &mut self,
        ty_conv: &ast::TypeConversion,
        span: Span,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::Value> {
        let expr_val = self.gen_expr(&*ty_conv.expr.borrow(), bb, scope.clone())?;
        let end_ty = resolve_ty(&*ty_conv.to.borrow(), &scope.borrow())?;
        let tgt = self.add_assign_entry(None, end_ty, mir::VarKind::Temp, span.end, bb);
        self.insert_ins(
            bb,
            mir::MirCode {
                ins: mir::Ins::TyCon(expr_val),
                tgt,
            },
        )?;
        Ok(mir::Value::Var(tgt))
    }

    fn gen_unary_op(
        &mut self,
        op: &ast::UnaryOp,
        span: Span,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::Value> {
        let expr_val = self.gen_expr(&*op.val.borrow(), bb, scope.clone())?;
        let expr_ty = self
            .val_ty(&expr_val)
            .ok_or_else(|| compile_err(CompileErrorVar::NoTypeInformation, Some(span)))?;
        let op_ty = op.op.res_ty(&expr_ty).ok_or_else(|| {
            compile_err(CompileErrorVar::OperatorDoesNotSupportedType, Some(span))
        })?;

        let temp_var = self.add_assign_entry(None, op_ty, mir::VarKind::Temp, span.end, bb);

        self.insert_ins(
            bb,
            mir::MirCode {
                ins: mir::Ins::Una(op.op.into(), expr_val),
                tgt: temp_var,
            },
        )?;

        Ok(mir::Value::Var(temp_var))
    }

    fn gen_binary_op(
        &mut self,
        op: &ast::BinaryOp,
        span: Span,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::Value> {
        match op.op {
            ast::OpVar::_Asn => self.gen_assignment(op, span, bb, scope),
            _ => self.gen_regular_binary_op(op, span, bb, scope),
        }
    }

    fn gen_lvalue_ident(
        &mut self,
        lval: &str,
        span: Span,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::VarRef> {
        // get the definition of this variable.
        // Typing stuff is finished in the 1st pass so we don't need to
        // typecheck again here. Just return the variable reference
        // and typings can be retrieved from variable info
        let (_, depth) = scope.borrow().find_def_depth(&lval).ok_or_else(|| {
            compile_err(CompileErrorVar::NonExistVar(lval.to_owned()), Some(span))
        })?;
        let lval_name = format_ident(&lval, depth);
        if depth == 0 {
            // global value
            let global_id = self
                .global_names
                .get(lval)
                .ok_or_else(|| CompileErrorVar::NonExistVar(lval.to_owned()))
                .with_span(span)?;
            Ok(mir::VarRef(mir::VarTy::Global, *global_id))
        } else {
            let (_, v_ref) = self
                .var_pos_table
                .get(&lval_name)
                .unwrap()
                .range(span.end..)
                .next()
                .unwrap();
            Ok(mir::VarRef(mir::VarTy::Local, *v_ref))
        }
    }

    fn gen_lvalue(
        &mut self,
        lval: &ast::Expr,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::VarRef> {
        if !lval.is_lvalue() {
            return Err(CompileErrorVar::NotLValue(lval.to_string())).with_span(lval.span);
        }

        match &lval.var {
            ast::ExprVariant::Ident(ident) => {
                self.gen_lvalue_ident(&ident.name, lval.span, bb, scope)
            }
            _ => Err(CompileErrorVar::NotLValue(format!("{}", lval))).with_span(lval.span),
        }
    }

    fn gen_assignment(
        &mut self,
        op: &ast::BinaryOp,
        span: Span,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::Value> {
        let lval = &*op.lhs.borrow();
        let var = self.gen_lvalue(lval, bb, scope.clone())?;
        let rval = self.gen_expr(&*op.rhs.borrow(), bb, scope.clone())?;

        let lval_ty = if var.0 == mir::VarTy::Local {
            &self.var_table.get(&var.1).expect("Variable must exist").ty
        } else {
            &self
                .pkg
                .global_var_table
                .get(&var.1)
                .expect("Variable must exist")
                .ty
        };
        let rval_ty = self.val_ty(&rval).expect("Right value must have type");

        if *lval_ty != rval_ty {
            return Err(CompileErrorVar::TypeMismatch).with_span(span);
        } else if !rval_ty.is_assignable() {
            return Err(CompileErrorVar::AssignVoid).with_span(span);
        }

        self.insert_ins(
            bb,
            mir::MirCode {
                ins: mir::Ins::Asn(rval),
                tgt: var,
            },
        )?;

        Ok(mir::Value::Void)
    }

    fn gen_regular_binary_op(
        &mut self,
        op: &ast::BinaryOp,
        span: Span,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::Value> {
        let lhs_val = self.gen_expr(&*op.lhs.borrow(), bb, scope.clone())?;
        let lhs_ty = self
            .val_ty(&lhs_val)
            .ok_or_else(|| compile_err(CompileErrorVar::NoTypeInformation, Some(span)))?;

        let rhs_val = self.gen_expr(&*op.rhs.borrow(), bb, scope.clone())?;
        let rhs_ty = self
            .val_ty(&rhs_val)
            .ok_or_else(|| compile_err(CompileErrorVar::NoTypeInformation, Some(span)))?;

        // TODO: Check for special binary operators
        if lhs_ty != rhs_ty {
            return Err(CompileErrorVar::TypeMismatch).with_span(span);
        } else if !lhs_ty.is_assignable() {
            return Err(CompileErrorVar::AssignVoid).with_span(span);
        }

        let op_ty = op.op.res_ty(&lhs_ty).ok_or_else(|| {
            compile_err(CompileErrorVar::OperatorDoesNotSupportedType, Some(span))
        })?;

        let temp_var = self.add_assign_entry(None, op_ty, mir::VarKind::Temp, span.end, bb);

        self.insert_ins(
            bb,
            mir::MirCode {
                ins: mir::Ins::Bin(op.op.into(), lhs_val, rhs_val),
                tgt: temp_var,
            },
        )?;

        Ok(mir::Value::Var(temp_var))
    }

    fn gen_fn_call(
        &mut self,
        op: &ast::FunctionCall,
        span: Span,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::Value> {
        let mut params = Vec::new();

        let func_id = *self
            .global_names
            .get(&op.func)
            .ok_or_else(|| CompileErrorVar::NonExistFunc(op.func.to_owned()))
            .with_span(span)?;

        // TODO: reduce clone
        let func_ty = self.pkg.global_var_table.get(&func_id).unwrap().ty.clone();

        for (param, param_ty) in op.params.iter().zip(func_ty.get_fn_params().unwrap()) {
            let val = self.gen_expr(&*param.borrow(), bb, scope.clone())?;
            let val_ty = self.val_ty(&val).expect("Value must have type");
            if val_ty != *param_ty {
                return Err(CompileErrorVar::TypeMismatch).with_span(span);
            }
            // Assign every value calculated into a temporary variable
            let temp_var = self.add_assign_entry(
                None,
                val_ty,
                mir::VarKind::FixedTemp,
                param.borrow().span.start,
                bb,
            );
            self.insert_ins(
                bb,
                mir::MirCode {
                    ins: mir::Ins::Asn(val),
                    tgt: temp_var,
                },
            )?;

            params.push(mir::Value::Var(temp_var));
        }

        let temp_var = self.add_assign_entry(
            None,
            func_ty.get_fn_ret().unwrap().borrow().clone(),
            mir::VarKind::FixedTemp,
            span.end,
            bb,
        );

        self.insert_ins(
            bb,
            mir::MirCode {
                ins: mir::Ins::Call(mir::VarRef(mir::VarTy::Global, func_id), params),
                tgt: temp_var,
            },
        )?;

        Ok(mir::Value::Var(temp_var))
    }

    fn gen_blk(
        &mut self,
        blk: &ast::Block,
        bb: mir::BBId,
        _scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::BBId> {
        let blk_scope = blk.scope.clone();
        let mut cur_bb = bb;
        for stmt in &blk.stmts {
            cur_bb = self.gen_stmt(stmt, cur_bb, blk_scope.clone())?;
        }
        Ok(cur_bb)
    }

    fn gen_if_stmt(
        &mut self,
        stmt: &ast::IfConditional,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::BBId> {
        // condition
        let cond = stmt.cond.borrow();

        let true_start = stmt.if_block.borrow().span.start;
        let true_bb = *self.bb_pos_table.get(&true_start).unwrap();

        let false_start = stmt.else_block.as_ref().map(|blk| blk.borrow().span.start);
        let false_bb = false_start
            .clone()
            .map(|false_start| *self.bb_pos_table.get(&false_start).unwrap());

        let end_start = stmt
            .else_block
            .as_ref()
            .map(|blk| blk.borrow().span.end)
            .unwrap_or_else(|| stmt.if_block.borrow().span.end);
        let end_bb = self
            .bb_pos_table
            .range(end_start..)
            .next()
            .map(|(_, id)| *id)
            .unwrap();

        let cond_val = self.gen_expr(&cond, bb, scope.clone())?;
        let cond_val_ty = self.val_ty(&cond_val).unwrap();

        if !cond_val_ty.is_bool() {
            return Err(CompileErrorVar::TypeMismatch).with_span(cond.span());
        }

        // [start_bb] -- true --> [true_bb]...[true_end] ---> [end_bb]
        //         |--- false --> [false_bb]...[false_end] ---^
        // -or-
        // [start_bb] -- true --> [true_bb]...[true_end] ---> [end_bb]
        //         |--- false --------------------------------^
        self.bbs.get_mut(&bb).unwrap().end =
            mir::JumpInst::Conditional(cond_val, true_bb, false_bb.unwrap_or(end_bb));

        {
            // true_bb
            let true_end = self.gen_stmt(&stmt.if_block.borrow(), true_bb, scope.clone())?;

            self.bbs.get_mut(&true_end).unwrap().end = mir::JumpInst::Jump(end_bb);
        }

        if let Some(else_blk) = &stmt.else_block {
            let else_end = self.gen_stmt(&else_blk.borrow(), false_bb.unwrap(), scope.clone())?;

            self.bbs.get_mut(&else_end).unwrap().end = mir::JumpInst::Jump(end_bb);
        }

        Ok(end_bb)
    }

    // fn gen_scan_stmt(&mut self,stmt:&ast::)

    fn gen_while_stmt(
        &mut self,
        stmt: &ast::WhileConditional,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::BBId> {
        let cond = stmt.cond.borrow();

        let while_blk = stmt.block.borrow();
        let while_blk_span = while_blk.span;
        let while_bb = *self.bb_pos_table.get(&while_blk_span.start).unwrap();

        let next_bb = *self
            .bb_pos_table
            .range(while_blk_span.end..)
            .next()
            .unwrap()
            .1;

        //         v---------------------------- true -- |
        // [start_bb] -- true --> [while_bb]...[while_end] -- false --> [end_bb]
        //         |--- false -----------------------------------------^

        let start_cond_val = self.gen_expr(&cond, bb, scope.clone())?;
        self.bbs.get_mut(&bb).unwrap().end =
            mir::JumpInst::Conditional(start_cond_val, while_bb, next_bb);

        let while_end = self.gen_stmt(&while_blk, while_bb, scope.clone())?;
        let while_cond_val = self.gen_expr(&cond, while_end, scope.clone())?;
        self.bbs.get_mut(&while_end).unwrap().end =
            mir::JumpInst::Conditional(while_cond_val, while_bb, next_bb);

        Ok(next_bb)
    }

    fn gen_return(
        &mut self,
        ret_val: Option<Ptr<ast::Expr>>,
        span: Span,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::BBId> {
        let ret = if let Some(val) = ret_val {
            let val = self.gen_expr(&val.borrow(), bb, scope)?;
            let val_ty = self.val_ty(&val).unwrap();
            let val = self.add_assign_entry(None, val_ty, mir::VarKind::Ret, span.start, bb);
            Some(mir::Value::Var(val))
        } else {
            None
        };
        let bb = self.bbs.get_mut(&bb).unwrap();
        bb.end = mir::JumpInst::Return(ret);

        let next_bb = *self.bb_pos_table.get(&span.end).unwrap();
        Ok(next_bb)
    }

    fn gen_scan(
        &mut self,
        ident: &str,
        span: Span,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::BBId> {
        let varref = self.gen_lvalue_ident(ident, span, bb, scope)?;
        let ty = if varref.0 == mir::VarTy::Local {
            self.get_ty(varref.1)
        } else {
            self.get_global_ty(varref.1)
        }
        .ok_or_else(|| CompileErrorVar::NonExistVar(ident.to_owned()))
        .with_span(span)?;

        if ty.is_int() {
            let scan_int = *self
                .global_names
                .get(crate::stdlib::STDLIB_SCAN_INT)
                .unwrap();
            self.insert_ins(
                bb,
                mir::MirCode {
                    ins: mir::Ins::Call(mir::VarRef(mir::VarTy::Global, scan_int), vec![]),
                    tgt: varref,
                },
            )?;
            Ok(bb)
        } else if ty.is_double() {
            let scan_double = *self
                .global_names
                .get(crate::stdlib::STDLIB_SCAN_DOUBLE)
                .unwrap();
            self.insert_ins(
                bb,
                mir::MirCode {
                    ins: mir::Ins::Call(mir::VarRef(mir::VarTy::Global, scan_double), vec![]),
                    tgt: varref,
                },
            )?;
            Ok(bb)
        } else {
            Err(CompileErrorVar::RequireScannable(ident.to_owned())).with_span(span)
        }
    }

    fn gen_print(
        &mut self,
        exprs: &Vec<Ptr<ast::Expr>>,
        span: Span,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::BBId> {
        let mut is_first_expr = true;
        let put_char = *self
            .global_names
            .get(crate::stdlib::STDLIB_PUT_CHAR)
            .unwrap();
        for expr in exprs {
            let expr = expr.borrow();

            let expr_val = self.gen_expr(&expr, bb, scope.clone())?;
            let expr_val_ty = self.val_ty(&expr_val).unwrap();

            if is_first_expr {
                is_first_expr = false;
            } else {
                let temp_var = self.add_assign_entry(
                    None,
                    mir::Ty::Void,
                    mir::VarKind::Dummy,
                    expr.span.end,
                    bb,
                );
                self.insert_ins(
                    bb,
                    mir::MirCode {
                        ins: mir::Ins::Call(
                            mir::VarRef(mir::VarTy::Global, put_char),
                            vec![mir::Value::IntImm(b' ' as i32)],
                        ),
                        tgt: temp_var,
                    },
                )?;
            }

            let temp_var =
                self.add_assign_entry(None, mir::Ty::Void, mir::VarKind::Dummy, expr.span.end, bb);
            if expr_val_ty.is_int() {
                let put_int = *self
                    .global_names
                    .get(crate::stdlib::STDLIB_PUT_INT)
                    .unwrap();

                self.insert_ins(
                    bb,
                    mir::MirCode {
                        ins: mir::Ins::Call(
                            mir::VarRef(mir::VarTy::Global, put_int),
                            vec![expr_val],
                        ),
                        tgt: temp_var,
                    },
                )?;
            } else if expr_val_ty.is_double() {
                let put_double = *self
                    .global_names
                    .get(crate::stdlib::STDLIB_PUT_DOUBLE)
                    .unwrap();

                self.insert_ins(
                    bb,
                    mir::MirCode {
                        ins: mir::Ins::Call(
                            mir::VarRef(mir::VarTy::Global, put_double),
                            vec![expr_val],
                        ),
                        tgt: temp_var,
                    },
                )?;
            } else if expr_val_ty.is_array_of(&mir::Ty::byte()) {
                let (_, size) = expr_val_ty.get_array_of().unwrap();
                if let Some(size) = size {
                    let put_str = *self
                        .global_names
                        .get(crate::stdlib::STDLIB_PUT_DOUBLE)
                        .unwrap();

                    self.insert_ins(
                        bb,
                        mir::MirCode {
                            ins: mir::Ins::Call(
                                mir::VarRef(mir::VarTy::Global, put_str),
                                vec![expr_val, mir::Value::IntImm(size as i32)],
                            ),
                            tgt: temp_var,
                        },
                    )?;
                } else {
                    return Err(CompileErrorVar::RequireSized("".into())).with_span(span);
                }
            }
        }
        Ok(bb)
    }
}
