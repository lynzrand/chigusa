use crate::c0::ast;
use crate::minivm::{
    compile_err, compile_err_n, CompileError, CompileErrorVar, CompileResult, WithSpan,
};
use crate::mir;
use crate::prelude::*;
use either::Either;
use std::collections::{BTreeMap, HashMap, HashSet};

#[derive(Debug)]
pub struct Codegen<'src> {
    src: &'src ast::Program,
    pkg: mir::MirPackage,
    global_var_counter: usize,
    global_var_table: HashMap<usize, mir::Var>,
    ty_counter: usize,
    ty_table: HashMap<usize, mir::Ty>,
}

impl<'src> Codegen<'src> {
    fn gen_fn(&mut self, func: &ast::FunctionType) -> CompileResult<mir::Func> {
        let fn_codegen: FnCodegen = todo!("Initialize function code-generator");
        // fn_codegen.
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
    src: &'src ast::FunctionType,
    pkg: &'src mut mir::MirPackage,

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
    var_table: HashMap<mir::VarId, mir::Var>,

    /// The start position of each basic block. Code will find its basic block
    /// ID based on this value.
    bb_pos_table: BTreeMap<Pos, mir::BBId>,

    // basic blocks
    /// How many basic block do we have now?
    bb_counter: usize,
    /// ID to block match
    bbs: HashMap<mir::BBId, mir::BasicBlk>,

    /// Break targets for basic block calculation
    break_target: Vec<mir::BBId>,
}

impl<'src> FnCodegen<'src> {
    fn new(src: &'src ast::FunctionType, pkg: &'src mut mir::MirPackage) -> FnCodegen<'src> {
        let init_bb_id = 0;
        let init_bb = mir::BasicBlk {
            jump_in: HashSet::new(),
            uses_var: HashSet::new(),
            // id: init_bb_id,
            inst: vec![],
            end: mir::JumpInst::Unknown,
        };
        let mut bbs = HashMap::new();
        bbs.insert(init_bb_id, init_bb);

        FnCodegen {
            src,
            var_id_counter: 0,
            bb_counter: 0,
            var_bb_table: HashMap::new(),
            var_table: HashMap::new(),
            var_pos_table: HashMap::new(),
            bb_pos_table: BTreeMap::new(),
            break_target: Vec::new(),
            bbs,
            pkg,
        }
    }

    fn gen(&mut self) -> CompileResult<mir::Func> {
        todo!("Generate code for function")
    }

    fn resolve_ty(&self, ty: &ast::TypeDef, scope: Ptr<ast::Scope>) -> CompileResult<mir::Ty> {
        todo!("Resolve type information")
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
        let var = self.var_id_counter;
        self.var_id_counter += 1;

        let assignment = assignment
            .map(|x| x.to_owned())
            .unwrap_or_else(|| format!("${}", var));

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
            next_bb = mir::BasicBlk::new(&[if_end]);
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
        let next_bb = mir::BasicBlk::new(&[]);
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
        self.add_bb(span.end, mir::BasicBlk::new(&[]))
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
        self.add_bb(span.end, mir::BasicBlk::new(&[]))
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
        let ty = def
            .borrow()
            .get_typ()
            .ok_or_else(|| CompileErrorVar::NoTypeInformation)?;
        let ty = ty.borrow();
        let ty = self.resolve_ty(&*ty, scope.clone())?;
        Ok((name, ty))
    }

    fn get_l_value_name_and_ty(
        &self,
        expr: &ast::Expr,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<(String, mir::Ty)> {
        match &expr.var {
            ast::ExprVariant::Ident(i) => self.get_var_name_and_ty(&i.name, scope),
            _ => Err(CompileErrorVar::NotLValue(format!("{}", expr))).with_span(expr.span),
        }
    }

    /// Get the variable bindded to the specific identifier coming from
    /// different basic blocks
    fn resolve_ident_binding_bb_unique(
        &self,
        ident: &str,
        bb: mir::BBId,
        vis: &mut HashSet<mir::BBId>,
        res: &mut HashSet<(mir::BBId, mir::VarId)>,
    ) {
        let jump_in = &self.bbs.get(&bb).unwrap().jump_in;
        let bindings = jump_in
            .iter()
            .filter(|x| !vis.contains(x))
            .map(|bb_id| (*bb_id, self.var_bb_table.get(ident).unwrap().get(bb_id)))
            .collect::<Vec<_>>();
        for bb_id in jump_in {
            vis.insert(*bb_id);
        }
        for (id, val) in bindings {
            if let Some(val) = val {
                res.insert((id, *val));
            } else {
                self.resolve_ident_binding_bb_unique(ident, id, vis, res);
            }
        }
    }

    fn resolve_ident_binding(
        &self,
        ident: &str,
        pos: Pos,
        bb: mir::BBId,
    ) -> Option<Either<mir::VarId, HashSet<(mir::BBId, mir::VarId)>>> {
        let var_pos = self.var_pos_table.get(ident)?;
        let last_def = var_pos.range(..pos).last();
        match last_def {
            None => {
                let mut res = HashSet::new();
                let mut vis = HashSet::new();
                self.resolve_ident_binding_bb_unique(ident, bb, &mut vis, &mut res);

                if res.len() > 0 {
                    Some(Either::Right(res))
                } else {
                    None
                }
            }
            Some((pos, id)) => {
                let same_bb = self
                    .bb_pos_table
                    .range(..pos)
                    .last()
                    .map_or(false, |(_, last_id)| *last_id == bb);
                if same_bb {
                    Some(either::Left(*id))
                } else {
                    let mut res = HashSet::new();
                    let mut vis = HashSet::new();
                    self.resolve_ident_binding_bb_unique(ident, bb, &mut vis, &mut res);

                    Some(Either::Right(res))
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

    fn val_ty(&self, val: &mir::Value) -> Option<mir::Ty> {
        match val {
            mir::Value::IntImm(_) => Some(mir::Ty::int()),
            mir::Value::FloatImm(_) => Some(mir::Ty::double()),
            mir::Value::Var(v) => match v.0 {
                mir::VarTy::Local => self.get_ty(v.1).map(|ty| ty.clone()),
                mir::VarTy::Global => todo!("Implement global values"),
            },
            // TODO: Support register values?
            mir::Value::Reg(_) => None,
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
            ast::StmtVariant::While(_) => todo!("Generate code for statement {:#?}", stmt),
            ast::StmtVariant::Block(_) => todo!("Generate code for statement {:#?}", stmt),
            ast::StmtVariant::Expr(_) => todo!("Generate code for statement {:#?}", stmt),
            ast::StmtVariant::Print(_) => todo!("Generate code for statement {:#?}", stmt),
            ast::StmtVariant::Scan(_) => todo!("Generate code for statement {:#?}", stmt),
            ast::StmtVariant::ManyExpr(_) => todo!("Generate code for statement {:#?}", stmt),
            ast::StmtVariant::Return(_) => todo!("Generate code for statement {:#?}", stmt),
            ast::StmtVariant::Break => todo!("Generate code for statement {:#?}", stmt),
            ast::StmtVariant::Empty => todo!("Generate code for statement {:#?}", stmt),
        }
    }

    fn gen_expr(
        &mut self,
        expr: &ast::Expr,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::Value> {
        match &expr.var {
            ast::ExprVariant::Ident(ident) => self.gen_ident_expr(ident, expr.span, bb, scope),
            ast::ExprVariant::Literal(lit) => self.gen_literal_expr(lit, expr.span, bb, scope),
            ast::ExprVariant::TypeConversion(ty_conv) => {
                self.gen_type_conversion(ty_conv, expr.span, bb, scope)
            }
            ast::ExprVariant::UnaryOp(op) => self.gen_unary_op(op, expr.span, bb, scope),
            ast::ExprVariant::BinaryOp(op) => self.gen_binary_op(op, expr.span, bb, scope),
            ast::ExprVariant::FunctionCall(op) => self.gen_fn_call(op, expr.span, bb, scope),
            ast::ExprVariant::ArrayChild(_) => todo!(),
        }
    }

    fn gen_ident_expr(
        &mut self,
        ident: &ast::Identifier,
        span: Span,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::Value> {
        let (def, depth) = scope.borrow().find_def_depth(&ident.name).ok_or_else(|| {
            compile_err(CompileErrorVar::NonExistVar(ident.name.clone()), Some(span))
        })?;
        let ident_fmt = format_ident(&ident.name, depth);
        if depth == 0 {
            // global value
            todo!("Resolve global variable")
        } else {
            let ident = self.resolve_ident_binding(&ident_fmt, span.start, bb);
            let ty = self.resolve_ty(
                &def.borrow().get_sym().expect("is symbol").0.borrow(),
                scope.clone(),
            )?;

            match ident {
                None => Err(compile_err(
                    CompileErrorVar::NonExistVar(ident_fmt),
                    Some(span),
                )),
                Some(Either::Left(id)) => Ok(mir::Value::Var(mir::VarRef(mir::VarTy::Local, id))),
                Some(Either::Right(ids)) => {
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
            ast::Literal::String { val } => todo!("Support strings"),
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
        let end_ty = self.resolve_ty(&*ty_conv.to.borrow(), scope.clone())?;
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
        todo!()
    }

    fn gen_fn_call(
        &mut self,
        op: &ast::FunctionCall,
        span: Span,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::Value> {
        todo!()
    }

    fn gen_if_stmt(
        &mut self,
        stmt: &ast::IfConditional,
        bb: mir::BBId,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<mir::BBId> {
        todo!("If conditional")
    }
}
