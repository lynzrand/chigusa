use crate::c0::ast;
use crate::minivm::{
    compile_err, compile_err_n, CompileError, CompileErrorVar, CompileResult, WithSpan,
};
use crate::mir;
use crate::prelude::*;
use std::{
    borrow::Borrow,
    collections::{BTreeMap, HashMap, HashSet},
};

#[derive(Debug)]
pub struct Codegen<'src> {
    src: &'src ast::Program,
    pkg: mir::MirPackage,
    global_var_table: HashMap<usize, mir::Var>,
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
        todo!()
    }

    fn add_assign_entry(&mut self, assignment: &str, pos: Pos, bb: mir::BBId) {
        let var = self.var_id_counter;
        self.var_id_counter += 1;

        if !self.var_bb_table.contains_key(assignment) {
            self.var_bb_table.insert(assignment.into(), HashMap::new());
        }
        if !self.var_pos_table.contains_key(assignment) {
            self.var_pos_table
                .insert(assignment.into(), BTreeMap::new());
        }

        // ! The variable ID will replace the one specified in the last assignment,
        // ! this is intended behavior! The hashmap tracks for the **last** occurring
        // ! variable ID inside this basic block.
        self.var_bb_table
            .get_mut(assignment)
            .expect("The bb entry must be present")
            .insert(bb, var);

        self.var_pos_table
            .get_mut(assignment)
            .expect("The span entry must be present")
            .insert(pos, var);
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
                    let name = self.get_l_value_name(&*info.lhs.borrow(), scope.clone())?;
                    self.add_assign_entry(&name, expr.span.end, bb);
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
        let (_, depth) = scope
            .borrow()
            .find_def_depth(name)
            .ok_or_else(|| compile_err(CompileErrorVar::NonExistVar(name.clone()), Some(span)))?;
        let name = format_ident(name, depth);
        self.add_assign_entry(&name, span.end, bb);

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

        // ! This is the only place we set the value of BasicBlk.end to
        // ! not `Unknown`, because break target information will not be
        // ! available afterward.
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

    fn get_l_value_name(
        &mut self,
        expr: &ast::Expr,
        scope: Ptr<ast::Scope>,
    ) -> CompileResult<String> {
        match &expr.var {
            ast::ExprVariant::Ident(i) => {
                let (_, depth) = scope.borrow().find_def_depth(&i.name).ok_or_else(|| {
                    compile_err(
                        CompileErrorVar::NonExistVar(i.name.clone()),
                        Some(expr.span),
                    )
                })?;
                let name = format_ident(&i.name, depth);
                Ok(name)
            }
            _ => Err(CompileErrorVar::NotLValue(format!("{}", expr))).with_span(expr.span),
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
    ) -> CompileResult<(mir::Value, mir::BBId)> {
        let value = match &expr.var {
            ast::ExprVariant::Ident(ident) => self.gen_ident_expr(ident, expr.span, bb, scope)?,
            ast::ExprVariant::Literal(_) => todo!(),
            ast::ExprVariant::TypeConversion(_) => todo!(),
            ast::ExprVariant::UnaryOp(_) => todo!(),
            ast::ExprVariant::BinaryOp(_) => todo!(),
            ast::ExprVariant::FunctionCall(_) => todo!(),
            ast::ExprVariant::ArrayChild(_) => todo!(),
        };
        Ok((value, bb))
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
        if depth == 0 {
            // global value
            todo!("Resolve global variable")
        } else {
            // local value
            // self.var_bb_table
            //     .get((&ident.name, depth))
            //     .ok_or(|| compile_err(CompileErrorVar::NonExistVar(ident.name.into()), Some(span)))?
            //     .get();
            todo!()
        }
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
