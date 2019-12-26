use super::ast::*;
use super::err::*;
use super::lexer::*;
use crate::prelude::*;
use std::iter::Iterator;

pub trait IntoParser<T>
where
    T: Iterator<Item = Token>,
{
    fn into_parser(self) -> Parser<T>;
}

impl<L> IntoParser<Lexer<L>> for Lexer<L>
where
    L: Iterator<Item = char>,
{
    fn into_parser(self) -> Parser<Lexer<L>> {
        Parser::new(self)
    }
}

pub struct Parser<T>
where
    T: Iterator<Item = Token>,
{
    lexer: T,
    cur: Token,
}

impl<T> Parser<T>
where
    T: Iterator<Item = Token>,
{
    pub fn new(lexer: T) -> Parser<T> {
        log::info!("Created a new parser.");

        let mut parser = Parser {
            lexer,
            // type_var: TypeVar::new(),
            cur: Token::dummy(),
        };
        parser.bump();
        parser
    }

    fn bump(&mut self) -> Token {
        let mut next = self.lexer.next().unwrap_or_else(|| Token::eof());
        std::mem::swap(&mut self.cur, &mut next);

        log::trace!("Bump token pointer. Current: {:#}", self.cur);
        next
    }

    fn check(&self, accept: &TokenType) -> bool {
        variant_eq(&self.cur.var, accept)
    }

    fn expect(&mut self, accept: &TokenType) -> bool {
        if self.check(accept) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn check_report(&mut self, accept: &TokenType) -> ParseResult<()> {
        if self.check(accept) {
            Ok(())
        } else {
            Err(parse_err(
                // We used clone here, because once we meet an error we no longer
                // need to worry about performance. Things're gonna fail anyway.
                ParseErrVariant::ExpectToken(accept.clone(), self.cur.var.clone()),
                self.cur.span,
            ))
        }
    }

    fn expect_report(&mut self, accept: &TokenType) -> ParseResult<()> {
        if self.expect(accept) {
            Ok(())
        } else {
            Err(parse_err(
                // We used clone here, because once we meet an error we no longer
                // need to worry about performance. Things're gonna fail anyway.
                ParseErrVariant::ExpectToken(accept.clone(), self.cur.var.clone()),
                self.cur.span,
            ))
        }
    }

    fn check_one_of(&mut self, accept: &[TokenType]) -> bool {
        accept.iter().any(|x| variant_eq(&self.cur.var, x))
    }

    fn expect_one_of(&mut self, accept: &[TokenType]) -> bool {
        if self.check_one_of(accept) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn check_one_of_report(&mut self, accept: &[TokenType]) -> ParseResult<()> {
        if self.check_one_of(accept) {
            Ok(())
        } else {
            Err(parse_err(
                // We used clone here, because once we meet an error we no longer
                // need to worry about performance. Things're gonna fail anyway.
                ParseErrVariant::ExpectTokenOneOf(
                    accept.iter().map(|x| x.clone()).collect(),
                    self.cur.var.clone(),
                ),
                self.cur.span,
            ))
        }
    }

    fn expect_one_of_report(&mut self, accept: &[TokenType]) -> ParseResult<()> {
        if self.expect_one_of(accept) {
            Ok(())
        } else {
            Err(parse_err(
                // We used clone here, because once we meet an error we no longer
                // need to worry about performance. Things're gonna fail anyway.
                ParseErrVariant::ExpectTokenOneOf(
                    accept.iter().map(|x| x.clone()).collect(),
                    self.cur.var.clone(),
                ),
                self.cur.span,
            ))
        }
    }

    pub fn parse(&mut self) -> ParseResult<Program> {
        log::info!("Init parsing");
        self.p_program()
    }

    fn inject_std(scope: Ptr<Scope>) {
        log::info!("Injecting std types");
        let mut scope = scope.borrow_mut();

        // Declaration of `int`: i32
        scope
            .insert_def(
                "void",
                SymbolDef::Typ {
                    def: Ptr::new(TypeDef::Unit),
                },
            )
            .expect("Failed to inject primitive type `int`");

        // Declaration of `int`: i32
        scope
            .insert_def(
                "int",
                SymbolDef::Typ {
                    def: Ptr::new(TypeDef::Primitive(PrimitiveType {
                        var: PrimitiveTypeVar::SignedInt,
                        occupy_bytes: 4,
                    })),
                },
            )
            .expect("Failed to inject primitive type `int`");

        // Declaration of `double` - f64
        scope
            .insert_def(
                "double",
                SymbolDef::Typ {
                    def: Ptr::new(TypeDef::Primitive(PrimitiveType {
                        var: PrimitiveTypeVar::Float,
                        occupy_bytes: 8,
                    })),
                },
            )
            .expect("Failed to inject primitive type `float`");

        // Declaration of `char - u8
        scope
            .insert_def(
                "char",
                SymbolDef::Typ {
                    def: Ptr::new(TypeDef::Primitive(PrimitiveType {
                        var: PrimitiveTypeVar::UnsignedInt,
                        occupy_bytes: 1,
                    })),
                },
            )
            .expect("Failed to inject primitive type `char`");
    }

    fn p_program(&mut self) -> ParseResult<Program> {
        log::info!("Starts parsing program");
        let root_scope = Ptr::new(Scope::new());
        Self::inject_std(root_scope.cp());
        let mut stmts = Vec::new();
        while self.cur.var != TokenType::EndOfFile {
            stmts.push(self.p_decl_stmt(root_scope.cp())?)
        }
        log::info!("Finished parsing program");
        Ok(Program {
            blk: Block {
                scope: root_scope,
                stmts,
                span: None,
            },
        })
    }

    fn p_stmt(&mut self, scope: Ptr<Scope>) -> ParseResult<Stmt> {
        log::debug!("Parse statement");

        match &self.cur.var {
            TokenType::LCurlyBrace => self.p_block_stmt(scope),
            TokenType::Identifier(..) => self.p_decl_or_expr(scope),
            TokenType::If => self.p_if_stmt(scope),
            TokenType::While => self.p_while_stmt(scope),
            TokenType::Scan => self.p_scan_stmt(scope),
            TokenType::Print => self.p_print_stmt(scope),
            TokenType::Break => self.p_break_stmt(scope),
            TokenType::Return => {
                let ret = self.bump();
                if self.expect(&TokenType::Semicolon) {
                    Ok(Stmt {
                        var: StmtVariant::Return(None),
                        span: ret.span,
                    })
                } else {
                    let expr =
                        self.p_base_expr(&[TokenType::Semicolon, TokenType::RCurlyBrace], scope)?;
                    let span = expr.borrow().span();
                    self.expect_report(&TokenType::Semicolon)?;
                    Ok(Stmt {
                        var: StmtVariant::Return(Some(expr)),
                        span,
                    })
                }
            }
            // TokenType::Do => todo!("Parse do-while loop"),
            // TokenType::For => todo!("Parse for loop"),
            TokenType::Const => self.p_decl_stmt(scope),
            TokenType::LParenthesis
            | TokenType::LBracket
            | TokenType::Literal(..)
            | TokenType::Multiply
            | TokenType::Increase
            | TokenType::Decrease => self.p_expr_stmt(scope),
            _ => Err(parse_err(
                ParseErrVariant::UnexpectedTokenMsg {
                    typ: self.cur.var.clone(),
                    msg: "This token cannot start a statement",
                },
                self.cur.span,
            )),
        }
    }

    fn p_decl_or_expr(&mut self, scope: Ptr<Scope>) -> ParseResult<Stmt> {
        match &self.cur.var {
            TokenType::Identifier(ident) => {
                let entry = scope.borrow().find_def(ident);
                match entry {
                    None => Err(parse_err(
                        ParseErrVariant::CannotFindIdent(ident.into()),
                        self.cur.span,
                    )),
                    Some(entry) => {
                        let entry = entry.borrow();
                        match &*entry {
                            &SymbolDef::Typ { .. } => self.p_decl_stmt(scope.cp()),
                            &SymbolDef::Var { .. } => self.p_expr_stmt(scope.cp()),
                        }
                    }
                }
            }

            // Statements starting with bracket `[` are always declarations (of arrays)
            TokenType::LBracket => self.p_decl_stmt(scope),

            // Statements starting with `++`, `--`, `*` and `(` are always expressions
            TokenType::Increase
            | TokenType::Decrease
            | TokenType::Multiply
            | TokenType::LParenthesis => self.p_expr_stmt(scope),

            _ => unreachable!(),
        }
    }

    fn p_block_stmt(&mut self, scope: Ptr<Scope>) -> ParseResult<Stmt> {
        log::trace!("Creating new scope for upcoming block");

        let new_scope = Ptr::new(Scope::new_with_parent(scope));
        self.p_block_stmt_no_scope(new_scope)
    }
    fn p_block_stmt_no_scope(&mut self, scope: Ptr<Scope>) -> ParseResult<Stmt> {
        let (block, span) = self.p_block_no_scope(scope)?;
        Ok(Stmt {
            var: StmtVariant::Block(block),
            span,
        })
    }
    fn p_block_no_scope(&mut self, scope: Ptr<Scope>) -> ParseResult<(Block, Span)> {
        log::debug!("Parsing block");

        let l_span = self.cur.span;
        self.expect_report(&TokenType::LCurlyBrace)?;
        let mut stmts = Vec::new();

        // For each statement, parse
        while !self.check(&TokenType::RCurlyBrace) {
            let stmt = self.p_stmt(scope.cp())?;
            stmts.push(stmt);
        }

        let r_span = self.cur.span;
        self.expect_report(&TokenType::RCurlyBrace)?;

        log::debug!("Block ends");
        Ok((
            Block {
                scope,
                stmts,
                span: Some(l_span + r_span),
            },
            l_span + r_span,
        ))
    }

    fn p_type_name(&mut self, scope: Ptr<Scope>) -> ParseResult<Ptr<TypeDef>> {
        log::trace!("Parsing type name");

        let tok = self.bump();
        match tok.var {
            TokenType::BinaryAnd => Ok(Ptr::new(TypeDef::Ref(RefType {
                target: self.p_type_name(scope)?,
            }))),
            TokenType::LBracket => {
                let typ = TypeDef::Array(ArrayType {
                    target: self.p_type_name(scope)?,
                    length: None,
                });
                self.expect_report(&TokenType::RBracket)?;
                Ok(Ptr::new(typ))
            }
            TokenType::Identifier(ident) => {
                let span = tok.span;
                match scope.borrow().find_def(&ident) {
                    None => Err(parse_err(
                        ParseErrVariant::CannotFindType(ident.into()),
                        span,
                    )),
                    Some(def) => match &*def.borrow() {
                        // TODO: Add generics?
                        SymbolDef::Typ { .. } => Ok(Ptr::new(TypeDef::NamedType(ident.into()))),
                        _ => Err(parse_err(
                            ParseErrVariant::CannotFindType(ident.into()),
                            span,
                        )),
                    },
                }
            }
            _ => Err(parse_err(
                ParseErrVariant::UnexpectedToken(tok.var),
                tok.span,
            )),
        }
    }

    /// Parse a function, optionally with its body.
    fn p_fn(
        &mut self,
        type_decl: Ptr<TypeDef>,
        decl_token: Token,
        scope: Ptr<Scope>,
    ) -> ParseResult<Stmt> {
        let left_span = self.cur.span;
        self.expect_report(&TokenType::LParenthesis)?;
        // The expressions in function call
        let mut expr_vec = Vec::new();
        let mut inner_scope = Scope::new_with_parent(scope.cp());

        if !self.check(&TokenType::RParenthesis) {
            let param_type = self.p_type_name(scope.cp())?;
            self.check_report(&TokenType::Identifier(String::new()))?;
            let ident = self.bump();
            let ident_str = ident.get_ident().unwrap();
            inner_scope.insert_def(
                ident_str,
                SymbolDef::Var {
                    typ: param_type.cp(),
                    is_const: false,
                    decl_span: ident.span,
                },
            )?;
            expr_vec.push((param_type, ident_str.to_owned()));
            while self.expect(&TokenType::Comma) {
                let param_type = self.p_type_name(scope.cp())?;
                self.check_report(&TokenType::Identifier(String::new()))?;
                let ident = self.bump();
                let ident_str = ident.get_ident().unwrap();
                inner_scope.insert_def(
                    ident_str,
                    SymbolDef::Var {
                        typ: param_type.cp(),
                        is_const: false,
                        decl_span: ident.span,
                    },
                )?;
                expr_vec.push((param_type, ident_str.to_owned()));
            }
        }
        let inner_scope = Ptr::new(inner_scope);

        log::info!(
            "Parse function \"{}\" with type {:?}, params: {:?}",
            decl_token.get_ident().unwrap(),
            type_decl,
            expr_vec
        );

        let right_span = self.cur.span;
        self.expect_report(&TokenType::RParenthesis)?;
        let span = left_span + right_span;

        // Insert function declaration
        scope.borrow_mut().insert_def(
            decl_token.get_ident().unwrap(),
            SymbolDef::Var {
                typ: Ptr::new(TypeDef::Function(FunctionType {
                    return_type: type_decl.cp(),
                    params: expr_vec.iter().map(|x| x.0.clone()).collect(),
                    body: None,
                    is_extern: false,
                })),
                is_const: false,
                decl_span: span,
            },
        )?;

        let (body, body_span) = self.p_block_no_scope(inner_scope.cp())?;

        // Insert function declaration again with body
        scope.borrow_mut().insert_def(
            decl_token.get_ident().unwrap(),
            SymbolDef::Var {
                typ: Ptr::new(TypeDef::Function(FunctionType {
                    return_type: type_decl.cp(),
                    params: expr_vec.iter().map(|x| x.0.clone()).collect(),
                    body: Some(body),
                    is_extern: false,
                })),
                is_const: false,
                decl_span: span,
            },
        )?;

        Ok(Stmt {
            var: StmtVariant::Empty,
            span: left_span + right_span + body_span,
        })
    }

    fn p_decl_stmt(&mut self, scope: Ptr<Scope>) -> ParseResult<Stmt> {
        // This is the identifier token

        let init_span = self.cur.span;
        let is_const = self.expect(&TokenType::Const);
        let type_decl = self.p_type_name(scope.cp())?;
        let mut has_next = true;
        let mut exprs = Vec::new();

        while has_next {
            self.check_report(&TokenType::Identifier(String::new()))?;
            let mut span = self.cur.span;
            let ident = self.bump();

            if self.check(&TokenType::LParenthesis) {
                // * This checks if the ident declared is a function. If true,
                // * immediately end this algorithm and switch to function
                // * parsing.
                // TODO: Any possible changes?
                return self.p_fn(type_decl, ident, scope);
            }

            let init_val = if self.expect(&TokenType::Assign) {
                let expr =
                    self.p_base_expr(&[TokenType::Comma, TokenType::Semicolon], scope.cp())?;
                span = span + expr.borrow().span;
                Some(expr)
            } else {
                None
            };

            if is_const && init_val.is_none() {
                Err(parse_err(
                    ParseErrVariant::ConstTypeNeedExplicitInitialization,
                    ident.span,
                ))?;
            }

            scope.borrow_mut().insert_def(
                ident.get_ident().unwrap(),
                SymbolDef::Var {
                    typ: type_decl.cp(),
                    is_const,
                    decl_span: span,
                },
            )?;

            if let Some(val) = init_val {
                let span = ident.span + val.borrow().span();
                exprs.push(Ptr::new(Expr {
                    var: ExprVariant::BinaryOp(BinaryOp {
                        op: if is_const { OpVar::_Csn } else { OpVar::_Asn },
                        lhs: Ptr::new(Expr {
                            var: ExprVariant::Ident(Identifier {
                                name: ident.get_ident().unwrap().into(),
                            }),
                            span: ident.span,
                        }),
                        rhs: val,
                    }),
                    span,
                }))
            }

            has_next = self.expect(&TokenType::Comma);
        }

        let span = exprs
            .iter()
            .fold(init_span, |last, this| this.borrow().span() + last);

        self.expect_report(&TokenType::Semicolon)?;
        Ok(Stmt {
            var: StmtVariant::ManyExpr(exprs),
            span,
        })
    }

    fn p_while_stmt(&mut self, scope: Ptr<Scope>) -> ParseResult<Stmt> {
        let mut span = self.cur.span;

        self.expect_report(&TokenType::While)?;

        self.expect_report(&TokenType::LParenthesis)?;

        let cond = self.p_base_expr(&[TokenType::RParenthesis], scope.cp())?;

        self.expect_report(&TokenType::RParenthesis)?;

        let block = Ptr::new({
            let stmt = self.p_stmt(scope.cp())?;
            span = span + stmt.span();
            stmt
        });

        Ok(Stmt {
            var: StmtVariant::While(WhileConditional { cond, block }),
            span,
        })
    }

    fn p_if_stmt(&mut self, scope: Ptr<Scope>) -> ParseResult<Stmt> {
        let mut span = self.cur.span;

        self.expect_report(&TokenType::If)?;

        self.expect_report(&TokenType::LParenthesis)?;

        let cond = self.p_base_expr(&[TokenType::RParenthesis], scope.cp())?;

        self.expect_report(&TokenType::RParenthesis)?;

        let if_block = Ptr::new({
            let stmt = self.p_stmt(scope.cp())?;
            span = span + stmt.span();
            stmt
        });

        let else_block = if self.expect(&TokenType::Else) {
            let stmt = self.p_stmt(scope.cp())?;
            span = span + stmt.span();
            Some(Ptr::new(stmt))
        } else {
            None
        };

        Ok(Stmt {
            var: StmtVariant::If(IfConditional {
                cond,
                if_block,
                else_block,
            }),
            span,
        })
    }

    fn p_print_stmt(&mut self, scope: Ptr<Scope>) -> ParseResult<Stmt> {
        let mut span = self.cur.span;
        self.expect_report(&TokenType::Print)?;
        self.expect_report(&TokenType::LParenthesis)?;

        let mut exprs = Vec::new();

        {
            // Cannot have an empty print statement
            let first_expr =
                self.p_base_expr(&[TokenType::RParenthesis, TokenType::Comma], scope.cp())?;
            span = span + first_expr.borrow().span();
            exprs.push(first_expr);
        }

        while !self.expect(&TokenType::RParenthesis) {
            self.expect_report(&TokenType::Comma)?;
            let expr =
                self.p_base_expr(&[TokenType::RParenthesis, TokenType::Comma], scope.cp())?;
            span = span + expr.borrow().span();
            exprs.push(expr);
        }
        self.expect_report(&TokenType::Semicolon)?;

        Ok(Stmt {
            var: StmtVariant::Print(exprs),
            span,
        })
    }

    fn p_scan_stmt(&mut self, scope: Ptr<Scope>) -> ParseResult<Stmt> {
        let span = self.cur.span;
        self.expect_report(&TokenType::Scan)?;
        self.expect_report(&TokenType::LParenthesis)?;
        self.check_report(&TokenType::Identifier(String::new()))?;
        let ident = self.bump();
        let ident = ident.get_ident().unwrap().to_owned();
        let ident = Identifier { name: ident };
        self.expect_report(&TokenType::RParenthesis)?;
        let span = span + self.cur.span;
        self.expect_report(&TokenType::Semicolon)?;
        Ok(Stmt {
            var: StmtVariant::Scan(ident),
            span,
        })
    }

    fn p_break_stmt(&mut self, scope: Ptr<Scope>) -> ParseResult<Stmt> {
        let span = self.cur.span;
        self.expect_report(&TokenType::Break)?;
        self.expect_report(&TokenType::Semicolon)?;

        Ok(Stmt {
            var: StmtVariant::Break,
            span,
        })
    }

    fn p_expr_stmt(&mut self, scope: Ptr<Scope>) -> ParseResult<Stmt> {
        // TODO: Subject to change
        let expr = self.p_base_expr(
            &[
                TokenType::Semicolon,
                TokenType::RParenthesis,
                TokenType::RBracket,
                TokenType::RCurlyBrace,
            ],
            scope,
        )?;
        let span = expr.borrow().span();
        self.expect_report(&TokenType::Semicolon)?;
        Ok(Stmt {
            var: StmtVariant::Expr(expr),
            span,
        })
    }

    fn p_base_expr(
        &mut self,
        close_delim: &[TokenType],
        scope: Ptr<Scope>,
    ) -> ParseResult<Ptr<Expr>> {
        let mut expr = None;
        while !self.check_one_of(close_delim) {
            expr = Some(self.p_binary_op(expr, 0, close_delim, scope.cp())?);
        }
        expr.ok_or_else(|| {
            parse_err_z(ParseErrVariant::InternalErr(
                "Invalid branching into expression parsing".into(),
            ))
        })
    }

    /// Parses a binary operator with at least the precedence specified.
    ///
    /// Design stolen from https://github.com/rust-lang/rust/blob/b5f265eeed23ac87ec6b4a7e6bc7cb4ea3e67c31/src/librustc_parse/parser/expr.rs#L141
    fn p_binary_op(
        &mut self,
        lhs: Option<Ptr<Expr>>,
        expect_prec: isize,
        close_delim: &[TokenType],
        scope: Ptr<Scope>,
    ) -> ParseResult<Ptr<Expr>> {
        let lhs = if lhs.is_some() {
            lhs.unwrap()
        } else {
            self.p_prefix_unary_op(scope.cp())?
        };

        // Op should be self.cur
        if let Some(op) = self.cur.var.into_op(false, false) {
            let mut lhs = lhs;
            let mut op = op;
            while !close_delim.contains(&self.cur.var)
                && ((op.is_left_associative() && op.priority() > expect_prec)
                    || (op.is_right_associative() && op.priority() >= expect_prec))
            {
                self.bump();
                let rhs = self.p_binary_op(None, op.priority(), close_delim, scope.cp())?;
                let span = { lhs.borrow().span() + rhs.borrow().span() };
                lhs = Ptr::new(Expr {
                    var: ExprVariant::BinaryOp(BinaryOp { lhs, rhs, op }),
                    span,
                });

                if let Some(op_) = self.cur.var.into_op(false, false) {
                    op = op_;
                } else {
                    break;
                }
            }
            Ok(lhs)
        } else {
            if close_delim.contains(&self.cur.var) {
                Ok(lhs)
            } else {
                Err(parse_err(
                    ParseErrVariant::UnexpectedTokenMsg {
                        typ: self.cur.var.clone(),
                        msg: "Token cannot be here in an expression",
                    },
                    self.cur.span,
                ))
            }
        }
    }

    fn p_prefix_unary_op(&mut self, scope: Ptr<Scope>) -> ParseResult<Ptr<Expr>> {
        let mut op_vec = Vec::new();
        while let Some(op) = self.cur.var.into_op(true, false) {
            op_vec.push((op, self.cur.span));
            self.bump();
        }
        let mut expr = self.p_postfix_unary_op(scope)?;
        while let Some((op, span)) = op_vec.pop() {
            let span = span + expr.borrow().span();
            expr = Ptr::new(Expr {
                var: ExprVariant::UnaryOp(UnaryOp { op, val: expr }),
                span,
            });
        }
        Ok(expr)
    }

    fn p_postfix_unary_op(&mut self, scope: Ptr<Scope>) -> ParseResult<Ptr<Expr>> {
        let mut expr = self.p_item(scope.cp())?;
        loop {
            if let Some(op) = self.cur.var.into_op(false, true) {
                expr = Ptr::new(Expr {
                    var: ExprVariant::UnaryOp(UnaryOp { op, val: expr }),
                    span: self.cur.span,
                });
                self.bump();
            } else if self.cur.var == TokenType::LBracket {
                // Parse index operator
                self.bump();
                let idx = self.p_base_expr(&[TokenType::RBracket], scope.cp())?;
                self.expect_report(&TokenType::RBracket)?;
                expr = Ptr::new(Expr {
                    var: ExprVariant::ArrayChild(ArrayChild { val: expr, idx }),
                    span: self.cur.span,
                });
            // TODO: Add parsing for struct child (later)
            // } else if self.cur.var == TokenType::Dot {
            //     // Parse child operator
            //     self.bump();
            //     match self.cur.var{
            //         TokenType::Identifier(s)=>{
            //             expr = Ptr::new(Expr{
            //                 var: ExprVariant::StructChild(StructChild{
            //                     val: (),
            //                     idx: (),

            //                 }),
            //                 span: (),
            //             }),
            //             _=>todo!()
            //         }
            //     }
            } else {
                // There's no postfix unary operator for us to parse
                break;
            }
        }
        Ok(expr)
    }

    /// Parse an item in expression
    ///
    /// An item is either a expression wrapped in parentheses, or an identifier,
    /// or a literal value.
    fn p_item(&mut self, scope: Ptr<Scope>) -> ParseResult<Ptr<Expr>> {
        if self.expect(&TokenType::LParenthesis) {
            // * F_CK, there's a nasty explicit cast operation here.
            // * This should be a preceding operator, but it is placed here to avoid backtracking.
            let is_implicit_conv = if let TokenType::Identifier(i) = &self.cur.var {
                let def = scope.borrow().find_def(i);
                if let Some(def) = def {
                    if let SymbolDef::Typ { .. } = &*def.borrow() {
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            } else {
                false
            };

            if is_implicit_conv {
                // You like that implicit conversion? It nearly ruined this non-backtracking parser!
                let span = self.cur.span;
                let typ = self.p_type_name(scope.cp())?;
                self.expect_report(&TokenType::RParenthesis)?;
                let expr = self.p_item(scope.cp())?;
                let span = span + expr.borrow().span();
                Ok(Ptr::new(Expr {
                    var: ExprVariant::TypeConversion(TypeConversion { to: typ, expr }),
                    span,
                }))
            } else {
                // It's a new expression tree.
                // Start a new parsing cycle!
                let expr = self.p_base_expr(&[TokenType::RParenthesis], scope);
                self.expect_report(&TokenType::RParenthesis)?;
                expr
            }
        } else {
            if self.check(&TokenType::Literal(super::lexer::Literal::_Dummy)) {
                self.p_literal()
            } else if self.check(&TokenType::Identifier(String::new())) {
                self.p_ident_or_fn_call(scope)
            } else {
                Err(parse_err(
                    ParseErrVariant::ExpectTokenOneOf(
                        vec![
                            TokenType::Literal(super::lexer::Literal::_Dummy),
                            TokenType::Identifier(String::new()),
                            TokenType::LParenthesis,
                        ],
                        self.cur.var.clone(),
                    ),
                    self.cur.span,
                ))
            }
        }
    }

    /// Parse an identifier or function call.
    ///
    /// This parser accepts a starting state when `self.cur` is the first `Identifier`
    fn p_ident_or_fn_call(&mut self, scope: Ptr<Scope>) -> ParseResult<Ptr<Expr>> {
        self.check_report(&TokenType::Identifier(String::new()))?;
        let cur = self.bump();
        if self.check(&TokenType::LParenthesis) {
            self.p_fn_call(&cur, scope)
        } else {
            // * No parenthesis -> simple identifier!
            let ident = scope
                .borrow()
                .find_def(cur.get_ident().unwrap())
                .ok_or(parse_err(
                    ParseErrVariant::CannotFindIdent(cur.get_ident().unwrap().to_owned()),
                    cur.span,
                ))?;
            let ident = &*ident.borrow();
            match ident {
                SymbolDef::Typ { .. } => Err(parse_err(
                    ParseErrVariant::ExpectToBeVar(cur.get_ident().unwrap().into()),
                    cur.span,
                )),
                SymbolDef::Var { typ, .. } => {
                    let typ = typ.borrow();
                    match &*typ {
                        TypeDef::Function(..) => Err(parse_err(
                            ParseErrVariant::ExpectToBeVar(cur.get_ident().unwrap().into()),
                            cur.span,
                        )),
                        _ => Ok(()),
                    }
                }
            }?;

            Ok(Ptr::new(Expr {
                var: ExprVariant::Ident(Identifier {
                    name: cur.get_ident().unwrap().to_owned(),
                }),
                span: cur.span,
            }))
        }
    }

    fn p_fn_call(&mut self, fn_tok: &Token, scope: Ptr<Scope>) -> ParseResult<Ptr<Expr>> {
        self.expect_report(&TokenType::LParenthesis)?;

        let func = scope
            .borrow()
            .find_def(fn_tok.get_ident().unwrap())
            .ok_or(parse_err(
                ParseErrVariant::CannotFindFn(fn_tok.get_ident().unwrap().to_owned()),
                fn_tok.span,
            ))?;

        // * Check if this is really a function
        let func = &*func.borrow();
        match func {
            SymbolDef::Typ { .. } => Err(parse_err(
                ParseErrVariant::ExpectToBeFn(fn_tok.get_ident().unwrap().into()),
                fn_tok.span,
            )),
            SymbolDef::Var { typ, .. } => {
                let typ = typ.borrow();
                match &*typ {
                    TypeDef::Function(..) => Ok(()),
                    _ => Err(parse_err(
                        ParseErrVariant::ExpectToBeFn(fn_tok.get_ident().unwrap().into()),
                        fn_tok.span,
                    )),
                }
            }
        }?;

        // The expressions in function call
        let mut expr_vec = Vec::new();

        if !self.check(&TokenType::RParenthesis) {
            expr_vec
                .push(self.p_base_expr(&[TokenType::Comma, TokenType::RParenthesis], scope.cp())?);
            while self.expect(&TokenType::Comma) {
                expr_vec.push(
                    self.p_base_expr(&[TokenType::Comma, TokenType::RParenthesis], scope.cp())?,
                );
            }
        }
        let right_span = self.cur.span;
        self.expect_report(&TokenType::RParenthesis)?;

        Ok(Ptr::new(Expr {
            var: ExprVariant::FunctionCall(FunctionCall {
                // TODO: How do we identify functions?
                func: fn_tok.get_ident().unwrap().to_owned(),
                params: expr_vec,
            }),
            span: fn_tok.span + right_span,
        }))
    }

    fn p_literal(&mut self) -> ParseResult<Ptr<Expr>> {
        let t = self.bump();
        match t.var {
            TokenType::Literal(i) => Ok(Ptr::new(Expr {
                var: ExprVariant::Literal(i.into()),
                span: t.span,
            })),
            v @ _ => Err(parse_err(
                ParseErrVariant::InternalErr(format!(
                    "Bad branching into literal parsing while getting a token type of `{}`",
                    v
                )),
                t.span,
            )),
        }
    }
}

trait IntoOperator {
    fn into_op(&self, suggest_unary: bool) -> Option<OpVar>;
}

impl TokenType {
    fn into_op(&self, unary_prefix: bool, unary_postfix: bool) -> Option<OpVar> {
        use OpVar::*;
        use TokenType::*;
        if unary_prefix {
            match self {
                Minus => Some(Neg),
                Plus => Some(Pos),
                Multiply => Some(Der),
                BinaryAnd => Some(Ref),
                Increase => Some(Inb),
                Decrease => Some(Deb),
                _ => None,
            }
        } else if unary_postfix {
            match self {
                Increase => Some(Ina),
                Decrease => Some(Dea),
                _ => None,
            }
        } else {
            match self {
                Minus => Some(Sub),
                Plus => Some(Add),
                Multiply => Some(Mul),
                Divide => Some(Div),
                Not => Some(Inv),
                BinaryAnd => Some(Ban),
                BinaryOr => Some(Bor),
                TokenType::And => Some(OpVar::And),
                TokenType::Or => Some(OpVar::Or),
                TokenType::Xor => Some(OpVar::Xor),
                Equals => Some(Eq),
                NotEquals => Some(Neq),
                LessThan => Some(Lt),
                GreaterThan => Some(Gt),
                LessOrEqualThan => Some(Lte),
                GreaterOrEqualThan => Some(Gte),
                Assign => Some(_Asn),
                Comma => Some(_Com),
                _ => None,
            }
        }
    }
}

trait Operator {
    fn priority(&self) -> isize;
    fn is_unary(&self) -> bool;
    fn is_right_associative(&self) -> bool;
    fn is_left_associative(&self) -> bool {
        !self.is_right_associative()
    }
}

impl Operator for OpVar {
    fn priority(&self) -> isize {
        // According to https://zh.cppreference.com/w/cpp/language/operator_precedence
        use OpVar::*;
        match self {
            _Dum => 0,
            _Lpr | _Rpr => 2,
            _Com => 8,
            _Asn | _Csn => 0,
            Eq | Neq => 13,
            Gt | Lt | Gte | Lte => 14,
            Or => 15,
            And => 16,
            Bor => 17,
            Xor => 18,
            Ban => 19,
            Add | Sub => 20,
            Mul | Div => 30,
            Neg | Pos | Inv | Bin | Ref | Der | Ina | Inb | Dea | Deb => 40,
        }
    }

    fn is_unary(&self) -> bool {
        use OpVar::*;
        match self {
            Neg | Pos | Inv | Bin | Ref | Der | Ina | Inb | Dea | Deb => true,
            _ => false,
        }
    }

    fn is_right_associative(&self) -> bool {
        use OpVar::*;
        match self {
            Neg | Pos | Inv | Bin | Ref | Der | _Asn | _Lpr | _Rpr => true,
            _ => false,
        }
    }
}
