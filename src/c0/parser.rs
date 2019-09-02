use super::{ast::*, infra::*, lexer::*};
use crate::set;
use either::Either;
use itertools::Itertools;
use lazy_static::lazy_static;
use std::collections::*;
use std::iter::{Iterator, Peekable};
use std::{cell::RefCell, fmt, fmt::Display, fmt::Formatter, rc::Rc};

use LoopCtrl::*;

pub trait IntoParser<'a> {
    fn into_parser(self) -> Parser<'a>;
}

impl<'a> IntoParser<'a> for LexerIterator<'a> {
    fn into_parser(self) -> Parser<'a> {
        Parser::new(self)
    }
}

pub trait TokenIterator<'a>: Iterator<Item = Token<'a>> + itertools::PeekingNext {
    fn expect(&mut self, token: TokenVariant<'a>) -> ParseResult<'a, Token<'a>> {
        // * separated variables because lifetime concerns.
        match self.next() {
            Some(t) => {
                if variant_eq(&t.var, &token) {
                    Ok(t)
                } else {
                    Err(parse_err(ParseErrVariant::ExpectToken(token), t.span))
                }
            }
            None => Err(parse_err(ParseErrVariant::ExpectToken(token), Span::zero())),
        }
    }

    fn expect_map_or<T>(
        &mut self,
        token: TokenVariant<'a>,
        map: impl FnOnce(Token<'a>) -> T,
        f: impl FnOnce(Token<'a>) -> Result<T, ParseError<'a>>,
    ) -> ParseResult<'a, T> {
        let next = self.next();
        match next {
            Some(v) => {
                if variant_eq(&v.var, &token) {
                    Ok(map(v))
                } else {
                    f(v)
                }
            }
            None => Err(parse_err(ParseErrVariant::ExpectToken(token), Span::zero())),
        }
    }

    fn try_consume(&mut self, token: TokenVariant<'a>) -> bool {
        match self.peeking_next(|v| variant_eq(&v.var, &token)) {
            Some(_) => true,
            None => false,
        }
    }

    fn try_consume_log_span(&mut self, token: TokenVariant<'a>) -> Option<Span> {
        match self.peeking_next(|v| variant_eq(&v.var, &token)) {
            Some(v) => Some(v.span),
            None => None,
        }
    }
}

type LexerWrapped<'a> = Peekable<LexerIterator<'a>>;

impl<'a> TokenIterator<'a> for LexerWrapped<'a> {}

pub struct Parser<'a> {
    lexer: LexerWrapped<'a>,
}

lazy_static! {
    static ref STMT_END_ON: HashSet<TokenVariant<'static>> = set![
        TokenVariant::RCurlyBrace,
        TokenVariant::EndOfFile,
        TokenVariant::Semicolon
    ];
    static ref PARAM_END_ON: HashSet<TokenVariant<'static>> = set![
        TokenVariant::RCurlyBrace,
        TokenVariant::RParenthesis,
        TokenVariant::EndOfFile,
        TokenVariant::Semicolon,
        TokenVariant::Comma
    ];
}

impl<'a> Parser<'a> {
    pub fn new(lexer: LexerIterator<'a>) -> Parser<'a> {
        let lexer = lexer.peekable();
        Parser {
            lexer,
            // stack: VecDeque::new(),
        }
    }

    pub fn parse(&mut self) -> ParseResult<'a, Program> {
        self.parse_program()
    }

    fn parse_program(&mut self) -> ParseResult<'a, Program> {
        let scope = Ptr::new(Scope::new(None));

        let mut program_stmts = Vec::new();

        Self::inject_std(scope.clone());

        let mut span = Span::zero();

        while self.lexer.peek().is_some() {
            let result = self.parse_decl_or_stmt(scope.clone())?;
            match result {
                Either::Left(stmt) => {
                    span = span + stmt.span();
                    match stmt {
                        Statement::Empty(_) => {}
                        stmt @ _ => program_stmts.push(stmt),
                    }
                }
                Either::Right(mut stmts) => {
                    span = stmts.iter().fold(span, |span, stmt| span + stmt.span());
                    program_stmts.append(&mut stmts);
                }
            }
        }
        Ok(Program {
            scope: scope.clone(),
            span,
            stmts: program_stmts,
        })
    }

    fn inject_std(scope: Ptr<Scope>) {
        let mut scope = scope.borrow_mut();

        let int_type = Ptr::new(TokenEntry::Type(TypeScopeDecl {
            is_primitive: true,
            occupy_bytes: 4,
        }));

        scope.try_insert("int", int_type.clone());

        let void_type = Ptr::new(TokenEntry::Type(TypeScopeDecl {
            is_primitive: true,
            occupy_bytes: 0,
        }));

        scope.try_insert("void", void_type.clone());

        let string_type = Ptr::new(TokenEntry::Type(TypeScopeDecl {
            is_primitive: true,
            occupy_bytes: 4,
        }));

        scope.try_insert("String", string_type.clone());

        scope.try_insert(
            "print",
            Ptr::new(TokenEntry::Function(FnScopeDecl {
                returns_type: void_type.clone(),
                params: vec![int_type.clone()],
                is_ffi: true,
                decl: None,
            })),
        );
    }

    /// Parse a declaration. Could either be a function or variable declaration.
    /// After the parsing completed, the coresponding declaration entry will be
    /// inserted into the symbol table defined in `scope`.
    ///
    /// # Returns
    ///
    /// This function returns `Ok(())` when no error occurs and `Err(e)` when
    /// encountering an error.
    ///
    /// # Todo
    ///
    /// > When parsing statement with value assignment, return multiple
    /// > statements so assignment happens in the right place.
    fn parse_decl(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, (Span, Vec<Statement>)> {
        let is_const = self.lexer.try_consume(TokenVariant::Const);
        let type_decl = self.lexer.expect(TokenVariant::Identifier(""))?;
        let type_name = type_decl.get_ident().unwrap();

        let var_type = scope
            .borrow()
            .find_definition(type_name)
            .ok_or_else(|| parse_err(ParseErrVariant::CannotFindType(type_name), type_decl.span))
            .and_then(|ptr_token| {
                if ptr_token.borrow().is_type() {
                    Ok(ptr_token)
                } else {
                    Err(parse_err(
                        ParseErrVariant::ExpectToBeType(type_name),
                        type_decl.span,
                    ))
                }
            })?;

        let identifier = self.lexer.expect(TokenVariant::Identifier(""))?;
        let identifier_owned: String = match identifier.var {
            TokenVariant::Identifier(s) => s.to_owned(),
            _ => return Err(parse_err_z(ParseErrVariant::InternalErr)),
        };
        let is_fn = self.lexer.peek().map_or_else(
            || Err(parse_err_z(ParseErrVariant::EarlyEof)),
            |token| Ok(variant_eq(&token.var, &TokenVariant::LParenthesis)),
        )?;

        if is_fn {
            // Functions cannot be const
            if is_const {
                return Err(parse_err(ParseErrVariant::NoConstFns, identifier.span));
            }

            // This thing is a function! the insersion and other stuff are done
            // in function call route
            let span = self.parse_fn_decl_rest(
                scope.clone(),
                var_type,
                identifier_owned,
                identifier.span,
            )? + type_decl.span;

            Ok((span, Vec::new()))
        // return;
        } else {
            let mut span = type_decl.span;

            let (entry, stmt) =
                self.parse_single_var_decl(scope.clone(), var_type.clone(), is_const)?;

            let mut stmts = Vec::new();

            let entry_ptr = Ptr::new(entry);

            if !scope
                .borrow_mut()
                .try_insert(&identifier_owned, entry_ptr.clone())
            {
                return Err(parse_err(
                    ParseErrVariant::TokenExists(&identifier.get_ident().unwrap()),
                    identifier.span,
                ));
            }

            if stmt.is_some() {
                let span = stmt.unwrap().span();
                let stmt = Statement::Expr(Expr::BinOp(BinaryOp {
                    var: OpVar::_Csn,
                    lhs: Ptr::new(Expr::Var(Identifier(entry_ptr))),
                    rhs: stmt.unwrap(),
                    span,
                }));
                stmts.push(stmt);
            }

            while self.lexer.try_consume(TokenVariant::Comma) {
                let identifier = self.lexer.expect(TokenVariant::Identifier(""))?;

                let identifier_owned: String = match identifier.var {
                    TokenVariant::Identifier(s) => s.to_owned(),
                    _ => return Err(parse_err_z(ParseErrVariant::InternalErr)),
                };

                let (entry, stmt) =
                    self.parse_single_var_decl(scope.clone(), var_type.clone(), is_const)?;

                if !scope
                    .borrow_mut()
                    .try_insert(&identifier_owned, Ptr::new(entry))
                {
                    return Err(parse_err(
                        ParseErrVariant::TokenExists(identifier.get_ident().unwrap()),
                        identifier.span,
                    ));
                }

                if stmt.is_some() {
                    let span = stmt.unwrap().span();
                    let stmt = Statement::Expr(Expr::BinOp(BinaryOp {
                        var: OpVar::_Csn,
                        lhs: Ptr::new(Expr::Var(Identifier(entry_ptr))),
                        rhs: stmt.unwrap(),
                        span,
                    }));
                    stmts.push(stmt);
                }

                span = span + identifier.span;
            }

            self.lexer.expect(TokenVariant::Semicolon)?;

            Ok((span, stmts))
        }
    }

    /// Parse the rest part of a function declaration.
    ///
    /// Parsing starts from the first parameter, after the parenthesis, as
    /// shown below.
    ///
    /// ```plaintext
    /// int          some_func           (   int          y           )   { ...
    /// Ident("int") Ident("some_func") "("" Ident("int") Ident("y") ")" "{"
    ///                          <- parsed   ^ parse starts from here -------->
    /// ```
    ///
    /// #### Params
    ///
    /// - `scope`: the scope at where this function is declared from. Not the
    ///     function's own scope.
    ///
    /// Other parameters are just parts of declaration that has already been parsed.
    fn parse_fn_decl_rest(
        &mut self,
        scope: Ptr<Scope>,
        returns_type: Ptr<TokenEntry>,
        identifier: String,
        start_span: Span,
    ) -> ParseResult<'a, Span> {
        let new_scope = Ptr::new(Scope::new(Some(scope.clone())));
        let (params_span, params) = self.parse_fn_params(new_scope.clone())?;

        // // TODO: add function itself to scope for recursive calling

        let fn_scope_decl = Ptr::new(TokenEntry::Function(FnScopeDecl {
            returns_type,
            params,
            is_ffi: false,
            decl: None,
        }));

        scope
            .borrow_mut()
            .try_insert_or_replace_same(&identifier, fn_scope_decl.clone());

        let fn_body = self.parse_block_no_scope(new_scope.clone())?;
        let body_span = fn_body.span();

        let fn_decl = FnDeclaration {
            span: fn_body.span + start_span,
            body: fn_body,
        };

        fn_scope_decl
            .borrow_mut()
            .find_fn_mut(|| parse_err_z(ParseErrVariant::InternalErr))?
            .decl = Some(fn_decl);

        Ok(body_span + start_span)
    }

    fn parse_fn_params(
        &mut self,
        scope: Ptr<Scope>,
    ) -> ParseResult<'a, (Span, Vec<Ptr<TokenEntry>>)> {
        let mut params = Vec::new();
        let mut span = self
            .lexer
            .try_consume_log_span(TokenVariant::LParenthesis)
            .unwrap();

        while self.lexer.try_consume(TokenVariant::Comma) {
            // parse type definition
            let is_const = self.lexer.try_consume(TokenVariant::Const);

            let var_type_ident = self.lexer.expect(TokenVariant::Identifier(""))?;

            let var_type_decl = var_type_ident
                .get_ident()
                .map_err(|_| parse_err(ParseErrVariant::InternalErr, var_type_ident.span))?;

            let var_type = scope
                .borrow()
                .find_definition(var_type_decl)
                .ok_or_else(|| {
                    parse_err(
                        ParseErrVariant::CannotFindType(var_type_decl),
                        var_type_ident.span,
                    )
                })?;

            let var_ident_token = self.lexer.expect(TokenVariant::Identifier(""))?;
            let var_ident = var_ident_token
                .get_ident()
                .map_err(|_| parse_err(ParseErrVariant::InternalErr, var_type_ident.span))?;
            span = span + var_ident_token.span;

            let token_entry = Ptr::new(TokenEntry::Variable(VarScopeDecl { is_const, var_type }));

            scope
                .borrow_mut()
                .try_insert(var_ident, token_entry.clone());
            params.push(token_entry.clone());
        }

        self.lexer.expect(TokenVariant::RParenthesis)?;

        Ok((span, params))
    }

    fn parse_single_var_decl(
        &mut self,
        scope: Ptr<Scope>,
        var_type: Ptr<TokenEntry>,
        is_const: bool,
    ) -> ParseResult<'a, (TokenEntry, Option<Expr>)> {
        let def_span = self.lexer.try_consume_log_span(TokenVariant::Assign);

        let def = if def_span.is_some() {
            Some(self.parse_expr(scope.clone(), &PARAM_END_ON)?)
        } else {
            None
        };

        Ok((
            TokenEntry::Variable(VarScopeDecl { is_const, var_type }),
            def,
        ))
    }

    fn parse_block(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, Block> {
        let new_scope = Ptr::new(Scope::new(Some(scope)));
        self.parse_block_no_scope(new_scope)
    }

    fn parse_block_no_scope(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, Block> {
        let start = self.lexer.expect(TokenVariant::LCurlyBrace)?.span;

        let mut block_statements = Vec::new();

        // TODO: bad implementation, wait for more beautiful ones
        let end = loop_while_check(|| {
            let span: LoopCtrl<Span> = self
                .lexer
                .try_consume_log_span(TokenVariant::RCurlyBrace)
                .into();
            if !span.is_continue() {
                return Ok(span);
            }

            let stmt = self.parse_decl_or_stmt(scope.clone())?;
            match stmt {
                Either::Left(stmt) => block_statements.push(stmt),
                Either::Right(mut stmts) => block_statements.append(&mut stmts),
            }
            Ok(Continue)
        })?;

        Ok(Block {
            scope: scope.clone(),
            stmt: block_statements,
            span: start + end,
        })
    }

    fn parse_stmt(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, Statement> {
        let token = self
            .lexer
            .peek()
            .ok_or(parse_err_z(ParseErrVariant::EarlyEof))
            .and_then(|t| match t.var {
                TokenVariant::EndOfFile => Err(parse_err(ParseErrVariant::EarlyEof, t.span)),
                _ => Ok(t),
            })?;

        match token.var {
            TokenVariant::If => Ok(Statement::If(self.parse_if(scope)?)),
            TokenVariant::While => Ok(Statement::While(self.parse_while(scope)?)),
            TokenVariant::LCurlyBrace => Ok(Statement::Block(self.parse_block(scope)?)),
            TokenVariant::Semicolon => Ok(Statement::Empty(token.span)),
            TokenVariant::Return => Ok(Statement::Return(self.parse_return(scope)?)),
            _ => Ok(Statement::Expr({
                let expr = self.parse_expr(scope, &STMT_END_ON)?;
                self.lexer.expect(TokenVariant::Semicolon)?;
                expr
            })),
        }
    }

    fn parse_if(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, IfStatement> {
        let start = self.lexer.expect(TokenVariant::If)?.span;
        self.lexer.expect(TokenVariant::LParenthesis)?;

        let if_expr = Ptr::new(self.parse_expr(scope.clone(), &PARAM_END_ON)?);

        self.lexer.expect(TokenVariant::RParenthesis)?;

        let if_body = Ptr::new(self.parse_stmt(scope.clone())?);
        let mut span = if_body.borrow().span() + start;

        let else_body = if self.lexer.try_consume(TokenVariant::Else) {
            let else_body = Ptr::new(self.parse_stmt(scope.clone())?);
            span = else_body.borrow().span() + span;
            Some(else_body)
        } else {
            None
        };

        Ok(IfStatement {
            check: if_expr,
            if_body,
            else_body,
            span,
        })
    }

    fn parse_while(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, WhileStatement> {
        let start = self.lexer.expect(TokenVariant::While)?.span;
        self.lexer.expect(TokenVariant::LParenthesis)?;

        let while_expr = Ptr::new(self.parse_expr(scope.clone(), &PARAM_END_ON)?);

        self.lexer.expect(TokenVariant::RParenthesis)?;

        let while_body = Ptr::new(self.parse_stmt(scope.clone())?);
        let span = while_body.borrow().span() + start;

        Ok(WhileStatement {
            check: while_expr,
            body: while_body,
            span,
        })
    }

    fn parse_return(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, ReturnStatement> {
        let start = self.lexer.expect(TokenVariant::Return)?.span;
        let has_expr = self.lexer.try_consume_log_span(TokenVariant::Semicolon);

        if has_expr.is_none() {
            let expr = self.parse_expr(scope, &STMT_END_ON)?;
            let span = self.lexer.expect(TokenVariant::Semicolon)?.span + start;
            Ok(ReturnStatement {
                return_val: Some(expr),
                span,
            })
        } else {
            Ok(ReturnStatement {
                return_val: None,
                span: start + has_expr.unwrap(),
            })
        }
    }

    fn parse_decl_or_stmt(
        &mut self,
        scope: Ptr<Scope>,
    ) -> ParseResult<'a, Either<Statement, Vec<Statement>>> {
        // TODO: Parse declarations as statements, and differ them from expressions.
        //       (probably by checking if the identifier is a type or a variable,
        //       and reports error if else)

        //* this function allows parsing of the following
        //* - a VarDeclaration (like `int x, y;` and `const z = 14;`)
        //* - a FnDeclaration  (like `int f(int x) { ... }`)
        //* - an Expr          (like `x = 3 + y;`, `y = f(z);` and `scanf(&a);`)

        /*
            The key is to peek the first token and determine if it is a type name,
            as in the pseudo-code below:

            ```
            match peek()
            - Identifier =>
                |> is it a type? (check scope)
                - true => parse_decl()
                - false => parse_expr()
            - _ => parse_expr()
            ```

            Another problem is to match function declaration or variable declaration
            after consuming the second token

            ----

            Okay we may need a sole "type parser" to parse through the type definitions
        */
        let is_decl = self.lexer.peek().map_or(false, |token| match token.var {
            TokenVariant::Identifier(ident) => scope
                .borrow()
                .find_definition(ident)
                .map_or(false, |entry| entry.borrow().is_type()),
            TokenVariant::Const => true,
            // TokenVariant:
            _ => false,
        });

        if is_decl {
            let (span, stmts) = self.parse_decl(scope)?;
            Ok(Either::Right(stmts))
        } else {
            Ok(Either::Left(self.parse_stmt(scope)?))
        }
    }

    fn parse_expr<'b>(
        &mut self,
        scope: Ptr<Scope>,
        end_on: &'b HashSet<TokenVariant<'a>>,
    ) -> ParseResult<'a, Expr> {
        Ok(ExprParser::new(&mut self.lexer, &*scope.borrow(), end_on).collect()?)
    }
}

struct ExprParser<'a, 'b> {
    lexer: &'b mut LexerWrapped<'a>,
    scope: &'b Scope,
    lexer_ended: bool,
    suggest_unary: bool,
    err_fuse: bool,
    err: Option<ParseError<'a>>,
    op_stack: Vec<ExprPart>,
    end_on: &'b HashSet<TokenVariant<'a>>,
}

impl<'a, 'b> ExprParser<'a, 'b> {
    pub fn new(
        lexer: &'b mut LexerWrapped<'a>,
        scope: &'b Scope,
        end_on: &'b HashSet<TokenVariant<'a>>,
    ) -> ExprParser<'a, 'b> {
        ExprParser {
            lexer,
            scope,
            lexer_ended: false,
            suggest_unary: true,
            err_fuse: false,
            err: None,
            op_stack: Vec::new(),
            end_on,
        }
    }

    fn meltdown<T>(&mut self, err: ParseError<'a>) -> LoopCtrl<Option<T>> {
        self.err = Some(err);
        self.err_fuse = true;
        Stop(None)
    }

    fn end_lexer<T>(&mut self) -> LoopCtrl<T> {
        self.lexer_ended = true;
        Continue
    }

    fn is_stack_top_higher_than(&self, op: &impl Operator) -> bool {
        self.op_stack
            .last()
            .map(|stack_op| {
                if op.is_right_associative() {
                    stack_op.priority() > op.priority()
                } else {
                    stack_op.priority() >= op.priority()
                }
            })
            .unwrap_or(false)
    }

    /*
        Design idea:

        # push-type pipeline

        new token arriving
        |> is operator? (function calls are considered as operator)
        - false => pass on
        - true =>
            |> is priority higher than stack top (if any)?
            - false => pop until meeting that of same or lower priority
            - true => push onto stack

        For function calls, transform `f(x, y)` into `( Fn(f) ... )` and push as usual

        # converting to pull pipeline

        https://cp-algorithms.com/string/expression_parsing.html

        next() being called
        WHILE we cannot return token
            can lexer provide another token? (peek, false if EOF or semicolon)
            - true =>
                is the next token an operator?
                - false =>
                    CONSUME the token, store it
                    is the **next** upcoming token LParenthesis? (check if is function call)
                    - true => PUSH _Lpr FnCall(ident), CONSUME LParenthesis
                    - false => RETURN corresponding ExprPart
                - true =>
                    is the next token LParenthesis?
                    - true => CONSUME, PUSH _Lpr, CONTINUE
                    - false =>
                        does stack top operator have higher priority than current one?
                        - true => POP, RETRUN stack top
                        - false =>
                            what is this token?
                            - RParenthesis =>
                                is stack top LParenthesis?
                                - true => CONSUME, POP, CONTINUE
                                - false => ERROR unbalanced parenthesis
                            - Comma => CONSUME, CONTINUE
                            - other => PUSH, suggest unary, CONTINUE
            - false =>
                is stack empty?
                - true => RETURN None
                - false => POP, RETURN
    */

    fn _next(&mut self) -> Option<ExprPart> {
        if self.err_fuse {
            None
        } else {
            loop_while(|| {
                if self.lexer_ended {
                    if self.op_stack.is_empty() {
                        Stop(None)
                    } else {
                        Stop(self.op_stack.pop())
                    }
                } else {
                    match self.lexer.peek() {
                        None => self.end_lexer(),
                        Some(Token { var, .. }) if self.end_on.contains(var) => self.end_lexer(),
                        Some(token) => {
                            if token.is_op() {
                                self.parse_op()
                            } else {
                                // consume and check function
                                self.parse_val()
                            }
                        }
                    }
                }
            })
        }
    }

    fn parse_op(&mut self) -> LoopCtrl<Option<ExprPart>> {
        let token = self.lexer.peek().unwrap();
        let token_span = token.span;
        match token.var.into_op(self.suggest_unary) {
            Some(op) => {
                // there is a corresponding operator here
                if !(op == OpVar::_Lpr) && self.is_stack_top_higher_than(&op) {
                    Stop(self.op_stack.pop())
                } else {
                    self.lexer.next();
                    // special handling for parenthesis and comma
                    if variant_eq(&op, &OpVar::_Rpr) {
                        // clear corresponding parenthesis, or error if nothing to share
                        if variant_eq(
                            &self
                                .op_stack
                                .last()
                                .and_then(|expr_part| expr_part.into_op())
                                .unwrap_or(OpVar::_Dum),
                            &OpVar::_Lpr,
                        ) {
                            self.op_stack.pop();
                            self.suggest_unary = false;
                            Continue
                        } else {
                            self.meltdown(parse_err(
                                ParseErrVariant::UnbalancedParenthesisExpectL,
                                token_span,
                            ))
                        }
                    } else if variant_eq(&op, &OpVar::_Com) {
                        // pass
                        self.suggest_unary = true;
                        Continue
                    } else {
                        self.op_stack.push(ExprPart::Op(op, token_span));
                        if !self.suggest_unary && op.is_unary() {
                            self.suggest_unary = false;
                        } else {
                            self.suggest_unary = true;
                        }
                        Continue
                    }
                }
            }
            None => {
                // no corresponding operator, error!
                let t: TokenVariant = self.lexer.next().unwrap().var;
                self.meltdown(parse_err(
                    ParseErrVariant::UnexpectedTokenMsg(t, "No corresponding operator for token"),
                    token_span,
                ))
            }
        }
    }

    fn parse_val(&mut self) -> LoopCtrl<Option<ExprPart>> {
        let token = self.lexer.next().unwrap();
        let t = token.var;
        match t {
            TokenVariant::IntegerLiteral(i) => {
                self.suggest_unary = false;
                Stop(Some(ExprPart::Int(IntegerLiteral(i, token.span))))
            }
            TokenVariant::StringLiteral(s) => {
                self.suggest_unary = false;
                Stop(Some(ExprPart::Str(StringLiteral(s, token.span))))
            }
            TokenVariant::Identifier(i) => self.parse_ident(i, token.span),
            var @ _ => self.meltdown(parse_err(
                ParseErrVariant::UnexpectedTokenMsg(var, "Should be literal or identifier"),
                token.span,
            )),
        }
    }

    fn parse_ident(&mut self, ident: &'a str, span: Span) -> LoopCtrl<Option<ExprPart>> {
        match self.scope.find_definition(ident) {
            None => self.meltdown(parse_err(ParseErrVariant::CannotFindIdent(ident), span)),
            Some(def_ptr) => {
                let is_fn_lparen_span = self.lexer.try_consume_log_span(TokenVariant::LParenthesis);
                let is_fn = is_fn_lparen_span.is_some();
                let def_ptr_clone = def_ptr.clone();
                let def = def_ptr_clone.borrow();
                if is_fn {
                    match *def {
                        TokenEntry::Function { .. } => {
                            self.op_stack
                                .push(ExprPart::Op(OpVar::_Lpr, is_fn_lparen_span.unwrap()));
                            self.op_stack
                                .push(ExprPart::FnCall(Identifier(def_ptr.clone(), span)));
                            self.suggest_unary = true;
                            Continue
                        }
                        _ => self.meltdown(parse_err(ParseErrVariant::CannotFindFn(ident), span)),
                    }
                } else {
                    // is variable
                    match *def {
                        TokenEntry::Variable { .. } => {
                            self.suggest_unary = false;
                            Stop(Some(ExprPart::Var(Identifier(def_ptr.clone(), span))))
                        }
                        _ => self.meltdown(parse_err(ParseErrVariant::CannotFindVar(ident), span)),
                    }
                }
            }
        }
    }

    fn collect(mut self) -> ParseResult<'a, Expr> {
        self.try_fold(
            Vec::<Expr>::new(),
            |mut expr_stack: Vec<Expr>, next_val: ExprPart| {
                match next_val {
                    ExprPart::Int(i) => expr_stack.push(Expr::Int(i)),
                    ExprPart::Str(s) => expr_stack.push(Expr::Str(s)),
                    ExprPart::Var(v) => expr_stack.push(Expr::Var(v)),
                    ExprPart::Op(op, span) => {
                        if op.is_unary() {
                            // unary op
                            let operand = expr_stack
                                .pop()
                                .ok_or(parse_err(ParseErrVariant::MissingOperandUnary, span))?;
                            let op_span = operand.span();

                            expr_stack.push(Expr::UnaOp(UnaryOp {
                                var: op,
                                val: Ptr::new(operand),
                                span: op_span + span,
                            }));
                        } else {
                            // binary op
                            let r_operand = expr_stack
                                .pop()
                                .ok_or(parse_err(ParseErrVariant::MissingOperandR, span))?;
                            let r_span = r_operand.span();

                            let l_operand = expr_stack
                                .pop()
                                .ok_or(parse_err(ParseErrVariant::MissingOperandL, span))?;
                            let l_span = l_operand.span();

                            expr_stack.push(Expr::BinOp(BinaryOp {
                                var: op,
                                lhs: Ptr::new(l_operand),
                                rhs: Ptr::new(r_operand),
                                span: span + l_span + r_span,
                            }));
                        }
                    }
                    ExprPart::FnCall(func) => {
                        let len = match &*func.0.clone().borrow() {
                            TokenEntry::Function(FnScopeDecl { params, .. }) => params.len(),
                            _ => unreachable!(),
                        };
                        let mut span = func.span();
                        let mut params: Vec<Ptr<Expr>> =
                            (0..len).try_fold(Vec::new(), |mut vec: Vec<Ptr<Expr>>, _num| {
                                vec.push(Ptr::new(
                                    expr_stack
                                        .pop()
                                        .map(|v| {
                                            span = span + v.span();
                                            v
                                        })
                                        .ok_or(parse_err(
                                            ParseErrVariant::NotMatchFnArguments(len, _num),
                                            func.1,
                                        ))?,
                                ));
                                Ok(vec)
                            })?;
                        params.reverse();
                        expr_stack.push(Expr::FnCall(FuncCall {
                            fn_name: func,
                            params,
                            span,
                        }));
                    }
                }
                Ok(expr_stack)
            },
        )
        .and_then(|mut vector| {
            if self.err_fuse {
                Err(self.err.unwrap())
            } else {
                let len = vector.len();
                if len > 2 {
                    Err(parse_err_z(ParseErrVariant::InternalErr))
                } else if len == 1 {
                    Ok(vector.pop().unwrap())
                } else {
                    Err(parse_err_z(ParseErrVariant::InternalErr))
                }
            }
        })
    }
}

impl<'a, 'b> Iterator for ExprParser<'a, 'b> {
    type Item = ExprPart;

    fn next(&mut self) -> Option<ExprPart> {
        self._next()
    }
}

trait OptionalOperator {
    fn is_op(&self) -> bool;
}

impl OptionalOperator for Token<'_> {
    fn is_op(&self) -> bool {
        self.var.is_op()
    }
}

impl OptionalOperator for TokenVariant<'_> {
    fn is_op(&self) -> bool {
        use TokenVariant::*;
        match self {
            Minus | Plus | Multiply | Divide | Not | Increase | Decrease | Equals | NotEquals
            | LessThan | GreaterThan | LessOrEqualThan | GreaterOrEqualThan | Assign | Comma
            | LParenthesis | RParenthesis => true,
            _ => false,
        }
    }
}

trait IntoOperator {
    fn into_op(&self, suggest_unary: bool) -> Option<OpVar>;
}

impl IntoOperator for TokenVariant<'_> {
    fn into_op(&self, suggest_unary: bool) -> Option<OpVar> {
        use OpVar::*;
        use TokenVariant::*;
        if suggest_unary {
            match self {
                Minus => Some(Neg),
                Multiply => Some(Der),
                BinaryAnd => Some(Ref),
                Increase => Some(Inb),
                Decrease => Some(Deb),
                LParenthesis => Some(_Lpr),
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
                TokenVariant::And => Some(OpVar::And),
                TokenVariant::Or => Some(OpVar::Or),
                TokenVariant::Xor => Some(OpVar::Xor),
                Increase => Some(Ina),
                Decrease => Some(Dea),
                Equals => Some(Eq),
                NotEquals => Some(Neq),
                LessThan => Some(Lt),
                GreaterThan => Some(Gt),
                LessOrEqualThan => Some(Lte),
                GreaterOrEqualThan => Some(Gte),
                Assign => Some(_Asn),
                Comma => Some(_Com),
                // LParenthesis => Some(_Lpr),
                RParenthesis => Some(_Rpr),
                _ => None,
            }
        }
    }
}

trait Operator {
    fn priority(&self) -> isize;
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
            _Dum => -50,
            _Lpr | _Rpr => -10,
            _Com => -4,
            _Asn | _Csn => 0,
            Eq | Neq => 2,
            Gt | Lt | Gte | Lte => 3,
            Or => 4,
            And => 5,
            Bor => 6,
            Xor => 7,
            Ban => 8,
            Add | Sub => 15,
            Mul | Div => 20,
            Neg | Inv | Bin | Ref | Der | Ina | Inb | Dea | Deb => 40,
        }
    }

    fn is_right_associative(&self) -> bool {
        use OpVar::*;
        match self {
            Neg | Inv | Bin | Ref | Der | _Asn | _Lpr | _Rpr => true,
            _ => false,
        }
    }
}

///
// #[derive(Debug)]
enum ExprPart {
    Int(IntegerLiteral),
    Str(StringLiteral),
    FnCall(Identifier),
    Var(Identifier),
    Op(OpVar, Span),
}

impl ExprPart {
    pub fn into_op(&self) -> Option<OpVar> {
        match self {
            ExprPart::Op(op, ..) => Some(*op),
            _ => None,
        }
    }
}

impl OptionalOperator for ExprPart {
    fn is_op(&self) -> bool {
        match self {
            ExprPart::Op(..) | ExprPart::FnCall(..) => true,
            _ => false,
        }
    }
}

impl Operator for ExprPart {
    fn priority(&self) -> isize {
        match self {
            ExprPart::Op(op, ..) => op.priority(),
            ExprPart::FnCall(..) => -5,
            _ => panic!("Cannot use trait Operator on expression parts that are not operator!"),
        }
    }

    fn is_right_associative(&self) -> bool {
        match self {
            ExprPart::Op(op, ..) => op.is_right_associative(),
            ExprPart::FnCall(..) => true,
            _ => panic!("Cannot use trait Operator on expression parts that are not operator!"),
        }
    }
}

impl std::fmt::Debug for ExprPart {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprPart::Int(i) => write!(f, "Int({})", i.0),
            ExprPart::Str(s) => write!(f, "Str({})", s.0),
            ExprPart::FnCall(_) => write!(f, "FnCall"),
            ExprPart::Var(_) => write!(f, "Var"),
            ExprPart::Op(o, _) => write!(f, "Op({:?})", o),
        }
    }
}

// ======================

#[cfg(test)]
mod test {
    use super::*;
    use crate::c0::infra::*;

    #[test]
    fn test_parser() {
        use TokenVariant::*;
        let src = r#"
int x, y;
int main(){
    x = 1;
    y = (1 + 2) * 3 + 4;
    int z = x + y;
    y++;
    x+++++y;
    *x;
    print(z);
    return 0;
}
        "#;

        let ast = crate::c0::lexer::Lexer::new(src)
            .into_iter()
            .into_parser()
            .parse()
            .unwrap_or_else(|err| {
                panic!("{}\nCode with issue: \"{}\"", err, str_span(src, err.span))
            });

        println!("{:#?}", ast);
    }
}
