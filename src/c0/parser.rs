use super::{ast::*, infra::*, lexer::*};
use crate::set;
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
        self.next()
            .filter(|t| variant_eq(&t.var, &token))
            .ok_or(ParseError::ExpectToken(token))
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
            None => Err(ParseError::ExpectToken(token)),
        }
    }

    fn try_consume(&mut self, token: TokenVariant<'a>) -> bool {
        match self.peeking_next(|v| variant_eq(&v.var, &token)) {
            Some(_) => {
                self.next();
                true
            }
            None => false,
        }
    }

    fn try_consume_log_span(&mut self, token: TokenVariant<'a>) -> Option<Span> {
        match self.peeking_next(|v| variant_eq(&v.var, &token)) {
            Some(v) => {
                self.next();
                Some(v.span)
            }
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
        Self::inject_std(scope.clone());
        // let mut fns = vec![];
        // let mut vars = vec![];
        while self.lexer.peek().is_some() {
            self.parse_decl(scope.clone())?
        }
        Ok(Program {
            scope: scope.clone(),
        })
    }

    fn inject_std(scope: Ptr<Scope>) {
        let mut scope = scope.borrow_mut();
        scope.try_insert(
            "int",
            Ptr::new(TokenEntry::Type(TypeScopeDecl {
                is_primitive: true,
                occupy_bytes: 4,
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
    fn parse_decl(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, ()> {
        let is_const = self.lexer.try_consume(TokenVariant::Const);
        let type_name = self
            .lexer
            .expect(TokenVariant::Identifier(""))?
            .get_ident()
            .unwrap();

        let var_type = scope
            .borrow()
            .find_definition(type_name)
            .ok_or(ParseError::CannotFindType(type_name))
            .and_then(|ptr_token| {
                if ptr_token.borrow().is_type() {
                    Ok(ptr_token)
                } else {
                    Err(ParseError::ExpectToBeType(type_name))
                }
            })?;

        let identifier = self.lexer.expect(TokenVariant::Identifier(""))?;
        let identifier_owned: String = match identifier.var {
            TokenVariant::Identifier(s) => s.to_owned(),
            _ => return Err(ParseError::InternalErr),
        };
        let is_fn = self.lexer.try_consume(TokenVariant::LParenthesis);

        if is_fn {
            // Functions cannot be const
            if is_const {
                return Err(ParseError::NoConstFns);
            }

            // This thing is a function! the insersion and other stuff are done
            // in function call route
            self.parse_fn_decl_rest(scope.clone(), var_type, identifier_owned, identifier.span)?;

            Ok(())
        // return;
        } else {
            let first_entry =
                Ptr::new(self.parse_single_var_decl(scope.clone(), var_type.clone(), is_const)?);

            if !scope.borrow_mut().try_insert(&identifier_owned, first_entry) {
                return Err(ParseError::TokenExists(&identifier.get_ident().unwrap()));
            }

            while self.lexer.try_consume(TokenVariant::Comma) {
                let identifier = self.lexer.expect(TokenVariant::Identifier(""))?;
                let identifier_owned: String = match identifier.var {
                    TokenVariant::Identifier(s) => s.to_owned(),
                    _ => return Err(ParseError::InternalErr),
                };
                let entry =
                    Ptr::new(self.parse_single_var_decl(scope.clone(), var_type.clone(), is_const)?);

                if !scope.borrow_mut().try_insert(&identifier_owned, entry) {
                    return Err(ParseError::TokenExists(identifier.get_ident().unwrap()));
                }
            }
            self.lexer.expect(TokenVariant::Semicolon)?;
            Ok(())
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
    ) -> ParseResult<'a, ()> {
        let new_scope = Ptr::new(Scope::new(Some(scope.clone())));
        let params = self.parse_fn_params(new_scope.clone())?;
        // TODO: add function itself to scope for recursive calling
        let fn_scope_decl = Ptr::new(TokenEntry::Function(FnScopeDecl {
            returns_type,
            params,
            decl: None,
        }));

        scope
            .borrow_mut()
            .try_insert_or_replace_same(&identifier, fn_scope_decl.clone());

        let fn_body = self.parse_block_no_scope(new_scope.clone())?;
        let fn_decl = FnDeclaration {
            span: fn_body.span + start_span,
            body: fn_body,
        };

        fn_scope_decl
            .borrow_mut()
            .find_fn_mut(|| ParseError::InternalErr)?
            .decl = Some(fn_decl);

        Ok(())
    }

    fn parse_fn_params(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, Vec<Ptr<TokenEntry>>> {
        let mut params = Vec::new();

        while self.lexer.try_consume(TokenVariant::Comma) {
            // parse type definition
            let is_const = self.lexer.try_consume(TokenVariant::Const);

            let var_type_ident = self
                .lexer
                .expect(TokenVariant::Identifier(""))?
                .get_ident()
                .map_err(|_| ParseError::InternalErr)?;

            let var_type = scope
                .borrow()
                .find_definition(var_type_ident)
                .ok_or(ParseError::CannotFindType(var_type_ident))?;

            let var_ident = self
                .lexer
                .expect(TokenVariant::Identifier(""))?
                .get_ident()
                .map_err(|_| ParseError::InternalErr)?;

            let token_entry = Ptr::new(TokenEntry::Variable(VarScopeDecl {
                is_const,
                var_type,
                val: None,
            }));

            scope
                .borrow_mut()
                .try_insert(var_ident, token_entry.clone());
            params.push(token_entry.clone());
        }

        self.lexer.expect(TokenVariant::RParenthesis)?;

        Ok(params)
    }

    fn parse_single_var_decl(
        &mut self,
        scope: Ptr<Scope>,
        var_type: Ptr<TokenEntry>,
        is_const: bool,
    ) -> ParseResult<'a, TokenEntry> {
        let def_span = self.lexer.try_consume_log_span(TokenVariant::Equals);

        let def = if def_span.is_some() {
            Some(Ptr::new(self.parse_expr(scope.clone(), &PARAM_END_ON)?))
        } else {
            None
        };

        Ok(TokenEntry::Variable(VarScopeDecl {
            is_const,
            var_type,
            val: def,
        }))
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
            block_statements.push(stmt);
            Ok(Continue)
        })?;

        Ok(Block {
            scope: scope.clone(),
            stmt: block_statements,
            span: start + end,
        })
    }

    fn parse_stmt(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, Statement> {
        match self.lexer.peek().ok_or(ParseError::EarlyEof)?.var {
            TokenVariant::If => Ok(Statement::If(self.parse_if(scope)?)),
            TokenVariant::While => Ok(Statement::While(self.parse_while(scope)?)),
            TokenVariant::LCurlyBrace => Ok(Statement::Block(self.parse_block(scope)?)),
            TokenVariant::Semicolon => Ok(Statement::Empty),
            TokenVariant::Return => Ok(Statement::Return(self.parse_return(scope)?)),
            _ => Ok(Statement::Expr({
                let expr = self.parse_expr(scope, &STMT_END_ON)?;
                self.lexer.expect(TokenVariant::Semicolon)?;
                expr
            })),
        }
    }

    fn parse_if(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, IfStatement> {
        self.lexer.expect(TokenVariant::If)?;
        self.lexer.expect(TokenVariant::LParenthesis)?;

        let if_expr = Ptr::new(self.parse_expr(scope.clone(), &PARAM_END_ON)?);

        self.lexer.expect(TokenVariant::RParenthesis)?;

        let if_body = Ptr::new(self.parse_stmt(scope.clone())?);

        let else_body = if self.lexer.try_consume(TokenVariant::Else) {
            Some(Ptr::new(self.parse_stmt(scope.clone())?))
        } else {
            None
        };

        Ok(IfStatement {
            check: if_expr,
            if_body,
            else_body,
        })
    }

    fn parse_while(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, WhileStatement> {
        self.lexer.expect(TokenVariant::While)?;
        self.lexer.expect(TokenVariant::LParenthesis)?;

        let while_expr = Ptr::new(self.parse_expr(scope.clone(), &PARAM_END_ON)?);

        self.lexer.expect(TokenVariant::RParenthesis)?;

        let while_body = Ptr::new(self.parse_stmt(scope.clone())?);

        Ok(WhileStatement {
            check: while_expr,
            body: while_body,
        })
    }

    fn parse_return(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, Expr> {
        let expr = self.parse_expr(scope, &STMT_END_ON)?;
        self.lexer.expect(TokenVariant::Semicolon)?;
        Ok(expr)
    }

    fn parse_decl_or_stmt(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, Statement> {
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
            self.parse_decl(scope)?;
            Ok(Statement::Empty)
        } else {
            self.parse_stmt(scope)
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
        match token.var.into_op(self.suggest_unary) {
            Some(op) => {
                // there is a corresponding operator here
                if self.is_stack_top_higher_than(&op) {
                    Stop(self.op_stack.pop())
                } else {
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
                            self.meltdown(ParseError::UnbalancedParenthesisExpectL)
                        }
                    } else if variant_eq(&op, &OpVar::_Com) {
                        // pass
                        self.suggest_unary = true;
                        Continue
                    } else {
                        self.op_stack.push(ExprPart::Op(op));
                        self.suggest_unary = true;
                        Continue
                    }
                }
            }
            None => {
                // no corresponding operator, error!
                let t: TokenVariant = self.lexer.next().unwrap().var;
                self.meltdown(ParseError::UnexpectedToken(t))
            }
        }
    }

    fn parse_val(&mut self) -> LoopCtrl<Option<ExprPart>> {
        let t: TokenVariant = self.lexer.next().unwrap().var;
        match t {
            TokenVariant::IntegerLiteral(i) => {
                self.suggest_unary = false;
                Stop(Some(ExprPart::Int(IntegerLiteral(i))))
            }
            TokenVariant::StringLiteral(s) => {
                self.suggest_unary = false;
                Stop(Some(ExprPart::Str(StringLiteral(s))))
            }
            TokenVariant::Identifier(ident) => self.parse_ident(ident),
            var @ _ => self.meltdown(ParseError::UnexpectedToken(var)),
        }
    }

    fn parse_ident(&mut self, ident: &'a str) -> LoopCtrl<Option<ExprPart>> {
        match self.scope.find_definition(ident) {
            None => self.meltdown(ParseError::CannotFindIdent(ident)),
            Some(def_ptr) => {
                let is_fn = self.lexer.try_consume(TokenVariant::LParenthesis);
                let def_ptr_clone = def_ptr.clone();
                let def = def_ptr_clone.borrow();
                if is_fn {
                    match *def {
                        TokenEntry::Function { .. } => {
                            self.op_stack.push(ExprPart::Op(OpVar::_Lpr));
                            self.op_stack
                                .push(ExprPart::FnCall(Identifier(def_ptr.clone())));
                            self.suggest_unary = true;
                            Continue
                        }
                        _ => self.meltdown(ParseError::CannotFindFn(ident)),
                    }
                } else {
                    // is variable
                    match *def {
                        TokenEntry::Variable { .. } => {
                            self.suggest_unary = false;
                            Stop(Some(ExprPart::Var(Identifier(def_ptr.clone()))))
                        }
                        _ => self.meltdown(ParseError::CannotFindVar(ident)),
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
                    ExprPart::Op(op) => {
                        if op.is_unary() {
                            // unary op
                            let operand = expr_stack.pop().ok_or(ParseError::MissingOperand)?;
                            expr_stack.push(Expr::UnaOp(UnaryOp {
                                var: op,
                                val: Ptr::new(operand),
                            }));
                        } else {
                            // binary op
                            let r_operand = expr_stack.pop().ok_or(ParseError::MissingOperand)?;
                            let l_operand = expr_stack.pop().ok_or(ParseError::MissingOperand)?;
                            expr_stack.push(Expr::BinOp(BinaryOp {
                                var: op,
                                lhs: Ptr::new(l_operand),
                                rhs: Ptr::new(r_operand),
                            }));
                        }
                    }
                    ExprPart::FnCall(func) => {
                        let len = match &*func.0.clone().borrow() {
                            TokenEntry::Function(FnScopeDecl { params, .. }) => params.len(),
                            _ => unreachable!(),
                        };
                        let mut params: Vec<Ptr<Expr>> =
                            (0..len).try_fold(Vec::new(), |mut vec: Vec<Ptr<Expr>>, _num| {
                                vec.push(Ptr::new(
                                    expr_stack.pop().ok_or(ParseError::MissingOperand)?,
                                ));
                                Ok(vec)
                            })?;
                        params.reverse();
                        expr_stack.push(Expr::FnCall(FuncCall {
                            fn_name: func,
                            params,
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
                    Err(ParseError::InternalErr)
                } else if len == 1 {
                    Ok(vector.pop().unwrap())
                } else {
                    Err(ParseError::InternalErr)
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
                LParenthesis => Some(_Lpr),
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
            Neg | Inv | Bin | Ref | Der | _Asn => true,
            _ => false,
        }
    }
}

///
enum ExprPart {
    Int(IntegerLiteral),
    Str(StringLiteral),
    FnCall(Identifier),
    Var(Identifier),
    Op(OpVar),
}

impl ExprPart {
    pub fn into_op(&self) -> Option<OpVar> {
        match self {
            ExprPart::Op(op) => Some(*op),
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
            ExprPart::Op(op) => op.priority(),
            ExprPart::FnCall(..) => -5,
            _ => panic!("Cannot use trait Operator on expression parts that are not operator!"),
        }
    }

    fn is_right_associative(&self) -> bool {
        match self {
            ExprPart::Op(op) => op.is_right_associative(),
            ExprPart::FnCall(..) => true,
            _ => panic!("Cannot use trait Operator on expression parts that are not operator!"),
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
                y = 2 + 3;
                int z = x + y;
                print(z);
                return 0;
            }
        "#;

        let ast = crate::c0::lexer::Lexer::new(src)
            .into_iter()
            .into_parser()
            .parse()
            .unwrap();

        println!("{:#?}", ast);
    }
}
