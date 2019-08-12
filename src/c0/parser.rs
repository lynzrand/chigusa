use crate::c0::ast::*;
use crate::c0::lexer::*;
use itertools::Itertools;
use lazy_static::lazy_static;
use std::collections::*;
use std::iter::{Iterator, Peekable};
use std::{cell::RefCell, fmt, fmt::Display, fmt::Formatter, rc::Rc};

fn variant_eq<T>(a: &T, b: &T) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}

enum LoopCtrl<T> {
    Stop(T),
    Continue,
}

use self::LoopCtrl::*;

impl<T> LoopCtrl<T> {
    pub fn unwrap(self) -> T {
        match self {
            LoopCtrl::Stop(x) => x,
            _ => panic!("Cannot unwrap a LoopCtrl with Continue statement"),
        }
    }

    pub fn is_continue(&self) -> bool {
        match self {
            LoopCtrl::Continue => true,
            _ => false,
        }
    }
}

fn loop_while<F, T>(mut f: F) -> T
where
    F: FnMut() -> LoopCtrl<T>,
{
    let mut x: LoopCtrl<T> = Continue;
    while x.is_continue() {
        x = f();
    }
    // the following unwrap CANNOT panic because x is garanteed to be Some.
    x.unwrap()
}

pub trait IntoParser<'a> {
    fn into_parser(self: Box<Self>) -> Parser<'a>;
}

impl<'a> IntoParser<'a> for dyn Iterator<Item = Token<'a>> {
    fn into_parser(self: Box<Self>) -> Parser<'a> {
        Parser::new(self)
    }
}

type ParseResult<'a, T> = Result<T, ParseError<'a>>;

pub trait TokenIterator<'a>: Iterator<Item = Token<'a>> {
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

    fn try_consume(&mut self, token: TokenVariant<'a>) -> bool
    where
        Self: itertools::PeekingNext,
    {
        match self.peeking_next(|v| variant_eq(&v.var, &token)) {
            Some(_) => {
                self.next();
                true
            }
            None => false,
        }
    }
}

type Lexer<'a> = Peekable<Box<dyn Iterator<Item = Token<'a>>>>;

impl<'a> TokenIterator<'a> for Lexer<'a> {}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Box<dyn Iterator<Item = Token<'a>>>) -> Parser<'a> {
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
        // let mut fns = vec![];
        // let mut vars = vec![];
        while self.lexer.peek().is_some() {
            self.parse_decl(scope.clone())?
        }
        Ok(Program {
            scope: scope.clone(),
        })
        // unimplemented!()
    }

    /// Parse a declaration. Could either be a function or variable declaration.
    /// After the parsing completed, the coresponding declaration entry will be
    /// inserted into the symbol table defined in `scope`.
    fn parse_decl(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, ()> {
        let is_const = self.lexer.try_consume(TokenVariant::Const);
        let type_name = self.lexer.expect(TokenVariant::Identifier(""))?;
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

            // This thing is a function! Parse the rest stuff.
            let entry = Ptr::new(self.parse_fn_decl_rest(scope.clone(), type_name, identifier)?);;

            // Insert
            scope
                .borrow_mut()
                .token_table
                .insert(identifier_owned, entry);

            Ok(())
        // return;
        } else {
            while !self.lexer.try_consume(TokenVariant::Semicolon) {
                let entry = Ptr::new(self.parse_single_var_decl(scope.clone())?);
                // TODO: write parser for single entries
                // scope
                //     .borrow_mut()
                //     .token_table
                //     .insert(identifier_owned, entry);
            }
            unimplemented!()
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
        return_type: Token<'a>,
        identifier: Token<'a>,
    ) -> ParseResult<'a, TokenEntry> {
        let ident = identifier
            .get_ident()
            .map_err(|_| ParseError::InternalErr)?;
        let new_scope = Ptr::new(Scope::new(Some(scope)));
        let params = self.parse_fn_params(new_scope.clone())?;
        let fn_body = self.parse_block_no_scope(new_scope.clone())?;
        unimplemented!()
    }

    fn parse_fn_params(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, Vec<Ptr<VarDecalaration>>> {
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

            let token_entry = Ptr::new(TokenEntry::Variable { is_const, var_type });
            let var_decl = Ptr::new(VarDecalaration {
                is_const,
                symbol: token_entry.clone(),
                val: None,
            });

            scope.borrow_mut().try_insert(var_ident, token_entry);
            params.push(var_decl.clone());
        }

        self.lexer.expect(TokenVariant::RParenthesis)?;

        Ok(params)
    }

    fn parse_single_var_decl(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, TokenEntry> {
        unimplemented!()
    }

    fn parse_block(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, Block> {
        let new_scope = Ptr::new(Scope::new(Some(scope)));
        self.parse_block_no_scope(new_scope)
    }

    fn parse_block_no_scope(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, Block> {
        self.lexer.expect(TokenVariant::LCurlyBrace)?;

        let mut block_statements = Vec::new();

        while !self.lexer.try_consume(TokenVariant::RCurlyBrace) {
            let stmt = self.parse_stmt(scope.clone())?;
            block_statements.push(stmt);
        }
        unimplemented!()
    }

    fn parse_stmt(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, Statement> {
        match self.lexer.peek().ok_or(ParseError::EarlyEof)?.var {
            TokenVariant::If => {
                // todo: parse If statement
                unimplemented!()
            }
            TokenVariant::While => {
                // todo: parse while statement
                unimplemented!()
            }
            TokenVariant::LCurlyBrace => {
                // todo: parse block
                unimplemented!()
            }
            TokenVariant::Semicolon => Ok(Statement::Empty),
            _ => {
                // todo: parse expression
                unimplemented!()
            }
        }
    }

    fn parse_expr(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, Ptr<Expr>> {
        /*
            The whole process is like this:
                token iter
                -> expression part iter (inverse-poland expression stream)
                -> expression tree
        */

        // let mut op_stack = Vec::new();
        // let mut expr_stack = Vec::new();

        unimplemented!()
    }

    /*
        fn __old_parse_expr(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, Ptr<Expr>> {
            let mut op_stack = Vec::new();
            let mut expr_root = None;

            while !self.lexer.try_consume(TokenVariant::Semicolon) {
                let item: Ptr<Expr> = match self.lexer.peek().ok_or(ParseError::EarlyEof)?.var {
                    TokenVariant::IntegerLiteral(i) => Ptr::new(Expr::Int(IntegerLiteral(i))),
                    TokenVariant::Identifier(i) => {
                        self.lexer.next();
                        let ident = scope
                            .borrow()
                            .find_definition(i)
                            .ok_or(ParseError::CannotFindVar(i))?;
                        match *ident.borrow() {
                            TokenEntry::Variable { .. } => {
                                // This is a variable, stop and add as root
                                Ptr::new(Expr::Var(Identifier(ident)))
                            }
                            TokenEntry::Function { .. } => {
                                // This is a function call, parse all params
                                let params = self.parse_fn_call_params(scope)?;
                                Ptr::new(Expr::FnCall(FuncCall {
                                    fn_name: Identifier(ident),
                                    params,
                                }))
                            }
                            _ => Err(ParseError::CannotCallType(i))?,
                        }
                    }
                    TokenVariant::LParenthesis => {
                        op_stack.push(OpVar::_Lpr);
                        continue;
                    }
                    TokenVariant::RParenthesis => unimplemented!(),
                    // token@TokenVariant::
                    token @ _ => Err(ParseError::UnexpectedToken(token))?,
                };
            }

            unimplemented!()
        }
    */

    fn parse_fn_call_params(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, Vec<Ptr<Expr>>> {
        unimplemented!()
    }
}

struct ExprParser<'a> {
    lexer: &'a mut Lexer<'a>,
    scope: &'a Scope,
    lexer_ended: bool,
    op_stack: Vec<ExprPart>,
}

impl<'a> ExprParser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>, scope: &'a Scope) -> ExprParser<'a> {
        ExprParser {
            lexer,
            scope,
            lexer_ended: false,
            op_stack: Vec::new(),
        }
    }

    fn end_lexer<T>(&mut self) -> LoopCtrl<T> {
        self.lexer_ended = true;
        Continue
    }

    fn is_stack_top_higher_than(&self, op: &impl Operator) -> bool {
        self.op_stack
            .last()
            .map(|stack_op| stack_op.priority() >= op.priority())
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

        For function calls, transform `f(x, y)` into (f (x) (y)) and push as usual

        # converting to pull pipeline

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
                            - other => PUSH, CONTINUE
            - false =>
                is stack empty?
                - true => RETURN None
                - false => POP, RETURN
    */

    fn _next(&mut self) -> Option<ExprPart> {
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
                    Some(token) => match &token.var {
                        TokenVariant::EndOfFile | TokenVariant::Semicolon => self.end_lexer(),
                        op @ _ if token.is_op() => {
                            // TODO: Convert TokenVariant into OpVar
                            let op: OpVar = unimplemented!();
                            if self.is_stack_top_higher_than(op) {
                                Stop(self.op_stack.pop())
                            } else {
                                self.op_stack.push(unimplemented!());
                                Continue
                            }
                        }
                        _ => {
                            // TODO: consume and check function
                            unimplemented!()
                        }
                    },
                }
            }
        })
    }
}

impl<'a> Iterator for ExprParser<'a> {
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

trait Operator {
    fn priority(&self) -> isize;
}

impl Operator for OpVar {
    fn priority(&self) -> isize {
        use OpVar::*;
        match self {
            _Lpr | _Rpr => -10,
            _Com => -4,
            _Asn => 0,
            Gt | Lt | Eq | Gte | Lte | Neq => 5,
            Add | Sub => 10,
            Mul | Div => 20,
            Neg | Inv | Bin | Ref | Der => 30,
            Ina | Inb | Dea | Deb => 40,
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
}

pub enum ParseError<'a> {
    ExpectToken(TokenVariant<'a>),
    UnexpectedToken(TokenVariant<'a>),
    NoConstFns,
    CannotFindType(&'a str),
    CannotFindVar(&'a str),
    CannotFindFn(&'a str),
    CannotCallType(&'a str),
    UnsupportedToken(TokenVariant<'a>),
    EarlyEof,
    InternalErr,
}

impl<'a> ParseError<'a> {
    pub fn get_err_code(&self) -> usize {
        use self::ParseError::*;
        match self {
            ExpectToken(_) => 1,
            NoConstFns => 2,
            InternalErr => 1023,
            _ => 1024,
        }
    }

    pub fn get_err_desc(&self) -> String {
        use self::ParseError::*;
        match self {
            ExpectToken(token) => format!("Expected {}", token),
            NoConstFns => "Functions cannot be marked as constant".to_string(),
            InternalErr => "Something went wrong inside the compiler".to_string(),
            _ => "Unknown Error".to_string(),
        }
    }
}

impl<'a> Display for ParseError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "E{:4}: {}", self.get_err_code(), self.get_err_desc())
    }
}
