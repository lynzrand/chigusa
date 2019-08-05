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

impl<'a> TokenIterator<'a> for Peekable<Box<dyn Iterator<Item = Token<'a>>>> {}

pub struct Parser<'a> {
    lexer: Peekable<Box<dyn Iterator<Item = Token<'a>>>>,
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
        let mut fns = vec![];
        let mut vars = vec![];
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
                scope
                    .borrow_mut()
                    .token_table
                    .insert(identifier_owned, entry);
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
                symbol: token_entry,
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
        unimplemented!()
    }

    fn parse_expr(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, Ptr<Expr>> {
        let mut op_stack = Vec::new();
        let mut expr_root = match self.lexer.peek().ok_or(ParseError::EarlyEof)?.var {
            TokenVariant::IntegerLiteral(i) => Ptr::new(Expr::Int(IntegerLiteral(i))),
            TokenVariant::LParenthesis => {
                op_stack.push(OpVar::_Lpr);
                self.parse_expr(scope)?
            }
            TokenVariant::Identifier(i) => {
                self.lexer.next();
                let ident = scope
                    .borrow()
                    .find_definition(i)
                    .ok_or(ParseError::CannotFindVar(i))?;
                match *ident.borrow() {
                    TokenEntry::Variable { .. } => {
                        // This is a variable, stop and add as root
                        Ptr::new(Expr::Ident(Identifier(ident)))
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
            token @ _ => Err(ParseError::UnexpectedToken(token))?,
        };

        while !self.lexer.try_consume(TokenVariant::Semicolon)
            && !self.lexer.try_consume(TokenVariant::RParenthesis)
            && !self.lexer.try_consume(TokenVariant::Comma)
        {}

        unimplemented!()
    }

    fn parse_fn_call_params(&mut self, scope: Ptr<Scope>) -> ParseResult<'a, Vec<Ptr<Expr>>> {
        unimplemented!()
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

    pub fn get_err_desc(&self) -> &str {
        use self::ParseError::*;
        match self {
            ExpectToken(token) => format!("Expected {}", token),
            NoConstFns => "Functions cannot be marked as constant",
            InternalErr => "Something went wrong inside the compiler",
            _ => "Unknown Error",
        }
    }
}

impl<'a> Display for ParseError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "E{:4}: {}", self.get_err_code(), self.get_err_desc())
    }
}
