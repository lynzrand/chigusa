use super::ast::Literal;
use super::ast::*;
use super::err::*;
use super::lexer::*;
use crate::prelude::*;
use bimap::BiMap;
use std::iter::Peekable;

use either::Either;
use LoopCtrl::*;

pub trait IntoParser {
    fn into_parser(self) -> Parser;
}

impl IntoParser for Lexer {
    fn into_parser(self) -> Parser {
        Parser::new(self)
    }
}

pub trait TokenIterator: Iterator<Item = Token> + itertools::PeekingNext {
    fn expect(&mut self, token: TokenVariant) -> ParseResult<Token> {
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

    fn expect_peek(&mut self, token: TokenVariant) -> ParseResult<Token> {
        // * separated variables because lifetime concerns.
        match self.peeking_next(|_| false) {
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
        token: TokenVariant,
        map: impl FnOnce(Token) -> T,
        f: impl FnOnce(Token) -> Result<T, ParseError>,
    ) -> ParseResult<T> {
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

    fn try_consume(&mut self, token: TokenVariant) -> bool {
        match self.peeking_next(|v| variant_eq(&v.var, &token)) {
            Some(_) => true,
            None => false,
        }
    }

    fn try_consume_log_span(&mut self, token: TokenVariant) -> Option<Span> {
        match self.peeking_next(|v| variant_eq(&v.var, &token)) {
            Some(v) => Some(v.span),
            None => None,
        }
    }
}

type LexerWrapped = Peekable<Lexer>;

impl TokenIterator for LexerWrapped {}

pub struct TypeVar {
    types: Vec<TypeDef>,
    type_names: BiMap<usize, String>,
    // vars: Vec<VarDef>,
    // var_names: BiMap<usize, String>,
}

impl TypeVar {
    pub fn new() -> TypeVar {
        TypeVar {
            types: Vec::new(),
            type_names: BiMap::new(),
            // vars: Vec::new(),
            // var_names: BiMap::new(),
        }
    }

    pub fn insert_type(&mut self, type_name: &str, type_def: TypeDef) -> usize {
        unimplemented!()
    }
}

pub struct Parser {
    lexer: LexerWrapped,
    type_var: TypeVar,
    cur_token: Token,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        Parser {
            lexer: lexer.peekable(),
            type_var: TypeVar::new(),
            cur_token: Token {
                var: TokenVariant::Dummy,
                // src: Span::zero(),
                span: Span::zero(),
            },
        }
    }

    pub fn parse(&mut self) -> ParseResult<Program> {
        unimplemented!();
        // Ok(Program {
        //     scope: (),
        //     vars: (),
        //     types: (),
        // })
    }

    fn p_stmt_or_expr(&mut self, scope: Ptr<Scope>) -> ParseResult<Either<Stmt, Expr>> {
        let next = self.lexer.peek().unwrap();

        match next.var {
            TokenVariant::While => self.p_while_stmt(scope).map(|inner| Either::Left(inner)),
            TokenVariant::If => self.p_if_expr(scope).map(|inner| Either::Right(inner)),
            TokenVariant::Identifier(i) => match scope.borrow().find_def(&i) {
                None => Err(parse_err(
                    ParseErrVariant::CannotFindIdent(i.to_owned()),
                    next.span,
                )),
                Some(def) => match &*def.borrow() {
                    SymbolDef::Typ { .. } => self
                        .p_decl_stmt(scope.clone())
                        .map(|inner| Either::Left(inner)),
                    SymbolDef::Var { .. } => {
                        self.p_expr(scope.clone()).map(|inner| Either::Right(inner))
                    }
                },
            },
            _ => Err(parse_err(
                ParseErrVariant::UnexpectedToken(next.var.clone()),
                next.span,
            )),
        }
    }

    fn p_block_expr(&mut self, scope: Ptr<Scope>) -> ParseResult<Expr> {
        unimplemented!()
    }

    fn p_block_expr_no_scope(&mut self, scope: Ptr<Scope>) -> ParseResult<Expr> {
        unimplemented!()
    }

    fn p_fn(&mut self, scope: Ptr<Scope>) -> ParseResult<Stmt> {
        unimplemented!()
    }

    fn p_while_stmt(&mut self, scope: Ptr<Scope>) -> ParseResult<Stmt> {
        unimplemented!()
    }

    fn p_if_expr(&mut self, scope: Ptr<Scope>) -> ParseResult<Expr> {
        let start_span = self.lexer.expect(TokenVariant::If)?.span;
        self.lexer.expect_peek(TokenVariant::LParenthesis);

        let mut span = start_span;

        let cond = Ptr::new(self.p_expr(scope.clone())?);
        let if_block = Ptr::new(
            if self.lexer.expect_peek(TokenVariant::LCurlyBrace).is_ok() {
                self.p_block_expr(scope.clone())
            } else {
                self.p_expr(scope.clone())
            }?,
        );

        span = span + if_block.borrow().span();
        let else_span = self.lexer.try_consume_log_span(TokenVariant::Else);
        let else_block = if else_span.is_some() {
            Some(Ptr::new(
                if self.lexer.expect_peek(TokenVariant::LCurlyBrace).is_ok() {
                    self.p_block_expr(scope.clone())
                } else {
                    self.p_expr(scope.clone())
                }?,
            ))
        } else {
            None
        };

        else_block.as_ref().map(|e| span = span + e.borrow().span());

        Ok(Expr {
            var: ExprVariant::IfConditional(IfConditional {
                cond,
                if_block,
                else_block,
            }),
            span,
        })
    }
    fn p_decl_stmt(&mut self, scope: Ptr<Scope>) -> ParseResult<Stmt> {
        unimplemented!()
    }

    fn p_expr(&mut self, scope: Ptr<Scope>) -> ParseResult<Expr> {
        let stack = Vec::<Expr>::new();
        unimplemented!();
    }

    fn p_literal(&mut self) -> ParseResult<Expr> {
        let t = self.lexer.next().unwrap();
        match t.var {
            TokenVariant::Literal(i) => Ok(Expr {
                var: ExprVariant::Literal(i.into()),
                span: t.span,
            }),
            _ => Err(parse_err(ParseErrVariant::InternalErr, t.span)),
        }
    }
}
