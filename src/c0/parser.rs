use super::ast::*;
use super::infra::*;
use super::lexer::*;
use bimap::BiMap;
use std::iter::Peekable;

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

impl<'a> Parser<'a> {
    pub fn new(lexer: LexerIterator<'a>) -> Parser<'a> {
        Parser {
            lexer: lexer.peekable(),
        }
    }

    fn p_stmt_or_expr(&mut self) -> either::Either<Stmt, Expr> {
        let next = self.lexer.peek().unwrap();


        unimplemented!()
    }

    fn p_expr(&mut self)->ParseResult<Expr> {
        unimplemented!()
    }

    
}
