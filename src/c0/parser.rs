use crate::c0::ast::*;
use crate::c0::lexer::*;
use lazy_static::lazy_static;
use std::collections::*;
use std::{cell::RefCell, fmt, fmt::Display, fmt::Formatter, rc::Rc};

pub trait IntoParser<'a> {
    fn into_parser(self: Box<Self>) -> Parser<'a>;
}

impl<'a> IntoParser<'a> for dyn Iterator<Item = Token<'a>> {
    fn into_parser(self: Box<Self>) -> Parser<'a> {
        Parser::new(self)
    }
}

pub struct Parser<'a> {
    lexer: Box<dyn Iterator<Item = Token<'a>>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Box<dyn Iterator<Item = Token<'a>>>) -> Parser<'a> {
        Parser {
            lexer,
            // stack: VecDeque::new(),
        }
    }

    pub fn parse(&mut self) -> Result<Program, ParseErrors> {
        self.parse_program()
    }

    fn parse_program(&mut self) -> Result<Program, ParseErrors> {
        let mut scope = Ptr::new(Scope::new(None));
        let decl = self.parse_decls(scope.clone())?;
        let fns = self.parse_fns(scope.clone())?;
        Ok(Program {
            scope: scope.clone(),
            fns,
            decl,
        })
        // unimplemented!()
    }

    fn parse_decls(&mut self, scope: Ptr<Scope>) -> Result<Vec<Ptr<VarDecalaration>>, ParseErrors> {
        unimplemented!()
    }

    fn parse_fns(&mut self, scope: Ptr<Scope>) -> Result<Vec<Ptr<FnDeclaration>>, ParseErrors> {
        unimplemented!()
    }

    // fn parse_decl(&mut self, scope: Ptr<Scope>)->Result
}

pub enum ParseErrors {}
