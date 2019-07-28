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
    stack: VecDeque<AstNode<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Box<dyn Iterator<Item = Token<'a>>>) -> Parser<'a> {
        Parser {
            lexer,
            stack: VecDeque::new(),
        }
    }

    pub fn parse(&mut self) -> AstNode<'a> {
        unimplemented!()
    }
}

pub enum TokenEntry<'a> {
    Variable {
        is_const: bool,
        var_type: &'a TokenEntry<'a>,
    },
    Type {},
    Function {
        return_type: &'a TokenEntry<'a>,
        params_type: Vec<&'a TokenEntry<'a>>,
    },
}

pub struct Scope<'a> {
    pub token_table: HashMap<&'a str, TokenEntry<'a>>,
}

type ClassRef<T> = Rc<RefCell<T>>;

#[derive(Debug)]
pub enum AstNodeVariation<'a> {
    ConstDeclaration {
        children: Rc<RefCell<Vec<AstNode<'a>>>>,
    },
    VariableDeclaration {
        children: Rc<RefCell<Vec<AstNode<'a>>>>,
    },
    Identifier {
        ident: &'a str,
    },
    BinaryOperation {
        opcode: char,
        is_left_combine: bool,
        lhs: Rc<RefCell<AstNode<'a>>>,
        rhs: Rc<RefCell<AstNode<'a>>>,
    },
    UnaryOperation {
        opcode: char,
        is_left_combine: bool,
        side: Rc<RefCell<AstNode<'a>>>,
    },
    IfStatement {
        checked_expression: Rc<RefCell<AstNode<'a>>>,
        has_else: bool,
        if_body: Rc<RefCell<AstNode<'a>>>,
        else_body: Option<Rc<RefCell<AstNode<'a>>>>,
    },
    WhileStatement {
        checked_expression: Rc<RefCell<AstNode<'a>>>,
        body: Rc<RefCell<AstNode<'a>>>,
    },
    Block {
        children: Rc<RefCell<Vec<AstNode<'a>>>>,
    },
    Document {
        children: Rc<RefCell<Vec<AstNode<'a>>>>,
    },
}

#[derive(Debug)]
pub struct AstNode<'a> {
    pub var: AstNodeVariation<'a>,
    pub start_pos: usize,
    pub end_pos: usize,
}
