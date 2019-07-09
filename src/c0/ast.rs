use std::{rc::Rc, cell::RefCell};
pub enum TokenVariant<'a> {
    Const,
    Void,
    If,
    Else,
    Int,
    While,
    Main,
    Return,
    Semicolon,
    Negate,
    Positive,
    Multiply,
    Divide,
    Identifier(&'a str),
    Integer(i64),
    LParenthesis,
    RParenthesis,
    LCurlyBrace,
    RCurlyBrace,
    Assignment,
    Comma,
}

pub struct Token<'a> {
    pub var: TokenVariant<'a>,
    pub src: &'a str,
}

pub enum AstNode<'a> {
    ConstDeclaration {
        children: Rc<RefCell<Vec<AstNode<'a>>>>,
    },
    Variable {
        children: Rc<RefCell<Vec<AstNode<'a>>>>,
    },
    Identifier {
        ident: &'a str,
    },
    Operation {
        opcode: i8,
        lhs: Rc<RefCell<AstNode<'a>>>,
        rhs: Rc<RefCell<AstNode<'a>>>,
    },
}
