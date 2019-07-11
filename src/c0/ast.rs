use std::{cell::RefCell, rc::Rc};

/// This enum defines the variants of token in C0 language.
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

/// A single token
pub struct Token<'a> {
    /// Its variant
    pub var: TokenVariant<'a>,

    /// its source string
    pub src: &'a str,

    /// its position along source code
    pub pos: usize,

    /// its end position along source code
    pub end_pos: usize,
}

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
        left_combine: bool,
        lhs: Rc<RefCell<AstNode<'a>>>,
        rhs: Rc<RefCell<AstNode<'a>>>,
    },
    UnaryOperation {
        opcode: char,
        rhs: Rc<RefCell<AstNode<'a>>>,
    },
    IfStatement {
        checked_expression: Rc<RefCell<AstNode<'a>>>,
        has_else: bool,
        if_block: Rc<RefCell<AstNode<'a>>>,
        else_block: Option<Rc<RefCell<AstNode<'a>>>>,
    },
    WhileStatement {
        checked_expression: Rc<RefCell<AstNode<'a>>>,
        block: Rc<RefCell<AstNode<'a>>>,
    },
}

pub struct AstNode<'a> {
    pub var: AstNodeVariation<'a>,
}
