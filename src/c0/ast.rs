use std::{cell::RefCell, fmt, fmt::Display, fmt::Formatter, rc::Rc};

#[derive(Debug)]
/// This enum defines the variants of token in C0 language.
pub enum TokenVariant<'a> {
    Const,
    Void,
    If,
    Else,
    Int,
    While,
    Return,
    Semicolon,
    Minus,
    Plus,
    Multiply,
    Divide,
    Not,
    Equals,
    NotEquals,
    LessThan,
    LessOrEqualThan,
    GreaterThan,
    GreaterOrEqualThan,
    Identifier(&'a str),
    IntegerLiteral(i64),
    StringLiteral(&'a str),
    LParenthesis,
    RParenthesis,
    LCurlyBrace,
    RCurlyBrace,
    Assign,
    Comma,
}

impl<'a> Display for TokenVariant<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use self::TokenVariant::*;
        match self {
            Const => write!(f, "Const"),
            If => write!(f, "If"),
            Else => write!(f, "Else"),
            While => write!(f, "While"),
            Return => write!(f, "Return"),
            Semicolon => write!(f, ";\n"),
            Minus => write!(f, "-"),
            Plus => write!(f, "+"),
            Multiply => write!(f, "*"),
            Divide => write!(f, "/"),
            Not => write!(f, "!"),
            Equals => write!(f, "=="),
            NotEquals => write!(f, "!="),
            LessThan => write!(f, "<"),
            LessOrEqualThan => write!(f, "<="),
            GreaterThan => write!(f, ">"),
            GreaterOrEqualThan => write!(f, ">="),
            Identifier(ident) => write!(f, "Identifier({})", ident),
            IntegerLiteral(num) => write!(f, "Integer({})", num),
            StringLiteral(string) => write!(f, "String(\"{}\")", string),
            LParenthesis => write!(f, "("),
            RParenthesis => write!(f, ")"),
            LCurlyBrace => write!(f, "{{\n"),
            RCurlyBrace => write!(f, "\n}}\n"),
            Assign => write!(f, "="),
            Comma => write!(f, ","),
            _ => write!(f, "???"),
        }
    }
}

/// A single token
#[derive(Debug)]
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

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.var)
    }
}

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
        if_block: Rc<RefCell<AstNode<'a>>>,
        else_block: Option<Rc<RefCell<AstNode<'a>>>>,
    },
    WhileStatement {
        checked_expression: Rc<RefCell<AstNode<'a>>>,
        block: Rc<RefCell<AstNode<'a>>>,
    },
}

#[derive(Debug)]
pub struct AstNode<'a> {
    pub var: AstNodeVariation<'a>,
}
