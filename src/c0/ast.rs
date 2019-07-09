use std::vec::Vec;

pub enum AstNodeType {}

pub enum AstNode {
    List {
        children: Box<Vec<AstNode>>,
    },
    Binary {
        left_child: Box<AstNode>,
        right_child: Box<AstNode>,
    },
    Unary {
        child: Box<AstNode>,
    },
    Endpoint {},
}
