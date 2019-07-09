use crate::c0::ast;
use std::collections::HashMap;
pub struct TokenEntry {}

pub struct Lexer<'a> {
    pub token_table: HashMap<&'a str, TokenEntry>,
    identifier_counter: i32,
    pub lex_result: Vec<ast::Token<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn tokenize(&mut self, tgt: &str) {}
}
