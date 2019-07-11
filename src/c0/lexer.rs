use crate::c0::ast;
use std::collections::HashMap;
pub struct TokenEntry {}

struct PushBackIterator<'a ,T> {
    iterator: &'a mut dyn Iterator<Item = T>,
    push_back_pool: Box<Vec<T>>,
}

impl<'a, T> PushBackIterator<'a ,T> {
    pub fn new(iterator: &'a mut dyn Iterator<Item = T>) -> PushBackIterator<T> {
        PushBackIterator {
            iterator: iterator,
            push_back_pool: Box::new(Vec::new()),
        }
    }

    pub fn push(&mut self, item: T) {
        self.push_back_pool.push(item)
    }
}

impl<'a,T> Iterator for PushBackIterator<'a,T> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        if self.push_back_pool.len() == 0 {
            self.iterator.next()
        } else {
            self.push_back_pool.pop()
        }
    }
}
pub struct Lexer<'a> {
    pub lex_result: Vec<ast::Token<'a>>,
    src_iterator: PushBackIterator<'a,char>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Lexer<'a> {
        let mut src_chars = src.chars();
        Lexer {
            lex_result: vec![],
            src: src,
            src_iterator: PushBackIterator::new(),
            src_chars: src_chars,
        }
    }

    pub fn tokenize(&mut self) {}

    // fn get_next_token(& self, cur_ptr: usize)->(ast::Token,  usize) {
    // }

    // fn get_identifier_token(& self, cur_ptr: usize)->(ast::Token, usize){

    // }
}
