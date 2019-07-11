use crate::c0::ast;
use std::collections::HashMap;
use std::iter::{Iterator, Peekable};
use std::str::{CharIndices, FromStr};

pub struct TokenEntry {}

pub struct Lexer<'a> {
    pub lex_result: Vec<ast::Token<'a>>,
    src: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Lexer<'a> {
        Self {
            lex_result: vec![],
            src: src,
        }
    }

    pub fn tokenize(&mut self) -> &mut Lexer<'a> {
        let mut iter: Peekable<CharIndices> = self.src.char_indices().peekable();
        while iter.peek() != None {
            Self::skip_spaces(&mut iter);
            /// the first character of next token
            let c = match iter.peek() {
                Some((_, c)) => c,
                // spaces may occur at the end of file
                None => break,
            };

            match c {
                '0'..='9' => self.lex_number(&mut iter),
                'a'..='z' | 'A'..='Z' | '_' => self.lex_identifier(&mut iter),
                '+' | '-' | '*' | '/' | '<' | '>' | '=' | '!' | '(' | ')' | '{' | '}' | ',' => {
                    self.lex_operator(&mut iter)
                }
                // TODO: Add to errors and skip this line
                _ => panic!("Unexpected character '{}'", c),
            };
        }
        self
    }

    fn lex_number(&mut self, iter: &mut Peekable<CharIndices>) {
        let start_index = iter.peek().expect("This value should be valid").0;
        let mut end_index = start_index;
        while iter.peek().map_or(false, |ch_ind| (*ch_ind).1.is_digit(10)) {
            iter.next();
            end_index += 1;
        }
        let entry = ast::Token {
            var: ast::TokenVariant::Integer(
                i64::from_str(&self.src[start_index..end_index]).unwrap(),
            ),
            src: &self.src[start_index..end_index],
            pos: start_index,
            end_pos: end_index,
        };
        self.lex_result.push(entry);
    }

    fn lex_identifier(&mut self, iter: &mut Peekable<CharIndices>) {
        // TODO: implement
    }

    fn lex_operator(&mut self, iter: &mut Peekable<CharIndices>) {
        // TODO: implement

    }

    fn skip_spaces(iter: &mut Peekable<CharIndices>) {
        while match iter.peek() {
            None => false,
            Some((_, ch)) => ch.is_whitespace(),
        } {
            iter.next();
        }
    }

    // fn get_next_token(& self, cur_ptr: usize)->(ast::Token,  usize) {
    // }

    // fn get_identifier_token(& self, cur_ptr: usize)->(ast::Token, usize){

    // }
}
