use crate::c0::ast::{self, Token, TokenVariant};
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::iter::{Iterator, Peekable};
use std::str::{CharIndices, FromStr};

pub struct TokenEntry {}

lazy_static! {
    static ref __OP_COMBINATION: HashMap<char, Box<Vec<char>>> = {
        [
            ('<', Box::new(vec!['='])),
            ('>', Box::new(vec!['='])),
            ('=', Box::new(vec!['='])),
            ('!', Box::new(vec!['='])),
        ]
        .iter()
        .cloned()
        .collect()
    };
}

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
            // the first character of next token
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
            var: TokenVariant::Integer(i64::from_str(&self.src[start_index..end_index]).unwrap()),
            src: &self.src[start_index..end_index],
            pos: start_index,
            end_pos: end_index,
        };
        self.lex_result.push(entry);
    }

    fn lex_identifier(&mut self, iter: &mut Peekable<CharIndices>) {
        let start_index = iter.peek().expect("This value should be valid").0;
        let mut end_index = start_index;
        while iter
            .peek()
            .map_or(false, |ch_ind| (*ch_ind).1.is_alphanumeric())
        {
            iter.next();
            end_index += 1;
        }
        let variation = match &self.src[start_index..end_index] {
            "int" => TokenVariant::Int,
            "void" => TokenVariant::Void,
            "if" => TokenVariant::If,
            "else" => TokenVariant::Else,
            "while" => TokenVariant::While,
            "return" => TokenVariant::Return,
            "const" => TokenVariant::Const,
            _ => TokenVariant::Identifier(&self.src[start_index..end_index]),
        };
        let entry = ast::Token {
            var: variation,
            src: &self.src[start_index..end_index],
            pos: start_index,
            end_pos: end_index,
        };
        self.lex_result.push(entry);
    }

    fn lex_operator(&mut self, iter: &mut Peekable<CharIndices>) {
        let (start_index, first_char) = iter.next().expect("This value should be valid");
        let mut end_index = start_index + 1;
        let second_char: Option<char> =
            __OP_COMBINATION
                .get(&first_char)
                .map_or(None, |vec: &Box<Vec<char>>| {
                    iter.peek().map_or(None, |&(_, ch)| {
                        if vec[..].contains(&ch) {
                            Some(ch)
                        } else {
                            None
                        }
                    })
                });
        if second_char.is_some() {
            iter.next();
            end_index += 1;
        }

        let variation = match first_char {
            '+' => TokenVariant::Plus,
            '-' => TokenVariant::Minus,
            '*' => TokenVariant::Multiply,
            '/' => TokenVariant::Divide,
            '=' => match second_char {
                None => TokenVariant::Assign,
                Some('=') => TokenVariant::Equals,
                _ => panic!(),
            },
            '<' => match second_char {
                None => TokenVariant::LessThan,
                Some('=') => TokenVariant::LessOrEqualThan,
                _ => panic!(),
            },
            '>' => match second_char {
                None => TokenVariant::GreaterThan,
                Some('=') => TokenVariant::GreaterOrEqualThan,
                _ => panic!(),
            },
            '!' => match second_char {
                None => TokenVariant::Not,
                Some('=') => TokenVariant::NotEquals,
                _ => panic!(),
            },
            '(' => TokenVariant::LParenthesis,
            ')' => TokenVariant::RParenthesis,
            '{' => TokenVariant::LCurlyBrace,
            '}' => TokenVariant::RCurlyBrace,
            _ => panic!(
                "Unexpected character \'{}\' at {}",
                &self.src[start_index..end_index],
                start_index
            ),
        };
        let entry = ast::Token {
            var: variation,
            src: &self.src[start_index..end_index],
            pos: start_index,
            end_pos: end_index,
        };
        self.lex_result.push(entry);
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
