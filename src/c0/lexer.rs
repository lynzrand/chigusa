use lazy_static::lazy_static;
use std::collections::HashMap;
use std::iter::{Iterator, Peekable};
use std::str::{CharIndices, FromStr};
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
    src: &'a str,
}

pub struct LexerIterator<'a> {
    lexer: Lexer<'a>,
    iter: Option<Peekable<CharIndices<'a>>>,
}

impl<'a> LexerIterator<'a> {
    pub fn new(src: Lexer<'a>) -> LexerIterator<'a> {
        let mut new_iter = LexerIterator {
            lexer: src,
            iter: None,
        };
        new_iter.iter = Some(new_iter.lexer.src.char_indices().peekable());
        new_iter
    }
}

impl<'a> Iterator for LexerIterator<'a> {
    type Item = Token<'a>;
    fn next(&mut self) -> Option<Token<'a>> {
        self.lexer.get_next_token(self.iter.as_mut().expect(""))
    }
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Lexer<'a> {
        Self { src }
    }

    pub fn get_next_token(&mut self, iter: &mut Peekable<CharIndices>) -> Option<Token<'a>> {
        Self::skip_spaces(iter);
        // the first character of next token
        let c = match iter.peek() {
            Some((_, c)) => c,
            // spaces may occur at the end of file
            None => return None,
        };

        Some(match c {
            '0'..='9' => self.lex_number(iter),
            'a'..='z' | 'A'..='Z' | '_' => self.lex_identifier(iter),
            '+' | '-' | '*' | '/' | '<' | '>' | '=' | '!' | '(' | ')' | '{' | '}' | ',' | ';' => {
                self.lex_operator(iter)
            }
            // TODO: Add to errors and skip this line
            _ => panic!("Unexpected character '{}'", c),
        })
    }

    fn lex_number(&mut self, iter: &mut Peekable<CharIndices>) -> Token<'a> {
        let start_index = iter.peek().expect("This value should be valid").0;
        let mut end_index = start_index;
        while iter.peek().map_or(false, |ch_ind| (*ch_ind).1.is_digit(10)) {
            iter.next();
            end_index += 1;
        }

        Token {
            var: TokenVariant::IntegerLiteral(
                i64::from_str(&self.src[start_index..end_index]).unwrap(),
            ),
            src: &self.src[start_index..end_index],
            pos: start_index,
            end_pos: end_index,
        }
    }

    fn lex_identifier(&mut self, iter: &mut Peekable<CharIndices>) -> Token<'a> {
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
            "if" => TokenVariant::If,
            "else" => TokenVariant::Else,
            "while" => TokenVariant::While,
            "return" => TokenVariant::Return,
            "const" => TokenVariant::Const,
            _ => TokenVariant::Identifier(&self.src[start_index..end_index]),
        };

        Token {
            var: variation,
            src: &self.src[start_index..end_index],
            pos: start_index,
            end_pos: end_index,
        }
    }

    fn lex_operator(&mut self, iter: &mut Peekable<CharIndices>) -> Token<'a> {
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
            ',' => TokenVariant::Comma,
            ';' => TokenVariant::Semicolon,
            _ => panic!(
                "Unexpected character \'{}\' at {}",
                &self.src[start_index..end_index],
                start_index
            ),
        };

        Token {
            var: variation,
            src: &self.src[start_index..end_index],
            pos: start_index,
            end_pos: end_index,
        }
    }

    fn skip_spaces(iter: &mut Peekable<CharIndices>) {
        while match iter.peek() {
            None => false,
            Some((_, ch)) => ch.is_whitespace(),
        } {
            iter.next();
        }
    }
}

impl<'a> IntoIterator for Lexer<'a> {
    type Item = Token<'a>;
    type IntoIter = LexerIterator<'a>;
    fn into_iter(self) -> Self::IntoIter {
        LexerIterator::new(self)
    }
}
