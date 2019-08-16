use lazy_static::lazy_static;
use std::collections::HashMap;
use std::iter::{Iterator, Peekable};
use std::str::{Chars, FromStr};
use std::{cell::RefCell, fmt, fmt::Display, fmt::Formatter, hash::Hash, rc::Rc, string::String};

#[derive(Debug, Eq, PartialEq, Hash)]
/// This enum defines the variants of token in C0 language. Variants are pretty
/// self-explanatory.
pub enum TokenVariant<'a> {
    Const,
    If,
    Else,
    While,
    Return,
    Semicolon,
    Minus,
    Plus,
    Multiply,
    Divide,
    Not,
    BinaryAnd,
    BinaryOr,
    And,
    Or,
    Xor,
    Increase,
    Decrease,
    Equals,
    NotEquals,
    LessThan,
    LessOrEqualThan,
    GreaterThan,
    GreaterOrEqualThan,
    Identifier(&'a str),
    IntegerLiteral(i64),
    StringLiteral(String),
    LParenthesis,
    RParenthesis,
    LCurlyBrace,
    RCurlyBrace,
    Assign,
    Comma,
    EndOfFile,
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
            BinaryAnd => write!(f, "&"),
            BinaryOr => write!(f, "|"),
            And => write!(f, "&&"),
            Or => write!(f, "||"),
            Xor => write!(f, "^"),
            Increase => write!(f, "++"),
            Decrease => write!(f, "--"),
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
            EndOfFile => write!(f, "#"),
            // _ => write!(f, "???"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Pos {
    ln: usize,
    pos: usize,
    index: usize,
}

impl Pos {
    pub fn new(ln: usize, pos: usize, index: usize) -> Pos {
        Pos { ln, pos, index }
    }

    pub fn zero() -> Pos {
        Pos {
            ln: 0,
            pos: 0,
            index: 0,
        }
    }

    #[must_use]
    pub fn bump(mut self) -> Pos {
        self.index += 1;
        self
    }

    #[must_use]
    pub fn inc(mut self) -> Pos {
        self.pos += 1;
        self.index += 1;
        self
    }

    #[must_use]
    pub fn lf(mut self) -> Pos {
        self.pos = 0;
        self.ln += 1;
        self.index += 1;
        self
    }
}

impl Display for Pos {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "line {}, col {} ({} from start)",
            self.ln, self.pos, self.index
        )
    }
}

pub struct StringPosIter<'a> {
    chars: std::iter::Chain<Chars<'a>, std::iter::Once<char>>,
    pos: Pos,
    is_last_cr: bool,
}

impl<'a> StringPosIter<'a> {
    pub fn new(string: &'a str) -> StringPosIter<'a> {
        let chars = string.chars().chain(std::iter::once('\0'));
        StringPosIter {
            chars,
            pos: Pos::zero(),
            is_last_cr: false,
        }
    }
}

impl<'a> Iterator for StringPosIter<'a> {
    type Item = (Pos, char);
    fn next(&mut self) -> Option<Self::Item> {
        let next_char = self.chars.next();
        match next_char {
            None => None,
            Some(ch) => {
                match ch {
                    '\n' => {
                        if !self.is_last_cr {
                            self.pos.lf();
                        } else {
                            self.pos.bump();
                        }
                        self.is_last_cr = false;
                    }
                    '\r' => {
                        self.pos.lf();
                        self.is_last_cr = true;
                    }
                    ch @ _ => {
                        self.is_last_cr = false;
                        self.pos.inc();
                    }
                };
                Some((self.pos, ch))
            }
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
    pub pos: Pos,

    /// its end position along source code
    pub end_pos: Pos,
}

impl<'a> Token<'a> {
    pub fn get_ident(&self) -> Result<&'a str, ()> {
        match self.var {
            TokenVariant::Identifier(s) => Ok(s),
            _ => Err(()),
        }
    }
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
            ('+', Box::new(vec!['+'])),
            ('-', Box::new(vec!['-'])),
            ('&', Box::new(vec!['&'])),
            ('|', Box::new(vec!['|'])),
        ]
        .iter()
        .cloned()
        .collect()
    };
}

pub struct Lexer<'a> {
    src: &'a str,
    completed: bool,
}

pub struct LexerIterator<'a> {
    lexer: Lexer<'a>,
    iter: Option<Peekable<StringPosIter<'a>>>,
}

impl<'a> LexerIterator<'a> {
    pub fn new(src: Lexer<'a>) -> LexerIterator<'a> {
        let mut new_iter = LexerIterator {
            lexer: src,
            iter: None,
        };
        new_iter.iter = Some(StringPosIter::new(new_iter.lexer.src).peekable());
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
        Self {
            src,
            completed: false,
        }
    }

    /// **Not Intended For Direct Use.**
    ///
    /// This method requires creating a character index iterator for the `src`
    /// field. Thus it must be used inside an `LexerIterator`.
    pub fn get_next_token(&mut self, iter: &mut Peekable<StringPosIter>) -> Option<Token<'a>> {
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
            '\"' => self.lex_string_literal(iter),
            '+' | '-' | '*' | '/' | '<' | '>' | '=' | '!' | '|' | '&' | '^' | '(' | ')' | '{'
            | '}' | ',' | ';' => self.lex_operator(iter),
            // TODO: Add to errors and skip this line
            _ => panic!("Unexpected character '{}'", c),
        })
    }

    /// Lex a number
    fn lex_number(&mut self, iter: &mut Peekable<StringPosIter>) -> Token<'a> {
        let start_pos = iter.peek().expect("This value should be valid").0;
        while iter.peek().map_or(false, |ch_ind| (*ch_ind).1.is_digit(10)) {
            iter.next();
        }
        let end_pos = iter.peek().unwrap().0;

        let start_index = start_pos.index;
        let end_index = end_pos.index;

        Token {
            var: TokenVariant::IntegerLiteral(
                i64::from_str(&self.src[start_index..end_index]).unwrap(),
            ),
            src: &self.src[start_index..end_index],
            pos: start_pos,
            end_pos,
        }
    }

    /// Lex a string literal.
    fn lex_string_literal(&mut self, iter: &mut Peekable<StringPosIter>) -> Token<'a> {
        let (start_index, start_quotation_mark) = iter.next().expect("This value should be valid");

        if start_quotation_mark != '"' {
            panic!(
                "A string literal must start with a quotation mark! found at {}",
                start_index
            );
        }

        let end_index: Pos;
        let mut tgt_string = String::new();

        loop {
            let (this_index, this_char) = iter.next().expect("Unexpected EOF in string literal");
            match this_char {
                '\\' => {
                    // Deal with escaping stuff
                    let (_, next_char) = iter.next().expect("Unexpected EOF in string literal");
                    let pushed_char = match next_char {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '\\' => '\\',
                        '"' => '"',
                        // 'u'=>,
                        _ => panic!(
                            "Invalid escape sequence '\\{}' at {}",
                            this_char, this_index
                        ),
                    };
                    tgt_string.push(pushed_char);
                }
                '"' => {
                    end_index = this_index.inc();
                    break;
                }
                '\n' | '\r' => {
                    panic!("Unexpected EOL in string literal");
                }
                _ => {
                    tgt_string.push(this_char);
                }
            }
        }

        Token {
            var: TokenVariant::StringLiteral(tgt_string),
            src: &self.src[start_index.index..end_index.index],
            pos: start_index,
            end_pos: end_index,
        }
    }

    /// Lex an identifier.
    fn lex_identifier(&mut self, iter: &mut Peekable<StringPosIter>) -> Token<'a> {
        let start_index = iter.peek().expect("This value should be valid").0;
        while iter
            .peek()
            .map_or(false, |ch_ind| (*ch_ind).1.is_alphanumeric())
        {
            iter.next();
        }
        let mut end_index = iter.peek().unwrap().0;
        let variation = match &self.src[start_index.index..end_index.index] {
            "if" => TokenVariant::If,
            "else" => TokenVariant::Else,
            "while" => TokenVariant::While,
            "return" => TokenVariant::Return,
            "const" => TokenVariant::Const,
            _ => TokenVariant::Identifier(&self.src[start_index.index..end_index.index]),
        };

        Token {
            var: variation,
            src: &self.src[start_index.index..end_index.index],
            pos: start_index,
            end_pos: end_index,
        }
    }

    /// Lex an operator.
    fn lex_operator(&mut self, iter: &mut Peekable<StringPosIter>) -> Token<'a> {
        let (start_index, first_char) = iter.next().expect("This value should be valid");
        let mut end_index = start_index .inc();
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
            end_index =end_index.inc();
        }

        let variation = match first_char {
            '+' => match second_char {
                None => TokenVariant::Plus,
                Some('+') => TokenVariant::Increase,
                _ => unreachable!(),
            },
            '-' => match second_char {
                None => TokenVariant::Minus,
                Some('-') => TokenVariant::Decrease,
                _ => unreachable!(),
            },
            '*' => TokenVariant::Multiply,
            '/' => TokenVariant::Divide,
            '=' => match second_char {
                None => TokenVariant::Assign,
                Some('=') => TokenVariant::Equals,
                _ => unreachable!(),
            },
            '<' => match second_char {
                None => TokenVariant::LessThan,
                Some('=') => TokenVariant::LessOrEqualThan,
                _ => unreachable!(),
            },
            '>' => match second_char {
                None => TokenVariant::GreaterThan,
                Some('=') => TokenVariant::GreaterOrEqualThan,
                _ => unreachable!(),
            },
            '!' => match second_char {
                None => TokenVariant::Not,
                Some('=') => TokenVariant::NotEquals,
                _ => unreachable!(),
            },
            '|' => match second_char {
                None => TokenVariant::BinaryOr,
                Some('|') => TokenVariant::Or,
                _ => unreachable!(),
            },
            '&' => match second_char {
                None => TokenVariant::BinaryAnd,
                Some('&') => TokenVariant::And,
                _ => unreachable!(),
            },
            '^' => TokenVariant::Xor,
            '(' => TokenVariant::LParenthesis,
            ')' => TokenVariant::RParenthesis,
            '{' => TokenVariant::LCurlyBrace,
            '}' => TokenVariant::RCurlyBrace,
            ',' => TokenVariant::Comma,
            ';' => TokenVariant::Semicolon,
            _ => panic!(
                "Unexpected character \'{}\' at {}",
                &self.src[start_index.index..end_index.index],
                start_index
            ),
        };

        Token {
            var: variation,
            src: &self.src[start_index.index..end_index.index],
            pos: start_index,
            end_pos: end_index,
        }
    }

    /// Skip spaces and stop before the next non-space character.
    fn skip_spaces(iter: &mut Peekable<StringPosIter>) {
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

// ======================

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lex_number() {
        let real: Vec<TokenVariant> = Lexer::new("123+456")
            .into_iter()
            .map(|token| token.var)
            .collect();
        let expected = vec![
            TokenVariant::IntegerLiteral(123),
            TokenVariant::Plus,
            TokenVariant::IntegerLiteral(456),
        ];
        assert_eq!(real, expected);
    }

    #[test]
    fn test_lex_string() {
        let real: Vec<TokenVariant> = Lexer::new(r#""123+\r\n\t456""#)
            .into_iter()
            .map(|token| token.var)
            .collect();
        let expected = vec![TokenVariant::StringLiteral("123+\r\n\t456".to_owned())];
        assert_eq!(real, expected);
    }

    #[test]
    fn test_lex_ident() {
        let real: Vec<TokenVariant> = Lexer::new("int x = 3;")
            .into_iter()
            .map(|token| token.var)
            .collect();
        let expected = vec![
            TokenVariant::Identifier("int"),
            TokenVariant::Identifier("x"),
            TokenVariant::Assign,
            TokenVariant::IntegerLiteral(3),
            TokenVariant::Semicolon,
        ];
        assert_eq!(real, expected);
    }

    #[test]
    fn test_lex_ops() {
        use TokenVariant::*;
        let real: Vec<TokenVariant> =
            Lexer::new("+ - * / ++ -- ! != == >= > <= < & | && || ^ ( ) { } = ,")
                .into_iter()
                .map(|token| token.var)
                .collect();
        let expected = vec![
            Plus,
            Minus,
            Multiply,
            Divide,
            Increase,
            Decrease,
            Not,
            NotEquals,
            Equals,
            GreaterOrEqualThan,
            GreaterThan,
            LessOrEqualThan,
            LessThan,
            BinaryAnd,
            BinaryOr,
            And,
            Or,
            Xor,
            LParenthesis,
            RParenthesis,
            LCurlyBrace,
            RCurlyBrace,
            Assign,
            Comma,
        ];
        assert_eq!(real, expected);
    }

    #[test]
    #[should_panic]
    fn test_lexing_panic_op() {
        let real: Vec<TokenVariant> = Lexer::new("int x = what?is:this;")
            .into_iter()
            .map(|token| token.var)
            .collect();
    }

    #[test]
    #[should_panic]
    fn test_lexing_panic_string_escape() {
        let real: Vec<TokenVariant> = Lexer::new(r#""\y""#)
            .into_iter()
            .map(|token| token.var)
            .collect();
    }

    #[test]
    #[should_panic]
    fn test_lexing_panic_string_eol() {
        let real: Vec<TokenVariant> = Lexer::new(
            r#""
        ""#,
        )
        .into_iter()
        .map(|token| token.var)
        .collect();
    }
}
