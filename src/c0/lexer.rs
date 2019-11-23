use crate::prelude::*;
use once_cell::sync::*;
use ramp;
use std::collections::HashMap;
use std::iter::{Iterator, Peekable};
use std::str::{Chars, FromStr};
use std::{cell::RefCell, fmt, fmt::Display, fmt::Formatter, hash::Hash, rc::Rc, string::String};

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
/// This enum defines the variants of token in C0 language. Variants are pretty
/// self-explanatory.
pub enum TokenVariant {
    Const,
    As,
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
    Identifier(String),
    Literal(Literal),
    LParenthesis,
    RParenthesis,
    LCurlyBrace,
    RCurlyBrace,
    Assign,
    Comma,
    EndOfFile,
    Dummy,
}

impl Display for TokenVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use self::TokenVariant::*;
        match self {
            Const => write!(f, "Const"),
            As => write!(f, "As"),
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
            Literal(b) => write!(f, "Literal(\"{}\")", b),
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

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
enum Literal {
    Char(char),
    String(String),
    Boolean(bool),
    Integer(ramp::Int),
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use self::Literal::*;
        match self {
            Char(c) => write!(f, "Char({})", c.escape_debug()),
            String(s) => write!(f, "String(\"{}\")", s.escape_debug()),
            Boolean(b) => write!(f, "Boolean({})", b),
            Integer(i) => write!(f, "Integer({})", i),
        }
    }
}

/// A single token
#[derive(Debug, Clone)]
pub struct Token {
    /// Its variant
    pub var: TokenVariant,

    /// The space the token occupies
    pub span: Span,
}

impl Token {
    pub fn get_ident(&self) -> Result<&str, ()> {
        match self.var {
            TokenVariant::Identifier(s) => Ok(&s),
            _ => Err(()),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Token{{var: {}, span: {} }}", self.var, self.span)
    }
}

static OperatorCombination: Lazy<HashMap<char, Box<Vec<char>>>> = Lazy::new(|| {
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
});

pub struct StringPosIter {
    chars: std::iter::Chain<Box<dyn Iterator<Item = char>>, std::iter::Once<char>>,
    pos: Pos,
    is_last_cr: bool,
}

impl StringPosIter {
    pub fn new(src: Box<dyn Iterator<Item = char>>) -> StringPosIter {
        let chars = src.chain(std::iter::once('\0'));
        StringPosIter {
            chars,
            pos: Pos::zero(),
            is_last_cr: false,
        }
    }
}

impl Iterator for StringPosIter {
    type Item = (Pos, char);
    fn next(&mut self) -> Option<Self::Item> {
        let next_char = self.chars.next();
        match next_char {
            None => None,
            Some(ch) => {
                let ret = Some((self.pos, ch));
                match ch {
                    '\n' => {
                        if !self.is_last_cr {
                            self.pos.lf_self();
                        } else {
                            self.pos.bump_self();
                        }
                        self.is_last_cr = false;
                    }
                    '\r' => {
                        self.pos.lf_self();
                        self.is_last_cr = true;
                    }
                    _ => {
                        self.is_last_cr = false;
                        self.pos.inc_self();
                    }
                };
                ret
            }
        }
    }
}

pub struct Lexer {
    iter: Peekable<StringPosIter>,
}

impl Iterator for Lexer {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        self.get_next_token()
    }
}

impl Lexer {
    pub fn new(iter: Box<dyn Iterator<Item = char>>) -> Lexer {
        Lexer {
            iter: StringPosIter::new(iter).peekable(),
        }
    }

    /// **Not Intended For Direct Use.**
    ///
    /// This method requires creating a character index iterator for the `src`
    /// field. Thus it must be used inside an `LexerIterator`.
    pub fn get_next_token(&mut self) -> Option<Token> {
        Self::skip_spaces(&mut self.iter);
        // the first character of next token
        let c = match self.iter.peek() {
            Some((_, '\0')) => return None,
            Some((_, c)) => c,
            // spaces may occur at the end of file
            None => return None,
        };

        Some(match c {
            '0'..='9' => self.lex_number(),
            'a'..='z' | 'A'..='Z' | '_' => self.lex_identifier(),
            '\"' => self.lex_string_literal(),
            '\'' => self.lex_char_literal(),
            '+' | '-' | '*' | '/' | '<' | '>' | '=' | '!' | '|' | '&' | '^' | '(' | ')' | '{'
            | '}' | ',' | ';' => self.lex_operator(),
            // TODO: Add to errors and skip this line
            _ => panic!("Unexpected character '{}'", c),
        })
    }

    /// Lex a number
    fn lex_number(&mut self) -> Token {
        let start_pos = self.iter.peek().expect("This value should be valid").0;
        let number = String::new();

        // radix check
        let radix = if self.iter.peek().map_or(false, |ch_ind| ch_ind.1 == '0') {
            // this digit is '0'. consume and advance
            self.iter.next();
            match self.iter.peek().map_or('_', |i| i.1) {
                'b' => 2,
                'o' => 8,
                'x' => 16,
                '0'..='9' => 10,
                _ => {
                    // This does not match any number format, return zero
                    return Token {
                        var: TokenVariant::Literal(Literal::Integer(ramp::Int::zero())),
                        // src: &self.src[start_pos.index..start_pos.index + 1],
                        span: Span::from(start_pos, start_pos.bump()),
                    };
                }
            }
        } else {
            10
        };

        while self
            .iter
            .peek()
            .map_or(false, |ch_ind| (*ch_ind).1.is_digit(10))
        {
            number.push(self.iter.next().unwrap().1);
        }

        let end_pos = self.iter.peek().unwrap().0;

        let start = start_pos.index;
        let end = end_pos.index;

        Token {
            var: TokenVariant::Literal(Literal::Integer(
                ramp::Int::from_str_radix(&number, radix).unwrap(),
            )),
            // src: &self.src[start..end],
            span: Span::from(start_pos, end_pos),
        }
    }

    fn lex_char_literal(&mut self) -> Token {
        let (start, start_quote) = self.iter.next().expect("Should be valid");
        if start_quote != '\'' {
            panic!(
                "A character literal must start with a quotation mark! found at {}",
                start
            );
        }

        let ch = match self
            .iter
            .next()
            .expect("A character literal should have a character")
            .1
        {
            '\\' => Self::unescape_character(&mut self.iter),
            ch @ _ => ch,
        };

        let (end, end_quote) = self.iter.next().expect("Should be valid");
        if end_quote != '\'' {
            panic!(
                "A character literal must end with a quotation mark! found at {}",
                start
            );
        }

        Token {
            var: TokenVariant::Literal(Literal::Char(ch)),
            span: Span::from(start, end),
        }
    }

    /// Lex a string literal.
    fn lex_string_literal(&mut self) -> Token {
        let (start, start_quotation_mark) = self.iter.next().expect("This value should be valid");

        if start_quotation_mark != '"' {
            panic!(
                "A string literal must start with a quotation mark! found at {}",
                start
            );
        }

        let end: Pos;
        let mut tgt_string = String::new();

        loop {
            let (this_index, this_char) =
                self.iter.next().expect("Unexpected EOF in string literal");
            match this_char {
                '\\' => {
                    // Deal with escaping stuff
                    let (_, next_char) =
                        self.iter.next().expect("Unexpected EOF in string literal");
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
                    end = this_index.map_inc(-1, 0, -1);
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
            var: TokenVariant::Literal(Literal::String(tgt_string)),
            span: Span::from(start, end),
        }
    }

    /// Lex an identifier.
    fn lex_identifier(&mut self) -> Token {
        let start = self.iter.peek().expect("This value should be valid").0;
        let ident = String::new();
        while self
            .iter
            .peek()
            .map_or(false, |ch_ind| (*ch_ind).1.is_alphanumeric())
        {
            ident.push(self.iter.next().unwrap().1);
        }
        let end = self.iter.peek().unwrap().0;
        let variation = match &ident[..] {
            "if" => TokenVariant::If,
            "else" => TokenVariant::Else,
            "while" => TokenVariant::While,
            "return" => TokenVariant::Return,
            "const" => TokenVariant::Const,
            "as" => TokenVariant::As,
            "true" => TokenVariant::Literal(Literal::Boolean(true)),
            "false" => TokenVariant::Literal(Literal::Boolean(false)),
            _ => TokenVariant::Identifier(ident),
        };

        Token {
            var: variation,
            span: Span::from(start, end),
        }
    }

    /// Lex an operator.
    fn lex_operator(&mut self) -> Token {
        let (start, first_char) = self.iter.next().expect("This value should be valid");
        let mut end = start.inc();
        let second_char: Option<char> =
            OperatorCombination
                .get(&first_char)
                .map_or(None, |vec: &Box<Vec<char>>| {
                    self.iter.peek().map_or(None, |&(_, ch)| {
                        if vec[..].contains(&ch) {
                            Some(ch)
                        } else {
                            None
                        }
                    })
                });
        if second_char.is_some() {
            self.iter.next();
            end = end.inc();
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
            _ => panic!("Unexpected character \'{}\' at {}", first_char, start),
        };

        Token {
            var: variation,
            span: Span::from(start, end),
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

    /// Unescape a character. The escape sequence follows JSON's rules.
    ///
    /// `iter` should be in the state described below (aka before the first
    /// escaped character):
    ///
    /// ```plaintext
    ///     "\n"
    ///       ^ next
    ///     "\u1234"
    ///       ^ next
    ///     "\u{56789}"
    ///       ^ next
    /// ```
    ///
    /// # Rules
    ///
    /// | Sequence | Meaning |
    /// | -------- | ------------ |
    /// | `\n`     | Line Feed (LF)   |
    /// | `\r`     | Carrige Return (CR) |
    /// | `\t`     | Tab character |
    /// | `\\`     | Backslash (`\`) |
    /// | `\'`     | Signle quote (`'`) |
    /// | `\"`     | Double quote (`"`) |
    /// | `\xNN`   | Character of value `0xNN` |
    /// | `\uNNNN` | Unicode character of value `0xNNNN` |
    /// | `\u{NN...N} | Unicode character of value `0xNN...N` |
    fn unescape_character(iter: &mut Peekable<StringPosIter>) -> char {
        match iter.next().expect("Bad escaped value").1 {
            'n' => '\n',
            't' => '\t',
            'r' => '\r',
            '\\' => '\\',
            '"' => '"',
            '\'' => '\'',

            'x' => unimplemented!("Read two characters in and make it a hexadecimal"),

            'u' => match iter.next().expect("Bad escape value") {
                _ => unimplemented!("Read 4 chars in or until closed curly brace"),
            },

            ch @ _ => panic!("Bad escape: {}", ch),
        }
    }
}

// ======================
/*
TODO: Rewrite tests

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lex_number() {
        let real: Vec<TokenVariant> = LexerInner::new("123+456")
            .into_iter()
            .map(|token| token.var)
            .collect();
        let expected = vec![
            TokenVariant::IntegerLiteral(123.into()),
            TokenVariant::Plus,
            TokenVariant::IntegerLiteral(456.into()),
        ];
        assert_eq!(real, expected);
    }

    #[test]
    fn test_lex_string() {
        let real: Vec<TokenVariant> = LexerInner::new(r#""123+\r\n\t456""#)
            .into_iter()
            .map(|token| token.var)
            .collect();
        let expected = vec![TokenVariant::StringLiteral("123+\r\n\t456".to_owned())];
        assert_eq!(real, expected);
    }

    #[test]
    fn test_lex_ident() {
        let real: Vec<TokenVariant> = LexerInner::new("int x = 3;")
            .into_iter()
            .map(|token| token.var)
            .collect();
        let expected = vec![
            TokenVariant::Identifier("int"),
            TokenVariant::Identifier("x"),
            TokenVariant::Assign,
            TokenVariant::IntegerLiteral(3.into()),
            TokenVariant::Semicolon,
        ];
        assert_eq!(real, expected);
    }

    #[test]
    fn test_lex_ops() {
        use TokenVariant::*;
        let real: Vec<TokenVariant> =
            LexerInner::new("+ - * / ++ -- ! != == >= > <= < & | && || ^ ( ) { } = ,")
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
        let real: Vec<TokenVariant> = LexerInner::new("int x = what?is:this;")
            .into_iter()
            .map(|token| token.var)
            .collect();
    }

    #[test]
    #[should_panic]
    fn test_lexing_panic_string_escape() {
        let real: Vec<TokenVariant> = LexerInner::new(r#""\y""#)
            .into_iter()
            .map(|token| token.var)
            .collect();
    }

    #[test]
    #[should_panic]
    fn test_lexing_panic_string_eol() {
        let real: Vec<TokenVariant> = LexerInner::new(
            r#""
        ""#,
        )
        .into_iter()
        .map(|token| token.var)
        .collect();
    }
}
*/
