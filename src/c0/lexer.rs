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
    IntegerLiteral(ramp::Int),
    StringLiteral(String),
    BooleanLiteral(bool),
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
            IntegerLiteral(num) => write!(f, "Integer({})", num),
            StringLiteral(string) => write!(f, "String(\"{}\")", string),
            BooleanLiteral(b) => write!(f, "Bool(\"{}\")", b),
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
            TokenVariant::Identifier(s) => Ok(s),
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
    lexer: LexerInner,
    iter: Option<Peekable<StringPosIter>>,
}

impl Lexer {
    pub fn new(src: LexerInner) -> Lexer {
        let mut new_iter = Lexer {
            lexer: src,
            iter: None,
        };
        new_iter.iter = Some(StringPosIter::new(new_iter.lexer.src).peekable());
        new_iter
    }
}
pub struct LexerInner {
    // src: Box<dyn Iterator<Item = (Pos, char)>>,
    completed: bool,
}

impl Iterator for Lexer {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        self.lexer.get_next_token(self.iter.as_mut().expect(""))
    }
}

impl LexerInner {
    pub fn new() -> LexerInner {
        Self { completed: false }
    }

    /// **Not Intended For Direct Use.**
    ///
    /// This method requires creating a character index iterator for the `src`
    /// field. Thus it must be used inside an `LexerIterator`.
    pub fn get_next_token(&mut self, iter: &mut Peekable<StringPosIter>) -> Option<Token> {
        Self::skip_spaces(iter);
        // the first character of next token
        let c = match iter.peek() {
            Some((_, '\0')) => return None,
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
    fn lex_number(&mut self, iter: &mut Peekable<StringPosIter>) -> Token {
        let start_pos = iter.peek().expect("This value should be valid").0;
        let number = String::new();

        // radix check
        let radix = if iter.peek().map_or(false, |ch_ind| ch_ind.1 == '0') {
            // this digit is '0'. consume and advance
            iter.next();
            match iter.peek().map_or('_', |i| i.1) {
                'b' => 2,
                'o' => 8,
                'x' => 16,
                '0'..='9' => 10,
                _ => {
                    // This does not match any number format, return zero
                    return Token {
                        var: TokenVariant::IntegerLiteral(ramp::Int::zero()),
                        // src: &self.src[start_pos.index..start_pos.index + 1],
                        span: Span::from(start_pos, start_pos.bump()),
                    };
                }
            }
        } else {
            10
        };

        while iter.peek().map_or(false, |ch_ind| (*ch_ind).1.is_digit(10)) {
            number.push(iter.next().unwrap().1);
        }
        let end_pos = iter.peek().unwrap().0;

        let start = start_pos.index;
        let end = end_pos.index;

        Token {
            var: TokenVariant::IntegerLiteral(
                ramp::Int::from_str_radix(&self.src[start..end], radix).unwrap(),
            ),
            // src: &self.src[start..end],
            span: Span::from(start_pos, end_pos),
        }
    }

    /// Lex a string literal.
    fn lex_string_literal(&mut self, iter: &mut Peekable<StringPosIter>) -> Token {
        let (start, start_quotation_mark) = iter.next().expect("This value should be valid");

        if start_quotation_mark != '"' {
            panic!(
                "A string literal must start with a quotation mark! found at {}",
                start
            );
        }

        let end: Pos;
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
            var: TokenVariant::StringLiteral(tgt_string),
            span: Span::from(start, end),
        }
    }

    /// Lex an identifier.
    fn lex_identifier(&mut self, iter: &mut Peekable<StringPosIter>) -> Token {
        let start = iter.peek().expect("This value should be valid").0;
        while iter
            .peek()
            .map_or(false, |ch_ind| (*ch_ind).1.is_alphanumeric())
        {
            iter.next();
        }
        let end = iter.peek().unwrap().0;
        let variation = match &self.src[start.index..end.index] {
            "if" => TokenVariant::If,
            "else" => TokenVariant::Else,
            "while" => TokenVariant::While,
            "return" => TokenVariant::Return,
            "const" => TokenVariant::Const,
            "as" => TokenVariant::As,
            "true" => TokenVariant::BooleanLiteral(true),
            "false" => TokenVariant::BooleanLiteral(false),
            _ => TokenVariant::Identifier(&self.src[start.index..end.index]),
        };

        Token {
            var: variation,
            span: Span::from(start, end),
        }
    }

    /// Lex an operator.
    fn lex_operator(&mut self, iter: &mut Peekable<StringPosIter>) -> Token {
        let (start, first_char) = iter.next().expect("This value should be valid");
        let mut end = start.inc();
        let second_char: Option<char> =
            OperatorCombination
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
            _ => panic!(
                "Unexpected character \'{}\' at {}",
                &self.src[start.index..end.index],
                start
            ),
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
}

impl IntoIterator for LexerInner {
    type Item = Token;
    type IntoIter = Lexer;
    fn into_iter(self) -> Self::IntoIter {
        Lexer::new(self)
    }
}

// ======================

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
