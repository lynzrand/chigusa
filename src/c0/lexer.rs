use super::err::*;
use crate::prelude::*;
use once_cell::sync::*;
use ramp;
use std::collections::HashMap;
use std::iter::{Iterator, Peekable};
use std::str::{Chars, FromStr};
use std::{
    cell::RefCell,
    convert::{Into, TryInto},
    fmt,
    fmt::Display,
    fmt::Formatter,
    hash::Hash,
    rc::Rc,
    string::String,
};

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
/// This enum defines the variants of token in C0 language. Variants are pretty
/// self-explanatory.
pub enum TokenType {
    // Keywords
    Const,
    As,
    If,
    Else,
    While,
    Break,
    Continue,
    Return,
    Print,
    Scan,

    // Operators
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
    LParenthesis,
    RParenthesis,
    LBracket,
    RBracket,
    LCurlyBrace,
    RCurlyBrace,
    Assign,
    Comma,
    Dot,

    // Identifier
    Identifier(String),
    Literal(Literal),

    // Comment, will be discarded before handed out
    Comment(String),

    // Special
    EndOfFile,
    Dummy,
    Error(LexError),
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use self::TokenType::*;
        match self {
            Const => write!(f, "Const"),
            As => write!(f, "As"),
            If => write!(f, "If"),
            Else => write!(f, "Else"),
            While => write!(f, "While"),
            Break => write!(f, "Break"),
            Continue => write!(f, "Continue"),
            Return => write!(f, "Return"),
            Print => write!(f, "Print"),
            Scan => write!(f, "Scan"),

            Semicolon => write!(f, "';'"),
            Minus => write!(f, "'-'"),
            Plus => write!(f, "'+'"),
            Multiply => write!(f, "'*'"),
            Divide => write!(f, "'/'"),
            Not => write!(f, "'!'"),
            BinaryAnd => write!(f, "'&'"),
            BinaryOr => write!(f, "'|'"),
            And => write!(f, "'&&'"),
            Or => write!(f, "'||'"),
            Xor => write!(f, "'^'"),
            Increase => write!(f, "'++'"),
            Decrease => write!(f, "'--'"),
            Equals => write!(f, "'=='"),
            NotEquals => write!(f, "'!='"),
            LessThan => write!(f, "'<'"),
            LessOrEqualThan => write!(f, "'<='"),
            GreaterThan => write!(f, "'>'"),
            GreaterOrEqualThan => write!(f, "'>='"),
            LParenthesis => write!(f, "'('"),
            RParenthesis => write!(f, "')'"),
            LBracket => write!(f, "'['"),
            RBracket => write!(f, "']'"),
            LCurlyBrace => write!(f, "'{{'"),
            RCurlyBrace => write!(f, "'}}'"),
            Assign => write!(f, "'='"),
            Comma => write!(f, "','"),
            Dot => write!(f, "'.'"),

            Identifier(ident) => write!(f, "Identifier(\"{}\")", ident),
            Literal(b) => write!(f, "Literal({})", b),

            Comment(s) => write!(f, "Comment({})", s),

            EndOfFile => write!(f, "#EOF"),
            Dummy => write!(f, "<dummy>"),
            Error(reason) => write!(f, "<Error token>(Reason: {:?})", reason),
            // _ => write!(f, "???"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum Literal {
    Char(char),
    String(String),
    Boolean(bool),
    Integer(ramp::Int),
    Float(ramp::rational::Rational),
    _Dummy,
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use self::Literal::*;
        match self {
            Char(c) => write!(f, "Char({})", c.escape_debug()),
            String(s) => write!(f, "String(\"{}\")", s.escape_debug()),
            Boolean(b) => write!(f, "Boolean({})", b),
            Integer(i) => write!(f, "Integer({})", i),
            Float(i) => write!(f, "Float({})", i),
            _Dummy => Ok(()),
        }
    }
}

/// A single token
#[derive(Debug, Clone)]
pub struct Token {
    /// Its variant
    pub var: TokenType,

    /// The space the token occupies
    pub span: Span,
}

impl Token {
    pub fn get_ident(&self) -> Result<&str, ()> {
        match &self.var {
            TokenType::Identifier(s) => Ok(&s),
            _ => Err(()),
        }
    }

    pub fn dummy() -> Token {
        Token {
            var: TokenType::Dummy,
            span: Span::zero(),
        }
    }

    pub fn eof() -> Token {
        Token {
            var: TokenType::EndOfFile,
            span: Span::zero(),
        }
    }

    pub fn is_err(&self) -> bool {
        match self.var {
            TokenType::Error(_) => true,
            _ => false,
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
        ('/', Box::new(vec!['/', '*'])),
    ]
    .iter()
    .cloned()
    .collect()
});

pub struct StringPosIter<T>
where
    T: Iterator<Item = char>,
{
    chars: std::iter::Chain<T, std::iter::Once<char>>,
    pos: Pos,
    is_last_cr: bool,
}

impl<T> StringPosIter<T>
where
    T: Iterator<Item = char>,
{
    pub fn new(src: T) -> StringPosIter<T> {
        let chars = src.chain(std::iter::once('\0'));
        StringPosIter {
            chars,
            pos: Pos::zero(),
            is_last_cr: false,
        }
    }
}

impl<T> Iterator for StringPosIter<T>
where
    T: Iterator<Item = char>,
{
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

pub struct Lexer<T>
where
    T: Iterator<Item = char>,
{
    iter: Peekable<StringPosIter<T>>,
    err: Option<Vec<super::err::ParseError>>,
}

impl<T> Iterator for Lexer<T>
where
    T: Iterator<Item = char>,
{
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        loop {
            let tok = self.get_next_token();
            if let Some(Token {
                var: TokenType::Comment(..),
                ..
            }) = tok
            {
            } else {
                break tok;
            }
        }
    }
}

impl<T> Lexer<T>
where
    T: Iterator<Item = char>,
{
    pub fn new(iter: T) -> Lexer<T> {
        Lexer {
            iter: StringPosIter::new(iter).peekable(),
            err: None,
        }
    }

    pub fn get_next_token(&mut self) -> Option<Token> {
        Self::skip_spaces(&mut self.iter);
        // the first character of next token
        let (pos, c) = match self.iter.peek() {
            Some((_, '\0')) => return None,
            Some((pos, c)) => (*pos, *c),
            // spaces may occur at the end of file
            None => return None,
        };

        let tok = match c {
            '0'..='9' => self.lex_number(),
            'a'..='z' | 'A'..='Z' | '_' => self.lex_identifier(),
            '\"' => self.lex_string_literal(),
            '\'' => self.lex_char_literal(),
            '+' | '-' | '*' | '/' | '<' | '>' | '=' | '!' | '|' | '&' | '^' | '(' | ')' | '['
            | ']' | '{' | '}' | ',' | ';' => self.lex_operator(),
            // TODO: Add to errors and skip this line
            c @ _ => Err(LexError::UnexpectedCharacter(c)),
        };

        let tok = match tok {
            Ok(t) => t,
            Err(e) => {
                let end_pos = self.skip_error();
                Token {
                    var: TokenType::Error(e),
                    span: Span::from(pos, end_pos),
                }
            }
        };

        Some(tok)
    }

    fn skip_error(&mut self) -> Pos {
        let start_pos = self
            .iter
            .peek()
            .map(|x| *x)
            .or(Some((
                Pos {
                    ln: usize::max_value(),
                    pos: usize::max_value(),
                    index: usize::max_value(),
                },
                '\0',
            )))
            .unwrap()
            .0;
        let mut end_pos = start_pos;
        while self.iter.peek().map_or(false, |x| !x.1.is_whitespace()) {
            self.iter.next();
            end_pos = end_pos.inc();
        }
        end_pos
    }

    fn lex_int(&mut self, base: u8) -> Result<ramp::Int, ramp::int::ParseIntError> {
        let mut number = String::new();

        while self
            .iter
            .peek()
            .map_or(false, |ch_ind| (*ch_ind).1.is_digit(10))
        {
            number.push(self.iter.next().unwrap().1);
        }

        return ramp::Int::from_str_radix(&number, base);
    }

    /// Lex a number
    fn lex_number(&mut self) -> LexResult<Token> {
        let start_pos = self.iter.peek().expect("This value should be valid").0;

        // radix check.
        let (radix, possibly_double) = if self.iter.peek().map_or(false, |ch_ind| ch_ind.1 == '0') {
            // this digit is '0'. consume and advance
            self.iter.next();
            match self.iter.peek().map_or('_', |i| i.1) {
                'b' => {
                    self.iter.next();
                    (2, false)
                }
                '0' | 'o' => {
                    self.iter.next();
                    (8, false)
                }
                'x' => {
                    self.iter.next();
                    (16, false)
                }
                '1'..='9' => (10, true),
                _ => (10, true),
            }
        } else {
            (10, true)
        };

        let mut number = String::from_str("0").unwrap();
        let mut is_float = false;

        while self
            .iter
            .peek()
            .map_or(false, |ch_ind| (*ch_ind).1.is_digit(radix))
        {
            number.push(self.iter.next().unwrap().1);
        }

        // original * 10 ^ exponent
        let mut exponent = 0;

        // Decimal part
        if possibly_double && self.iter.peek().map_or(false, |x| x.1 == '.') {
            // Consume decimal point
            self.iter.next();
            is_float = true;

            while self
                .iter
                .peek()
                .map_or(false, |ch_ind| (*ch_ind).1.is_digit(10))
            {
                exponent -= 1;
                number.push(self.iter.next().unwrap().1);
            }
        }

        // Exponent part

        if possibly_double && self.iter.peek().map_or(false, |x| x.1 == 'e' || x.1 == 'E') {
            // Consume exponent `e`
            self.iter.next();
            let mut exp = String::new();
            is_float = true;

            match self.iter.peek() {
                Some((_, '+')) | Some((_, '-')) => exp.push(self.iter.next().unwrap().1),
                _ => (),
            };

            while self
                .iter
                .peek()
                .map_or(false, |ch_ind| (*ch_ind).1.is_digit(10))
            {
                exp.push(self.iter.next().unwrap().1);
            }

            let exp = match i32::from_str(&exp) {
                Ok(i) => i,
                Err(e) => Err(LexError::BadInteger)?,
            };

            exponent += exp;
        }

        let number = match ramp::Int::from_str_radix(&number, radix as u8) {
            Ok(i) => i,
            Err(e) => Err(LexError::BadInteger)?,
        };

        if is_float {
            let (number, denominator) = if exponent >= 0 {
                let exp = ramp::Int::from(10).pow(exponent as usize);
                (number * exp, ramp::Int::one())
            } else {
                let exp = ramp::Int::from(10).pow((-exponent) as usize);
                (number, exp)
            };

            let end_pos = self.iter.peek().unwrap().0;

            Ok(Token {
                var: TokenType::Literal(Literal::Float(ramp::rational::Rational::new(
                    number,
                    denominator,
                ))),
                // src: &self.src[start..end],
                span: Span::from(start_pos, end_pos),
            })
        } else {
            let end_pos = self.iter.peek().unwrap().0;

            Ok(Token {
                var: TokenType::Literal(Literal::Integer(number)),
                // src: &self.src[start..end],
                span: Span::from(start_pos, end_pos),
            })
        }
    }

    fn lex_char_literal(&mut self) -> LexResult<Token> {
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
            '\\' => Self::unescape_character(&mut self.iter)?,
            ch @ _ => ch,
        };

        let (end, end_quote) = self.iter.next().expect("Should be valid");
        if end_quote != '\'' {
            return Err(LexError::UnexpectedCharacter(end_quote));
        }

        Ok(Token {
            var: TokenType::Literal(Literal::Char(ch)),
            span: Span::from(start, end),
        })
    }

    /// Lex a string literal.
    fn lex_string_literal(&mut self) -> LexResult<Token> {
        let (start, start_quotation_mark) = self.iter.next().expect("This value should be valid");

        if start_quotation_mark != '"' {
            return Err(LexError::MalformedString);
        }

        let end: Pos;
        let mut tgt_string = String::new();

        loop {
            let (this_index, this_char) = self.iter.next().ok_or(LexError::UnexpectedEOF)?;
            match this_char {
                '\\' => tgt_string.push(Self::unescape_character(&mut self.iter)?),

                '"' => {
                    end = this_index.map_inc(-1, 0, -1);
                    break;
                }

                '\n' | '\r' => Err(LexError::UnexpectedEOL)?,

                _ => tgt_string.push(this_char),
            }
        }

        Ok(Token {
            var: TokenType::Literal(Literal::String(tgt_string)),
            span: Span::from(start, end),
        })
    }

    /// Lex an identifier.
    fn lex_identifier(&mut self) -> LexResult<Token> {
        let start = self.iter.peek().expect("This value should be valid").0;
        let mut ident = String::new();
        while self.iter.peek().map_or(false, |ch_ind| {
            ch_ind.1.is_alphanumeric() || ch_ind.1 == '_'
        }) {
            ident.push(self.iter.next().unwrap().1);
        }
        let end = self.iter.peek().unwrap().0;
        let variation = match &ident[..] {
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "while" => TokenType::While,
            "break" => TokenType::Break,
            "continue" => TokenType::Continue,
            "return" => TokenType::Return,
            "const" => TokenType::Const,
            "print" => TokenType::Print,
            "scan" => TokenType::Scan,
            "as" => TokenType::As,
            "true" => TokenType::Literal(Literal::Boolean(true)),
            "false" => TokenType::Literal(Literal::Boolean(false)),

            "struct" | "switch" | "case" | "default" | "for" | "do" => {
                Err(LexError::ReservedWord(ident))?
            }

            _ => TokenType::Identifier(ident),
        };

        Ok(Token {
            var: variation,
            span: Span::from(start, end),
        })
    }

    /// Lex an operator.
    fn lex_operator(&mut self) -> LexResult<Token> {
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
                None => TokenType::Plus,
                Some('+') => TokenType::Increase,
                _ => unreachable!(),
            },
            '-' => match second_char {
                None => TokenType::Minus,
                Some('-') => TokenType::Decrease,
                _ => unreachable!(),
            },
            '*' => TokenType::Multiply,
            '/' => match second_char {
                None => TokenType::Divide,
                Some('*') => self.lex_comments(true)?,
                Some('/') => self.lex_comments(false)?,
                _ => unreachable!(),
            },
            '=' => match second_char {
                None => TokenType::Assign,
                Some('=') => TokenType::Equals,
                _ => unreachable!(),
            },
            '<' => match second_char {
                None => TokenType::LessThan,
                Some('=') => TokenType::LessOrEqualThan,
                _ => unreachable!(),
            },
            '>' => match second_char {
                None => TokenType::GreaterThan,
                Some('=') => TokenType::GreaterOrEqualThan,
                _ => unreachable!(),
            },
            '!' => match second_char {
                None => TokenType::Not,
                Some('=') => TokenType::NotEquals,
                _ => unreachable!(),
            },
            '|' => match second_char {
                None => TokenType::BinaryOr,
                Some('|') => TokenType::Or,
                _ => unreachable!(),
            },
            '&' => match second_char {
                None => TokenType::BinaryAnd,
                Some('&') => TokenType::And,
                _ => unreachable!(),
            },
            '^' => TokenType::Xor,
            '(' => TokenType::LParenthesis,
            ')' => TokenType::RParenthesis,
            '[' => TokenType::LBracket,
            ']' => TokenType::RBracket,
            '{' => TokenType::LCurlyBrace,
            '}' => TokenType::RCurlyBrace,
            ',' => TokenType::Comma,
            '.' => TokenType::Dot,
            ';' => TokenType::Semicolon,
            _ => panic!("Unexpected character \'{}\' at {}", first_char, start),
        };

        Ok(Token {
            var: variation,
            span: Span::from(start, end),
        })
    }

    fn lex_comments(&mut self, multiline: bool) -> LexResult<TokenType> {
        let mut comment_data = String::new();
        if multiline {
            loop {
                let c = self.iter.next();
                match c {
                    Some((_, '*')) => match self.iter.peek() {
                        Some((_, '/')) => {
                            self.iter.next();
                            break;
                        }
                        _ => comment_data.push('*'),
                    },
                    None => Err(LexError::UnexpectedEOF)?,
                    Some((_, c)) => comment_data.push(c),
                }
            }
        } else {
            loop {
                let c = self.iter.next();
                match c {
                    Some((_, '\r')) | Some((_, '\n')) | Some((_, '\0')) => {
                        self.iter.next();
                        break;
                    }
                    None => break,
                    Some((_, c)) => comment_data.push(c),
                }
            }
        }
        Ok(TokenType::Comment(comment_data))
    }

    /// Skip spaces and stop before the next non-space character.
    fn skip_spaces(iter: &mut Peekable<StringPosIter<T>>) {
        while match iter.peek() {
            None => false,
            Some((_, '\0')) => false,
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
    fn unescape_character(iter: &mut Peekable<StringPosIter<T>>) -> LexResult<char> {
        // TODO: Return a result so we can continue to parse
        Ok(match iter.next().ok_or(LexError::BadEscaping)?.1 {
            'n' => '\n',
            't' => '\t',
            'r' => '\r',
            '\\' => '\\',
            '"' => '"',
            '\'' => '\'',

            'x' => {
                let x: String = iter.take(2).map(|x| x.1).collect();
                let x = u8::from_str_radix(&x, 16).map_err(|_| LexError::BadEscaping)?;
                x as char
            }

            'u' => match iter.peek().ok_or(LexError::BadEscaping)?.1 {
                '{' => {
                    iter.next();
                    let x: String = iter.take_while(|x| x.1 != '}').map(|x| x.1).collect();
                    let x = u32::from_str_radix(&x, 16).map_err(|_| LexError::BadEscaping)?;
                    x.try_into().map_err(|_| LexError::BadEscaping)?
                }
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    let x: String = iter.take(4).map(|x| x.1).collect();
                    let x = u32::from_str_radix(&x, 16).map_err(|_| LexError::BadEscaping)?;
                    x.try_into().map_err(|_| LexError::BadEscaping)?
                }
                _ => Err(LexError::BadEscaping)?,
            },

            ch @ _ => Err(LexError::BadEscaping)?,
        })
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
