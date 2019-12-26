use crate::c0::err::*;
use crate::c0::lexer::*;
use crate::prelude::*;

#[test]
fn test_lex_valid_ints() {
    let src = r#"
0
0x01
0xabcd
    "#;

    let mut lexer = Lexer::new(src.chars());

    assert!(lexer.all(|token| {
        if let TokenType::Literal(Literal::Integer(_)) = token.var {
            true
        } else {
            false
        }
    }));
}

#[test]
fn test_lex_valid_floats() {
    let src = r#"
0.1
0.1e10
1e10
    "#;

    let mut lexer = Lexer::new(src.chars());

    assert!(lexer.all(|token| {
        if let TokenType::Literal(Literal::Float(_)) = token.var {
            true
        } else {
            false
        }
    }));
}

#[test]
fn test_lex_valid_strings() {
    let src = r#"
"hello, world"
"Hello\nworld"
""
"Hello\x32world"
"Hello\u1234world"
    "#;

    let lexer = Lexer::new(src.chars());

    lexer.for_each(|token| {
        if let TokenType::Literal(Literal::String(_)) = &token.var {
            ()
        } else {
            panic!("{:?}", token)
        }
    });
}

#[test]
fn test_lex_tokens() {
    let src = r#"
int
alt
i
i9
test
    "#;

    let lexer = Lexer::new(src.chars());

    lexer.for_each(|token| {
        dbg!(&token);
        if let TokenType::Identifier(_) = &token.var {
            ()
        } else {
            panic!("{:?}", token)
        }
    });
}

#[test]
fn test_lex_keywords() {
    let src = r#"
const
as
if
else
while
break
continue
return
print
scan
    "#;

    let lexer = Lexer::new(src.chars());

    let vars: Vec<_> = lexer.map(|token| token.var).collect();

    use TokenType::*;
    let expected = [
        Const, As, If, Else, While, Break, Continue, Return, Print, Scan,
    ];
    assert_eq!(vars, expected);
}

#[test]
fn test_lex_ops() {
    let src = r#"
; - + * / ! & | && || ^ ++ -- == != < <= > >= ( ) [ ] { } = ,
    "#;

    let lexer = Lexer::new(src.chars());

    let vars: Vec<_> = lexer.map(|token| token.var).collect();

    use TokenType::*;
    let expected = [
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
    ];
    assert_eq!(vars, expected);
}

#[test]
fn test_lex_err_chars() {
    let src = r#"@
#
$
%
`
~
\
:
?"#;

    let lines = src.lines();
    for line in lines {
        let chars = line.chars();
        let result = Lexer::new(chars).next().unwrap();

        assert!(
            result.is_err(),
            format!("token {:?} in '{}' does not result in error!", result, line)
        );
    }
}

#[test]
fn test_lex_err_strings() {
    let src = r#""\u"
    "\u123"
    "\x"
    "\xa"
    "\xz"
    "\xaz"
    "\xzx"
    "\w"
    "\z"
"#;

    let lines = src.lines();
    for line in lines {
        let chars = line.chars();
        let result = Lexer::new(chars).next().unwrap();

        assert!(
            result.is_err(),
            format!("token {:?} in '{}' does not result in error!", result, line)
        );
    }
}
