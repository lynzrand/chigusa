use crate::c0::ast::*;
use crate::c0::err::*;
use crate::c0::lexer::Lexer;
use crate::c0::parser::*;
use crate::prelude::*;

fn parse(input: &str) -> ParseResult<Program> {
    let lexer = Lexer::new(input.chars());
    let mut parser = Parser::new(lexer);

    parser.parse()
}

#[test]
fn test_variable_decl() {
    let input = r#"
int x;
int y = 1;
const int a = 1;
double z;
double w = 2.0;
const double b = 1.0;
    "#;

    let res = parse(input);

    assert!(res.is_ok(), format!("{:#?}", res));
}

#[test]
fn test_fn_decl() {
    let input = r#"
int x(){
    return 0;
}

int y(int x){
    return x;
}

void z(){}
void main(int arg){}
    "#;

    let res = parse(input);

    assert!(res.is_ok(), format!("{:#?}", res));
}

#[test]
fn test_exprs() {
    let input = r#"
void main(){
    int a, b = 5, c = 7;
    a = a + b;
    a = a - b;
    a = a * b;
    a = a / b;
    a = a > b;
    a = a < b;
    a = a <= b;
    a = a >= b;
    a = a == b;
    a = a != b;
    a = -b;

    a = a + b * c;
    a = (a + b) * c;

    a = (1 + 2 - 3);
    a = b;
    a = a + b + 1;
    a = -(a + b + 1);
    a = b * -12345 + (2 + 3);
}
    "#;

    let res = parse(input);

    assert!(res.is_ok(), format!("{:#?}", res));
}

#[test]
fn test_wrong_exprs() {
    let inputs = [
        r#"
void main(){
    int a, b = 5;
    a = b +;
}
    "#,
        r#"
void main(){
    int a, b = 5;
    a = b -;
}
    "#,
        r#"
void main(){
    int a, b = 5;
    a = (b;
}
    "#,
        r#"
void main(){
    int a, b = 5;
    a = -;
}
    "#,
        r#"
void main(){
    int a, b = 5;
    a -;
}
    "#,
        r#"
void main(){
    int a, b = 5;
    a = );
}
    "#,
        r#"
void main(){
    int a, b = 5;
    a = *;
}
    "#,
        r#"
void main(){
    int a, b = 5;
    a = !;
}
    "#,
        r#"
void main(){
    int a, b = 5;
    a = b b;
}
    "#,
    ];

    for input in inputs.iter() {
        let res = parse(input);

        assert!(
            res.is_err(),
            format!("'{}' does not result in error!", input)
        );
    }
}

#[test]
fn test_wrong_fn_decls() {
    let inputs = [
        r#"
void main(int){}
    "#,
        r#"
void main(int arg int arg){}
    "#,
        r#"
void (int arg){}
    "#,
        r#"
void print(){}
    "#,
        r#"
void main()
    "#,
        r#"
void main(){
    "#,
        r#"
void main()}
    "#,
    ];

    for input in inputs.iter() {
        let res = parse(input);

        assert!(
            res.is_err(),
            format!("'{}' does not result in error!", input)
        );
    }
}
