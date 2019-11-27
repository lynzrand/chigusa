use chigusa::c0::lexer;
use crossterm;
// use crossterm::{cursor, Color, Colored};

const __INPUT_CODE: &'static str = r#"
int x, y;
const k = 3;
int main(){ printf("aaa", x); }
"#;

fn main() {
    let vec: Vec<lexer::Token> = lexer::Lexer::new(Box::new(__INPUT_CODE.chars()))
        .into_iter()
        .collect();
    print!("{:#?}", vec);
}
