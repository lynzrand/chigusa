use chigusa::c0::lexer;
use crossterm;

// use crossterm::{cursor, Color, Colored};

const __INPUT_CODE: &'static str = r#"
int x = 1, y = x + 1;
double c = 12.34666666666666666666666666666666e+50;
int z;
const int k = 3 * y + x * 5 - 8 * &x++;
int main(){ if (c > 0) print("aaa", x); else {int z = 2; print(z);} }
"#;

fn main() {
    cute_log::init().unwrap();
    let vec: Vec<lexer::Token> = lexer::Lexer::new(Box::new(__INPUT_CODE.chars()))
        .into_iter()
        .collect();
    // println!("{:#?}", vec);
    let tree = chigusa::c0::parser::Parser::new(vec.iter().map(|x| x.clone())).parse();
    println!("{:#?}", tree);
}
