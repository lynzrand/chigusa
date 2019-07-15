use chigusa::c0::{ast, lexer};
use crossterm;
use crossterm::{cursor, Color, Colored};

const __INPUT_CODE: &'static str = r"
int x, y;
const k = 3;
int main(){ printf(x); }
";

fn main() {
    let vec: Vec<ast::Token> = lexer::Lexer::new(__INPUT_CODE).into_iter().collect();
    print!("{:#?}", vec);
}
