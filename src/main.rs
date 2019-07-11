use chigusa::c0::ast;
use crossterm;
use crossterm::{cursor, Color, Colored};

fn main() {
    println!(
        "{}Hello, world!",
        crossterm::Colored::Fg(crossterm::Color::Red)
    );
}
