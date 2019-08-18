#![feature(try_trait)]

/// C0 is the main library hosting tools to tokenize, generate AST from and
/// compile C0. It compiles C0 code into Kurumi VM Instructions.
pub mod c0;

/// Kurumi is a simple virtual machine for this project.
#[cfg(kurumi)]
pub mod kurumi;
