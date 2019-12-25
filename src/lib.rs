// #![feature(try_trait)]
#![allow(dead_code)]

/// C0 is the main library hosting tools to tokenize, generate AST from and
/// compile C0.
pub mod c0;

pub mod minivm;

/// Kurumi is a simple virtual machine for this project.
// #[cfg(kurumi)]
// pub mod kurumi;

#[cfg(cranelift_codegen)]
/// x86 codegen using Cranelift
pub mod cranelift;

/// Essencial stuff
pub mod prelude;

/// Stuff for binary program
pub(crate) mod opt;

#[cfg(test)]
mod tests;
