// #![feature(try_trait)]
#![allow(dead_code)]

/// C0 is the main library hosting tools to tokenize, generate AST from and
/// compile C0.
pub mod c0;

pub mod minivm;

/// Essential stuff
pub mod prelude;

/// Stuff for binary program
pub(crate) mod opt;

pub mod arm;

pub mod mir;
pub mod stdlib;
// #[cfg(test)]
// mod tests;
