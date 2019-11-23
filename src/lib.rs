#![feature(try_trait)]

/// C0 is the main library hosting tools to tokenize, generate AST from and
/// compile C0.
pub mod c0;

/// Middle intermediate representation.
///
/// This is where type checking happens. This module compiles AST into MIR and
/// checks types and other constraints.
pub mod mir;

/// Kurumi is a simple virtual machine for this project.
// #[cfg(kurumi)]
pub mod kurumi;

#[cfg(llvm)]
pub mod llvm;

pub mod prelude;
