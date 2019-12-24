use std::fs::*;
use std::io::{Read, Write};
use std::path::PathBuf;
use structopt;
use structopt::StructOpt;

fn parse_verbosity(input: &str) -> Result<log::LevelFilter, &'static str> {
    match input {
        "info" => Ok(log::LevelFilter::Info),
        "warn" => Ok(log::LevelFilter::Warn),
        "error" => Ok(log::LevelFilter::Error),
        "trace" => Ok(log::LevelFilter::Trace),
        "debug" => Ok(log::LevelFilter::Debug),
        "off" => Ok(log::LevelFilter::Off),
        _ => Err("Bad verbosity level. Allowed values are: debug, trace, info, warn, error, off"),
    }
    // .map(|t| Some(Some(t)))
}

#[derive(StructOpt, Debug)]
#[structopt(
    name = "chigusa",
    about = r"
A compiler that parses c0 grammar and compiles it into o0 binary format.

C0: https://github.com/BUAA-SE-Compiling/c0-handbook
O0: https://github.com/BUAA-SE-Compiling/c0-vm-standards
"
)]
pub struct ParserConfig {
    /// Input file. Defaults to stdin if no file were supplied.
    #[structopt(name = "file", parse(from_os_str))]
    pub input_file: Option<PathBuf>,

    /// Output file.
    #[structopt(short, long = "out", default_value = "out", parse(from_os_str))]
    pub output_file: PathBuf,

    /// Verbossity. Allowed values are: debug, trace, info, warn, error, off.
    #[structopt(short, long, default_value = "warn", parse(try_from_str = parse_verbosity))]
    pub verbosity: log::LevelFilter,

    /// Write result to stdout. Overwrites `output-file`. Only for `token`, `ast` and `s0` targets.
    #[structopt(long)]
    pub stdout: bool,

    // /// Use JIT compilation and run immediately.
    // #[structopt(long)]
    // pub jit: bool,
    /// The type of code to emit. Allowed are: token, ast, s0, o0
    ///
    /// Emit result explanation:
    /// - Token: Direct result from lexer (tokenizer)
    /// - AST: Abstract Syntax Tree, direct result from parser (analyzer)
    /// - s0: C0 assembly file
    /// - o0: C0 binary file
    #[structopt(long, default_value = "o0", parse(try_from_str = EmitOption::parse))]
    pub emit: EmitOption,

    /// Emit C0 assembly file, same as `--emit s0`
    #[structopt(short = "s", long = "s0")]
    pub output_assembly: bool,

    /// Emit C0 binary file, same as `--emit o0`
    #[structopt(short = "c", long = "o0")]
    pub output_binary: bool,
}

#[derive(Debug, Eq, PartialEq)]
pub enum EmitOption {
    Token,
    Ast,
    S0,
    O0,
}

impl EmitOption {
    pub fn parse(s: &str) -> Result<Self, &'static str> {
        match s {
            "token" => Ok(EmitOption::Token),
            "ast" => Ok(EmitOption::Ast),
            "s0" => Ok(EmitOption::S0),
            "o0" => Ok(EmitOption::O0),
            _ => Err("Bad emit option. Allowed are: token, ast, s0, o0"),
        }
    }
}
