use chigusa::c0::lexer;
use clap;
use std::fs::*;
use std::io::{Read, Write};
use std::path::PathBuf;
use structopt;
use structopt::StructOpt;
// use crossterm::{cursor, Color, Colored};

const __INPUT_CODE: &'static str = r#"
int x = 1, y = x + 1;
double c = 12.34666666666666666666666666666666e+50;
int z;
const int k = 3 * y + x * 5 - 8 * &x++;
int main(){ if (c > 0) print("aaa", x); else {int z = 2; print(z);} }
"#;

fn main() -> Result<(), MainError> {
    let opt: ParserConfig = ParserConfig::from_args();
    cute_log::init_with_max_level(opt.verbosity).unwrap();

    let mut input = String::new();
    if let Some(f) = &opt.input_file {
        std::fs::File::open(f)
            .expect("File does not exist!")
            .read_to_string(&mut input)
            .expect("Failed to read");
    } else {
        std::io::stdin()
            .read_to_string(&mut input)
            .expect("Failed to read");
    };

    let token = lexer::Lexer::new(Box::new(input.chars())).into_iter();

    if opt.emit == EmitOption::Token {
        let tokens: Vec<_> = token.collect();
        write_output(&opt, tokens);
        return Ok(());
    }

    let tree = chigusa::c0::parser::Parser::new(token).parse()?;

    if opt.emit == EmitOption::Ast {
        write_output(&opt, tree);
        return Ok(());
    }

    let s0 = chigusa::minivm::Codegen::new(&tree).compile()?;

    if opt.emit == EmitOption::S0 {
        write_output(&opt, s0);
    } else {
        // Emit O0
        let mut f = File::create(&opt.output_file).expect("Failed to create output file");
        s0.write_binary(&mut f).expect("Failed to write");
    }
    Ok(())
}

fn write_output<T>(opt: &ParserConfig, val: T)
where
    T: std::fmt::Debug,
{
    if opt.stdout {
        print!("{:?}", val);
    } else {
        let mut f = File::create(&opt.output_file).expect("Failed to create output file");
        write!(f, "{:#?}", val).expect("Failed to write file");
    }
}

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
#[structopt(name = "chigusa")]
pub struct ParserConfig {
    /// Input file. Defaults to stdin if no file were supplied.
    #[structopt(name = "file", parse(from_os_str))]
    input_file: Option<PathBuf>,

    /// Output file.
    #[structopt(short, long = "out", default_value = "a.out", parse(from_os_str))]
    output_file: PathBuf,

    /// Verbossity. Allowed values are: debug, trace, info, warn, error, off.
    #[structopt(short, long, default_value = "warn", parse(try_from_str = parse_verbosity))]
    verbosity: log::LevelFilter,

    /// Write result to stdout. Overwrites `output-file`.
    #[structopt(long)]
    stdout: bool,

    /// Use JIT compilation and run immediately.
    #[structopt(long)]
    jit: bool,

    /// The type of code to emit. Allowed are: token, ast, s0, o0
    ///
    /// Emit result explanation:
    /// - Token: Direct result from lexer (tokenizer)
    /// - AST: Abstract Syntax Tree, direct result from parser (analyzer)
    #[structopt(long, default_value = "o0", parse(try_from_str = EmitOption::parse))]
    emit: EmitOption,
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

#[derive(Debug)]
enum MainError {
    Parsing(chigusa::c0::err::ParseError),
    Compiling(chigusa::minivm::err::CompileError),
}

impl From<chigusa::c0::err::ParseError> for MainError {
    fn from(p: chigusa::c0::err::ParseError) -> Self {
        MainError::Parsing(p)
    }
}

impl From<chigusa::minivm::err::CompileError> for MainError {
    fn from(p: chigusa::minivm::err::CompileError) -> Self {
        MainError::Compiling(p)
    }
}

impl std::fmt::Display for MainError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            MainError::Parsing(p) => write!(f, "Parse error: {:#?}", p),
            MainError::Compiling(p) => write!(f, "Compile error: {:#?}", p),
        }
    }
}
