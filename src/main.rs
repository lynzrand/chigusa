use chigusa::c0::lexer;
use clap;
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

fn main() {
    let opt: ParserConfig = ParserConfig::from_args();
    cute_log::init_with_max_level(opt.verbosity.unwrap_or(log::LevelFilter::Warn)).unwrap();

    let vec = lexer::Lexer::new(Box::new(__INPUT_CODE.chars())).into_iter();

    let tree = chigusa::c0::parser::Parser::new(vec).parse();

    println!("{:?}", tree);
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

    /// Output file. Defaults to stdout.
    #[structopt(short, long = "out", parse(from_os_str))]
    output_file: Option<PathBuf>,

    /// Verbossity. Allowed values are: debug, trace, info, warn, error, off.
    #[structopt(short, long, parse(try_from_str = parse_verbosity))]
    verbosity: Option<log::LevelFilter>,

    /// Write result to stdout. Overwrites `output-file`.
    #[structopt(long)]
    stdout: bool,

    /// Use JIT compilation and run immediately.
    #[structopt(long)]
    jit: bool,
}
