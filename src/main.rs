mod err_disp;
mod opt;
use chigusa::c0::lexer;
use failure::Fail;
use opt::{EmitOption, ParserConfig};
use std::fs::*;
use std::io::{Read, Write};
use std::path::PathBuf;
use structopt;
use structopt::StructOpt;

fn main() {
    let mut opt: ParserConfig = ParserConfig::from_args();
    cute_log::init_with_max_level(opt.verbosity).unwrap();

    if opt.output_assembly {
        opt.emit = EmitOption::S0;
    }
    if opt.output_binary {
        opt.emit = EmitOption::O0;
    }

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
        return;
    }

    let tree = chigusa::c0::parser::Parser::new(token).parse();

    let tree = match tree {
        Ok(t) => t,
        Err(e) => {
            let mut input_lines = input.lines();
            let err_des = format!("Parsing error: {}", &e.var);
            let span = e.span;
            err_disp::pretty_print_error(&mut input_lines, span, &err_des);
            std::process::exit(1);
        }
    };

    if opt.emit == EmitOption::Ast {
        write_output(&opt, tree);
        return;
    }

    let s0 = chigusa::minivm::Codegen::new(&tree).compile();
    let s0 = match s0 {
        Ok(t) => t,
        Err(e) => {
            let mut input_lines = input.lines();
            let err_des = format!("Compile error: {}", &e.var);

            if let Some(span) = e.span {
                err_disp::pretty_print_error(&mut input_lines, span, &err_des);
            } else {
                log::error!("{}", err_des);
            }
            std::process::exit(1);
        }
    };

    if opt.emit == EmitOption::S0 {
        let mut f = File::create(&opt.output_file).expect("Failed to create output file");
        write!(f, "{}", s0).expect("Failed to write");
    } else {
        // Emit O0
        let mut f = File::create(&opt.output_file).expect("Failed to create output file");
        s0.write_binary(&mut f).expect("Failed to write");
    }
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
