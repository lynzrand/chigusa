use chigusa::c0::err::ParseError;
use chigusa::prelude::Span;
use std::fs::*;
use std::io::*;
use std::path::PathBuf;

/// Lines to display around error line
const ERR_CONTEXT_LINES: usize = 3;

pub fn pretty_print_error(
    lines: &mut impl Iterator<Item = impl Into<String>>,
    span: Span,
    err_desc: &str,
) {
    let start_line = span.start.ln.saturating_sub(ERR_CONTEXT_LINES);
    let end_line = span.end.ln.saturating_add(ERR_CONTEXT_LINES);
    let take = end_line - start_line;

    let lines = lines.zip(0..).skip(start_line).take(take);

    for line in lines {
        let (line, ln) = line;
        let line: String = line.into();
        let is_err_line = ln >= span.start.ln && ln <= span.end.ln;
        let err_sign = if is_err_line { '>' } else { ' ' };

        println!("{}{:>5} | {}", err_sign, ln + 1, line);
        if is_err_line {
            print!("{:>6} | ", ' ');
            if ln == span.start.ln {
                if ln == span.end.ln {
                    let sign_len = span.end.pos - span.start.pos;
                    println!(
                        "{:prec_space$}{:^^sign_len$}",
                        ' ',
                        '^',
                        prec_space = span.start.pos,
                        sign_len = sign_len
                    );
                } else {
                    let sign_len = line.len() - span.start.pos;
                    println!(
                        "{:prec_space$}{:^^sign_len$}",
                        ' ',
                        '^',
                        prec_space = span.start.pos,
                        sign_len = sign_len
                    );
                }
            } else if ln == span.end.ln {
                let sign_len = span.end.pos;
                println!("{:^^sign_len$}", '^', sign_len = sign_len);
            } else {
                println!("{:^^sign_len$}", '^', sign_len = line.len());
            }
        }
    }

    println!("{}", err_desc);
}
