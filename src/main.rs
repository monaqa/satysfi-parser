use std::path::PathBuf;

use anyhow::Result;

use peg::str::LineCol;
use satysfi_parser::grammar;
use satysfi_parser::CstText;

use structopt::StructOpt;

#[derive(Debug, Clone, StructOpt)]
struct Opts {
    input: PathBuf,
}

fn main() -> Result<()> {
    let opts = Opts::from_args_safe()?;
    let text = std::fs::read_to_string(&opts.input)?;

    let res = CstText::parse(&text, grammar::program);

    match res {
        Ok(csttext) => {
            print!("{}", csttext);
        }
        Err(err) => {
            let filename = opts.input.to_string_lossy();
            let LineCol { line, column, .. } = err.location;
            let expected = err.expected;
            eprintln!("[Parse Error] {}:{}:{}", filename, line, column);
            eprintln!("Expected: {:?}", expected);
        }
    }

    Ok(())
}
