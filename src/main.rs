use std::path::PathBuf;

use anyhow::Result;

use peg::str::LineCol;
use satysfi_parser::grammar;
use satysfi_parser::CstText;

use structopt::StructOpt;

#[derive(Debug, Clone, StructOpt)]
struct Opts {
    input: PathBuf,
    #[structopt(short, long)]
    line: Option<usize>,
    #[structopt(short, long)]
    column: Option<usize>,
}

fn main() -> Result<()> {
    let opts = Opts::from_args_safe()?;
    let text = std::fs::read_to_string(&opts.input)?;

    let res = CstText::parse(&text, grammar::program);

    match res {
        Ok(csttext) => {
            if let (Some(line), Some(column)) = (opts.line, opts.column) {
                let pos = csttext
                    .from_line_col(line - 1, column - 1)
                    .expect("invalid line/column number.");
                let csts = csttext.cst.dig(pos);
                for cst in csts.iter().rev() {
                    let text = csttext.pritty_cst(cst);
                    println!("{}", text);
                }
            } else {
                println!("{}", csttext);
            }
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
