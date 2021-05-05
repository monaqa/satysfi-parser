//! ~/.satysfi/dist/packages 下にあるすべての satyh ファイルをパースする。

use glob::glob;
use std::path::PathBuf;

use anyhow::{Context, Result};
use satysfi_parser::CstText;

fn main() -> Result<()> {
    let home_dir = std::env::var("HOME").context("Could not find home directory ($HOME).")?;

    for file_satyh in glob(&format!("{}/.satysfi/dist/packages/**/*.satyh", home_dir))? {
        match file_satyh {
            Ok(file) => {
                let text = std::fs::read_to_string(&file)?;
                let csttext = CstText::parse(&text, satysfi_parser::grammar::program_satyh);
                match csttext {
                    Ok(csttext) => {
                        println!("Parsing of {file:?} was successful!", file = file);
                        println!();
                    }
                    Err((linecol, expect)) => {
                        println!("Parsing of {file:?} failed...", file = file);
                        println!(
                            "  {}:{}:{}",
                            file.to_string_lossy(),
                            linecol.line,
                            linecol.column
                        );
                        println!("  Expected:");
                        for s in expect {
                            println!("  {}", s);
                        }
                        println!();
                    }
                }
            }
            Err(_) => {
                continue;
            }
        }
    }

    Ok(())
}
