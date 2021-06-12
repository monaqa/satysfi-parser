//! ~/.satysfi/dist/packages 下にあるすべての satyh ファイルをパースする。

use glob::glob;

use anyhow::{anyhow, Context, Result};
use satysfi_parser::CstText;

fn main() -> Result<()> {
    let home_dir = std::env::var("HOME").context("Could not find home directory ($HOME).")?;

    let files = glob(&format!("{}/.satysfi/dist/packages/**/*.satyh", home_dir))?
        .into_iter()
        .chain(glob(&format!(
            "{}/.satysfi/dist/packages/**/*.satyg",
            home_dir
        ))?);

    let mut total = 0;
    let mut successful = 0;
    let mut fail = 0;

    for file_satyh in files {
        total += 1;
        if let Ok(file) = file_satyh {
            let text =
                std::fs::read_to_string(&file).context(anyhow!("No such file: {:?}", file))?;
            let csttext = CstText::parse(&text, satysfi_parser::grammar::program_satyh);
            match csttext {
                Ok(_) => {
                    // println!("Parsing of {file:?} was successful!", file = file);
                    // println!();
                    successful += 1;
                    continue;
                }
                Err((linecol, expect)) => {
                    fail += 1;
                    println!("Parsing of {file:?} failed...", file = file);
                    println!(
                        "  {}:{}:{}",
                        file.to_string_lossy(),
                        linecol.line + 1,
                        linecol.column + 1
                    );
                    println!("  Expected:");
                    for s in expect {
                        println!("  {}", s);
                    }
                    println!();
                }
            }
        }
    }

    println!(
        "{} files found. {} files successfully parsed. {} files failed to parse.",
        total, successful, fail
    );

    Ok(())
}
