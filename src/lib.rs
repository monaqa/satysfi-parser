#[macro_use]
extern crate pest_derive;

#[allow(missing_docs)]
mod pest_satysfi_parser {
    #[derive(Parser)]
    #[grammar = "satysfi.pest"]
    pub struct SatysfiParser;
}

pub use pest_satysfi_parser::{Rule, SatysfiParser};

/// CalculatorParser で用いられる Pair.
pub type Pair<'i> = pest::iterators::Pair<'i, Rule>;

#[cfg(test)]
mod tests;
