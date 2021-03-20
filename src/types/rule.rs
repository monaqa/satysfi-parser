use peg::str::LineCol;

use super::Cst;
use crate::grammar::satysfi_parser;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[allow(non_camel_case_types)]
pub enum Rule {
    constant,
    const_unit,
    const_bool,
    const_int,
    const_float,
    const_length,
    const_string,
}

impl Rule {
    pub fn parse(&self, text: &str) -> Result<Cst, peg::error::ParseError<LineCol>> {
        match self {
            Rule::constant => satysfi_parser::term_const(&text),
            Rule::const_unit => satysfi_parser::const_unit(&text),
            Rule::const_bool => satysfi_parser::const_bool(&text),
            Rule::const_int => satysfi_parser::const_int(&text),
            Rule::const_float => satysfi_parser::const_float(&text),
            Rule::const_length => satysfi_parser::const_length(&text),
            Rule::const_string => satysfi_parser::const_string(&text),
        }
    }
}
