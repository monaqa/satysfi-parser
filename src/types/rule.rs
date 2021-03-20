use peg::str::LineCol;

pub use self::Constant::*;
pub use self::Rule::*;
pub use self::Term::*;

use super::Cst;
use crate::grammar::satysfi_parser;

pub trait Parser {
    fn parse(&self, text: &str) -> Result<Cst, peg::error::ParseError<LineCol>>;
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[allow(non_camel_case_types)]
pub enum Rule {
    term(Term),
    constant(Constant),
}

impl Parser for Rule {
    fn parse(&self, text: &str) -> Result<Cst, peg::error::ParseError<LineCol>> {
        match self {
            term(r) => r.parse(text),
            constant(r) => r.parse(text),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[allow(non_camel_case_types)]
pub enum Term {
    term_constant,
}

impl Parser for Term {
    fn parse(&self, text: &str) -> Result<Cst, peg::error::ParseError<LineCol>> {
        match self {
            term_constant => satysfi_parser::term_const(&text),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[allow(non_camel_case_types)]
pub enum Constant {
    unit,
    boolean,
    int,
    float,
    length,
    string,
}

impl Parser for Constant {
    fn parse(&self, text: &str) -> Result<Cst, peg::error::ParseError<LineCol>> {
        match self {
            unit => satysfi_parser::const_unit(&text),
            boolean => satysfi_parser::const_bool(&text),
            int => satysfi_parser::const_int(&text),
            float => satysfi_parser::const_float(&text),
            length => satysfi_parser::const_length(&text),
            string => satysfi_parser::const_string(&text),
        }
    }
}
