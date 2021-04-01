use peg::str::LineCol;

use super::Cst;
use crate::grammar::satysfi_parser;

macro_rules! make_rule {
    ( $($variant:ident,)* ) => {
        #[derive(Debug, PartialEq, Eq, Clone, Copy)]
        #[allow(non_camel_case_types)]
        pub enum Rule {
            $($variant),*
        }

        impl Rule {
            pub fn parse(&self, text: &str) -> Result<Cst, peg::error::ParseError<LineCol>> {
                match self {
                    $(Rule::$variant => satysfi_parser::$variant(&text)),*
                }
            }
        }
    };
    ( $($variant:ident),* ) => {
        make_rule!($($variant),*,);
    };
}

make_rule! {
    misc,

    horizontal,
    horizontal_single,
    horizontal_list,
    horizontal_bullet_list,
    horizontal_bullet,
    horizontal_bullet_star,
    regular_text,

    // constants
    constant,
    const_unit,
    const_bool,
    const_int,
    const_float,
    const_length,
    const_string,

    // command
    // inline_cmd,
}
