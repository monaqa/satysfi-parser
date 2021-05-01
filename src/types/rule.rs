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

// TODO: expand macro when all implementation has done
make_rule! {
    misc,

    program_saty,
    program_satyh,

    // header
    stage,
    headers,
    header_require,
    header_import,
    pkgname,

    // types
    type_expr,
    type_optional,
    type_prod,
    type_list,
    type_list_unit_optional,
    type_application,
    type_name,
    type_record,
    type_record_unit,
    type_param,
    constraint,

    horizontal,
    horizontal_single,
    horizontal_list,
    horizontal_bullet_list,
    horizontal_bullet,
    horizontal_bullet_star,
    regular_text,
    inline_text_embedding,

    // constants
    constant,
    const_unit,
    const_bool,
    const_int,
    const_float,
    const_length,
    const_string,

    // command
    inline_cmd,
    inline_cmd_name,

    // expr
    module_name,
    modvar,
    var,
    var_ptn,
}
