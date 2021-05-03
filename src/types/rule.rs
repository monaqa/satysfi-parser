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

    // expr
    expr,
    math_text,

    // unary
    list,
    record,
    record_unit,
    tuple,

    // command
    cmd_name_ptn,
    cmd_expr_arg,
    cmd_expr_option,
    cmd_text_arg,
    inline_cmd,
    inline_cmd_name,
    block_cmd,
    block_cmd_name,
    math_cmd,
    math_cmd_name,
    math_cmd_expr_arg,
    math_cmd_expr_option,

    // horizontal
    horizontal_single,
    horizontal_list,
    horizontal_bullet_list,
    horizontal_bullet,
    horizontal_bullet_star,
    regular_text,
    horizontal_escaped_char,
    inline_text_embedding,

    vertical,
    block_text_embedding,

    // constants
    constant,
    const_unit,
    const_bool,
    const_int,
    const_float,
    const_length,
    const_string,

    // expr
    module_name,
    modvar,
    mod_cmd_name,
    var,
    var_ptn,

    // math
    math_single,
    math_list,
    math_token,
    math_sup,
    math_sub,
    math_unary,
}
