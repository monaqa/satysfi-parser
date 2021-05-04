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
    preamble,

    // header
    stage,
    headers,
    header_require,
    header_import,
    pkgname,

    // statement
    let_stmt,
    let_rec_stmt,
    let_rec_inner,
    let_rec_matcharm,
    let_inline_stmt_ctx,
    let_inline_stmt_noctx,
    let_block_stmt_ctx,
    let_block_stmt_noctx,
    let_math_stmt,
    let_mutable_stmt,
    type_stmt,
    type_inner,
    type_variant,
    module_stmt,
    open_stmt,
    arg,

    // struct
    sig_stmt,
    struct_stmt,
    sig_type_stmt,
    sig_val_stmt,
    sig_direct_stmt,

    // types
    type_expr,
    type_optional,
    type_prod,
    type_inline_cmd,
    type_block_cmd,
    type_math_cmd,
    type_list_unit_optional,
    type_application,
    type_name,
    type_record,
    type_record_unit,
    type_param,
    constraint,

    // pattern
    pat_as,
    pat_cons,
    pattern,
    pat_variant,
    pat_list,
    pat_tuple,

    // expr
    expr,
    match_expr,
    match_arm,
    match_guard,
    bind_stmt,
    ctrl_while,
    ctrl_if,
    lambda,
    assignment,
    dyadic_expr,
    unary_operator_expr,
    unary_operator,
    application,
    application_args_normal,
    application_args_optional,
    command_application,
    variant_constructor,

    // unary
    unary,
    unary_prefix,
    block_text,
    horizontal_text,
    math_text,
    list,
    record,
    record_unit,
    tuple,
    bin_operator,
    expr_with_mod,
    var,
    var_ptn,
    modvar,
    mod_cmd_name,
    module_name,
    variant_name,

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

    // math
    math_single,
    math_list,
    math_token,
    math_sup,
    math_sub,
    math_unary,
    math_embedding,
}
