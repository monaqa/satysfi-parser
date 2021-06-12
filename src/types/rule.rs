use peg::str::LineCol;

use super::Cst;
use crate::grammar;

macro_rules! make_rule {
    ( $($variant:ident,)* ) => {
        #[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
        #[allow(non_camel_case_types)]
        pub enum Rule {
            $($variant),*
        }

        impl Rule {
            pub fn parse(&self, text: &str) -> Result<Cst, peg::error::ParseError<LineCol>> {
                match self {
                    $(Rule::$variant => grammar::$variant(&text)),*
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

    // dummy
    dummy_header,
    dummy_sig_stmt,
    dummy_stmt,
    dummy_inline_cmd_incomplete,
    dummy_block_cmd_incomplete,
    dummy_modvar_incomplete,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Mode {
    /// プログラムモード
    Program,
    /// プログラムモードのうち、型を記述する箇所
    ProgramType,
    /// 垂直モード
    Vertical,
    /// 水平モード
    Horizontal,
    /// 数式モード
    Math,
    /// ヘッダ
    Header,
    /// リテラル
    Literal,
    /// コメント（不使用）
    Comment,
}

impl Rule {
    /// Mode が一意に定まるときは Mode を出力する。
    /// たとえば horizontal_single であれば水平モードであることが分かるため
    /// Some(Mode::Horizontal) を出力するが、
    /// var_ptn であれば不明のため None を返す。
    pub fn mode(&self) -> Option<Mode> {
        let mode = match self {
            Rule::stage
            | Rule::headers
            | Rule::header_require
            | Rule::header_import
            | Rule::dummy_header
            | Rule::pkgname => Mode::Header,

            Rule::let_stmt
            | Rule::let_rec_stmt
            | Rule::let_rec_inner
            | Rule::let_rec_matcharm
            | Rule::let_inline_stmt_ctx
            | Rule::let_inline_stmt_noctx
            | Rule::let_block_stmt_ctx
            | Rule::let_block_stmt_noctx
            | Rule::let_math_stmt
            | Rule::let_mutable_stmt
            | Rule::type_stmt
            | Rule::module_stmt
            | Rule::open_stmt
            | Rule::dummy_stmt
            | Rule::arg
            | Rule::pat_as
            | Rule::pat_cons
            | Rule::pattern
            | Rule::pat_variant
            | Rule::pat_list
            | Rule::pat_tuple
            | Rule::expr
            | Rule::match_expr
            | Rule::match_arm
            | Rule::match_guard
            | Rule::bind_stmt
            | Rule::ctrl_while
            | Rule::ctrl_if
            | Rule::lambda
            | Rule::assignment
            | Rule::dyadic_expr
            | Rule::unary_operator_expr
            | Rule::unary_operator
            | Rule::application
            | Rule::application_args_normal
            | Rule::application_args_optional
            | Rule::command_application
            | Rule::variant_constructor
            | Rule::unary
            | Rule::unary_prefix
            | Rule::list
            | Rule::record
            | Rule::record_unit
            | Rule::tuple
            | Rule::bin_operator
            | Rule::expr_with_mod
            | Rule::mod_cmd_name
            | Rule::module_name
            | Rule::variant_name => Mode::Program,

            Rule::sig_stmt
            | Rule::dummy_sig_stmt
            | Rule::type_inner
            | Rule::type_variant
            | Rule::sig_type_stmt
            | Rule::sig_val_stmt
            | Rule::sig_direct_stmt
            | Rule::type_expr
            | Rule::type_optional
            | Rule::type_prod
            | Rule::type_inline_cmd
            | Rule::type_block_cmd
            | Rule::type_math_cmd
            | Rule::type_list_unit_optional
            | Rule::type_application
            | Rule::type_name
            | Rule::type_record
            | Rule::type_record_unit
            | Rule::type_param
            | Rule::constraint => Mode::ProgramType,

            Rule::horizontal_single
            | Rule::horizontal_list
            | Rule::horizontal_bullet_list
            | Rule::horizontal_bullet
            | Rule::regular_text
            | Rule::horizontal_escaped_char => Mode::Horizontal,

            Rule::vertical => Mode::Vertical,

            Rule::const_unit
            | Rule::const_bool
            | Rule::const_int
            | Rule::const_float
            | Rule::const_length
            | Rule::const_string => Mode::Literal,

            Rule::math_single
            | Rule::math_list
            | Rule::math_token
            | Rule::math_sup
            | Rule::math_sub
            | Rule::math_unary => Mode::Math,

            _ => return None,
        };
        Some(mode)
    }

    pub fn is_error(&self) -> bool {
        self.error_description().is_some()
    }

    pub fn error_description(&self) -> Option<String> {
        let text = match self {
            Rule::dummy_header => "Missing Package name.",
            Rule::dummy_stmt => "Missing definition of the statement.",
            Rule::dummy_sig_stmt => "Incomplete signature syntax.",
            Rule::dummy_inline_cmd_incomplete => {
                "Incomplete inline command.\nTry adding semicolon or arguments after the command name."
            }
            Rule::dummy_block_cmd_incomplete => {
                "Incomplete block command.\nTry adding semicolon or arguments after the command name."
            }
            _ => return None,
        };
        Some(text.to_owned())
    }
}
