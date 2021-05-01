use crate::cst;
use crate::types::rule::Rule;
use crate::types::Cst;

trait Vectorize<T> {
    /// 色んなものを強引に vector にしてしまう恐ろしいメソッド。
    /// 構文解析ではオプショナルな構文要素があったり、配列状の構文要素があったりするため
    /// それらを楽にまとめて1つの Vec に格納してしまうための処置。
    fn into_vec(self) -> Vec<T>;
}

impl<T> Vectorize<T> for Vec<T> {
    /// vec![t] のときはそのまま
    fn into_vec(self) -> Vec<T> {
        self
    }
}

impl<T> Vectorize<T> for T {
    /// t のときは vec![t] に変換される
    fn into_vec(self) -> Vec<T> {
        vec![self]
    }
}

impl<T> Vectorize<T> for Vec<Vec<T>> {
    /// T の可変長配列の concat が一発でできる
    fn into_vec(self) -> Vec<T> {
        let mut vec = vec![];
        for v in self {
            vec.extend(v);
        }
        vec
    }
}

impl<T> Vectorize<T> for Option<T> {
    /// Some(t) は vec![t] に、 None は vec![] に変換される
    fn into_vec(self) -> Vec<T> {
        self.into_iter().collect()
    }
}

impl<T> Vectorize<T> for Vec<Option<T>> {
    /// vec![ Some(a), None, Some(c), Some(d), None ] は vec![a, c, d] になる
    fn into_vec(self) -> Vec<T> {
        self.into_iter().filter_map(|e| e).collect()
    }
}

/// 様々な型の変数をごちゃまぜに書くと、それをまとめて Vec<T> にしてくれる。
///
/// ```
/// assert_equal!(vectorize![1, vec![2, 3, 4], Some(5)], vec![1, 2, 3, 4])
/// ```
macro_rules! vectorize {
    ($($e:expr),*) => {
        vec![$($e.into_vec()),*].into_vec()
    };
}

peg::parser! {
    pub grammar satysfi_parser() for str {
        // §1. program

        /// WHITESPACE
        rule _() = ([' ' | '\t' | '\n' | '\r'] / comment())*
        rule __() = [' ' | '\t' | '\n' | '\r']*
        /// alias for position
        rule p() -> usize = pos:position!() {pos}
        /// dummy pattern (never match)
        rule DUMMY() = !"DUMMY" "DUMMY"
        /// end of file
        rule EOF() = ![_]

        /// arbitrary character except line feed
        rule NON_LF() = !['\r' | '\n'] [_]

        /// constants
        rule ASCII_DIGIT() = ['0'..='9']
        rule ASCII_ALPHANUMERIC_HYPHEN() = ['a'..='z' | 'A'..='Z' | '0'..='9' | '-']

        pub rule misc() -> Cst = DUMMY() { cst!((0, 0)) }

        /// comment
        rule comment() = "%" comment_inner() ['\r' | '\n']
        rule comment_inner() = NON_LF()*

        pub rule program() -> Cst = program_saty() / program_satyh()

        pub rule program_saty() -> Cst =
            s:p() _
            stage:header_stage()? _
            headers:headers() _
            pre:(pre:preamble() _ "in" {pre})? _
            expr:expr() _
            e:p()
        { cst!(program_saty (s, e); vectorize![stage, headers, pre, expr]) }

        pub rule program_satyh() -> Cst =
            s:p() _
            stage:header_stage()? _
            headers:headers() _
            pre:preamble() _
            e:p()
        { cst!(program_satyh (s, e); vectorize![stage, headers, pre]) }

        // §1. header

        rule header_stage() -> Cst =
            "@stage:" [' ' | '\t']* stg:stage() [' ' | '\t']* ['\r' | '\n']
        {stg}

        pub rule stage() -> Cst =
            s:p() ("0" / "1" / "persistent") e:p()
        { cst!(stage (s, e)) }

        pub rule headers() -> Cst =
            s:p() v:(h: header() _ {h})* e:p()
        { cst!(headers (s, e); v) }

        rule header() -> Cst = header_require() / header_import()

        pub rule header_require() -> Cst =
            s:p() "@require:" [' ' | '\t']* pkg:pkgname() ['\n' | '\r'] e:p()
        { cst!(header_require (s, e) [pkg]) }

        pub rule header_import() -> Cst =
            s:p() "@import:" [' ' | '\t']* pkg:pkgname() ['\n' | '\r'] e:p()
        { cst!(header_import (s, e) [pkg]) }

        pub rule pkgname() -> Cst =
            s:p() NON_LF()+ e:p()
        { cst!(pkgname (s, e)) }

        // §1. statement

        rule preamble() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule statement() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule let_stmt() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule let_stmt_argument() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule let_rec_stmt() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule let_rec_inner() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule let_rec_stmt_argument() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule let_rec_matcharm() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule let_inline_stmt() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule let_inline_stmt_ctx() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule let_inline_stmt_noctx() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule let_block_stmt() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule let_block_stmt_ctx() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule let_block_stmt_noctx() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule let_math_stmt() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule let_mutable_stmt() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule type_stmt() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule type_inner() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule type_varaiant() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule open_stmt() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule arg() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        // §1. module

        rule module_stmt() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule sig_stmt() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule struct_stmt() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule sig_inner() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule sig_type_stmt() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule sig_val_stmt() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule sig_direct_stmt() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        // §1. types

        pub rule type_expr() -> Cst =
            s:p()
            typeopts:(t:type_optional() _ "?->" _ {t})* _
            typeprods:(t:type_prod() _ "->" _ {t})* _
            typeprod:type_prod()
            e:p()
        {
            cst!(type_expr (s, e); vectorize![typeopts, typeprods, typeprod])
        }

        pub rule type_optional() -> Cst = s:p() t:type_prod() e:p() { cst!(type_optional (s, e); t.inner) }

        pub rule type_prod() -> Cst =
            s:p() t:type_unary() ts:(_ "*" _ t:type_unary() {t})* e:p()
        {
            cst!(type_prod (s, e); vectorize![t, ts])
        }

        #[cache]
        rule type_unary() -> Cst =
            t:type_list() _ "inline-cmd" {t}
            / t:type_list() _ "block-cmd" {t}
            / t:type_list() _ "math-cmd" {t}
            / type_application()
            / "(" _ t:type_expr() _ ")" {t}
            / type_record()
            / type_param()

        pub rule type_list() -> Cst =
            s:p() "[" _ "]" e:p() { cst!((s, e)) }
            / s:p() "[" _ t:type_list_unit()
                ts:(_ ";" _ t:type_list_unit() {t})* _
                ";"? _ "]" e:p()
                { cst!(type_list (s, e); vectorize![t, ts]) }

        rule type_list_unit() -> Cst =
            t:type_list_unit_optional() _ "?" {t}
            / type_expr()

        pub rule type_list_unit_optional() -> Cst =
            s:p() t:type_prod() e:p()
        { cst!(type_list_unit_optional (s, e); t.inner) }

        pub rule type_application() -> Cst =
            s:p() t:type_application_unit() _ ts:(type_application_unit()** _) e:p()
        { cst!(type_application (s, e); vectorize![t, ts]) }

        rule type_application_unit() -> Cst =
            "(" _ t:type_expr() _ ")" {t}
            / type_param()
            / type_name()

        pub rule type_name() -> Cst =
            s:p() t:(var() / modvar()) e:p()
        { cst!(type_name (s, e) [t]) }

        pub rule type_record() -> Cst =
            s:p() "(" _ "|" _ "|" _ ")" e:p() { cst!(type_record (s, e)) }
            / s:p() "(" _ "|" _ t:type_record_inner() _ "|" _ ")" e:p() { cst!(type_record (s, e); t) }

        rule type_record_inner() -> Vec<Cst> =
            s:p() t:type_record_unit() _ ts:(type_record_unit() ** (_ ";" _)) _ ";"? e:p()
        { vectorize![t, ts] }

        pub rule type_record_unit() -> Cst =
            s:p() v:var() _ ":" _ t:type_expr() e:p()
        { cst!(type_record_unit (s, e) [v, t]) }

        pub rule type_param() -> Cst =
            s:p() "'" _ t:var_ptn() e:p()
        { cst!(type_param (s, e) [t]) }

        pub rule constraint() -> Cst =
            s:p() "constraint" t:type_param() _ "::" _ r:type_record() e:p()
        { cst!(constraint (s, e) [t, r]) }

        // §1. pattern

        rule match_ptn() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule pattern() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule pat_variant() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule pat_list() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule pat_tuple() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        // §1. expr
        rule expr() -> Cst =
            match_expr()
            / ctrl_while()
            / ctrl_if()
            / lambda()
            / assignment()
            / dyadic_expr()
            / unary_operator_expr()
            / variant_constructor()
            / application()
            / record_member()
            / unary()

        rule match_expr() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule match_arm() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule ctrl_while() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule ctrl_if() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule lambda() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule assignment() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule dyadic_expr() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule unary_operator_expr() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule unary_operator() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule application() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule record_member() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule variant_constructor() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        // §1. unary

        rule unary() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule block_text() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule horizontal_text() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule math_text() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule record() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule record_inner() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule record_unit() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule list() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule tuple() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule bin_operator() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule expr_with_mod() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        pub rule var() -> Cst =
            s:p() !(reserved_word() !ASCII_ALPHANUMERIC_HYPHEN()) var_ptn() e:p()
        { cst!(var (s, e)) }

        pub rule var_ptn() -> Cst =
            s:p() ['a'..='z'] ASCII_ALPHANUMERIC_HYPHEN()* e:p()
        { cst!(var_ptn (s, e)) }

        // 予約語たち
        rule reserved_word() =
            ( "constraint"
            / "inline-cmd" / "block-cmd" / "math-cmd"
            / "let-mutable" / "let-inline" / "let-block" / "let-math" / "let-rec"
            / "controls" / "command" / "before" / "module" / "direct" / "struct"
            / "cycle" / "match" / "while" / "false"
            / "else" / "open" / "then" / "true" / "type" / "when" / "with"
            / "and" / "end" / "fun" / "let" / "mod" / "not" / "sig" / "val"
            / "as" / "do" / "if" / "in" / "of")

        pub rule modvar() -> Cst =
            s:p() modname:module_name() _ "." _ var:var_ptn() e:p()
        { cst!(modvar (s, e) [modname, var]) }

        pub rule module_name() -> Cst =
            s:p() DUMMY() e:p()
        { cst!(module_name (s, e)) }

        // §1. constants
        pub rule constant() -> Cst =
            inner:(
                const_unit()
                / const_bool()
                / const_string()
                / const_float()
                / const_length()
                / const_int())
            { cst!(constant [inner]) }

        pub rule const_unit() -> Cst =
            s:p() "(" _ ")" e:p() { cst!(const_unit (s, e)) }

        pub rule const_bool() -> Cst =
            s:p() ("true" / "false") e:p() {cst!(const_bool (s, e))}

        pub rule const_int() -> Cst =
            s:p() (const_int_decimal() / const_int_hex() / "0") e:p() { cst!(const_int (s, e)) }
        rule const_int_decimal() = ['1'..='9'] ASCII_DIGIT()*
        rule const_int_hex() = "0" ['x' | 'X'] ['0'..='9' | 'A'..='F']+

        pub rule const_float() -> Cst =
            s:p() const_float_inner() !length_unit() e:p()
            { cst!(const_float (s, e)) }
        rule const_float_inner() = ASCII_DIGIT()+ "." ASCII_DIGIT()* / "." ASCII_DIGIT()+

        pub rule const_length() -> Cst =
            s:p() length_digit() length_unit() e:p()
            { cst!(const_length (s, e)) }

        rule length_digit() = "-"? (const_float_inner() / const_int_decimal())
        rule length_unit() = ['a'..='z'] ['0'..='9' | 'a'..='z' | 'A' ..='Z' | '-']*

        pub rule const_string() -> Cst =
            s:p() "#"? qs:string_quotes() (!string_inner(qs) [_])+ qe:string_quotes() "#"? e:p()
            {?
                if qs == qe {
                    Ok(cst!(const_string (s, e)))
                } else {
                    Err("number of back quotation does not match.")
                }
            }
        rule string_quotes() -> usize = n:"`"+ {n.len()}
        rule string_inner(qs: usize) = "`"*<{qs}>

        // §1. commands
        pub rule inline_cmd() -> Cst = (
            // \cmd()()...(); のパターン
            s:p() name:inline_cmd_name() _
            opts:((cmd_expr_arg() / cmd_expr_option())** _) _ ";" e:p()
            {
                cst!(inline_cmd (s, e); [vec![name], opts].concat())
            }
            /
            // \cmd()()...(){}<>...{} のパターン
            s:p() name:inline_cmd_name() _
            opt:((cmd_expr_arg() / cmd_expr_option())** _) opttext:(cmd_text_arg() ++ _) e:p()
            {
                cst!(inline_cmd (s, e); [vec![name], opt, opttext].concat())
            }
        )

        pub rule inline_cmd_name() -> Cst =
            s:p() r"\" inner:(modvar() / var_ptn()) e:p()
        { cst!(inline_cmd_name (s, e) [inner]) }

        rule cmd_expr_arg() -> Cst =
            s:p() inner:(
                const_unit()
                / ("(" _ inner:expr() _ ")" { inner })
                / list()
                / record()
                ) e:p()
        { inner }

        rule cmd_expr_option() -> Cst =
            s:p() "?:" _ inner:cmd_expr_arg() e:p() { cst!((s, e) [inner]) }
            / s:p() "?*" e:p() { cst!((s, e)) }

        rule cmd_text_arg() -> Cst =
            "<" _ inner:vertical() _ ">" {inner}
            / "{" _ inner:horizontal() _ "}" {inner}

        pub rule inline_text_embedding() -> Cst =
            s:p() "#" _ inner:(var_ptn() / modvar()) _ ";" e:p()
        { cst!(inline_text_embedding (s, e) [inner]) }

        // §1. horizontal mode
        pub rule horizontal() -> Cst = s:p() inner:(
            horizontal_list()
            / horizontal_bullet_list()
            / horizontal_single()
        ) e:p()
        { cst!(horizontal (s, e) [inner]) }

        pub rule horizontal_single() -> Cst =
            inners:horizontal_token()* { cst!(horizontal_single; inners) }

        rule horizontal_token() -> Cst =
            _ inner: (
                const_string()
                / inline_cmd()
                / inline_text_embedding()
                / math_text()
                / horizontal_escaped_char()
                / regular_text()
                ) _
        { inner }

        pub rule regular_text() -> Cst =
            s:p() $(!horizontal_special_char() [_])+ e:p()
        { cst!(regular_text (s, e)) }

        rule horizontal_escaped_char() -> Cst =
            s:p() "\\" horizontal_special_char() e:p()
        { cst!((s, e)) }

        rule horizontal_special_char() =
            ['@' | '`' | '\\' | '{' | '}' | '%' | '|' | '*' | '$' | '#' | ';']

        pub rule horizontal_list() -> Cst =
            s:p() "|" inners:horizontal_list_inner()+ e:p()
            { cst!(horizontal_list (s, e); inners) }
        rule horizontal_list_inner() -> Cst = _ inner:horizontal_single() _ "|" {inner}

        pub rule horizontal_bullet_list() -> Cst =
            inners:horizontal_bullet()+ { cst!(horizontal_bullet_list; inners) }

        pub rule horizontal_bullet() -> Cst =
            _ star:horizontal_bullet_star() _ single:horizontal_single() _
            { cst!(horizontal_bullet [star, single]) }

        pub rule horizontal_bullet_star() -> Cst =
            s:p() "*"+ e:p()
            { cst!(horizontal_bullet_star (s, e)) }

        // §1. vertical mode

        rule vertical() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        // §1. math mode

        rule math() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        // §1. uncategorized

    }
}
