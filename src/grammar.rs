use crate::cst;
use crate::types::rule::Rule;
use crate::types::Cst;

peg::parser! {
    pub grammar satysfi_parser() for str {
        /// WHITESPACE
        rule _() = [' ' | '\t' | '\n' | '\r']* {}
        /// position (short name)
        rule p() -> usize = pos:position!() {pos}
        /// dummy pattern (never match)
        rule DUMMY() = !"DUMMY" "DUMMY"

        /// return only
        rule cr() = ['\n' | '\r'] {}
        /// constants
        rule ASCII_DIGIT() = ['0'..='9']
        rule ASCII_ALPHANUMERIC_HYPHEN() = ['a'..='z' | 'A'..='Z' | '0'..='9' | '-']

        pub rule misc() -> Cst = DUMMY() { cst!((0, 0)) }

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

        // §1. expr
        rule expr() -> Cst =
            s:p() inner:() e:p()
        { cst!((s, e)) }

        rule match_expr() -> Cst =
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

        rule application() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule record_member() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule variant_constructor() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule unary() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule list() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        rule record() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

        // §1. uncategorized

        pub rule var() -> Cst =
            s:p() !(reserved_word() ASCII_ALPHANUMERIC_HYPHEN()) var_ptn() e:p()
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

        rule math_text() -> Cst =
            s:p() DUMMY() e:p()
        { cst!((s, e)) }

    }
}
