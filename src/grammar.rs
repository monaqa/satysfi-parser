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
        rule NEVER() = !"DUMMY" "DUMMY"

        /// return only
        rule cr() = ['\n' | '\r'] {}
        /// constants
        rule ASCII_DIGIT() = ['0'..='9']

        pub rule misc() -> Cst = NEVER() { cst!((0, 0)) }

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
        pub rule horizontal() -> Cst = s:p() "{" _ inner:(
            horizontal_list()
            / horizontal_bullet_list()
            / horizontal_single()
        )_ "}" e:p()
        { cst!(horizontal (s, e) [inner]) }

        pub rule horizontal_single() -> Cst =
            inners:horizontal_token()* { cst!(horizontal_single; inners) }
        rule horizontal_token() -> Cst =
            _ inner: (
                const_string()
                // / inline_cmd()
                / regular_text()
                ) _
        { inner }

        pub rule regular_text() -> Cst =
            s:p() $(!horizontal_special_char() [_])+ e:p()
        { cst!(regular_text (s, e)) }
        rule horizontal_special_char() = ['@' | '`' | '\\' | '{' | '}' | '%' | '|' | '*' | '$' | '#' | ';']

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

        // §1. commands
        // pub rule inline_cmd() -> Cst =
        //     s:p() inline_cmd_name() e:p()
        // { Cst::new_leaf(Rule::inline_cmd, (s, e)) }
        // pub rule inline_cmd_name() -> Cst =
        //     s:p() r"\" inner:(modvar() / var_ptn()) e:p()
        // { Cst::new_leaf(Rule::inline_cmd, (s, e)) }

        // rule var_ptn() -> Cst =
        //
        rule dummy() -> Cst =
            s:p() NEVER() e:p()
        { cst!((s, e)) }

    }
}
