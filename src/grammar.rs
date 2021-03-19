use crate::types::rule::*;
use crate::types::Cst;

peg::parser! {
    pub grammar satysfi_parser() for str {
        // constants
        /// WHITESPACE
        rule _() = [' ' | '\t' | '\n' | '\r']* {}
        /// position (short name)
        rule p() -> usize = pos:position!() {pos}

        rule cr() = ['\n' | '\r'] {}
        rule ascii_digit() = ['0'..='9']

        pub rule const_unit() -> Cst =
            s:p() "(" _ ")" e:p() { Cst::new_leaf(constant(unit), (s, e)) }

        pub rule const_bool() -> Cst =
            s:p() ("true" / "false") e:p() {Cst::new_leaf(constant(boolean), (s, e))}

        pub rule const_int() -> Cst =
            s:p() (const_int_decimal() / const_int_hex() / "0") e:p() { Cst::new_leaf(constant(int), (s, e)) }
        rule const_int_decimal() = ['1'..='9'] ascii_digit()*
        rule const_int_hex() = "0" ['x' | 'X'] ['0'..='9' | 'A'..='F']+

        pub rule const_float() -> Cst =
            s:p() const_float_inner() !length_unit() e:p()
            { Cst::new_leaf(constant(float), (s, e)) }
        rule const_float_inner() = ascii_digit()+ "." ascii_digit()* / "." ascii_digit()+

        pub rule const_length() -> Cst =
            s:p() length_digit() length_unit() e:p()
            { Cst::new_leaf(constant(length), (s, e)) }

        rule length_digit() = "-"? (const_float_inner() / const_int_decimal())
        rule length_unit() = ['a'..='z'] ['0'..='9' | 'a'..='z' | 'A' ..='Z' | '-']*

        pub rule const_string() -> Cst =
            s:p() "#"? qs:string_quotes() (!string_inner(qs) [_])+ qe:string_quotes() "#"? e:p()
            {?
                if qs == qe {
                    Ok(Cst::new_leaf(constant(string), (s, e)))
                } else {
                    Err("number of back quotation does not match.")
                }

            }
        rule string_quotes() -> usize = n:"`"+ {n.len()}
        rule string_inner(qs: usize) = "`"*<{qs}>

    }
}
