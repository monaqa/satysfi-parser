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
            s:p() (const_int_decimal() / const_int_hex()) e:p() { Cst::new_leaf(constant(int), (s, e)) }
        rule const_int_decimal() = ['1'..='9'] ascii_digit()*
        rule const_int_hex() = "0" ['x' | 'X'] ['0'..='9' | 'A'..='F']+

        pub rule const_float() -> Cst =
            s:p()  (ascii_digit()+ "." ascii_digit()* / "." ascii_digit()+)  e:p()
            { Cst::new_leaf(constant(float), (s, e)) }

    }
}
