use crate::types::rule::*;
use crate::types::Cst;

peg::parser! {
    pub grammar satysfi_parser() for str {
        /// WHITESPACE
        rule _() = [' ' | '\t' | '\n' | '\r']* {}
        rule cr() = ['\n' | '\r'] {}
        /// position (short name)
        rule p() -> usize = pos:position!() {pos}

        pub rule const_unit() -> Cst
            = s:p() "(" _ ")" e:p() { Cst::new_leaf(constant(unit), (s, e)) }
    }
}
