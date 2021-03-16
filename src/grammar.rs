peg::parser! {
    pub grammar satysfi_parser() for str {
        use crate::types::{Pair, Rule};
        use crate::pair;

        rule unit() -> Pair
            = s:position!() "()" e:position!() { pair!(program, (s, e)) }

        pub rule list() -> Pair =
            s:position!() "[" l:unit() ** "," "]" e:position!()
            { pair!(program, l) }
    }
}
