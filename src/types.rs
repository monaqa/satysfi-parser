#[derive(Debug, PartialEq, Eq)]
pub struct Pair {
    pub rule: Rule,
    pub range: Option<(usize, usize)>,
    pub inner: Vec<Pair>,
}

impl Pair {
    pub fn new(rule: Rule, range: Option<(usize, usize)>, inner: Vec<Pair>) -> Self {
        Self { rule, range, inner }
    }
}

#[macro_export]
macro_rules! pair {
    ($rule:ident, ($start:expr, $end:expr), $inner:expr) => {
        Pair::new (
            Rule::$rule,
            Some(($start, $end)),
            $inner
        )
    };
    ($rule:ident, ($start:expr, $end:expr); $inner:tt) => {
        Pair::new (
            Rule::$rule,
            Some(($start, $end)),
            pair!($inner)
        )
    };
    ($rule:ident, ($start:expr, $end:expr)) => {
        Pair::new (
            Rule::$rule,
            Some(($start, $end)),
            vec![]
        )
    };
    ($rule:ident, $inner:expr) => {
        Pair::new (
            Rule::$rule,
            None,
            $inner
        )
    };
    ($rule:ident; $inner:tt) => {
        Pair::new (
            Rule::$rule,
            None,
            pair!($inner)
        )
    };
    ([ $($elem:expr),* ]) => {
        vec![ $($elem),* ]
    }
}

#[derive(Debug, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub enum Rule {
    program,
}
