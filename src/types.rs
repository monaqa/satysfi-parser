use self::rule::Rule;

#[derive(Debug, PartialEq, Eq)]
pub struct CstText {
    pub text: String,
    pub cst: Cst,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Cst {
    pub rule: Rule,
    pub range: (usize, usize),
    pub inner: Vec<Cst>,
}

pub mod rule {
    pub use self::Constant::*;
    pub use self::Rule::*;

    #[derive(Debug, PartialEq, Eq)]
    #[allow(non_camel_case_types)]
    pub enum Rule {
        constant(Constant),
    }

    #[derive(Debug, PartialEq, Eq)]
    #[allow(non_camel_case_types)]
    pub enum Constant {
        unit,
        boolean,
        int,
        float,
        length,
        string,
    }
}

impl Cst {
    /// 新たな CST を作成する。
    pub fn new(rule: Rule, range: (usize, usize), inner: Vec<Cst>) -> Self {
        Self { rule, range, inner }
    }

    /// Rule と range を指定して、葉に相当する（子のない） node を生成する。
    pub fn new_leaf(rule: Rule, range: (usize, usize)) -> Self {
        Self {
            rule,
            range,
            inner: vec![],
        }
    }

    /// Rule と子を指定して、枝に相当する（子のある） node を生成する。
    /// 省略した range は子の range の最小・最大から計算される。
    pub fn new_node(rule: Rule, inner: Vec<Cst>) -> Self {
        let range = inner.iter().fold((usize::MAX, 0), |acc, cst| {
            let (acc_start, acc_end) = acc;
            let (cst_start, cst_end) = cst.range;
            (acc_start.min(cst_start), acc_end.max(cst_end))
        });
        Self { rule, range, inner }
    }
}
