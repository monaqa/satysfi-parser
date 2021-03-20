use self::rule::Rule;

/// CST にテキストの情報を付加したもの。
// TODO: 自己参照構造体にする。
#[derive(Debug, PartialEq, Eq)]
pub struct CstText {
    pub text: String,
    pub cst: Cst,
}

impl CstText {
    /// 与えられたパーサに基づき、与えられたテキストをパースする。
    pub fn parse<F, E: std::error::Error>(text: &str, parser: F) -> std::result::Result<Self, E>
    where
        F: Fn(&str) -> std::result::Result<Cst, E>,
        E: Send,
    {
        let cst = parser(text)?;
        Ok(CstText {
            text: text.to_owned(),
            cst,
        })
    }

    /// self.cst の子要素である Cst について、その要素に相当する text を取得する。
    pub fn get_text(&self, cst: &Cst) -> &str {
        let text = self.text.as_str();
        let (s, e) = cst.range;
        &text[s..e]
    }
}

/// Concrete syntax tree.
/// 1つの CST は構文規則、テキストの範囲、子要素からなり、全体として木構造をなす。
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Cst {
    pub rule: Rule,
    pub range: (usize, usize),
    pub inner: Vec<Cst>,
}

pub mod rule;

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

    pub fn as_str<'a>(&'a self, text: &'a str) -> &'a str {
        let (s, e) = self.range;
        &text[s..e]
    }
}
