use std::fmt::Display;

use crate::Mode;

use self::rule::Rule;

pub trait Vectorize<T> {
    /// 色んなものを強引に vector にしてしまう恐ろしいメソッド。
    /// 構文解析ではオプショナルな構文要素があったり、配列状の構文要素があったりするため
    /// それらを楽にまとめて1つの Vec に格納してしまうための処置。
    fn vectorize(self) -> Vec<T>;
}

impl<T> Vectorize<T> for Vec<T> {
    /// vec![t] のときはそのまま
    fn vectorize(self) -> Vec<T> {
        self
    }
}

impl<T> Vectorize<T> for T {
    /// t のときは vec![t] に変換される
    fn vectorize(self) -> Vec<T> {
        vec![self]
    }
}

impl<T> Vectorize<T> for Vec<Vec<T>> {
    /// T の可変長配列の concat が一発でできる
    fn vectorize(self) -> Vec<T> {
        let mut vec = vec![];
        for v in self {
            vec.extend(v);
        }
        vec
    }
}

impl<T> Vectorize<T> for Option<T> {
    /// Some(t) は vec![t] に、 None は vec![] に変換される
    fn vectorize(self) -> Vec<T> {
        self.into_iter().collect()
    }
}

impl<T> Vectorize<T> for Vec<Option<T>> {
    /// vec![ Some(a), None, Some(c), Some(d), None ] は vec![a, c, d] になる
    fn vectorize(self) -> Vec<T> {
        self.into_iter().filter_map(|e| e).collect()
    }
}

impl<T> Vectorize<T> for Option<Vec<T>> {
    fn vectorize(self) -> Vec<T> {
        self.unwrap_or_default()
    }
}

/// コードの範囲を表すもの。 usize 2 個ぶんなので Copyable.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    /// span が特定の位置を含んでいるか。
    pub fn includes(&self, pos: usize) -> bool {
        self.start <= pos && pos < self.end
    }

    /// span が特定のspanを完全に内部に含んでいるか。
    pub fn contains(&self, other: &Span) -> bool {
        self.start <= other.start && other.end <= self.end
    }
}

/// コードを line & column 形式 (0-index) で指定したもの。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineCol {
    pub line: usize,
    pub column: usize,
}

/// CST にテキストの情報を付加したもの。
// TODO: 自己参照構造体にする。
#[derive(Debug, PartialEq, Eq)]
pub struct CstText {
    pub text: String,
    pub lines: Vec<usize>,
    pub cst: Cst,
}

impl CstText {
    /// 与えられたパーサに基づき、与えられたテキストをパースする。
    pub fn parse<F, E: std::error::Error>(text: &str, parser: F) -> std::result::Result<Self, E>
    where
        F: Fn(&str) -> std::result::Result<Cst, E>,
        E: Send,
    {
        let mut lines = vec![0usize];
        lines.extend(text.match_indices('\n').map(|(p, _)| p + 1));
        let cst = parser(text)?;
        Ok(CstText {
            text: text.to_owned(),
            lines,
            cst,
        })
    }

    /// self.cst の子要素である Cst について、その要素に相当する text を取得する。
    pub fn get_text(&self, cst: &Cst) -> &str {
        let text = self.text.as_str();
        let Span { start, end } = cst.span;
        &text[start..end]
    }

    /// 与えられた position の line 及び col を出力する。
    pub fn get_line_col(&self, pos: usize) -> Option<LineCol> {
        if pos > self.text.len() {
            return None;
        }
        let line = match self.lines.binary_search(&pos) {
            Ok(i) => i,
            Err(i) => i - 1,
        };
        let column = pos - self.lines[line];
        Some(LineCol { line, column })
    }

    /// 与えられた line 及び col の position を出力する。
    pub fn from_line_col(&self, line: usize, column: usize) -> Option<usize> {
        let idxline = self.lines.get(line)?;
        let max_idx = match self.lines.get(line + 1) {
            Some(idx) => *idx,
            None => self.text.len(),
        };
        if (*idxline + column) < max_idx {
            Some(*idxline + column)
        } else {
            None
        }
    }

    /// CST の構造を string にして出力する。
    pub fn pritty_cst_recursive(&self, cst: &Cst) -> String {
        fn print_cst(csttext: &CstText, cst: &Cst, indent: usize) -> String {
            let mut s = String::new();

            s.push_str(&"  ".repeat(indent));
            s.push_str(&csttext.pritty_cst(cst));
            s.push('\n');

            for child in &cst.inner {
                s.push_str(&print_cst(csttext, child, indent + 1))
            }
            s
        }

        print_cst(&self, cst, 0)
    }

    /// Cst を pritty 表示。
    pub fn pritty_cst(&self, cst: &Cst) -> String {
        let mut s = String::new();
        s.push_str(&format!("[{:?}]", cst.rule));

        // 長すぎないものだけテキストを表示
        let Span { start, end } = cst.span;
        let slice = &self.text[start..end];
        if !slice.contains('\n') && slice.len() < 80 {
            s.push_str(&format!(": \"{}\"", slice));
        } else {
            let LineCol {
                line: start_line,
                column: start_column,
            } = self.get_line_col(start).unwrap();
            let LineCol {
                line: end_line,
                column: end_column,
            } = self.get_line_col(end).unwrap();
            s.push_str(&format!(
                " (L{}-C{} .. L{}-C{})",
                start_line + 1,
                start_column + 1,
                end_line + 1,
                end_column + 1
            ));
        }
        s
    }
}

impl Display for CstText {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = self.pritty_cst_recursive(&self.cst);
        write!(f, "{}", text)
    }
}

/// Concrete syntax tree.
/// 1つの CST は構文規則、テキストの範囲、子要素からなり、全体として木構造をなす。
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Cst {
    /// 構文規則。
    pub rule: Rule,
    pub span: Span,
    pub inner: Vec<Cst>,
}

pub mod rule;

impl Cst {
    /// 新たな CST を作成する。
    pub fn new(rule: Rule, range: (usize, usize), inner: Vec<Cst>) -> Self {
        let span = Span {
            start: range.0,
            end: range.1,
        };
        Self { rule, span, inner }
    }

    pub fn as_str<'a>(&'a self, text: &'a str) -> &'a str {
        let Span { start, end } = self.span;
        &text[start..end]
    }

    /// 与えられたルールの Cst を再帰的に抽出する。
    pub fn pickup(&self, rule: Rule) -> Vec<&Cst> {
        let mut vec = vec![];
        for cst in &self.inner {
            if cst.rule == rule {
                vec.push(cst)
            }
            let v = cst.pickup(rule);
            vec.extend(v);
        }
        vec
    }

    /// 自分の子のうち、与えられた pos を含むものを返す。
    pub fn choose(&self, pos: usize) -> Option<&Cst> {
        for cst in &self.inner {
            if cst.span.includes(pos) {
                return Some(cst);
            }
        }
        None
    }

    /// 与えられた pos を含む Pair を再帰的に探索する。
    pub fn dig(&self, pos: usize) -> Vec<&Cst> {
        let child = self.choose(pos);
        if let Some(child) = child {
            let mut v = child.dig(pos);
            v.push(child);
            v
        } else {
            vec![]
        }
    }

    pub fn mode(&self, pos: usize) -> Mode {
        let csts = self.dig(pos);
        let rules = csts.iter().map(|cst| cst.rule);

        for rule in rules {
            if let Some(mode) = rule.mode() {
                return mode;
            }
        }
        Mode::Program
    }

    /// 自身及び子要素の Cst を羅列する。
    pub fn listup(&self) -> Vec<&Cst> {
        let mut v = vec![];
        v.push(self);
        for cst in &self.inner {
            let inner = cst.listup();
            v.extend(inner);
        }
        v
    }
}

#[macro_export]
macro_rules! cst {
    // - Rule name: 省略可能
    // - range: inner があるときのみ省略可能
    // - inner: 省略可能、リストの形で直接記載可能

    // 省略なし + inner リスト形式
    ($rule:ident ($s:expr, $e:expr) [$($inner:expr),*]) => {
        Cst {
            rule: Rule::$rule,
            span: Span {start: $s, end: $e},
            inner: vec![$($inner.vectorize()),*].vectorize()
        }
    };
    // 省略なし
    ($rule:ident ($s:expr, $e:expr); $inner:expr) => {
        Cst {
            rule: Rule::$rule,
            span: Span {start: $s, end: $e},
            inner: $inner
        }
    };

    // inner 省略
    ($rule:ident ($s:expr, $e:expr)) => {
        Cst {
            rule: Rule::$rule,
            span: Span {start: $s, end: $e},
            inner: vec![]
        }
    };

    // rule 省略
    (($s:expr, $e:expr) [$($inner:expr),*]) => {
        Cst {
            rule: Rule::misc,
            span: Span {start: $s, end: $e},
            inner: vec![$($inner.vectorize()),*].vectorize()
        }
    };
    (($s:expr, $e:expr); $inner:expr) => {
        Cst {
            rule: Rule::misc,
            span: Span {start: $s, end: $e},
            inner: $inner
        }
    };

    // rule, inner 省略
    (($s:expr, $e:expr)) => {
        Cst {
            rule: Rule::misc,
            span: Span {start: $s, end: $e},
            inner: vec![]
        }
    };
}
