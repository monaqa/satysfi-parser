use std::fmt::Display;

use itertools::Itertools;

use crate::Mode;

use self::rule::Rule;

pub mod rule;
pub mod structure;

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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

/// コードを line & column 形式 (0-indexed) で指定したもの。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LineCol {
    pub line: usize,
    pub column: usize,
}

/// CST にテキストの情報を付加したもの。
// TODO: 自己参照構造体にする。
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct CstText {
    pub text: String,
    pub lines: Vec<usize>,
    pub cst: Cst,
}

impl CstText {
    /// 与えられたパーサに基づき、与えられたテキストをパースする。
    pub fn parse<F>(
        text: &str,
        parser: F,
    ) -> std::result::Result<Self, (LineCol, Vec<&'static str>)>
    where
        F: Fn(&str) -> std::result::Result<Cst, peg::error::ParseError<peg::str::LineCol>>,
    {
        let mut lines = vec![0usize];
        lines.extend(text.match_indices('\n').map(|(p, _)| p + 1));
        match parser(text) {
            Ok(cst) => Ok(CstText {
                text: text.to_owned(),
                lines,
                cst,
            }),
            Err(e) => {
                let peg::str::LineCol { line, column, .. } = e.location;
                let lc = LineCol {
                    line: line - 1,
                    column: column - 1,
                }; // 0-indexed に変換
                let expected: Vec<_> = e.expected.tokens().collect();
                Err((lc, expected))
            }
        }
    }

    /// self.cst の子要素である Cst について、その要素に相当する text を取得する。
    pub fn get_text_from_span(&self, span: Span) -> &str {
        let text = self.text.as_str();
        let Span { start, end } = span;
        &text[start..end]
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

    /// 与えられた場所がコメント内かどうか判定する。
    pub fn is_comment(&self, pos: usize) -> bool {
        let dig = self.cst.dig(pos);
        let cst = dig.get(0);
        if cst.is_none() {
            return false;
        }
        let cst = cst.unwrap();
        // cst: その pos を含む最小の cst
        // span: pos を含む終端要素
        let span = cst
            .get_terminal_spans()
            .into_iter()
            .find(|span| span.includes(pos));
        if span.is_none() {
            // span が見つからないことは無いと思うんだけど
            return false;
        }
        let span = span.unwrap();

        // TODO: まあまあアドホックなのでなんとかしたい
        let text = self.get_text_from_span(span);
        let char_indices = text.char_indices().map(|(idx, _)| idx).collect_vec();
        let pos_char = char_indices
            .binary_search(&(pos - span.start))
            .unwrap_or_else(|x| x);
        for c in text.chars().take(pos_char).collect_vec().into_iter().rev() {
            match c {
                // 改行が見つかったらそこで探索打ち切り。コメントでないこと確定
                '\n' => return false,
                // コメント文字が見つかったらコメント確定。
                '%' => return true,
                _ => continue,
            }
        }
        // 改行もコメント文字も何も見つからなかったらコメントでないこと確定。
        false
    }
}

#[test]
fn test_is_comment() {
    let csttext = CstText::parse("let x = 1 in% foo \n  2", grammar::program).unwrap();
    assert_eq!(csttext.is_comment(11), false); // let x = 1 i"n" foo
    assert_eq!(csttext.is_comment(12), false); // let x = 1 in"%" foo
    assert_eq!(csttext.is_comment(13), true); // let x = 1 in%" "foo
    assert_eq!(csttext.is_comment(14), true); // let x = 1 in% "f"oo
    assert_eq!(csttext.is_comment(17), true); // let x = 1 in% foo" "\n
    assert_eq!(csttext.is_comment(18), true); // let x = 1 in% foo"\n"
    assert_eq!(csttext.is_comment(19), false); // let x = 1 in% foo \n" "
}

impl Display for CstText {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = self.pritty_cst_recursive(&self.cst);
        write!(f, "{}", text)
    }
}

/// Concrete syntax tree.
/// 1つの CST は構文規則、テキストの範囲、子要素からなり、全体として木構造をなす。
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Cst {
    /// 構文規則。
    pub rule: Rule,
    pub span: Span,
    pub inner: Vec<Cst>,
}

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
    /// 範囲が小さいものから順に返す。
    pub fn dig(&self, pos: usize) -> Vec<&Cst> {
        let child = self.choose(pos);
        let mut v = if let Some(child) = child {
            let mut v = child.dig(pos);
            v.push(child);
            v
        } else {
            vec![]
        };
        if self.span.includes(pos) {
            v.push(self);
        }
        v
    }

    /// 自分の Cst の内部で、 child の親となる Cst
    /// （child の scope を内包する最小の Cst）を探し、あればそれを返す。
    pub fn get_parent(&self, child: &Cst) -> Option<&Cst> {
        let child_pos = child.span.start;
        self.dig(child_pos)
            .into_iter()
            .find(|&cst| cst.span.contains(&child.span) && cst.span != child.span)
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

    /// 終端要素の Span を返す。ここでいう終端要素とは、
    /// 自身の Cst の span には含まれているが、子の span には含まれていない範囲。
    pub fn get_terminal_spans(&self) -> Vec<Span> {
        let mut v = vec![];
        let mut i = self.span.start;
        for inner in &self.inner {
            if i != inner.span.start {
                v.push(Span {
                    start: i,
                    end: inner.span.start,
                })
            }
            i = inner.span.end;
        }
        if i != self.span.end {
            v.push(Span {
                start: i,
                end: self.span.end,
            })
        }
        v
    }
}

/// CST を楽に構成するためのマクロ。
#[macro_export]
macro_rules! cst {

    ($rule:ident ($s:expr, $e:expr) []) => {
        Cst {
            rule: Rule::$rule,
            span: Span {start: $s, end: $e},
            inner: vec![]
        }
    };

    ($rule:ident ($s:expr, $e:expr) [$($inner:expr),*]) => {
        Cst {
            rule: Rule::$rule,
            span: Span {start: $s, end: $e},
            inner: vec![$($inner.vectorize()),*].vectorize()
        }
    };

}
