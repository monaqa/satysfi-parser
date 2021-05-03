mod util;
use std::fmt::Display;

use crate::types::Cst;

use crate::types::rule::Rule;
use crate::{assert_not_parsed, assert_parsed, comp_cst, cst_inner};

mod commands;
mod constant;
mod expr;
mod header;
mod horizontal;
mod math;
mod pattern;
mod statement;
mod term;
mod types;
mod unary;
mod vertical;

use itertools::Itertools;
use peg::str::LineCol;
use thiserror::Error;

/// Cst との比較を行うための構造体。
#[derive(Debug, PartialEq, Eq)]
pub struct CompCstText {
    pub text: String,
    pub rule: Rule,
    pub inner: Option<Vec<CompCstText>>,
}

impl CompCstText {
    pub fn check_cst(&self) -> std::result::Result<Cst, CstParseError> {
        let text = &self.text;
        let rule = self.rule;

        let cst = rule.parse(text).map_err(CstParseError::ParseFailed)?;

        self.check_equality(text, cst.clone(), CstBranchSequence(vec![(0, rule)]))?;

        Ok(cst)
    }

    fn check_equality(
        &self,
        text: &str,
        cst: Cst,
        branch: CstBranchSequence,
    ) -> std::result::Result<(), CstParseError> {
        if self.text != cst.as_str(&text) {
            return Err(CstParseError::TextDoesNotMatch {
                expect: self.text.to_owned(),
                actual: cst.as_str(&text).to_owned(),
                branch,
            });
        }
        if self.rule != cst.rule {
            return Err(CstParseError::RuleDoesNotMatch {
                expect: self.rule,
                actual: cst.rule,
                branch,
            });
        }
        if self.inner.is_none() {
            return Ok(());
        }

        let actual_inners = cst.inner;
        let expect_inners = self.inner.as_ref().unwrap();

        if expect_inners.len() < actual_inners.len() {
            return Err(CstParseError::InnerExcessive {
                actual: actual_inners.iter().map(|i| i.rule).collect_vec(),
                expect: expect_inners.iter().map(|i| i.rule).collect_vec(),
                branch,
            });
        }

        if expect_inners.len() > actual_inners.len() {
            return Err(CstParseError::InnerLacked {
                actual: actual_inners.iter().map(|i| i.rule).collect_vec(),
                expect: expect_inners.iter().map(|i| i.rule).collect_vec(),
                branch,
            });
        }

        for (idx, (expect_inner, actual_inner)) in
            expect_inners.iter().zip(actual_inners).enumerate()
        {
            let cst_branch = {
                let mut branch = branch.0.clone();
                branch.push((idx, expect_inner.rule));
                CstBranchSequence(branch)
            };
            expect_inner.check_equality(text, actual_inner, cst_branch)?;
        }

        Ok(())
    }
}

#[derive(Debug, Error)]
pub enum CstParseError {
    /// parse に失敗した
    #[error("Failed to Parse: {:?}", .0)]
    ParseFailed(peg::error::ParseError<LineCol>),
    /// text が合わない
    #[error("Text does not match at {}.\n  written in test: \"{}\"\n  actually parsed: \"{}\"", .branch, .expect, .actual)]
    TextDoesNotMatch {
        actual: String,
        expect: String,
        branch: CstBranchSequence,
    },
    /// rule が合わない
    #[error("Rule does not match at {}.\n  written in test: {:?}\n  actually parsed: {:?}", .branch, .expect, .actual)]
    RuleDoesNotMatch {
        actual: Rule,
        expect: Rule,
        branch: CstBranchSequence,
    },
    #[error("Inner element in {} is excessive.\n  written in test: {:?}\n  actually parsed: {:?}", .branch, .expect, .actual)]
    InnerExcessive {
        actual: Vec<Rule>,
        expect: Vec<Rule>,
        branch: CstBranchSequence,
    },
    #[error("Inner element in {} is lacked.\n  written in test: {:?}\n  actually parsed: {:?}", .branch, .expect, .actual)]
    InnerLacked {
        actual: Vec<Rule>,
        expect: Vec<Rule>,
        branch: CstBranchSequence,
    },
}

#[derive(Debug, Default)]
pub struct CstBranchSequence(Vec<(usize, Rule)>);

impl Display for CstBranchSequence {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = self
            .0
            .iter()
            .map(|(idx, rule)| format!("{:?}({}-th child)", rule, idx))
            .join(" -> ");
        write!(f, "{}", s)
    }
}

/// 以下のようなルールで CompCstText を構築する。
///
/// ```
/// // 最も単純な例
/// assert_equal!(
///     comp_cst!("foo" var : []),
///     CompCstText {text: "foo", rule: Rule::var, inner: Some(vec![])}
/// );
///
/// // inner に相当する箇所を `[_]` とすれば、 inner のチェックを無効化出来る
/// assert_equal!(
///     comp_cst!("foo" var : [_]),
///     CompCstText {text: "foo", rule: Rule::var, inner: None}
/// );
///
/// // inner に相当する箇所は親と同様の文法 + セミコロン区切りで記述する
/// assert_equal!(
///     comp_cst!("List.map" modvar : [
///         "List" module_name : [_];
///         "map" var_ptn : [];
///     ]),
///     CompCstText {text: "List.map", rule: Rule::modvar, inner: vec![
///         CompCstText {text: "List", rule: Rule::module_name, inner: None}
///         CompCstText {text: "map", rule: Rule::var_ptn, inner: vec![]}
///     ]}
/// );
///
/// // inner が 1 つしかなく text も同じ場合、 rule はカンマ区切りで連結させられる
/// assert_equal!(
///     // parsed_ast!("foo": expr ["foo": unary [_]]) と同じことになる
///     comp_cst!("foo" expr, unary : [_]),
///     CompCstText {text: "foo", rule: Rule::expr, inner: vec![
///         CompCstText {text: "foo", rule: Rule::unary, inner: None}
///     ]}
/// );
/// ```
#[macro_export]
macro_rules! comp_cst {
    ($s:literal $r:ident : $t:tt) => {
        CompCstText {
            rule: Rule::$r,
            text: $s.to_string(),
            inner: cst_inner!($t),
        }
    };
    ($s:literal $r:ident, $($rest:ident),+ : $t:tt) => {
        CompCstText {
            rule: Rule::$r,
            text: $s.to_string(),
            inner: Some(vec![comp_cst!($s $($rest),+ : $t)]),
        }
    };
}

#[macro_export]
macro_rules! cst_inner {
    ([_]) => {
        None
    };
    ([]) => {
        Some(vec![])
    };
    ([$s:literal $($r:ident),+ : $t:tt $(;$s2:literal $($r2:ident),+: $t2:tt)*]) => {
        Some(
        vec![
            comp_cst!($s $($r),+ $t),
            $( comp_cst!($s2 $($r2),+: $t2), )*
        ]
        )
    };
    ([$($s:literal $($r:ident),+ : $t:tt;)+]) => {
        Some(
        vec![
            $( comp_cst!($s $($r),+: $t), )*
        ]
        )
    };
}

#[macro_export]
macro_rules! assert_parsed {
    ($s:literal $($rest:ident),+ : $t:tt) => {
        let comp_cst = comp_cst!($s $($rest),+: $t );
        if let Err(e) = comp_cst.check_cst() {
            panic!("assertion failed (parse failed): {}", e)
        }
    };
}

#[macro_export]
macro_rules! assert_not_parsed {
    ($s:literal $($rest:ident),+ : $t:tt) => {
        let comp_cst = comp_cst!($s $($rest),+: $t );
        if let Ok(pair) = comp_cst.check_cst() {
            panic!(
                "assertion failed (successfully parsed): \"{}\" as {:?}. pair: {:?}",
                comp_cst.text, comp_cst.rule, pair
            )
        }
    };
}
