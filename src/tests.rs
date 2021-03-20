mod util;
use std::fmt::Display;

use crate::types::{Cst, CstText};

use crate::types::rule::*;

mod constant;
mod term;

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
    #[error("Text does not match at {}.\n  written in test: {:?}\n  actually parsed: {:?}", .branch, .expect, .actual)]
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
