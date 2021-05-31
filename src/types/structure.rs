//! saty ファイル、satyh ファイルの大まかな構造を格納したデータ構造。

use crate::{Cst, CstText, LineCol, Rule};
use anyhow::{anyhow, Result};

trait FromCst: Sized {
    fn from_cst(cst: &Cst) -> Result<Self>;
}

/// CstText と似ているが、こちらは構文要素が構造体で分かれている。
#[derive(Debug)]
pub struct ProgramText {
    pub structure: Result<Program>,
    pub lines: Vec<usize>,
    pub text: String,
}

impl ProgramText {
    pub fn parse(text: &str) -> std::result::Result<Self, (LineCol, Vec<&'static str>)> {
        match crate::grammar::program(text) {
            Ok(cst) => {
                let mut lines = vec![0usize];
                lines.extend(text.match_indices('\n').map(|(p, _)| p + 1));
                let structure = Program::from_cst(&cst);
                Ok(ProgramText {
                    text: text.to_owned(),
                    lines,
                    structure,
                })
            }
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
}

#[derive(Debug, PartialEq, Eq)]
pub enum Program {
    Saty {
        header_stage: Option<Cst>,
        header: Vec<Header>,
        preamble: Vec<Statement>,
        expr: Cst,
    },
    Satyh {
        header_stage: Option<Cst>,
        header: Vec<Header>,
        preamble: Vec<Statement>,
    },
}

impl FromCst for Program {
    fn from_cst(cst: &Cst) -> Result<Self> {
        let mut inner = cst.inner.iter().peekable();
        let header_stage = if inner
            .peek()
            .ok_or(anyhow!("expected stage or headers."))?
            .rule
            == Rule::stage
        {
            let stage = inner.next().unwrap();
            Some(stage.clone())
        } else {
            None
        };
        let header = {
            let headers = inner.next().unwrap();
            let v: Result<Vec<_>> = headers
                .inner
                .iter()
                .map(|cst| Header::from_cst(cst))
                .collect();
            v?
        };

        match cst.rule {
            Rule::program_saty => {
                let preamble = if inner
                    .peek()
                    .ok_or(anyhow!("expected preamble or expr"))?
                    .rule
                    == Rule::preamble
                {
                    let preamble = inner.next().unwrap();
                    let v: Result<Vec<_>> = preamble
                        .inner
                        .iter()
                        .map(|cst| Statement::from_cst(cst))
                        .collect();
                    v?
                } else {
                    vec![]
                };
                let expr = inner.next().ok_or(anyhow!("expected expr"))?.clone();
                Ok(Program::Saty {
                    header_stage,
                    header,
                    preamble,
                    expr,
                })
            }
            Rule::program_satyh => {
                let preamble = {
                    let preamble = inner.next().ok_or(anyhow!("expected preamble"))?;
                    let v: Result<Vec<_>> = preamble
                        .inner
                        .iter()
                        .map(|cst| Statement::from_cst(cst))
                        .collect();
                    v?
                };
                Ok(Program::Satyh {
                    header_stage,
                    header,
                    preamble,
                })
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Header {
    pub name: Cst,
    pub kind: HeaderKind,
}

impl FromCst for Header {
    fn from_cst(cst: &Cst) -> Result<Self> {
        let name = cst.inner.get(0).ok_or(anyhow!("expected pkgname"))?.clone();
        let kind = match cst.rule {
            Rule::header_require => HeaderKind::Require,
            Rule::header_import => HeaderKind::Import,
            _ => unreachable!(),
        };
        Ok(Header { name, kind })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum HeaderKind {
    Require,
    Import,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Let {
        pat: Cst,
        type_annot: Option<Cst>,
        args: Vec<Cst>,
        expr: Cst,
    },
    LetRec(Vec<LetRecInner>),
    LetInline {
        var_context: Option<Cst>,
        cmd: Cst,
        args: Vec<Cst>,
        expr: Cst,
    },
    LetBlock {
        var_context: Option<Cst>,
        cmd: Cst,
        args: Vec<Cst>,
        expr: Cst,
    },
    LetMath {
        cmd: Cst,
        args: Vec<Cst>,
        expr: Cst,
    },
    LetMutable {
        var: Cst,
        expr: Cst,
    },
    Type(Vec<TypeInner>),
    Module {
        name: Cst,
        signature: Vec<Signature>,
        statements: Vec<Statement>,
    },
    Open(Cst),
}

impl FromCst for Statement {
    fn from_cst(cst: &Cst) -> Result<Self> {
        let stmt = match cst.rule {
            Rule::let_stmt => {
                let mut inner = cst.inner.iter().peekable();
                let pat = inner.next().ok_or(anyhow!("expected pattern"))?.clone();
                let type_annot = if inner
                    .peek()
                    .ok_or(anyhow!("expected type_expr or arg"))?
                    .rule
                    == Rule::type_expr
                {
                    Some(inner.next().unwrap().clone())
                } else {
                    None
                };
                let mut args = vec![];
                while inner.peek().ok_or(anyhow!("expected expr"))?.rule == Rule::arg {
                    let arg = inner.next().unwrap().clone();
                    args.push(arg);
                }
                let expr = inner.next().unwrap().clone();
                Statement::Let {
                    pat,
                    type_annot,
                    args,
                    expr,
                }
            }

            Rule::let_rec_stmt => {
                let let_rec_inner: Result<Vec<_>> = cst
                    .inner
                    .iter()
                    .map(|rec_inner| LetRecInner::from_cst(rec_inner))
                    .collect();
                Statement::LetRec(let_rec_inner?)
            }

            Rule::let_inline_stmt_ctx => {
                let mut inner = cst.inner.iter().peekable();
                let var_context = Some(inner.next().ok_or(anyhow!("expected context"))?.clone());
                let cmd = inner
                    .next()
                    .ok_or(anyhow!("expected command name"))?
                    .clone();
                let mut args = vec![];
                while inner.peek().ok_or(anyhow!("expected expr"))?.rule == Rule::pattern {
                    let arg = inner.next().unwrap().clone();
                    args.push(arg);
                }
                let expr = inner.next().unwrap().clone();
                Statement::LetInline {
                    var_context,
                    cmd,
                    args,
                    expr,
                }
            }
            Rule::let_inline_stmt_noctx => {
                let mut inner = cst.inner.iter().peekable();
                let var_context = None;
                let cmd = inner
                    .next()
                    .ok_or(anyhow!("expected command name"))?
                    .clone();
                let mut args = vec![];
                while inner.peek().ok_or(anyhow!("expected expr"))?.rule == Rule::arg {
                    let arg = inner.next().unwrap().clone();
                    args.push(arg);
                }
                let expr = inner.next().unwrap().clone();
                Statement::LetInline {
                    var_context,
                    cmd,
                    args,
                    expr,
                }
            }

            Rule::let_block_stmt_ctx => {
                let mut inner = cst.inner.iter().peekable();
                let var_context = Some(inner.next().ok_or(anyhow!("expected context"))?.clone());
                let cmd = inner
                    .next()
                    .ok_or(anyhow!("expected command name"))?
                    .clone();
                let mut args = vec![];
                while inner.peek().ok_or(anyhow!("expected expr"))?.rule == Rule::pattern {
                    let arg = inner.next().unwrap().clone();
                    args.push(arg);
                }
                let expr = inner.next().unwrap().clone();
                Statement::LetBlock {
                    var_context,
                    cmd,
                    args,
                    expr,
                }
            }
            Rule::let_block_stmt_noctx => {
                let mut inner = cst.inner.iter().peekable();
                let var_context = None;
                let cmd = inner
                    .next()
                    .ok_or(anyhow!("expected command name"))?
                    .clone();
                let mut args = vec![];
                while inner.peek().ok_or(anyhow!("expected expr"))?.rule == Rule::arg {
                    let arg = inner.next().unwrap().clone();
                    args.push(arg);
                }
                let expr = inner.next().unwrap().clone();
                Statement::LetBlock {
                    var_context,
                    cmd,
                    args,
                    expr,
                }
            }

            Rule::let_math_stmt => {
                let mut inner = cst.inner.iter().peekable();
                let cmd = inner
                    .next()
                    .ok_or(anyhow!("expected command name"))?
                    .clone();
                let mut args = vec![];
                while inner.peek().ok_or(anyhow!("expected expr"))?.rule == Rule::arg {
                    let arg = inner.next().unwrap().clone();
                    args.push(arg);
                }
                let expr = inner.next().unwrap().clone();
                Statement::LetMath { cmd, args, expr }
            }

            Rule::let_mutable_stmt => {
                let var = cst
                    .inner
                    .get(0)
                    .ok_or(anyhow!("expected var name"))?
                    .clone();
                let expr = cst.inner.get(1).ok_or(anyhow!("expected expr"))?.clone();
                Statement::LetMutable { var, expr }
            }

            Rule::type_stmt => {
                let type_inners: Result<Vec<_>> = cst
                    .inner
                    .iter()
                    .map(|type_inner| TypeInner::from_cst(type_inner))
                    .collect();
                Statement::Type(type_inners?)
            }

            Rule::module_stmt => {
                let name = cst.inner.get(0).ok_or(anyhow!("expected name"))?.clone();
                let sig_stmt = cst.inner.get(1).ok_or(anyhow!("expected sig_stmt"))?;
                let struct_stmt = cst.inner.get(2).ok_or(anyhow!("expected struct_stmt"))?;
                let signature: Result<Vec<_>> = sig_stmt
                    .inner
                    .iter()
                    .map(|sig_inner| Signature::from_cst(sig_inner))
                    .collect();
                let signature = signature?;
                let statements: Result<Vec<_>> = struct_stmt
                    .inner
                    .iter()
                    .map(|stmt| Statement::from_cst(stmt))
                    .collect();
                let statements = statements?;
                Statement::Module {
                    name,
                    signature,
                    statements,
                }
            }

            Rule::open_stmt => Statement::Open(
                cst.inner
                    .get(0)
                    .ok_or(anyhow!("expected module name"))?
                    .clone(),
            ),

            Rule::dummy_stmt => return Err(anyhow!("dummy statement")),
            _ => unreachable!(),
        };
        Ok(stmt)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Signature {
    Type {
        param: Vec<Cst>,
        name: Cst,
        constraint: Vec<Cst>,
    },
    Val {
        var: Cst,
        signature: Cst,
        constraint: Vec<Cst>,
    },
    Direct {
        var: Cst,
        signature: Cst,
        constraint: Vec<Cst>,
    },
}

impl FromCst for Signature {
    fn from_cst(cst: &Cst) -> Result<Self> {
        match cst.rule {
            Rule::sig_type_stmt => {
                let mut inner = cst.inner.iter().peekable();
                let mut param = vec![];
                while inner.peek().ok_or(anyhow!("expect type name"))?.rule == Rule::type_param {
                    let p = inner.next().unwrap().clone();
                    param.push(p);
                }
                let name = inner.next().unwrap().clone();
                let constraint = inner.map(|cst| cst.clone()).collect();
                Ok(Signature::Type {
                    param,
                    name,
                    constraint,
                })
            }

            Rule::sig_val_stmt => {
                let mut inner = cst.inner.iter();
                let var = inner.next().ok_or(anyhow!("expect var"))?.clone();
                let signature = inner.next().ok_or(anyhow!("expect signature"))?.clone();
                let constraint = inner.map(|cst| cst.clone()).collect();
                Ok(Signature::Val {
                    var,
                    signature,
                    constraint,
                })
            }

            Rule::sig_direct_stmt => {
                let mut inner = cst.inner.iter();
                let var = inner.next().ok_or(anyhow!("expect var"))?.clone();
                let signature = inner.next().ok_or(anyhow!("expect signature"))?.clone();
                let constraint = inner.cloned().collect();
                Ok(Signature::Direct {
                    var,
                    signature,
                    constraint,
                })
            }

            Rule::dummy_sig_stmt => Err(anyhow!("dummy signature statement")),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct LetRecInner {
    pub pattern: Cst,
    pub type_expr: Option<Cst>,
    pub variant: Vec<LetRecVariant>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct LetRecVariant {
    pub args: Vec<Cst>,
    pub expr: Cst,
}

impl FromCst for LetRecInner {
    fn from_cst(cst: &Cst) -> Result<Self> {
        let mut inner = cst.inner.iter().peekable();
        let pattern = inner.next().ok_or(anyhow!("expected pattern"))?.clone();
        let type_expr = if inner
            .peek()
            .ok_or(anyhow!("expected variant or expr"))?
            .rule
            == Rule::type_expr
        {
            Some(inner.next().unwrap().clone())
        } else {
            None
        };
        let mut variant = vec![];
        while inner.peek().is_some() {
            let mut args = vec![];
            while inner.peek().ok_or(anyhow!("expected expr"))?.rule == Rule::arg {
                args.push(inner.next().unwrap().clone())
            }
            let expr = inner.next().unwrap().clone();
            variant.push(LetRecVariant { args, expr });
        }
        Ok(LetRecInner {
            pattern,
            type_expr,
            variant,
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeInner {
    pub param: Vec<Cst>,
    pub name: Cst,
    pub constraint: Vec<Cst>,
    pub body: TypeBody,
}

impl FromCst for TypeInner {
    fn from_cst(cst: &Cst) -> Result<Self> {
        let mut inner = cst.inner.iter().peekable();
        let mut param = vec![];
        while inner.peek().ok_or(anyhow!("expected type name"))?.rule == Rule::type_param {
            let p = inner.next().unwrap().clone();
            param.push(p);
        }
        let name = inner.next().unwrap().clone();

        let body = if inner
            .peek()
            .ok_or(anyhow!("expected type expr or type variant"))?
            .rule
            == Rule::type_expr
        {
            TypeBody::Expr(inner.next().unwrap().clone())
        } else {
            let mut variants = vec![];
            while inner.peek().is_some() && inner.peek().unwrap().rule == Rule::type_variant {
                let v = inner.next().unwrap().clone();
                variants.push(v);
            }
            TypeBody::Variants(variants)
        };
        let constraint = inner.cloned().collect();
        Ok(TypeInner {
            param,
            name,
            constraint,
            body,
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeBody {
    Variants(Vec<Cst>),
    Expr(Cst),
}
