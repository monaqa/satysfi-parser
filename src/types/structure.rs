//! saty ファイル、satyh ファイルの大まかな構造を格納したデータ構造。

use anyhow::{anyhow, Result};
use itertools::Itertools;

use crate::{Cst, CstText, Rule};

trait FromCst: Sized {
    fn parse_from_cst(cst: &Cst) -> Result<Self>;
}

pub struct ProgramText {
    structure: Program,
    text: String,
}

impl ProgramText {
    pub fn parse(text: &str) -> Result<ProgramText> {
        let csttext =
            CstText::parse(text, crate::grammar::program).map_err(|_| anyhow!("parse failed!"))?;
        let structure = Program::parse_from_cst(&csttext.cst)?;
        Ok(ProgramText {
            structure,
            text: text.to_owned(),
        })
    }
}

pub enum Program {
    Saty {
        header_stage: Option<Cst>,
        header: Vec<Header>,
        preamble: Vec<Statement>,
        expr: Expr,
    },
    Satyh {
        header_stage: Option<Cst>,
        header: Vec<Header>,
        preamble: Vec<Statement>,
    },
}

impl FromCst for Program {
    fn parse_from_cst(cst: &Cst) -> Result<Self> {
        let mut inner = cst.inner.iter().peekable();
        let header_stage = if inner.peek().unwrap().rule == Rule::stage {
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
                .map(|cst| Header::parse_from_cst(cst))
                .collect();
            v?
        };

        match cst.rule {
            Rule::program_saty => {
                let preamble = if inner.peek().unwrap().rule == Rule::preamble {
                    let preamble = inner.next().unwrap();
                    let v: Result<Vec<_>> = preamble
                        .inner
                        .iter()
                        .map(|cst| Statement::parse_from_cst(cst))
                        .collect();
                    v?
                } else {
                    vec![]
                };
                let expr = Expr::parse_from_cst(inner.next().unwrap())?;
                Ok(Program::Saty {
                    header_stage,
                    header,
                    preamble,
                    expr,
                })
            }
            Rule::program_satyh => {
                let preamble = {
                    let preamble = inner.next().unwrap();
                    let v: Result<Vec<_>> = preamble
                        .inner
                        .iter()
                        .map(|cst| Statement::parse_from_cst(cst))
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

pub struct Header {
    name: Cst,
    kind: HeaderKind,
}

impl FromCst for Header {
    fn parse_from_cst(cst: &Cst) -> Result<Self> {
        let name = cst.inner.get(0).unwrap().clone();
        let kind = match cst.rule {
            Rule::header_require => HeaderKind::Require,
            Rule::header_import => HeaderKind::Import,
            _ => unreachable!(),
        };
        Ok(Header { name, kind })
    }
}

pub enum HeaderKind {
    Require,
    Import,
}

pub enum Statement {
    Let {
        pat: Cst,
        args: Cst,
        expr: Cst,
    },
    LetRec(Vec<LetRecInner>),
    LetInline {
        var_context: Option<Cst>,
        cmd: Cst,
        args: Cst,
        expr: Cst,
    },
    LetBlock {
        var_context: Option<Cst>,
        cmd: Cst,
        args: Cst,
        expr: Cst,
    },
    LetMath {
        cmd: Cst,
        args: Cst,
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
    fn parse_from_cst(cst: &Cst) -> Result<Self> {
        match cst.rule {
            Rule::let_stmt => {}
            Rule::let_rec_stmt => {}
            Rule::let_inline_stmt_ctx => {}
            Rule::let_inline_stmt_noctx => {}
            Rule::let_block_stmt_ctx => {}
            Rule::let_block_stmt_noctx => {}
            Rule::let_math_stmt => {}
            Rule::let_mutable_stmt => {}
            Rule::type_stmt => {}
            Rule::module_stmt => {}
            Rule::open_stmt => {}
            Rule::dummy_stmt => return Err(anyhow!("dummy statement!")),
            _ => unreachable!(),
        }
        todo!()
    }
}

pub enum Signature {
    Type {
        param: Vec<Cst>,
        name: Cst,
        constraint: Vec<Cst>,
    },
    Val {
        var: Cst,
        signature: Cst,
        constraint: Cst,
    },
    Direct {
        var: Cst,
        signature: Cst,
        constraint: Cst,
    },
}

impl FromCst for Signature {
    fn parse_from_cst(cst: &Cst) -> Result<Self> {
        todo!()
    }
}

pub struct LetRecInner {
    pat: Cst,
    args: Cst,
    expr: Cst,
}

impl FromCst for LetRecInner {
    fn parse_from_cst(cst: &Cst) -> Result<Self> {
        todo!()
    }
}
pub struct TypeInner {
    param: Vec<Cst>,
    name: Cst,
    constraint: Vec<Cst>,
    body: TypeBody,
}

impl FromCst for TypeInner {
    fn parse_from_cst(cst: &Cst) -> Result<Self> {
        todo!()
    }
}
pub enum TypeBody {
    Variants(Vec<Cst>),
    Expr(Cst),
}

impl FromCst for TypeBody {
    fn parse_from_cst(cst: &Cst) -> Result<Self> {
        todo!()
    }
}

pub struct Expr(pub Cst);

impl FromCst for Expr {
    fn parse_from_cst(cst: &Cst) -> Result<Self> {
        todo!()
    }
}
