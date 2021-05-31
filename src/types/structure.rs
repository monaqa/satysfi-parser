//! saty ファイル、satyh ファイルの大まかな構造を格納したデータ構造。

use crate::{Cst, CstText, Rule};

trait FromCst: Sized {
    fn from_cst(cst: &Cst) -> Option<Self>;
}

pub struct ProgramText {
    pub structure: Program,
    pub text: String,
}

impl ProgramText {
    pub fn parse(text: &str) -> Option<ProgramText> {
        let csttext = CstText::parse(text, crate::grammar::program).ok()?;
        let structure = Program::from_cst(&csttext.cst)?;
        Some(ProgramText {
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
        expr: Cst,
    },
    Satyh {
        header_stage: Option<Cst>,
        header: Vec<Header>,
        preamble: Vec<Statement>,
    },
}

impl FromCst for Program {
    fn from_cst(cst: &Cst) -> Option<Self> {
        let mut inner = cst.inner.iter().peekable();
        let header_stage = if inner.peek()?.rule == Rule::stage {
            let stage = inner.next()?;
            Some(stage.clone())
        } else {
            None
        };
        let header = {
            let headers = inner.next()?;
            let v: Option<Vec<_>> = headers
                .inner
                .iter()
                .map(|cst| Header::from_cst(cst))
                .collect();
            v?
        };

        match cst.rule {
            Rule::program_saty => {
                let preamble = if inner.peek()?.rule == Rule::preamble {
                    let preamble = inner.next()?;
                    let v: Option<Vec<_>> = preamble
                        .inner
                        .iter()
                        .map(|cst| Statement::from_cst(cst))
                        .collect();
                    v?
                } else {
                    vec![]
                };
                let expr = inner.next()?.clone();
                Some(Program::Saty {
                    header_stage,
                    header,
                    preamble,
                    expr,
                })
            }
            Rule::program_satyh => {
                let preamble = {
                    let preamble = inner.next()?;
                    let v: Option<Vec<_>> = preamble
                        .inner
                        .iter()
                        .map(|cst| Statement::from_cst(cst))
                        .collect();
                    v?
                };
                Some(Program::Satyh {
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
    pub name: Cst,
    pub kind: HeaderKind,
}

impl FromCst for Header {
    fn from_cst(cst: &Cst) -> Option<Self> {
        let name = cst.inner.get(0)?.clone();
        let kind = match cst.rule {
            Rule::header_require => HeaderKind::Require,
            Rule::header_import => HeaderKind::Import,
            _ => unreachable!(),
        };
        Some(Header { name, kind })
    }
}

pub enum HeaderKind {
    Require,
    Import,
}

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
    fn from_cst(cst: &Cst) -> Option<Self> {
        let stmt = match cst.rule {
            Rule::let_stmt => {
                let mut inner = cst.inner.iter().peekable();
                let pat = inner.next()?.clone();
                let type_annot = if inner.peek()?.rule == Rule::type_expr {
                    Some(inner.next()?.clone())
                } else {
                    None
                };
                let mut args = vec![];
                while inner.peek()?.rule == Rule::arg {
                    let arg = inner.next()?.clone();
                    args.push(arg);
                }
                let expr = inner.next()?.clone();
                Statement::Let {
                    pat,
                    type_annot,
                    args,
                    expr,
                }
            }

            Rule::let_rec_stmt => {
                let let_rec_inner: Option<Vec<_>> = cst
                    .inner
                    .iter()
                    .map(|rec_inner| LetRecInner::from_cst(rec_inner))
                    .collect();
                Statement::LetRec(let_rec_inner?)
            }

            Rule::let_inline_stmt_ctx => {
                let mut inner = cst.inner.iter().peekable();
                let var_context = Some(inner.next()?.clone());
                let cmd = inner.next()?.clone();
                let mut args = vec![];
                while inner.peek()?.rule == Rule::pattern {
                    let arg = inner.next()?.clone();
                    args.push(arg);
                }
                let expr = inner.next()?.clone();
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
                let cmd = inner.next()?.clone();
                let mut args = vec![];
                while inner.peek()?.rule == Rule::arg {
                    let arg = inner.next()?.clone();
                    args.push(arg);
                }
                let expr = inner.next()?.clone();
                Statement::LetInline {
                    var_context,
                    cmd,
                    args,
                    expr,
                }
            }

            Rule::let_block_stmt_ctx => {
                let mut inner = cst.inner.iter().peekable();
                let var_context = Some(inner.next()?.clone());
                let cmd = inner.next()?.clone();
                let mut args = vec![];
                while inner.peek()?.rule == Rule::pattern {
                    let arg = inner.next()?.clone();
                    args.push(arg);
                }
                let expr = inner.next()?.clone();
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
                let cmd = inner.next()?.clone();
                let mut args = vec![];
                while inner.peek()?.rule == Rule::arg {
                    let arg = inner.next()?.clone();
                    args.push(arg);
                }
                let expr = inner.next()?.clone();
                Statement::LetBlock {
                    var_context,
                    cmd,
                    args,
                    expr,
                }
            }

            Rule::let_math_stmt => {
                let mut inner = cst.inner.iter().peekable();
                let cmd = inner.next()?.clone();
                let mut args = vec![];
                while inner.peek()?.rule == Rule::arg {
                    let arg = inner.next()?.clone();
                    args.push(arg);
                }
                let expr = inner.next()?.clone();
                Statement::LetMath { cmd, args, expr }
            }

            Rule::let_mutable_stmt => {
                let var = cst.inner.get(0)?.clone();
                let expr = cst.inner.get(1)?.clone();
                Statement::LetMutable { var, expr }
            }

            Rule::type_stmt => {
                let type_inners: Option<Vec<_>> = cst
                    .inner
                    .iter()
                    .map(|type_inner| TypeInner::from_cst(type_inner))
                    .collect();
                Statement::Type(type_inners?)
            }

            Rule::module_stmt => {
                let name = cst.inner.get(0)?.clone();
                let sig_stmt = cst.inner.get(1)?;
                let struct_stmt = cst.inner.get(2)?;
                let signature: Option<Vec<_>> = sig_stmt
                    .inner
                    .iter()
                    .map(|sig_inner| Signature::from_cst(sig_inner))
                    .collect();
                let signature = signature?;
                let statements: Option<Vec<_>> = struct_stmt
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

            Rule::open_stmt => Statement::Open(cst.inner.get(0)?.clone()),

            Rule::dummy_stmt => return None,
            _ => unreachable!(),
        };
        Some(stmt)
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
        constraint: Vec<Cst>,
    },
    Direct {
        var: Cst,
        signature: Cst,
        constraint: Vec<Cst>,
    },
}

impl FromCst for Signature {
    fn from_cst(cst: &Cst) -> Option<Self> {
        match cst.rule {
            Rule::sig_type_stmt => {
                let mut inner = cst.inner.iter().peekable();
                let mut param = vec![];
                while inner.peek()?.rule == Rule::type_param {
                    let p = inner.next()?.clone();
                    param.push(p);
                }
                let name = inner.next()?.clone();
                let constraint = inner.map(|cst| cst.clone()).collect();
                Some(Signature::Type {
                    param,
                    name,
                    constraint,
                })
            }

            Rule::sig_val_stmt => {
                let mut inner = cst.inner.iter();
                let var = inner.next()?.clone();
                let signature = inner.next()?.clone();
                let constraint = inner.map(|cst| cst.clone()).collect();
                Some(Signature::Val {
                    var,
                    signature,
                    constraint,
                })
            }

            Rule::sig_direct_stmt => {
                let mut inner = cst.inner.iter();
                let var = inner.next()?.clone();
                let signature = inner.next()?.clone();
                let constraint = inner.cloned().collect();
                Some(Signature::Direct {
                    var,
                    signature,
                    constraint,
                })
            }

            Rule::dummy_sig_stmt => None,
            _ => unreachable!(),
        }
    }
}

pub struct LetRecInner {
    pub pattern: Cst,
    pub type_expr: Option<Cst>,
    pub variant: Vec<LetRecVariant>,
}

pub struct LetRecVariant {
    pub args: Vec<Cst>,
    pub expr: Cst,
}

impl FromCst for LetRecInner {
    fn from_cst(cst: &Cst) -> Option<Self> {
        let mut inner = cst.inner.iter().peekable();
        let pattern = inner.next()?.clone();
        let type_expr = if inner.peek().unwrap().rule == Rule::type_expr {
            Some(inner.next()?.clone())
        } else {
            None
        };
        let mut variant = vec![];
        while inner.peek().is_some() {
            let mut args = vec![];
            while inner.peek().unwrap().rule == Rule::arg {
                args.push(inner.next()?.clone())
            }
            let expr = inner.next()?.clone();
            variant.push(LetRecVariant { args, expr });
        }
        Some(LetRecInner {
            pattern,
            type_expr,
            variant,
        })
    }
}
pub struct TypeInner {
    pub param: Vec<Cst>,
    pub name: Cst,
    pub constraint: Vec<Cst>,
    pub body: TypeBody,
}

impl FromCst for TypeInner {
    fn from_cst(cst: &Cst) -> Option<Self> {
        let mut inner = cst.inner.iter().peekable();
        let mut param = vec![];
        while inner.peek().unwrap().rule == Rule::type_param {
            let p = inner.next()?.clone();
            param.push(p);
        }
        let name = inner.next()?.clone();

        let body = if inner.peek().unwrap().rule == Rule::type_expr {
            TypeBody::Expr(inner.next()?.clone())
        } else {
            let mut variants = vec![];
            while inner.peek()?.rule == Rule::type_variant {
                let v = inner.next()?.clone();
                variants.push(v);
            }
            TypeBody::Variants(variants)
        };
        let constraint = inner.cloned().collect();
        Some(TypeInner {
            param,
            name,
            constraint,
            body,
        })
    }
}

pub enum TypeBody {
    Variants(Vec<Cst>),
    Expr(Cst),
}
