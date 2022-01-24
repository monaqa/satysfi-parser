#![allow(non_snake_case)]

use crate::cst;
use crate::types::rule::Rule;
use crate::types::Span;
use crate::types::{Cst, Vectorize};

/// 様々な型の変数をごちゃまぜに書くと、それをまとめて Vec<T> にしてくれる。
///
/// ```
/// #[macro_use] extern crate satysfi_parser;
/// use satysfi_parser::types::Vectorize;
/// let v: Vec<usize> = vectorize![1, vec![2, 3, 4], None, Some(5)];
/// assert_eq!(v, vec![1, 2, 3, 4, 5])
/// ```
macro_rules! vectorize {
    ($($e:expr),*) => {
        vec![$($e.vectorize()),*].vectorize()
    };
}

peg::parser! {
    grammar satysfi_parser() for str {
        use peg::ParseLiteral;
        // §1. common

        /// WHITESPACE
        rule _() = spaces()
        /// COMMENT
        /// 水平モードにおいて空白は重要な情報だが、コメントは無視できる。
        rule __() = comment()*

        pub rule spaces() -> Vec<Span> =
            ranges:([' ' | '\t' | '\n' | '\r'] {None} / range:comment() {Some(range)})*
            {vectorize![ranges]}

        /// alias for position
        rule p() -> usize = pos:position!() {pos}

        /// dummy pattern (never match)
        rule DUMMY() = !"DUMMY" "DUMMY"
        /// end of file
        rule EOF() = ![_]

        /// arbitrary character except line feed
        rule NON_LF() = !['\r' | '\n'] [_]

        /// constants
        rule ASCII_DIGIT() = quiet!{['0'..='9']} / expected!("number")
        rule ASCII_ALPHANUMERIC_HYPHEN() = quiet!{['a'..='z' | 'A'..='Z' | '0'..='9' | '-']} / expected!("alphabet, number, or hyphen")

        /// keywords
        /// 例えば kwd("let") とすると、
        /// "let" にはマッチするが "leti" や "let-inline" にはマッチしなくなる。
        rule kwd(word: &'static str) = ##parse_string_literal(word) !ASCII_ALPHANUMERIC_HYPHEN()

        pub rule misc() -> Cst = DUMMY() { cst!(misc (0, 0) []) }

        /// comment
        rule comment() -> Span = "%" s:p() comment_inner() e:p() ['\r' | '\n'] { Span{ start: s, end: e } }
        rule comment_inner() = NON_LF()*

        // §1. program

        pub rule program() -> Cst = p:(program_saty() / program_satyh()) EOF() {p}

        pub rule program_saty() -> Cst =
            s:p() _
            stage:header_stage()? _
            headers:headers() _
            pre:(pre:preamble() _ kwd("in") {pre})? _
            expr:expr() _
            e:p()
        { cst!(program_saty (s, e) [stage, headers, pre, expr]) }

        pub rule program_satyh() -> Cst =
            s:p() _
            stage:header_stage()? _
            headers:headers() _
            pre:preamble() _
            e:p()
        { cst!(program_satyh (s, e) [stage, headers, pre]) }

        // §1. header

        rule header_stage() -> Cst =
            "@stage:" [' ' | '\t']* stg:stage() [' ' | '\t']* ['\r' | '\n']
        {stg}

        pub rule stage() -> Cst =
            s:p() ("0" / "1" / "persistent") e:p()
        { cst!(stage (s, e) []) }

        pub rule headers() -> Cst =
            s:p() v:(h: header() _ {h})* e:p()
        { cst!(headers (s, e) [v]) }

        rule header() -> Cst = header_require() / header_import() / dummy_header()

        pub rule header_require() -> Cst =
            s:p() "@require:" [' ' | '\t']* pkg:pkgname() ['\n' | '\r'] e:p()
        { cst!(header_require (s, e) [pkg]) }

        pub rule header_import() -> Cst =
            s:p() "@import:" [' ' | '\t']* pkg:pkgname() ['\n' | '\r'] e:p()
        { cst!(header_import (s, e) [pkg]) }

        pub rule dummy_header() -> Cst =
            s:p()
            (
                ("@require:" / "@import") [' ' | '\t']* ['\n' | '\r']
                / "@" var_ptn() ['\n' | '\r']
                / "@" ['\n' | '\r']
            )
            e:p()
        { cst!(dummy_header (s, e) []) }

        pub rule pkgname() -> Cst =
            s:p() NON_LF()+ e:p()
        { cst!(pkgname (s, e) []) }

        // §1. statement

        pub rule preamble() -> Cst =
            s:p() stmts:(statement() ++ _) e:p()
        { cst!(preamble (s, e) [stmts]) }

        #[cache]
        rule statement() -> Cst =
            let_stmt()
            / let_rec_stmt()
            / let_inline_stmt()
            / let_block_stmt()
            / let_math_stmt()
            / let_mutable_stmt()
            / type_stmt()
            / module_stmt()
            / open_stmt()
            / dummy_stmt()

        pub rule let_stmt() -> Cst =
            s:p() kwd("let") _ pat:pattern() _ arg:let_stmt_argument()? _ "=" _ expr:expr() e:p()
        { cst!(let_stmt (s, e) [pat, arg, expr]) }

        rule let_stmt_argument() -> Vec<Cst> =
            // TODO: type_expr は本当は txfunc
            s:p() ":" _ t:type_expr() _ "|" _ a:(arg() ++ _) e:p() { vectorize![t, a] }
            / s:p() ":" _ t:type_expr() e:p() { vectorize![t] }
            / s:p() "|"? _ a:(arg() ++ _) e:p() { vectorize![a] }

        pub rule let_rec_stmt() -> Cst =
            s:p() kwd("let-rec") _ inner:let_rec_inner() _ inners:(let_rec_inner() ** (_ kwd("and") _)) e:p()
        { cst!(let_rec_stmt (s, e) [inner, inners]) }

        pub rule let_rec_inner() -> Cst =
            s:p()
            pat:pattern() _ arg:let_rec_stmt_argument()? _ "=" _ expr:expr()
            _ arms:("|" _ arm:let_rec_matcharm() _ {arm})*
            e:p()
            { cst!(let_rec_inner (s, e) [pat, arg, expr, arms]) }
            /
            s:p()
            pat:pattern() _ arms:("|" _ arm:let_rec_matcharm() _ {arm})*
            e:p()
            { cst!(let_rec_inner (s, e) [pat, arms]) }

        rule let_rec_stmt_argument() -> Vec<Cst> =
            s:p() ":" _ t:type_expr() _ "|" _ a:(arg() ++ _) e:p() { vectorize![t, a] }
            / s:p() ":" _ t:type_expr() e:p() { vectorize![t] }
            / s:p() a:(arg() ++ _) e:p() { vectorize![a] }

        pub rule let_rec_matcharm() -> Cst =
            s:p() a:(arg() ** _) _ "=" _ expr:expr() e:p()
        { cst!(let_rec_matcharm (s, e) [a, expr]) }

        rule let_inline_stmt() -> Cst = let_inline_stmt_ctx() / let_inline_stmt_noctx()
        pub rule let_inline_stmt_ctx() -> Cst =
            s:p() kwd("let-inline") _ ctx:var() _ cmd:inline_cmd_name() _ a:(arg() ** _) _ "=" _ expr:expr() e:p()
        { cst!(let_inline_stmt_ctx (s, e) [ctx, cmd, a, expr]) }
        pub rule let_inline_stmt_noctx() -> Cst =
            s:p() kwd("let-inline") _ cmd:inline_cmd_name() _ pat:(pattern() ** _) _ "=" _ expr:expr() e:p()
        { cst!(let_inline_stmt_noctx (s, e) [cmd, pat, expr]) }

        rule let_block_stmt() -> Cst = let_block_stmt_ctx() / let_block_stmt_noctx()
        pub rule let_block_stmt_ctx() -> Cst =
            s:p() kwd("let-block") _ ctx:var() _ cmd:block_cmd_name() _ a:(arg() ** _) _ "=" _ expr:expr() e:p()
        { cst!(let_block_stmt_ctx (s, e) [ctx, cmd, a, expr]) }
        pub rule let_block_stmt_noctx() -> Cst =
            s:p() kwd("let-block") _ cmd:block_cmd_name() _ pat:(pattern() ** _) _ "=" _ expr:expr() e:p()
        { cst!(let_block_stmt_noctx (s, e) [cmd, pat, expr]) }

        pub rule let_math_stmt() -> Cst =
            s:p() kwd("let-math") _ cmd:math_cmd_name() _ a:(arg() ** _) _ "=" _ expr:expr() e:p()
        { cst!(let_math_stmt (s, e) [cmd, a, expr]) }

        pub rule let_mutable_stmt() -> Cst =
            s:p() kwd("let-mutable") _ var:var() _ "<-" _ expr:expr() e:p()
        { cst!(let_mutable_stmt (s, e) [var, expr]) }

        pub rule type_stmt() -> Cst =
            s:p() kwd("type") _ ts:(type_inner() ++ (_ kwd("and") _)) e:p()
        { cst!(type_stmt (s, e) [ts]) }

        pub rule type_inner() -> Cst =
            s:p()
            tp:(type_param() ** _) _ t:type_name() _ "=" tvs:(_ "|" _ tv:type_variant() {tv})+ _ c:constraint()*
            e:p()
            { cst!(type_inner (s, e) [tp, t, tvs, c]) }
            /
            s:p()
            tp:(type_param() ** _) _ t:type_name() _ "=" _ tvs:(type_variant() ++  (_ "|" _)) _ c:constraint()*
            e:p()
            { cst!(type_inner (s, e) [tp, t, tvs, c]) }
            /
            s:p()
            tp:(type_param() ** _) _ t:type_name() _ "=" _ te:type_expr() _ c:constraint()*
            e:p()
            { cst!(type_inner (s, e) [tp, t, te, c]) }

        pub rule type_variant() -> Cst =
            s:p() v:variant_name() _ kwd("of") _ t:type_expr() e:p() { cst!(type_variant (s, e) [v, t]) }
            / s:p() v:variant_name() e:p() { cst!(type_variant (s, e) [v]) }

        pub rule open_stmt() -> Cst =
            s:p() kwd("open") _ m:module_name() e:p()
        { cst!(open_stmt (s, e) [m]) }

        pub rule dummy_stmt() -> Cst =
            s:p()
            (kwd("let") / kwd("module") / kwd("type")) _ var() ** _
            e:p()
        { cst!(dummy_stmt (s, e) []) }

        pub rule arg() -> Cst =
            s:p() p:pattern() e:p() { cst!(arg (s, e) [p]) }
            / s:p() "?:" _ p:var_ptn() e:p() { cst!(arg (s, e) [p]) }

        // §1. module

        pub rule module_stmt() -> Cst =
            s:p()
            kwd("module") _ n:module_name() _
            sig:(":" _ s:sig_stmt() {s})? _
            "=" _ stmt:struct_stmt()
            e:p()
        { cst!(module_stmt (s, e) [n, sig, stmt]) }

        pub rule sig_stmt() -> Cst =
            s:p() kwd("sig") _ inners:(sig_inner() ** _) _ kwd("end") e:p()
        { cst!(sig_stmt (s, e) [inners]) }

        pub rule struct_stmt() -> Cst =
            s:p() kwd("struct") _ stmts:(statement() ** _) _ kwd("end") e:p()
        { cst!(struct_stmt (s, e) [stmts]) }

        rule sig_inner() -> Cst = sig_type_stmt() / sig_val_stmt() / sig_direct_stmt() / dummy_sig_stmt()

        pub rule sig_type_stmt() -> Cst =
            s:p() kwd("type") _ tps:(type_param() ** _) _ v:var() _ cs:(constraint() ** _) e:p()
        { cst!(sig_type_stmt (s, e) [tps, v, cs]) }

        pub rule sig_val_stmt() -> Cst =
            s:p()
            kwd("val") _
            v:(var() / "(" _ b:bin_operator() _ ")" {b} / inline_cmd_name() / block_cmd_name()) _
            ":" _ t: type_expr() _ cs:(constraint() ** _)
            e:p()
        { cst!(sig_val_stmt (s, e) [v, t, cs]) }

        pub rule sig_direct_stmt() -> Cst =
            s:p()
            kwd("direct") _
            cmd:(inline_cmd_name() / block_cmd_name()) _
            ":" _ t: type_expr() _ cs:(constraint() ** _)
            e:p()
        { cst!(sig_direct_stmt (s, e) [cmd, t, cs]) }

        pub rule dummy_sig_stmt() -> Cst =
            s:p()
            !kwd("end") _ var_ptn()
            e:p()
        { cst!(dummy_sig_stmt (s, e) []) }

        // §1. types

        pub rule type_expr() -> Cst =
            s:p()
            typeprods:(
                t:type_optional() _ "?->" _ {t}
                /
                t:type_prod() _ "->" _ {t}
            )* _
            typeprod:type_prod()
            e:p()
        { cst!(type_expr (s, e) [typeprods, typeprod]) }

        pub rule type_optional() -> Cst = s:p() t:type_prod() e:p() { cst!(type_optional (s, e) [t.inner]) }

        pub rule type_prod() -> Cst =
            s:p() t:type_unary() ts:(_ "*" _ t:type_unary() {t})* e:p()
        { cst!(type_prod (s, e) [t, ts]) }

        #[cache]
        rule type_unary() -> Cst =
            type_inline_cmd()
            / type_block_cmd()
            / type_math_cmd()
            / type_application()
            / "(" _ t:type_expr() _ ")" {t}
            / type_record()
            / type_param()

        pub rule type_inline_cmd() -> Cst = s:p() t:type_list() _ kwd("inline-cmd") e:p() { cst!(type_inline_cmd (s, e) [t]) }
        pub rule type_block_cmd() -> Cst = s:p() t:type_list() _ kwd("block-cmd") e:p() { cst!(type_block_cmd (s, e) [t]) }
        pub rule type_math_cmd() -> Cst = s:p() t:type_list() _ kwd("math-cmd") e:p() { cst!(type_math_cmd (s, e) [t]) }

        rule type_list() -> Vec<Cst> =
            s:p() "[" _ "]" e:p() { vec![] }
            / s:p() "[" _ t:type_list_unit()
                ts:(_ ";" _ t:type_list_unit() {t})* _
                ";"? _ "]" e:p()
                { vectorize![t, ts] }

        rule type_list_unit() -> Cst =
            type_list_unit_optional()
            / type_expr()

        pub rule type_list_unit_optional() -> Cst =
            s:p() t:type_prod() _ "?" e:p()
        { cst!(type_list_unit_optional (s, e) [t]) }

        pub rule type_application() -> Cst =
            s:p() t:type_application_unit() _ ts:(type_application_unit()** _) e:p()
        { cst!(type_application (s, e) [t, ts]) }

        rule type_application_unit() -> Cst =
            "(" _ t:type_expr() _ ")" {t}
            / type_param()
            / type_name()

        pub rule type_name() -> Cst =
            s:p() t:(var() / modvar()) e:p()
        { cst!(type_name (s, e) [t]) }

        pub rule type_record() -> Cst =
            s:p() "(|" _ "|)" e:p() { cst!(type_record (s, e) []) }
            / s:p() "(|" _ ts:(type_record_unit() ++ (_ ";" _)) _ ";"? _ "|)" e:p() { cst!(type_record (s, e) [ts]) }

        pub rule type_record_unit() -> Cst =
            s:p() v:var() _ ":" _ t:type_expr() e:p()
        { cst!(type_record_unit (s, e) [v, t]) }

        pub rule type_param() -> Cst =
            s:p() "'" t:var_ptn() e:p()
        { cst!(type_param (s, e) [t]) }

        pub rule constraint() -> Cst =
            s:p() kwd("constraint") _ t:type_param() _ "::" _ r:type_record() e:p()
        { cst!(constraint (s, e) [t, r]) }

        // §1. pattern

        pub rule pat_as() -> Cst =
            s:p() p:pat_cons() v:(_ kwd("as") _ v:var() {v})? e:p() { cst!(pat_as (s, e) [p, v]) }

        #[cache]
        pub rule pat_cons() -> Cst =
            s:p() p:pattern() _ "::" _ m:pat_as() e:p() { cst!(pat_cons (s, e) [p, m]) }
            / s:p() p:pat_variant() e:p() { cst!(pat_cons (s, e) [p]) }
            / s:p() p:pattern() e:p() { cst!(pat_cons (s, e) [p]) }

        #[cache]
        pub rule pattern() -> Cst =
            s:p() p:pat_list() e:p() { cst!(pattern (s, e) [p]) }
            / s:p() "(" _ b:bin_operator() _ ")" e:p() { cst!(pattern (s, e) [b]) }
            / s:p() "(" _ u:unary_operator() _ ")" e:p() { cst!(pattern (s, e) [u]) }
            / s:p() "(" _ p:pat_as() _ ")" e:p() { cst!(pattern (s, e) [p]) }
            / s:p() p:pat_tuple() e:p() { cst!(pattern (s, e) [p]) }
            / s:p() "_" e:p() { cst!(pattern (s, e) []) }
            / s:p() v:var() e:p() { cst!(pattern (s, e) [v]) }
            / s:p() l:constant() e:p() { cst!(pattern (s, e) [l]) }

        pub rule pat_variant() -> Cst =
            s:p() v:variant_name() _ p:pattern()? e:p()
        { cst!(pat_variant (s, e) [v, p]) }

        pub rule pat_list() -> Cst =
            s:p() "[" _ "]" e:p() { cst!(pat_list (s, e) []) }
            / s:p() "[" _ ms:(pat_as() ** (_ ";" _)) _ ";"? _ "]" e:p() { cst!(pat_list (s, e) [ms]) }

        pub rule pat_tuple() -> Cst =
            s:p() "(" _ m:pat_as() _ "," _ ms:(pat_as() ++ (_ "," _)) _ ")" e:p()
            { cst!(pat_tuple (s, e) [m, ms]) }

        // §1. expr
        pub rule expr() -> Cst =
            s:p() expr:(
                match_expr()
                / bind_stmt()
                / ctrl_while()
                / ctrl_if()
                / lambda()
                / assignment()
                / dyadic_expr()
                / unary_operator_expr()
                / command_application()
                / application()
                / unary()
                / variant_constructor()
            ) e:p()
        { cst!(expr (s, e) [expr]) }

        pub rule match_expr() -> Cst =
            s:p() kwd("match") _ expr:expr() _ kwd("with") _ "|"? _ arms:(match_arm() ** (_ "|" _)) e:p()
        { cst!(match_expr (s, e) [expr, arms]) }

        pub rule bind_stmt() -> Cst =
            s:p() stmt:(let_stmt() / let_rec_stmt() / let_math_stmt() / let_mutable_stmt() / open_stmt())
            _ kwd("in") _ expr:expr() e:p()
        { cst!(bind_stmt (s, e) [stmt, expr]) }

        pub rule match_arm() -> Cst =
            s:p()
            ptn:pat_as() _ guard:match_guard()? _ "->" _ expr:(!match_expr() e:expr() {e})
            e:p()
            { cst!(match_arm (s, e) [ptn, guard, expr]) }

        pub rule match_guard() -> Cst =
            s:p()
            kwd("when") _ cond:(!match_expr() e:expr() {e})
            e:p()
            { cst!(match_guard (s, e) [cond]) }

        pub rule ctrl_while() -> Cst =
            s:p() kwd("while") _ cond:expr() _ kwd("do") _
            expr:(!(match_expr() / bind_stmt()) e:expr() {e})
            e:p()
        { cst!(ctrl_while (s, e) [cond, expr]) }

        pub rule ctrl_if() -> Cst =
            s:p() kwd("if") _ cond:expr() _ kwd("then") _ et:expr() _ kwd("else") _ ee:expr() e:p()
        { cst!(ctrl_if (s, e) [cond, et, ee]) }

        pub rule lambda() -> Cst =
            s:p() kwd("fun") _ ptns:(pattern() ++ _) _ "->" _ expr: expr_inner_lambda() e:p()
        { cst!(lambda (s, e) [ptns, expr]) }

        pub rule assignment() -> Cst =
            s:p() v:var() _ "<-" _ expr: expr_inner_lambda() e:p()
        { cst!(assignment (s, e) [v, expr]) }

        rule expr_inner_lambda() -> Cst =
            dyadic_expr() / unary_operator_expr() / application() / unary() / variant_constructor()

        pub rule dyadic_expr() -> Cst =
            s:p()
            t1:(unary_operator_expr() / application() / unary() / variant_constructor())
            _ op:bin_operator()
            _ t2:(dyadic_expr() / application() / unary() / variant_constructor())
            e:p()
        { cst!(dyadic_expr (s, e) [t1, op, t2]) }

        pub rule unary_operator_expr() -> Cst =
            s:p() op:unary_operator() _ expr:(application() / unary()) e:p()
        { cst!(unary_operator_expr (s, e) [op, expr]) }

        pub rule unary_operator() -> Cst =
            s:p() ("-" / kwd("not") ) e:p()
        { cst!(unary_operator (s, e) []) }

        pub rule application() -> Cst =
            s:p()
            v:(unary()) _
            args:(application_args() ++ _)
            e:p()
        { cst!(application (s, e) [v, args]) }

        rule application_args() -> Cst = application_args_optional() / application_args_normal()

        pub rule application_args_optional() -> Cst =
            s:p() "?:" _ u:(unary()) e:p() { cst!(application_args_optional (s, e) [u]) }
            / s:p() "?*" e:p() { cst!(application_args_optional (s, e) []) }

        pub rule application_args_normal() -> Cst =
            s:p() u:(unary() / variant_name()) e:p()
        { cst!(application_args_normal (s, e) [u]) }

        pub rule command_application() -> Cst =
            s:p() kwd("command") _ n:inline_cmd_name() e:p()
        { cst!(command_application (s, e) [n]) }

        pub rule variant_constructor() -> Cst =
            s:p() v:variant_name() _ u:unary()? e:p()
        { cst!(variant_constructor (s, e) [v, u]) }

        // §1. unary

        #[cache]
        pub rule unary() -> Cst =
            s:p()
            prefix:(p:unary_prefix() _ {p})?
            body:((
                block_text()
                / horizontal_text()
                / math_text()
                / record()
                / list()
                / tuple()
                / "(" _ op:bin_operator() _ ")" {op}
                / "(" _ expr:expr() _ ")" {expr}
                / constant()
                / expr_with_mod()
                / modvar()
                / var()
                / dummy_modvar_incomplete()
            ) ++ (_ "#" _))
            e:p()
        { cst!(unary (s, e) [prefix, body]) }

        pub rule unary_prefix() -> Cst = s:p() ("!" / "&" / "~") e:p() { cst!(unary_prefix (s, e) []) }

        pub rule block_text() -> Cst =
            s:p() "'" v:vertical() e:p()
        { cst!(block_text (s, e) [v]) }

        pub rule horizontal_text() -> Cst =
            s:p() h:horizontal() e:p()
        { cst!(horizontal_text (s, e) [h]) }

        pub rule math_text() -> Cst =
            s:p() "${" _ m:math() _ "}" e:p()
        { cst!(math_text (s, e) [m]) }

        pub rule record() -> Cst =
            s:p() "(|" _ "|)" e:p() { cst!(record (s, e) []) }
            / s:p() "(|" _ u:unary() _ kwd("with") _ i:record_inner() _ "|)" e:p() { cst!(record (s, e) [u, i]) }
            / s:p() "(|" _ i:record_inner() _ "|)" e:p() { cst!(record (s, e) [i]) }

        rule record_inner() -> Vec<Cst> =
            s:p() units:(record_unit() ++ (_ ";" _)) _ ";"? e:p()
        { units }

        pub rule record_unit() -> Cst =
            s:p() v:var_ptn() _ "=" _ expr:expr() e:p()
        { cst!(record_unit (s, e) [v, expr]) }

        pub rule list() -> Cst =
            s:p() "[" _ "]" e:p() { cst!(list (s, e) []) }
            / s:p() "[" _ exprs:(expr() ++ (_ ";" _)) _ ";"? _ "]" e:p() { cst!(list (s, e) [exprs]) }

        pub rule tuple() -> Cst =
            s:p() "(" _ expr:expr() _ "," _ exprs:(expr() ++ (_ "," _)) _ ")" e:p() { cst!(tuple (s, e) [expr, exprs]) }

        pub rule bin_operator() -> Cst =
            s:p() !bin_operator_except() (bin_operator_start() bin_operator_succ()* / "::" / kwd("mod")) e:p()
        { cst!(bin_operator (s, e) []) }
        rule bin_operator_start() =
            [ '-' | '+' | '*' | '/' | '^' | '&' | '|' | '=' | '<' | '>' ]
        rule bin_operator_succ() =
            [ '-' | '+' | '*' | '/' | '^' | '&' | '|' | '=' | '<' | '>'
                  | '!' | ':' | '~' | '\'' | '.' | '?' ]
        rule bin_operator_except() = ("->" / "<-" / "|") !bin_operator_succ()

        pub rule expr_with_mod() -> Cst =
            s:p() m:module_name() ".(" _ expr:expr() _ ")" e:p()
        { cst!(expr_with_mod (s, e) [m, expr]) }

        pub rule var() -> Cst =
            s:p() !(reserved_word() !ASCII_ALPHANUMERIC_HYPHEN()) var_ptn() e:p()
        { cst!(var (s, e) []) }

        pub rule var_ptn() -> Cst =
            s:p() ['a'..='z'] ASCII_ALPHANUMERIC_HYPHEN()* e:p()
        { cst!(var_ptn (s, e) []) }

        pub rule dummy_modvar_incomplete() -> Cst =
            s:p() module_name() "." e:p()
            { cst!(dummy_modvar_incomplete (s, e) []) }

        // 予約語たち
        rule reserved_word() =
            ( "constraint"
            / "inline-cmd" / "block-cmd" / "math-cmd"
            / "let-mutable" / "let-inline" / "let-block" / "let-math" / "let-rec"
            / "controls" / "command" / "before" / "module" / "direct" / "struct"
            / "cycle" / "match" / "while" / "false"
            / "else" / "open" / "then" / "true" / "type" / "when" / "with"
            / "and" / "end" / "fun" / "let" / "mod" / "not" / "sig" / "val"
            / "as" / "do" / "if" / "in" / "of")


        pub rule modvar() -> Cst =
            s:p() modname:module_name() "." var:var_ptn() e:p()
        { cst!(modvar (s, e) [modname, var]) }

        pub rule mod_cmd_name() -> Cst =
            s:p() modname:module_name() "." var:cmd_name_ptn() e:p()
        { cst!(mod_cmd_name (s, e) [modname, var]) }

        pub rule module_name() -> Cst =
            s:p() ['A' ..= 'Z'] ASCII_ALPHANUMERIC_HYPHEN()* e:p()
        { cst!(module_name (s, e) []) }

        pub rule variant_name() -> Cst =
            s:p() ['A' ..= 'Z'] ASCII_ALPHANUMERIC_HYPHEN()* e:p()
        { cst!(variant_name (s, e) []) }

        // §1. constants
        rule constant() -> Cst =
            const_unit()
            / const_bool()
            / const_string()
            / const_float()
            / const_length()
            / const_int()

        pub rule const_unit() -> Cst =
            s:p() "(" _ ")" e:p() { cst!(const_unit (s, e) []) }

        pub rule const_bool() -> Cst =
            s:p() (kwd("true") / kwd("false")) e:p() {cst!(const_bool (s, e) [])}

        pub rule const_int() -> Cst =
            s:p() (const_int_decimal() / const_int_hex() / "0") e:p() { cst!(const_int (s, e) []) }
        rule const_int_decimal() = ['1'..='9'] ASCII_DIGIT()*
        rule const_int_hex() = "0" ['x' | 'X'] ['0'..='9' | 'A'..='F']+

        pub rule const_float() -> Cst =
            s:p() const_float_inner() !length_unit() e:p()
            { cst!(const_float (s, e) []) }
        rule const_float_inner() = ASCII_DIGIT()+ "." ASCII_DIGIT()* / "." ASCII_DIGIT()+

        pub rule const_length() -> Cst =
            s:p() length_digit() length_unit() e:p()
            { cst!(const_length (s, e) []) }

        rule length_digit() = "-"? (const_float_inner() / const_int_decimal() / "0")
        rule length_unit() = ['a'..='z'] ['0'..='9' | 'a'..='z' | 'A' ..='Z' | '-']*

        pub rule const_string() -> Cst =
            s:p() "@"? "#"? qs:string_quotes() (!string_inner(qs) [_])+ qe:string_quotes() "#"? e:p()
            {?
                if qs == qe {
                    Ok(cst!(const_string (s, e) []))
                } else {
                    Err("number of back quotation does not match.")
                }
            }
        rule string_quotes() -> usize = n:"`"+ {n.len()}
        rule string_inner(qs: usize) = "`"*<{qs}>

        // §1. commands
        pub rule inline_cmd() -> Cst = (
            s:p() name:inline_cmd_name() _
            opt:((cmd_expr_arg() / cmd_expr_option())** _) _ opttext:(";" {vec![]} / cmd_text_arg() ++ _) e:p()
            {
                cst!(inline_cmd (s, e) [name, opt, opttext])
            }
        )

        pub rule inline_cmd_name() -> Cst =
            s:p() r"\" inner:(mod_cmd_name() / cmd_name_ptn()) "@"? e:p()
        { cst!(inline_cmd_name (s, e) [inner]) }

        pub rule cmd_name_ptn() -> Cst =
            s:p() ['A'..='Z' | 'a'..='z'] ASCII_ALPHANUMERIC_HYPHEN()* e:p()
        { cst!(cmd_name_ptn (s, e) []) }

        pub rule block_cmd() -> Cst = (
            s:p() name:block_cmd_name() _
            opt:((cmd_expr_arg() / cmd_expr_option())** _) _ opttext:(";" {vec![]} / cmd_text_arg() ++ _) e:p()
            {
                cst!(block_cmd (s, e) [name, opt, opttext])
            }
        )

        pub rule block_cmd_name() -> Cst =
            s:p() "+" inner:(mod_cmd_name() / cmd_name_ptn()) "@"? e:p()
        { cst!(block_cmd_name (s, e) [inner]) }

        pub rule cmd_expr_arg() -> Cst =
            s:p() inner:cmd_expr_arg_inner() e:p() { cst!(cmd_expr_arg (s, e) [inner]) }

        pub rule cmd_expr_option() -> Cst =
            s:p() "?:" _ inner:cmd_expr_arg_inner() e:p() { cst!(cmd_expr_option (s, e) [inner]) }
            / s:p() "?*" e:p() { cst!(cmd_expr_option (s, e) []) }

        rule cmd_expr_arg_inner() -> Cst =
            const_unit()
            / list()
            / record()
            / "~"? "(" _ expr:expr() _ ")" { expr }

        pub rule cmd_text_arg() -> Cst =
            s:p()
            inner:(vertical() / horizontal())
            e:p()
        { cst!(cmd_text_arg (s, e) [inner]) }

        pub rule math_cmd() -> Cst =
            s:p() cmd:math_cmd_name() _ args:((math_cmd_expr_arg() / math_cmd_expr_option()) ** _) e:p()
        { cst!(math_cmd (s, e) [cmd, args]) }

        pub rule math_cmd_name() -> Cst =
            s:p() r"\" inner:(mod_cmd_name() / cmd_name_ptn()) e:p()
        { cst!(math_cmd_name (s, e) [inner]) }

        pub rule math_cmd_expr_arg() -> Cst =
            s:p() arg:math_cmd_expr_arg_inner() e:p() { cst!(math_cmd_expr_arg (s, e) [arg]) }

        pub rule math_cmd_expr_option() -> Cst =
            s:p() "?:" _ arg:math_cmd_expr_arg_inner() e:p() { cst!(math_cmd_expr_option (s, e) [arg]) }

        rule math_cmd_expr_arg_inner() -> Cst =
            "{" _ m:math() _ "}" {m}
            / "!" h:horizontal() {h}
            / "!" v:vertical() {v}
            / "!(" _ expr:expr() _ ")" {expr}
            / "!" l:list() {l}
            / "!" r:record() {r}

        // rule math_cmd_list_arg() -> Cst =
        //     s:p() "![" _ exprs:(expr() ** (_ ";" _)) _ "]" e:p() { cst!((s, e) [exprs]) }
        //
        // rule math_cmd_record_arg() -> Cst =
        //     s:p() "!(" _ "|" _ "|" _ ")" e:p() { cst!((s, e)) }
        //     / s:p() "!(" _ "|" _ u:unary() _ "with" _ i:record_inner() _ "|" _ ")" e:p() { cst!((s, e) [u, i]) }
        //     / s:p() "!(" _ "|" _ i:record_inner() _ "|" _ ")" e:p() { cst!((s, e) [i]) }

        // §1. horizontal mode
        rule horizontal() -> Cst =
            // horizontal の前後空白は regular_text で食う
            "{"
            h:(
            horizontal_list()
            / horizontal_bullet_list()
            / horizontal_single()
            )
            "}"
            {h}

        pub rule horizontal_single() -> Cst =
            s:p() __ inners:(horizontal_token() ** __) __ e:p() { cst!(horizontal_single (s, e) [inners]) }

        rule horizontal_token() -> Cst =
            const_string()
            / inline_cmd()
            / inline_text_embedding()
            / math_text()
            / horizontal_escaped_char()
            / regular_text()
            / dummy_inline_cmd_incomplete()

        pub rule inline_text_embedding() -> Cst =
            s:p() "#" inner:(var_ptn() / modvar()) _ ";" e:p()
        { cst!(inline_text_embedding (s, e) [inner]) }

        pub rule regular_text() -> Cst =
            s:p() $(!horizontal_special_char() [_])+ e:p()
        { cst!(regular_text (s, e) []) }

        pub rule dummy_inline_cmd_incomplete() -> Cst =
            s:p()
            "\\" (var_ptn() / modvar() / dummy_modvar_incomplete())?
            e:p()
        { cst!(dummy_inline_cmd_incomplete (s, e) []) }

        pub rule horizontal_escaped_char() -> Cst =
            s:p() "\\" horizontal_special_char() e:p()
        { cst!(horizontal_escaped_char (s, e) []) }

        rule horizontal_special_char() =
            ['@' | '`' | '\\' | '{' | '}' | '%' | '|' | '*' | '$' | '#' | ';']

        pub rule horizontal_list() -> Cst =
            s:p() _ "|" inners:horizontal_list_inner()+ _ e:p()
            { cst!(horizontal_list (s, e) [inners]) }
        rule horizontal_list_inner() -> Cst = inner:horizontal_single() "|" {inner}

        pub rule horizontal_bullet_list() -> Cst =
            s:p() inners:horizontal_bullet()+ e:p() { cst!(horizontal_bullet_list (s, e) [inners]) }

        pub rule horizontal_bullet() -> Cst =
            s:p()
            _ star:horizontal_bullet_star() _ single:horizontal_single() _
            e:p()
            { cst!(horizontal_bullet (s, e) [star, single]) }

        pub rule horizontal_bullet_star() -> Cst =
            s:p() "*"+ e:p()
            { cst!(horizontal_bullet_star (s, e) []) }

        // §1. vertical mode

        pub rule vertical() -> Cst =
            s:p() "<" _ vs:(vertical_element() ** _) _ ">" e:p()
        { cst!(vertical (s, e) [vs]) }

        rule vertical_element() -> Cst = block_cmd() / block_text_embedding() / dummy_block_cmd_incomplete()

        pub rule block_text_embedding() -> Cst =
            s:p() "#" inner:(var_ptn() / modvar()) _ ";" e:p()
        { cst!(block_text_embedding (s, e) [inner]) }

        pub rule dummy_block_cmd_incomplete() -> Cst =
            s:p()
            "+" (var_ptn() / modvar() / dummy_modvar_incomplete())?
            e:p()
        { cst!(dummy_block_cmd_incomplete (s, e) []) }

        // §1. math mode

        rule math() -> Cst = math_list() / math_single()

        pub rule math_list() -> Cst =
            s:p() "|" ms:(_ m:math_single() _ "|" {m})+ e:p() { cst!(math_list (s, e) [ms]) }

        pub rule math_single() -> Cst =
            s:p() ts:(math_token() ** _) e:p()
        { cst!(math_single (s, e) [ts]) }

        pub rule math_token() -> Cst =
            s:p() u:math_unary() _ sup:math_sup() _ sub:math_sub() e:p() { cst!(math_token (s, e) [u, sup, sub]) }
            / s:p() u:math_unary() _ sub:math_sub() _ sup:math_sup() e:p() { cst!(math_token (s, e) [u, sub, sup]) }
            / s:p() u:math_unary() _ sup:math_sup() e:p() { cst!(math_token (s, e) [u, sup]) }
            / s:p() u:math_unary() _ sub:math_sub() e:p() { cst!(math_token (s, e) [u, sub]) }
            / s:p() u:math_unary() e:p() { cst!(math_token (s, e) [u]) }

        pub rule math_sup() -> Cst = "^" s:p() _ g:math_group() e:p() { cst!(math_sup (s, e) [g]) }
        pub rule math_sub() -> Cst = "_" s:p() _ g:math_group() e:p() { cst!(math_sub (s, e) [g]) }
        rule math_group() -> Cst = "{" _ m:math_single() _ "}" {m} / math_unary()

        pub rule math_unary() -> Cst =
            // TODO: allow unicode characters
            s:p() ['A'..='Z' | 'a'..='z' | '0'..='9'] e:p() { cst!(math_unary (s, e) []) }
            / s:p() r"\" math_special_char() e:p() { cst!(math_unary (s, e) []) }
            / s:p() math_symbol() e:p() { cst!(math_unary (s, e) []) }
            / s:p() m:math_cmd() e:p() { cst!(math_unary (s, e) [m]) }
            / s:p() m:math_embedding() e:p() { cst!(math_unary (s, e) [m]) }

        pub rule math_embedding() -> Cst =
            s:p() "#" inner:(var_ptn() / modvar()) e:p()
        { cst!(math_embedding (s, e) [inner]) }

        rule math_special_char() = [
            ' ' | '!' | '"' | '#' | '$' | '%' | '&' | '\''
                | '(' | ')' | '*' | '+' | ',' | '-' | '.' | '/'
                | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
                | ':' | ';' | '<' | '=' | '>' | '?' | '@' | '[' | '\\'
                | ']' | '^' | '_' | '`' | '{' | '|' | '}' | '~'
            ]
        rule math_symbol() =
            ['-' | '+' | '*' | '/' | ':' | '=' | '<' | '>' | '~' | '\'' | '.' | ',' | '?' | '`']+

    }
}

pub use self::satysfi_parser::*;
