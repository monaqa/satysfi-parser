use super::*;

#[test]
fn math_cmd_expr_arg() {
    assert_parsed!("{}" math_cmd_expr_arg: ["" math_single: [];]);
    assert_parsed!("!{}" math_cmd_expr_arg: ["" horizontal_single: [];]);
    assert_parsed!("!<>" math_cmd_expr_arg: ["" vertical: [];]);
    // TODO: inner を埋める
    assert_parsed!("!(())" math_cmd_expr_arg: [_]);
    assert_parsed!("![]" math_cmd_expr_arg: [_]);
    assert_parsed!("!(||)" math_cmd_expr_arg: [_]);
}

#[test]
fn math_cmd_expr_option() {
    assert_parsed!("?:{}" math_cmd_expr_option: ["" math_single: [];]);
    assert_parsed!("?:!{}" math_cmd_expr_option: ["" horizontal_single: [];]);
    assert_parsed!("?:!<>" math_cmd_expr_option: ["" vertical: [];]);
    // TODO: inner を埋める
    assert_parsed!("?:!(())" math_cmd_expr_option: [_]);
    assert_parsed!("?:![]" math_cmd_expr_option: [_]);
    assert_parsed!("?:!(||)" math_cmd_expr_option: [_]);
}

#[test]
fn math_cmd_name() {
    assert_parsed!(r"\alpha" math_cmd_name: ["alpha" cmd_name_ptn: [];]);
    assert_parsed!(r"\let-inline" math_cmd_name: ["let-inline" cmd_name_ptn: [];]);
    assert_parsed!(r"\foo1-bar2" math_cmd_name: ["foo1-bar2" cmd_name_ptn: [];]);
    assert_parsed!(r"\Alpha" math_cmd_name: ["Alpha" cmd_name_ptn: [];]);
    assert_parsed!(r"\Foo.bar" math_cmd_name: ["Foo.bar" mod_cmd_name: [_];]);
    assert_not_parsed!(r"\-foo" math_cmd_name: [_]);
    assert_not_parsed!(r"\1foo" math_cmd_name: [_]);
}

#[test]
fn math_cmd() {
    assert_parsed!(r"\alpha" math_cmd, math_cmd_name: [_]);
    assert_parsed!(r"\frac{1}{2}" math_cmd: [
        r"\frac" math_cmd_name: ["frac" cmd_name_ptn: [];];
        "{1}" math_cmd_expr_arg: ["1" math_single, math_token, math_unary: [];];
        "{2}" math_cmd_expr_arg: ["2" math_single, math_token, math_unary: [];];
    ]);
}

#[test]
fn cmd_expr_arg() {
    assert_parsed!("()" cmd_expr_arg, const_unit: []);
    assert_parsed!("[]" cmd_expr_arg, list: [_]);
    assert_parsed!("(||)" cmd_expr_arg, record: [_]);
    // assert_parsed!("(a)" cmd_expr_arg: ["a" expr: [_];]);
    // TODO: expr
    assert_parsed!("(a)" cmd_expr_arg: [_]);
}

#[test]
fn cmd_expr_option() {
    assert_parsed!("?:()" cmd_expr_option: ["()" const_unit: [];]);
    assert_parsed!("?:[]" cmd_expr_option: ["[]" list: [];]);
    assert_parsed!("?:(||)" cmd_expr_option: ["(||)" record: [];]);
    // assert_parsed!("?:(a)" cmd_expr_option: ["a" expr: [_];]);
    // TODO: expr
    assert_parsed!("?:(a)" cmd_expr_option: [_]);
}

#[test]
fn cmd_text_arg() {
    assert_parsed!("{hoge}" cmd_text_arg: ["hoge" horizontal_single, regular_text: [];]);
    assert_parsed!("<>" cmd_text_arg: ["" vertical: [];]);
}

#[test]
fn block_cmd_name() {
    assert_parsed!("+p" block_cmd_name: ["p" cmd_name_ptn: [];]);
    assert_parsed!("+section" block_cmd_name: ["section" cmd_name_ptn: [];]);
    assert_parsed!("+StdJa.p" block_cmd_name: ["StdJa.p" mod_cmd_name: [_];]);
}

#[test]
fn block_cmd() {
    assert_parsed!("+p{ abc }" block_cmd: [
        "+p" block_cmd_name: [_];
        "{ abc }" cmd_text_arg: [" abc " horizontal_single, regular_text: []; ];
    ]);
    assert_parsed!("+section{#secname;}<+p{ abc }>" block_cmd: [
        "+section" block_cmd_name: [_];
        "{#secname;}" cmd_text_arg: [_];
        "<+p{ abc }>" cmd_text_arg: [
            "+p{ abc }" vertical, block_cmd: [
                "+p" block_cmd_name: [_];
                "{ abc }" cmd_text_arg: [" abc " horizontal_single, regular_text: []; ];
            ];
        ];
    ]);
}

#[test]
fn inline_cmd_name() {
    assert_parsed!(r"\q" inline_cmd_name: [r"q" cmd_name_ptn: []; ]);
    assert_parsed!(r"\emph" inline_cmd_name: [r"emph" cmd_name_ptn: []; ]);
    assert_parsed!(r"\emph-2" inline_cmd_name: [r"emph-2" cmd_name_ptn: []; ]);
    assert_parsed!(r"\Emph" inline_cmd_name: [r"Emph" cmd_name_ptn: []; ]);
    assert_parsed!(r"\StdJa.Emph" inline_cmd_name: [r"StdJa.Emph" mod_cmd_name: [_]; ]);
    assert_not_parsed!(r"\StdJa .Emph" inline_cmd_name: [_]);
    assert_not_parsed!(r"\StdJa. Emph" inline_cmd_name: [_]);
    assert_not_parsed!(r"\StdJa . Emph" inline_cmd_name: [_]);
}

#[test]
fn inline_cmd() {
    assert_parsed!(r"\a;" inline_cmd: [r"\a" inline_cmd_name: [_];]);
    assert_parsed!(r"\a{}" inline_cmd: [
        r"\a" inline_cmd_name: [_];
        "{}" cmd_text_arg: [_];
    ]);
}
