use super::*;

#[test]
fn variant_constructor() {
    assert_parsed!("None" variant_constructor: ["None" variant_name: []; ]);
    assert_parsed!("Some(1)" variant_constructor: [
        "Some" variant_name: [];
        "(1)" unary: ["1" expr: [_];];
    ]);
    assert_parsed!("Some 1" variant_constructor: [
        "Some" variant_name: [];
        "1" unary: ["1" constant, const_int: [];];
    ]);
    assert_parsed!("Paper(100pt, 50pt)" variant_constructor: [
        "Paper" variant_name: [];
        "(100pt, 50pt)" unary, tuple: [
            "100pt" expr: [_];
            "50pt" expr: [_];
        ];
    ]);

    assert_not_parsed!("Variant 1 2" variant_constructor: [_]);
}

#[test]
fn record_member() {
    assert_parsed!("foo#bar" record_member: ["foo" unary, var: []; "bar" var: []; ]);
    assert_parsed!("foo # bar" record_member: ["foo" unary, var: []; "bar" var: []; ]);
}

#[test]
fn command_application() {
    assert_parsed!(r"command \math" command_application: [
        r"\math" inline_cmd_name: ["math" cmd_name_ptn: []; ];
    ]);
    assert_not_parsed!(r"command +section" command_application: [_]);
}

#[test]
fn application_args_normal() {
    assert_parsed!("1" application_args_normal: ["1" unary: [_]; ]);
    assert_parsed!("foo" application_args_normal: ["foo" unary: [_]; ]);
    assert_parsed!("None" application_args_normal: ["None" variant_name: [_]; ]);
}

#[test]
fn application_args_optional() {
    assert_parsed!("?*" application_args_optional: []);
    assert_parsed!("?:1" application_args_optional: ["1" unary: [_];]);
    assert_parsed!("?:foo" application_args_optional: ["foo" unary: [_];]);
    assert_parsed!("?:(foo bar)" application_args_optional: ["(foo bar)" unary: [_];]);

    assert_not_parsed!("?:None" application_args_optional: [_]);
}

#[test]
fn application() {
    assert_parsed!("foo bar" application: [
        "foo" var: [];
        "bar" application_args_normal: [_];
    ]);
    assert_parsed!("foo ?:bar baz" application: [
        "foo" var: [];
        "?:bar" application_args_optional: [_];
        "baz" application_args_normal: [_];
    ]);
    assert_parsed!("Mod.foo bar" application: [
        "Mod.foo" modvar: [_];
        "bar" application_args_normal: [_];
    ]);
}

#[test]
fn unary_operator() {
    assert_parsed!("-" unary_operator: []);
    assert_parsed!("not" unary_operator: []);
}

#[test]
fn unary_operator_expr() {
    assert_parsed!("-1" unary_operator_expr: ["-" unary_operator: []; "1" unary: [_]; ]);
    assert_parsed!("not foo" unary_operator_expr: ["not" unary_operator: []; "foo" unary: [_]; ]);
    assert_not_parsed!("notfoo" unary_operator_expr: [_]);
}

#[test]
fn dyadic_expr() {
    assert_parsed!("2 + 3" dyadic_expr: ["2" unary: [_]; "+" bin_operator: []; "3" unary: [_]; ]);
    assert_parsed!("2 + 3 +4" dyadic_expr: [
        "2" unary: [_];
        "+" bin_operator: [];
        "3 +4" dyadic_expr: [
            "3" unary: [_];
            "+" bin_operator: [];
            "4" unary: [_];
        ];
    ]);
    assert_parsed!("-2 + 3" dyadic_expr: ["-2" unary_operator_expr: [_]; "+" bin_operator: []; "3" unary: [_]; ]);

    assert_parsed!("None +~ 3" dyadic_expr: ["None " variant_constructor: [_]; "+~" bin_operator: []; "3" unary: [_]; ]);
    assert_parsed!("Some(2) +~ 3" dyadic_expr: ["Some(2)" variant_constructor: [_]; "+~" bin_operator: []; "3" unary: [_]; ]);
    assert_parsed!("foo bar +~ 3" dyadic_expr: ["foo bar" application: [_]; "+~" bin_operator: []; "3" unary: [_]; ]);
    assert_parsed!("foo bar -. foo baz" dyadic_expr: ["foo bar" application: [_]; "-." bin_operator: []; "foo baz" application: [_]; ]);

    assert_not_parsed!("2 + - 3" dyadic_expr: [_]);
}

#[test]
fn assignment() {
    assert_parsed!("x <- 2" assignment: ["x" var: []; "2" unary: [_]; ]);
    assert_parsed!("x <- 2 + 3" assignment: ["x" var: []; "2 + 3" dyadic_expr: [_]; ]);

    assert_not_parsed!("x <- fun y -> 2" assignment: [_]);
    assert_not_parsed!("x <- if t then true else false" assignment: [_]);
}

#[test]
fn lambda() {
    assert_parsed!("fun x -> x + 1" lambda: ["x" pattern: [_]; "x + 1" dyadic_expr: [_]; ]);
    assert_parsed!("fun x y -> x + y" lambda: [
        "x" pattern: [_];
        "y" pattern: [_];
        "x + y" dyadic_expr: [_];
    ]);

    assert_not_parsed!("fun x y -> if x then x else y" lambda: [_]);
    assert_not_parsed!("funx -> x + 1" lambda: [_]);
}

#[test]
fn ctrl_if() {
    assert_parsed!("if x then y else z" ctrl_if: ["x" expr: [_]; "y" expr: [_]; "z" expr: [_]; ]);
    assert_parsed!("if if t then true else false then y else z" ctrl_if: [
        "if t then true else false" expr, ctrl_if: [
            "t" expr: [_];
            "true" expr: [_];
            "false" expr: [_];
        ];
        "y" expr: [_];
        "z" expr: [_];
    ]);
    assert_parsed!("if x then if foo bar then 1 + 2 else fun _ -> 1 else z" ctrl_if: [
        "x" expr: [_];
        "if foo bar then 1 + 2 else fun _ -> 1" expr, ctrl_if: [
            "foo bar" expr, application: [_];
            "1 + 2" expr, dyadic_expr: [_];
            "fun _ -> 1" expr, lambda: [_];
        ];
        "z" expr: [_];
    ]);
    assert_parsed!("if x then foo else t <- 2 + 3" ctrl_if: [
        "x" expr: [_];
        "foo" expr: [_];
        "t <- 2 + 3" expr, assignment: [_];
    ]);
}

#[test]
fn ctrl_while() {
    assert_parsed!("while foo n max do bar n" ctrl_while: [
        "foo n max" expr, application: [_];
        "bar n" expr, application: [_];
    ]);
}

#[test]
fn bind_stmt() {
    assert_parsed!("let x = 1 in x" bind_stmt: ["let x = 1" let_stmt: [_]; "x" expr: [_]; ]);
    assert_parsed!("let-rec aux x = x in aux" bind_stmt: ["let-rec aux x = x " let_rec_stmt: [_]; "aux" expr: [_]; ]);
    assert_parsed!(r"let-math \foo x = ${ab#x} in ${\foo{c}}" bind_stmt: [
        r"let-math \foo x = ${ab#x}" let_math_stmt: [_];
        r"${\foo{c}}" expr: [_];
    ]);
}

#[test]
fn match_arm() {
    assert_parsed!("Some(x) -> x" match_arm: ["Some(x)" match_ptn: [_]; "x" expr: [_]; ]);
    assert_parsed!("Some(x) when x < 5 -> x" match_arm: [
        "Some(x)" match_ptn: [_];
        "when x < 5" match_guard: [_];
        "x" expr: [_];
    ]);
    assert_not_parsed!("Some(x) -> x | None -> 1" match_arm: [_]);
}

#[test]
fn match_guard() {
    assert_parsed!("when x < 5" match_guard: ["x < 5" expr: [_]; ]);
}

#[test]
fn match_expr() {
    assert_parsed!("match x with | Some(x) -> x | None -> 1" match_expr: [
        "x" expr: [_];
        "Some(x) -> x" match_arm: [_];
        "None -> 1" match_arm: [_];
    ]);
}
