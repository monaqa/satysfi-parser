use super::*;

#[test]
fn math_unary() {
    assert_parsed!("A" math_unary: []);
    assert_parsed!("0" math_unary: []);
    assert_parsed!(r"\!" math_unary: []);
    assert_parsed!("+" math_unary: []);
    assert_parsed!(r"\bm{a}" math_unary: [r"\bm{a}" math_cmd: [_];]);
    assert_parsed!(r"\frac{1}{2}" math_unary: [r"\frac{1}{2}" math_cmd: [_];]);
}

#[test]
fn math_token() {
    assert_parsed!("a^b_c" math_token: [
        "a" math_unary: [];
        "b" math_sup, math_unary: [];
        "c" math_sub, math_unary: [];
    ]);
    assert_parsed!("a_b^c" math_token: [
        "a" math_unary: [];
        "b" math_sub, math_unary: [];
        "c" math_sup, math_unary: [];
    ]);
    assert_parsed!("a^b" math_token: [
        "a" math_unary: [];
        "b" math_sup, math_unary: [];
    ]);
    assert_parsed!("a_b" math_token: [
        "a" math_unary: [];
        "b" math_sub, math_unary: [];
    ]);
    assert_parsed!("a" math_token: [
        "a" math_unary: [];
    ]);

    assert_not_parsed!("a^^b" math_token: [_]);
    assert_not_parsed!("a^b^c" math_token: [_]);
}

#[test]
fn math_embedding() {
    assert_parsed!("#a" math_embedding: ["a" var_ptn: []; ]);
    assert_parsed!("#Foo.a" math_embedding: ["Foo.a" modvar: [_]; ]);
}

#[test]
fn math_single() {
    assert_parsed!("abc" math_single: [
        "a" math_token, math_unary: [];
        "b" math_token, math_unary: [];
        "c" math_token, math_unary: [];
    ]);
    assert_parsed!("ab c" math_single: [
        "a" math_token, math_unary: [];
        "b" math_token, math_unary: [];
        "c" math_token, math_unary: [];
    ]);
    assert_parsed!(r"\alpha\beta c" math_single: [
        r"\alpha" math_token, math_unary, math_cmd: [_];
        r"\beta " math_token, math_unary, math_cmd: [_];
        "c" math_token, math_unary: [];
    ]);
}

#[test]
fn math_list() {
    assert_parsed!("||" math_list: [ "" math_single: []; ]);
    assert_parsed!("| |" math_list: [ "" math_single: []; ]);
    assert_parsed!("| | |" math_list: [
        "" math_single: [];
        "" math_single: [];
    ]);
    assert_parsed!("| abc |" math_list: [
        "abc" math_single: [_];
    ]);
    assert_parsed!(r"| \alpha\beta c | abd | \frac{1}{2} |" math_list: [
        r"\alpha\beta c" math_single: [_];
        r"abd" math_single: [_];
        r"\frac{1}{2}" math_single: [_];
    ]);
}
