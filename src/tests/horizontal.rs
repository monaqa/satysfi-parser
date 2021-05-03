use super::*;

#[test]
fn horizontal_bullet_star() {
    assert_parsed!("***" horizontal_bullet_star: []);
    assert_not_parsed!("" horizontal_bullet_star: [_]);
}

#[test]
fn horizontal_bullet_list() {
    assert_parsed!("* foo" horizontal_bullet_list: [
        "* foo" horizontal_bullet: [
            "*" horizontal_bullet_star: [];
            "foo" horizontal_single, regular_text: [];
        ];
    ]);
    assert_parsed!("* foo ** bar" horizontal_bullet_list: [
        "* foo " horizontal_bullet: [
            "*" horizontal_bullet_star: [];
            "foo " horizontal_single, regular_text: [];
        ];
        "** bar" horizontal_bullet: [
            "**" horizontal_bullet_star: [];
            "bar" horizontal_single, regular_text: [];
        ];
    ]);
}

#[test]
fn horizontal_list() {
    assert_parsed!("||" horizontal_list: ["" horizontal_single: []; ]);
    assert_parsed!("|aaa|" horizontal_list: [ "aaa" horizontal_single, regular_text: []; ]);
    assert_parsed!("|aaa| bbb |" horizontal_list: [
        "aaa" horizontal_single, regular_text: [];
        "bbb " horizontal_single, regular_text: [];
    ]);
}

#[test]
fn horizontal_escaped_char() {
    assert_parsed!(r"\|" horizontal_escaped_char: []);
}

#[test]
fn horizontal_single() {
    assert_parsed!("aaa" horizontal_single: [ "aaa" regular_text: []; ]);
    assert_parsed!(" aaa" horizontal_single: [ " aaa" regular_text: []; ]);
    assert_parsed!(r"\foo;" horizontal_single: [
        r"\foo;" inline_cmd: [r"\foo" inline_cmd_name: [_];];
    ]);
    assert_parsed!(r" \foo;" horizontal_single: [
        " " regular_text: [];
        r"\foo;" inline_cmd: [r"\foo" inline_cmd_name: [_];];
    ]);
    assert_parsed!("#foo;" horizontal_single: [ "#foo;" inline_text_embedding: [_]; ]);
    assert_parsed!("${a}" horizontal_single: [ "${a}" math_text: [_]; ]);
    assert_parsed!("`foo`" horizontal_single: [ "`foo`" const_string: []; ]);
    assert_parsed!("aaa `foo`" horizontal_single: [
        "aaa " regular_text: [];
        "`foo`" const_string: [];
    ]);

    assert_parsed!(r"質量 \* 速さ (${v})" horizontal_single: [
        "質量 " regular_text: [];
        r"\*" horizontal_escaped_char: [];
        " 速さ (" regular_text: [];
        "${v}" math_text: [_];
        ")" regular_text: [];
    ]);
}

#[test]
fn test_horizontal_recursive() {
    assert_parsed!(r"\a;" horizontal_single: [
        r"\a;" inline_cmd: [
            r"\a" inline_cmd_name: ["a" cmd_name_ptn: [];];
        ];
    ]);

    assert_parsed!(r"\a{}" horizontal_single: [_]);
    assert_parsed!(r"\a{\a{}}" horizontal_single: [_]);
    assert_parsed!(r"\a{\a{\a{}}}" horizontal_single: [_]);
    assert_parsed!(r"\a{\a{\a{\a{}}}}" horizontal_single: [_]);
    assert_parsed!(r"\a{\a{\a{\a{\a{}}}}}" horizontal_single: [_]);
    assert_parsed!(r"\a{\a{\a{\a{\a{\a{}}}}}}" horizontal_single: [_]);
}
