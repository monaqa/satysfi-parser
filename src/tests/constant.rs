use super::*;

#[test]
fn test_unit() {
    assert_parsed!("()" const_unit: []);
    assert_parsed!("( )" const_unit: []);
}

#[test]
fn test_boolean() {
    assert_parsed!("true" const_bool: []);
    assert_parsed!("false" const_bool: []);

    assert_not_parsed!("True" const_bool: []);
    assert_not_parsed!("TRUE" const_bool: []);
}

#[test]
fn test_int() {
    assert_parsed!("0" const_int: []);
    assert_parsed!("1" const_int: []);
    assert_parsed!("1230" const_int: []);
    assert_parsed!("0x12AF" const_int: []);
    assert_parsed!("0xAF12" const_int: []);

    assert_not_parsed!("01" const_int: []);
    assert_not_parsed!("0x12af" const_int: []);
    assert_not_parsed!("0xaf12" const_int: []);
}

#[test]
fn test_float() {
    assert_parsed!("0.3" const_float: []);
    assert_parsed!("42.195" const_float: []);
    assert_parsed!(".195" const_float: []);
    assert_parsed!("42." const_float: []);

    assert_not_parsed!("." const_float: []);
    assert_not_parsed!("1..3" const_float: []);
}

#[test]
fn test_length() {
    assert_parsed!("1pt" const_length: []);
    assert_parsed!("1abc" const_length: []);
    assert_parsed!("1a5F9-" const_length: []);
    assert_parsed!("0.3pt" const_length: []);
    assert_parsed!("42.195pt" const_length: []);
    assert_parsed!(".195pt" const_length: []);
    assert_parsed!("42.pt" const_length: []);
    assert_parsed!("-1pt" const_length: []);

    assert_not_parsed!(".pt" const_length: []);
    assert_not_parsed!("1..3pt" const_length: []);
}

#[test]
fn test_string() {
    assert_parsed!("`a`" const_string: []);
    assert_parsed!("`a`" const_string: []);
    assert_parsed!("#`a`" const_string: []);
    assert_parsed!("#`a`#" const_string: []);
    assert_parsed!("`a`#" const_string: []);
    assert_parsed!("`あいう`" const_string: []);
    assert_parsed!("`` ` ``" const_string: []);
    assert_parsed!("```\n hoge fuga `` `piyo` ```" const_string: []);

    assert_not_parsed!("``" const_string: []);
    assert_not_parsed!("``` ``` ```" const_string: []);
    assert_not_parsed!("`` aaa ```" const_string: []);
}
