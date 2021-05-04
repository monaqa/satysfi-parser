use super::*;

#[test]
fn constraint() {
    assert_parsed!("constraint 'a :: (| foo: int |)" constraint: [
        "'a" type_param: [_];
        "(| foo: int |)" type_record: [_];
    ]);
}

#[test]
fn type_param() {
    assert_parsed!("'a" type_param: ["a" var_ptn: []; ]);
    assert_not_parsed!("' a" type_param: [_]);
}

#[test]
fn type_record_unit() {
    assert_parsed!("foo: int" type_record_unit: [
        "foo" var: [];
        "int" type_expr, type_prod, type_application, type_name, var: [];
    ]);
}

#[test]
fn type_record() {
    assert_parsed!("(| foo: int |)" type_record: [ "foo: int " type_record_unit: [_]; ]);
}

#[test]
fn type_name() {
    assert_parsed!("int" type_name: ["int" var: [];]);
    assert_parsed!("Mod.t" type_name, modvar: ["Mod" module_name: []; "t" var_ptn: []; ]);
}

#[test]
fn type_application() {
    assert_parsed!("list" type_application: ["list" type_name: [_]; ]);
    assert_parsed!("'a" type_application: ["'a" type_param: [_]; ]);
    assert_parsed!("'a list" type_application: ["'a" type_param: [_]; "list" type_name: [_]; ]);
    assert_parsed!("'a list option" type_application: [
        "'a" type_param: [_];
        "list" type_name: [_];
        "option" type_name: [_];
    ]);
}

#[test]
fn type_list_unit_optional() {
    assert_parsed!("int?" type_list_unit_optional: ["int" type_prod: [_]; ]);
    assert_parsed!("int * int ?" type_list_unit_optional: ["int * int " type_prod: [_]; ]);
}

#[test]
fn type_inline_cmd() {
    assert_parsed!("[] inline-cmd" type_inline_cmd: []);
    assert_parsed!("[ ]inline-cmd" type_inline_cmd: []);
    assert_parsed!("[ ] inline-cmd" type_inline_cmd: []);

    assert_parsed!("[int] inline-cmd" type_inline_cmd: [ "int" type_expr: [_]; ]);
    assert_parsed!("[int;] inline-cmd" type_inline_cmd: [ "int" type_expr: [_]; ]);
    assert_parsed!("[int; inline-text] inline-cmd" type_inline_cmd: [
        "int" type_expr: [_];
        "inline-text" type_expr: [_];
    ]);
}

#[test]
fn type_block_cmd() {
    assert_parsed!("[] block-cmd" type_block_cmd: []);
}

#[test]
fn type_math_cmd() {
    assert_parsed!("[] math-cmd" type_math_cmd: []);
}

#[test]
fn type_prod() {
    assert_parsed!("int" type_prod: ["int" type_application, type_name: [_];]);

    assert_parsed!("(int)" type_prod: [
        "(int)" type_application: ["int" type_expr: [_];];
    ]);

    assert_parsed!("int * int" type_prod: [
        "int " type_application: [_];
        "int" type_application: [_];
    ]);
}

#[test]
fn type_optional() {
    assert_parsed!("int" type_optional, type_application, type_name, var: []);
}

#[test]
fn type_expr() {
    assert_parsed!("int" type_expr, type_prod, type_application, type_name, var: []);

    assert_parsed!("(| width: length; |)" type_expr: [_]);
}
