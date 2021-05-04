use super::*;

#[test]
fn type_inner() {
    assert_parsed!("a = int" type_inner: ["a" type_name: [_]; "int" type_expr: [_]; ]);
}
