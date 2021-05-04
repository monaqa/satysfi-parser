use super::*;

#[test]
fn type_inner() {
    assert_parsed!("a = int" type_inner: ["a" type_name: [_]; "int" type_expr: [_]; ]);
}

#[test]
fn let_block_stmt_noctx() {
    assert_parsed!("let-block +p foo bar = '<>" let_block_stmt_noctx: [_]);
}
