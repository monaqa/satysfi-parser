use super::*;

// #[test]
// fn type_unary() {
//     assert_parsed!("()" type_expr: []);
// }

#[test]
fn type_prod() {
    assert_parsed!("int" type_prod: ["int" type_application, type_name: [_];]);

    assert_parsed!("(int)" type_prod: [
        "(int)" type_application: ["int" type_expr: [_];];
    ]);
}

#[test]
fn type_expr_performance_paren() {
    assert_parsed!("(int)" type_expr: [_]);
    assert_parsed!("((int))" type_expr: [_]);
    assert_parsed!("(((int)))" type_expr: [_]);
    assert_parsed!("((((int))))" type_expr: [_]);
    assert_parsed!("(((((int)))))" type_expr: [_]);
    assert_parsed!("((((((((((((((((((int))))))))))))))))))" type_expr: [_]);
}

#[test]
fn type_expr_performance_arrow() {
    assert_parsed!("int" type_expr: [_]);
    assert_parsed!("int -> int" type_expr: [_]);
    assert_parsed!("int -> int -> int" type_expr: [_]);
    assert_parsed!("int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int" type_expr: [_]);
}

#[test]
fn type_name() {
    assert_parsed!("int" type_name: ["int" var: [];]);
}
