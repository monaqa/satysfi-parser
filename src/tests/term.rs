use super::*;

#[test]
fn test_constant() {
    assert_parsed!("()" constant : [
        "()" const_unit: [];
    ]);
}
