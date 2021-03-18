use super::*;

#[test]
fn test_unit() {
    assert_eq!(
        satysfi_parser::const_unit("()"),
        Ok(Cst {
            rule: constant(unit),
            range: (0, 2),
            inner: vec![],
        })
    );
}
