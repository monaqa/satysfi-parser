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

#[test]
fn test_boolean() {
    assert_eq!(
        satysfi_parser::const_bool("true"),
        Ok(Cst {
            rule: constant(boolean),
            range: (0, 4),
            inner: vec![]
        })
    );
    assert_eq!(
        satysfi_parser::const_bool("false"),
        Ok(Cst {
            rule: constant(boolean),
            range: (0, 5),
            inner: vec![]
        })
    );

    assert!(satysfi_parser::const_bool("True").is_err());
    assert!(satysfi_parser::const_bool("TRUE").is_err());
}

#[test]
fn test_int() {
    assert_eq!(
        satysfi_parser::const_int("1"),
        Ok(Cst {
            rule: constant(int),
            range: (0, 1),
            inner: vec![]
        })
    );
    assert_eq!(
        satysfi_parser::const_int("1230"),
        Ok(Cst {
            rule: constant(int),
            range: (0, 4),
            inner: vec![]
        })
    );
    assert_eq!(
        satysfi_parser::const_int("0x12AF"),
        Ok(Cst {
            rule: constant(int),
            range: (0, 6),
            inner: vec![]
        })
    );
    assert_eq!(
        satysfi_parser::const_int("0XAF12"),
        Ok(Cst {
            rule: constant(int),
            range: (0, 6),
            inner: vec![]
        })
    );
    assert!(satysfi_parser::const_int("01").is_err());
    assert!(satysfi_parser::const_int("0x12af").is_err());
    assert!(satysfi_parser::const_int("0xaf12").is_err());
}