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
        satysfi_parser::const_int("0"),
        Ok(Cst {
            rule: constant(int),
            range: (0, 1),
            inner: vec![]
        })
    );
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

#[test]
fn test_float() {
    assert_eq!(
        satysfi_parser::const_float("0.3"),
        Ok(Cst {
            rule: constant(float),
            range: (0, 3),
            inner: vec![],
        })
    );
    assert_eq!(
        satysfi_parser::const_float("42.195"),
        Ok(Cst {
            rule: constant(float),
            range: (0, 6),
            inner: vec![],
        })
    );
    assert_eq!(
        satysfi_parser::const_float(".195"),
        Ok(Cst {
            rule: constant(float),
            range: (0, 4),
            inner: vec![],
        })
    );
    assert_eq!(
        satysfi_parser::const_float("42."),
        Ok(Cst {
            rule: constant(float),
            range: (0, 3),
            inner: vec![],
        })
    );
    assert!(satysfi_parser::const_float(".").is_err());
    assert!(satysfi_parser::const_float("1..3").is_err());
}

#[test]
fn test_length() {
    assert_eq!(
        satysfi_parser::const_length("1pt"),
        Ok(Cst {
            rule: constant(length),
            range: (0, 3),
            inner: vec![]
        })
    );
    assert_eq!(
        satysfi_parser::const_length("1abc"),
        Ok(Cst {
            rule: constant(length),
            range: (0, 4),
            inner: vec![]
        })
    );
    assert_eq!(
        satysfi_parser::const_length("1a5F9-"),
        Ok(Cst {
            rule: constant(length),
            range: (0, 6),
            inner: vec![]
        })
    );
    assert_eq!(
        satysfi_parser::const_length("0.3pt"),
        Ok(Cst {
            rule: constant(length),
            range: (0, 5),
            inner: vec![],
        })
    );
    assert_eq!(
        satysfi_parser::const_length("42.195pt"),
        Ok(Cst {
            rule: constant(length),
            range: (0, 8),
            inner: vec![],
        })
    );
    assert_eq!(
        satysfi_parser::const_length(".195pt"),
        Ok(Cst {
            rule: constant(length),
            range: (0, 6),
            inner: vec![],
        })
    );
    assert_eq!(
        satysfi_parser::const_length("42.pt"),
        Ok(Cst {
            rule: constant(length),
            range: (0, 5),
            inner: vec![],
        })
    );
    assert_eq!(
        satysfi_parser::const_length("-1pt"),
        Ok(Cst {
            rule: constant(length),
            range: (0, 4),
            inner: vec![]
        })
    );
    assert!(satysfi_parser::const_length(".pt").is_err());
    assert!(satysfi_parser::const_length("1..3pt").is_err());
}

#[test]
fn test_string() {
    assert_eq!(
        satysfi_parser::const_string("`a`"),
        Ok(Cst {
            rule: constant(string),
            range: (0, 3),
            inner: vec![],
        })
    );
    assert_eq!(
        satysfi_parser::const_string("#`a`"),
        Ok(Cst {
            rule: constant(string),
            range: (0, 4),
            inner: vec![],
        })
    );
    assert_eq!(
        satysfi_parser::const_string("#`a`#"),
        Ok(Cst {
            rule: constant(string),
            range: (0, 5),
            inner: vec![],
        })
    );
    assert_eq!(
        satysfi_parser::const_string("`a`#"),
        Ok(Cst {
            rule: constant(string),
            range: (0, 4),
            inner: vec![],
        })
    );
    assert_eq!(
        satysfi_parser::const_string("`あいう`"),
        Ok(Cst {
            rule: constant(string),
            range: (0, 11),
            inner: vec![],
        })
    );
    assert_eq!(
        satysfi_parser::const_string("`` ` ``"),
        Ok(Cst {
            rule: constant(string),
            range: (0, 7),
            inner: vec![],
        })
    );
    assert_eq!(
        satysfi_parser::const_string("```\n hoge fuga `` `piyo` ```"),
        Ok(Cst {
            rule: constant(string),
            range: (0, 28),
            inner: vec![],
        })
    );
    assert!(satysfi_parser::const_string("``").is_err());
    assert!(satysfi_parser::const_string("# `a`").is_err());
    assert!(satysfi_parser::const_string("``` ``` ```").is_err());
    assert!(satysfi_parser::const_string("``` aaa ``").is_err());
    assert!(satysfi_parser::const_string("`` aaa ```").is_err());
}
