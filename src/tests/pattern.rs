use super::*;

#[test]
fn pat_tuple() {
    assert_parsed!("(x, 1)" pat_tuple: ["x" pat_as: [_]; "1" pat_as: [_]; ]);
    assert_parsed!("( x, 1)" pat_tuple: ["x" pat_as: [_]; "1" pat_as: [_]; ]);

    assert_not_parsed!("(x)" pat_tuple: [_]);
}

#[test]
fn pat_list() {
    assert_parsed!("[x]" pat_list: ["x" pat_as: [_]; ]);
    assert_parsed!("[ x; y]" pat_list: ["x" pat_as: [_]; "y" pat_as: [_]; ]);
    assert_parsed!("[x; y; z]" pat_list: ["x" pat_as: [_]; "y" pat_as: [_]; "z" pat_as: [_]; ]);
}

#[test]
fn pat_variant() {
    assert_parsed!("Some(x)" pat_variant: ["Some" variant_name: []; "(x)" pattern: [_]; ]);
    assert_parsed!("Some(Some(x))" pat_variant: ["Some" variant_name: []; "(Some(x))" pattern: [
        "Some(x)" pat_as, pat_cons, pat_variant: [
            "Some" variant_name: [];
            "(x)" pattern: [_];
        ];
    ]; ]);
    assert_parsed!("None" pat_variant: ["None" variant_name: []; ]);
}

#[test]
fn pattern() {
    assert_parsed!("_" pattern: []);
    assert_parsed!("x" pattern, var: []);
    assert_parsed!("1" pattern, constant, const_int: []);
    assert_parsed!("1pt" pattern, constant, const_length: []);

    assert_parsed!("[x; y]" pattern, pat_list: [_]);
    assert_parsed!("(x)" pattern: ["x" pat_as: [_]; ]);
    assert_parsed!("(x, y)" pattern, pat_tuple: [_]);
}

#[test]
fn pat_cons() {
    assert_parsed!("x :: rest" pat_cons: ["x" pattern: [_]; "rest" pat_as: [_]; ]);
    assert_parsed!("Some(x)" pat_cons, pat_variant: [_]);
    assert_parsed!("x" pat_cons, pattern: [_]);
}

#[test]
fn pat_as() {
    assert_parsed!("Some(Some(x)) as some-x" pat_as: [
        "Some(Some(x))" pat_cons: [_];
        "some-x" var: [];
    ]);
}
