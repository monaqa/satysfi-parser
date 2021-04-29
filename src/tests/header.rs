use super::*;

#[test]
fn stage() {
    assert_parsed!("0" stage: []);
}

#[test]
fn headers() {
    assert_parsed!("@require: foo\n@import: bar\n" headers: [
        "@require: foo\n" header_require: [_];
        "@import: bar\n" header_import: [_];
    ]);
}

#[test]
fn header_require() {
    assert_parsed!("@require: foo\n" header_require: ["foo" pkgname: [];]);
}

#[test]
fn header_import() {
    assert_parsed!("@import: foo\n" header_import: ["foo" pkgname: [];]);
}
