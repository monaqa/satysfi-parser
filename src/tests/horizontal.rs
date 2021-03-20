use super::*;

#[test]
fn horizontal() {
    assert_parsed!("{}" horizontal: [_]);
    assert_parsed!("{aaa}" horizontal: [_]);
    assert_parsed!("{|aaa|}" horizontal: [_]);
    assert_parsed!("{* aaa}" horizontal: [_]);
}
