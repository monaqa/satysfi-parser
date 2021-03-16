use super::*;

#[test]
fn unit() {
    assert_eq!(
        satysfi_parser::list("[(),(),()]"),
        Ok(Pair {
            rule: Rule::program,
            range: Some((1, 3)),
            inner: vec![]
        })
    );
}
