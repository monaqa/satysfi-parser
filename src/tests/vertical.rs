use super::*;

#[test]
fn block_text_embedding() {
    assert_parsed!("#foo;" block_text_embedding: ["foo" var_ptn: [];]);
    assert_parsed!("#Foo.bar;" block_text_embedding: ["Foo.bar" modvar: [_];]);
    assert_not_parsed!("#foo" block_text_embedding: [_]);
    assert_not_parsed!("#Foo" block_text_embedding: [_]);
    assert_not_parsed!("# foo;" block_text_embedding: [_]);
}

#[test]
fn vertical() {
    assert_parsed!("<>" vertical: []);

    assert_parsed!("<+p{}>" vertical: [
        "+p{}" block_cmd: [_];
    ]);

    assert_parsed!("<#foo; #bar;>" vertical: [
        "#foo;" block_text_embedding: [_];
        "#bar;" block_text_embedding: [_];
    ]);

    assert_parsed!("<+p{}\n  #foo; %comment\n #bar;>" vertical: [
        "+p{}" block_cmd: [_];
        "#foo;" block_text_embedding: [_];
        "#bar;" block_text_embedding: [_];
    ]);

    assert_parsed!("<+p{}\n  #foo; +section<%comment\n #bar;>>" vertical: [
        "+p{}" block_cmd: [_];
        "#foo;" block_text_embedding: [_];
        "+section<%comment\n #bar;>" block_cmd: [_];
    ]);
}
