use super::*;

#[test]
fn variant_name() {
    assert_parsed!("Some" variant_name: []);
    assert_parsed!("Some-2" variant_name: []);
    assert_parsed!("UpperCamelCase" variant_name: []);
    assert_not_parsed!("lowerCamelCase" variant_name: [_]);
}

#[test]
fn module_name() {
    assert_parsed!("Mod" module_name: []);
    assert_parsed!("Mod-2" module_name: []);
    assert_parsed!("UpperCamelCase" module_name: []);
    assert_not_parsed!("lowerCamelCase" module_name: [_]);
}

#[test]
fn mod_cmd_name() {
    assert_parsed!("Mod.alpha" mod_cmd_name: [
        "Mod" module_name: [];
        "alpha" cmd_name_ptn: [];
    ]);
    assert_parsed!("Mod-2.Alpha" mod_cmd_name: [
        "Mod-2" module_name: [];
        "Alpha" cmd_name_ptn: [];
    ]);
    assert_not_parsed!("mod.alpha" mod_cmd_name: []);
    assert_not_parsed!("Mod.Alpha.beta" mod_cmd_name: []);
}

#[test]
fn modvar() {
    assert_parsed!("Mod.f" modvar: [
        "Mod" module_name: [];
        "f" var_ptn: [];
    ]);
    assert_not_parsed!("Mod.Alpha" mod_cmd_name: []);
    assert_not_parsed!("mod.alpha" mod_cmd_name: []);
}

#[test]
fn var_ptn() {
    assert_parsed!("foo" var_ptn: []);
    assert_parsed!("let" var_ptn: []);
    assert_parsed!("let-inline" var_ptn: []);
    assert_parsed!("foo2" var_ptn: []);
    assert_parsed!("foo-abc" var_ptn: []);
    assert_parsed!("foo-abc2" var_ptn: []);
    assert_not_parsed!("Foo" var_ptn: []);
    assert_not_parsed!("2foo" var_ptn: []);
    assert_not_parsed!("-foo" var_ptn: []);
}

#[test]
fn var() {
    assert_parsed!("foo" var: []);
    assert_parsed!("leti" var: []);
    assert_parsed!("foo2" var: []);
    assert_parsed!("foo-abc" var: []);
    assert_parsed!("foo-abc2" var: []);
    assert_not_parsed!("let" var: []);
    assert_not_parsed!("let-inline" var: []);
    assert_not_parsed!("Foo" var: []);
    assert_not_parsed!("2foo" var: []);
    assert_not_parsed!("-foo" var: []);
}

#[test]
fn expr_with_mod() {
    // TODO: より複雑な expr
    assert_parsed!("Mod.(1)" expr_with_mod: [
        "Mod" module_name: [];
        "1" expr: [_];
    ]);
}

#[test]
fn bin_operator() {
    assert_parsed!("::" bin_operator: []);
    assert_parsed!("mod" bin_operator: []);
    assert_parsed!("+" bin_operator: []);
    assert_parsed!(">" bin_operator: []);
    assert_parsed!("+." bin_operator: []);
    assert_parsed!("++" bin_operator: []);
    assert_parsed!("+++" bin_operator: []);
    assert_parsed!("+~" bin_operator: []);
    assert_parsed!("+~=" bin_operator: []);
    assert_parsed!("+^+" bin_operator: []);
    assert_parsed!("->>" bin_operator: []);
    assert_parsed!("|>" bin_operator: []);

    assert_not_parsed!("->" bin_operator: []);
    assert_not_parsed!("|" bin_operator: []);
    assert_not_parsed!("moda" bin_operator: []);
    assert_not_parsed!("!=" bin_operator: []);
}

#[test]
fn tuple() {
    assert_parsed!("(1,2)" tuple: [ "1" expr: [_]; "2" expr: [_]; ]);
    assert_parsed!("(1, 2)" tuple: [ "1" expr: [_]; "2" expr: [_]; ]);
    assert_parsed!("(1,\n2)" tuple: [ "1" expr: [_]; "2" expr: [_]; ]);
    assert_parsed!("(1, 2, 3)" tuple: [ "1" expr: [_]; "2" expr: [_]; "3" expr: [_]; ]);
    assert_parsed!("(0pt, 0pt -' dp)" tuple: [_]);
    // TODO: もっと複雑な expr（途中でカンマが出てくるような）

    assert_not_parsed!("(1)" tuple: [_]);
    assert_not_parsed!("(1,)" tuple: [_]);
    assert_not_parsed!("(1, )" tuple: [_]);
    assert_not_parsed!("(1, 2,)" tuple: [_]); // 末尾カンマNG
}

#[test]
fn list() {
    assert_parsed!("[]" list: []);
    assert_parsed!("[1]" list: ["1" expr: [_];]);
    assert_parsed!("[1;]" list: ["1" expr: [_];]);
    assert_parsed!("[1; ]" list: ["1" expr: [_];]);
    assert_parsed!("[ 1; ]" list: ["1" expr: [_];]);
    assert_parsed!("[1; 2]" list: ["1" expr: [_]; "2" expr: [_];]);
    assert_parsed!("[1; 2 ]" list: ["1" expr: [_]; "2" expr: [_];]);
    assert_parsed!("[ 1;\n 2 ]" list: ["1" expr: [_]; "2" expr: [_];]);
    assert_parsed!("[ 1;\n 2; ]" list: ["1" expr: [_]; "2" expr: [_];]);
    assert_parsed!("[ 1; 2; 3 ]" list: ["1" expr: [_]; "2" expr: [_]; "3" expr: [_];]);
    // TODO: もっと複雑な expr（リストのネストなど）

    assert_not_parsed!("[1,]" list: [_]);
    assert_not_parsed!("[1, ]" list: [_]);
    assert_not_parsed!("[1, 2]" list: [_]);
}

#[test]
fn record() {
    assert_parsed!("(||)" record: []);
    assert_parsed!("(| |)" record: []);

    assert_parsed!("(| key = 1 |)" record: [
        "key = 1" record_unit: ["key" var_ptn: []; "1" expr: [_];];
    ]);
    assert_parsed!("(| key = 1; |)" record: [
        "key = 1" record_unit: ["key" var_ptn: []; "1" expr: [_];];
    ]);
    assert_parsed!("(|key=1|)" record: [
        "key=1" record_unit: ["key" var_ptn: []; "1" expr: [_];];
    ]);

    assert_parsed!("(| foo = 1; bar = `2` |)" record: [
        "foo = 1" record_unit: ["foo" var_ptn: []; "1" expr: [_];];
        "bar = `2`" record_unit: ["bar" var_ptn: []; "`2`" expr: [_];];
    ]);
    assert_parsed!("(| foo = 1; bar = `2`; |)" record: [
        "foo = 1" record_unit: ["foo" var_ptn: []; "1" expr: [_];];
        "bar = `2`" record_unit: ["bar" var_ptn: []; "`2`" expr: [_];];
    ]);

    assert_not_parsed!("( | |)" record: []);
    assert_not_parsed!("(| | )" record: []);

    assert_not_parsed!("(| a = b = 1 |)" record: []);
    assert_not_parsed!("(| a : 1 |)" record: []);
}

#[test]
fn math_text() {
    assert_parsed!(r"${}" math_text: ["" math_single: [];]);
    assert_parsed!(r"${\alpha}" math_text: [r"\alpha" math_single: [_]; ]);
}

#[test]
fn horizontal_text() {
    assert_parsed!("{}" horizontal_text: ["" horizontal_single: []; ]);
    assert_parsed!("{abc}" horizontal_text: ["abc" horizontal_single: [_]; ]);
    assert_parsed!("{|abc|}" horizontal_text: [
        "|abc|" horizontal_list: ["abc" horizontal_single: [_]; ];
    ]);
    assert_parsed!("{ | abc | }" horizontal_text: [
        "| abc |" horizontal_list: [" abc " horizontal_single: [_]; ];
    ]);
    assert_parsed!("{ | abc | | }" horizontal_text: [
        "| abc | |" horizontal_list: [
            " abc " horizontal_single: [_];
            " " horizontal_single: [_];
        ];
    ]);
}

#[test]
fn block_text() {
    assert_parsed!("'<>" block_text: ["" vertical: [];]);
    assert_parsed!("'< +p{aaa} >" block_text: [
        "+p{aaa}" vertical, block_cmd: [_];
    ]);
    assert_parsed!("'< +toc; #sec1; +p{aaa} >" block_text: [
        "+toc; #sec1; +p{aaa}" vertical: [
            "+toc;" block_cmd: [_];
            "#sec1;" block_text_embedding: [_];
            "+p{aaa}" block_cmd: [_];
        ];
    ]);

    assert_not_parsed!("<>" block_text: [_]);
}

#[test]
fn unary() {
    assert_parsed!("'<>" unary: ["'<>" block_text: [_]; ]);
    assert_parsed!("{}" unary: ["{}" horizontal_text: [_]; ]);
    assert_parsed!("${}" unary: ["${}" math_text: [_]; ]);
    assert_parsed!("(||)" unary: ["(||)" record: [_]; ]);
    assert_parsed!("[]" unary: ["[]" list: [_]; ]);
    assert_parsed!("(1, 2)" unary: ["(1, 2)" tuple: [_]; ]);
    assert_parsed!("0pt" unary: [_]);
    assert_parsed!("foo#bar" unary: ["foo" var: []; "bar" var: [];]);
    assert_parsed!("foo # bar" unary: ["foo" var: []; "bar" var: [];]);

    assert_parsed!("(+)" unary: ["+" bin_operator: [_]; ]);
    assert_parsed!("( + )" unary: ["+" bin_operator: [_]; ]);
    assert_parsed!("( |>)" unary: ["|>" bin_operator: [_]; ]);
    assert_parsed!("(1)" unary: ["1" expr: [_]; ]);
    assert_parsed!("1" unary: ["1" const_int: [_]; ]);
    assert_parsed!("Mod.(1)" unary: ["Mod.(1)" expr_with_mod: [_];]);
    assert_parsed!("Mod.foo" unary: ["Mod.foo" modvar: [_];]);
    assert_parsed!("foo" unary: ["foo" var: [_];]);

    // TODO: Original SATySFi parser doesn't allow "(|>)" as binary operator
    // assert_not_parsed!("(|>)" unary: [_]);
}
