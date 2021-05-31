use satysfi_parser::{grammar, CstText};

fn main() {
    let text = "let x = 1 in% foo ほげ \n  2";
    let csttext = CstText::parse(text, grammar::program).unwrap();
    for i in 0..text.len() {
        println!("{:2}: {}", i, csttext.is_comment(i));
    }
}
