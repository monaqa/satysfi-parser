//! Packrat parsing が適切に動いていることの確認。

use criterion::Criterion;
use criterion::{criterion_group, criterion_main};

use satysfi_parser::grammar::satysfi_parser;

pub fn list_in_type_expr(c: &mut Criterion) {
    let mut group = c.benchmark_group("list_in_type_expr");
    group.significance_level(0.1).sample_size(100);

    fn task(nest: usize) {
        let init = "int;";
        let mut text = (0..nest)
            .into_iter()
            .fold(init.to_owned(), |acc, _| format!("{} int;", acc));
        text = format!("[{}] inline-cmd", text);
        assert!(satysfi_parser::type_expr(&text).is_ok());
    }

    for i in 0..=5 {
        let title = format!("task-{:02}", i);
        group.bench_function(title, |b| b.iter(|| task(i)));
    }
    group.finish();
}

pub fn nested_paren_in_type_expr(c: &mut Criterion) {
    let mut group = c.benchmark_group("nested_paren_in_type_expr");
    group.significance_level(0.1).sample_size(100);

    fn task(nest: usize) {
        let init = "int";
        let text = (0..nest)
            .into_iter()
            .fold(init.to_owned(), |acc, _| format!("({})", acc));
        assert!(satysfi_parser::type_expr(&text).is_ok());
    }

    for i in 0..=5 {
        let i = 2 * i;
        let title = format!("task-{:02}", i);
        group.bench_function(title, |b| b.iter(|| task(i)));
    }
    group.finish();
}

criterion_group!(
    benches,
    // nested_paren_in_type_expr,
    list_in_type_expr
);
criterion_main!(benches);
