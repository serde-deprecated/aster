use syntax::ast;

use aster::AstBuilder;

#[test]
fn test_arm() {
    let builder = AstBuilder::new();

    let arm = builder.arm()
        .pat().id("a")
        .pat().id("b")
        .body().unit();

    assert_eq!(
        arm,
        ast::Arm {
            attrs: Vec::new(),
            pats: vec![
                builder.pat().id("a"),
                builder.pat().id("b"),
            ],
            guard: None,
            body: builder.expr().unit(),
        }
    );
}

#[test]
fn test_arm_with_guard() {
    let builder = AstBuilder::new();

    let arm = builder.arm()
        .pat().id("a")
        .pat().id("b")
        .guard().bool(true)
        .body().unit();

    assert_eq!(
        arm,
        ast::Arm {
            attrs: Vec::new(),
            pats: vec![
                builder.pat().id("a"),
                builder.pat().id("b"),
            ],
            guard: Some(builder.expr().bool(true)),
            body: builder.expr().unit(),
        }
    );
}
