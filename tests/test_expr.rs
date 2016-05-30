use syntax::ast;
use syntax::codemap::{DUMMY_SP, Spanned, respan};
use syntax::ptr::P;

use aster::AstBuilder;

#[test]
fn test_lit() {
    let builder = AstBuilder::new();

    fn check(expr: P<ast::Expr>, lit: P<ast::Lit>) {
        assert_eq!(
            expr,
            P(ast::Expr {
                id: ast::DUMMY_NODE_ID,
                node: ast::ExprKind::Lit(lit),
                span: DUMMY_SP,
                attrs: None,
            })
        );
    }

    check(builder.expr().bool(true), builder.lit().bool(true));
    check(builder.expr().true_(), builder.lit().true_());
    check(builder.expr().false_(), builder.lit().false_());

    check(builder.expr().int(5), builder.lit().int(5));

    check(builder.expr().i8(5), builder.lit().i8(5));
    check(builder.expr().i16(5), builder.lit().i16(5));
    check(builder.expr().i32(5), builder.lit().i32(5));
    check(builder.expr().i64(5), builder.lit().i64(5));
    check(builder.expr().isize(5), builder.lit().isize(5));

    check(builder.expr().u8(5), builder.lit().u8(5));
    check(builder.expr().u16(5), builder.lit().u16(5));
    check(builder.expr().u32(5), builder.lit().u32(5));
    check(builder.expr().u64(5), builder.lit().u64(5));
    check(builder.expr().usize(5), builder.lit().usize(5));

    check(builder.expr().str("string"), builder.lit().str("string"));
}

#[test]
fn test_path() {
    let builder = AstBuilder::new();

    let expr = builder.expr().path()
        .id("x")
        .build();

    assert_eq!(
        expr,
        P(ast::Expr {
            id: ast::DUMMY_NODE_ID,
            node: ast::ExprKind::Path(
                None,
                builder.path().id("x").build(),
            ),
            span: DUMMY_SP,
            attrs: None,
        })
    );
}

#[test]
fn test_qpath() {
    let builder = AstBuilder::new();

    let expr = builder.expr().qpath()
        .ty().slice().infer()
        .id("into_vec");

    assert_eq!(
        expr,
        P(ast::Expr {
            id: ast::DUMMY_NODE_ID,
            node: ast::ExprKind::Path(
                Some(ast::QSelf {
                    ty: builder.ty().slice().infer(),
                    position: 0,
                }),
                builder.path().id("into_vec").build(),
            ),
            span: DUMMY_SP,
            attrs: None,
        })
    );

    let expr: P<ast::Expr> = builder.expr().qpath()
        .ty().slice().infer()
        .as_().id("Slice").build()
        .id("into_vec");

    assert_eq!(
        expr,
        P(ast::Expr {
            id: ast::DUMMY_NODE_ID,
            node: ast::ExprKind::Path(
                Some(ast::QSelf {
                    ty: builder.ty().slice().infer(),
                    position: 1,
                }),
                builder.path()
                    .id("Slice")
                    .id("into_vec")
                    .build(),
            ),
            span: DUMMY_SP,
            attrs: None,
        })
    );
}


#[test]
fn test_bin() {
    let builder = AstBuilder::new();

    assert_eq!(
        builder.expr().add().i8(1).i8(2),
        P(ast::Expr {
            id: ast::DUMMY_NODE_ID,
            node: ast::ExprKind::Binary(
                Spanned {
                    span: DUMMY_SP,
                    node: ast::BinOpKind::Add,
                },
                builder.expr().i8(1),
                builder.expr().i8(2),
            ),
            span: DUMMY_SP,
            attrs: None,
        })
    );
}

#[test]
fn test_unit() {
    let builder = AstBuilder::new();

    assert_eq!(
        builder.expr().unit(),
        P(ast::Expr {
            id: ast::DUMMY_NODE_ID,
            node: ast::ExprKind::Tup(vec![]),
            span: DUMMY_SP,
            attrs: None,
        })
    );

    assert_eq!(
        builder.expr().tuple().build(),
        P(ast::Expr {
            id: ast::DUMMY_NODE_ID,
            node: ast::ExprKind::Tup(vec![]),
            span: DUMMY_SP,
            attrs: None,
        })
    );
}

#[test]
fn test_tuple() {
    let builder = AstBuilder::new();

    let expr = builder.expr().tuple()
        .expr().i8(1)
        .expr().tuple()
            .expr().unit()
            .expr().isize(2)
            .build()
        .build();

    assert_eq!(
        expr,
        P(ast::Expr {
            id: ast::DUMMY_NODE_ID,
            node: ast::ExprKind::Tup(vec![
                builder.expr().i8(1),
                P(ast::Expr {
                    id: ast::DUMMY_NODE_ID,
                    node: ast::ExprKind::Tup(vec![
                        builder.expr().unit(),
                        builder.expr().isize(2),
                    ]),
                    span: DUMMY_SP,
                    attrs: None,
                })
            ]),
            span: DUMMY_SP,
            attrs: None,
        })
    );
}

#[test]
fn test_slice() {
    let builder = AstBuilder::new();

    let expr = builder.expr().slice()
        .expr().i8(1)
        .expr().i8(2)
        .expr().i8(3)
        .build();

    assert_eq!(
        expr,
        P(ast::Expr {
            id: ast::DUMMY_NODE_ID,
            node: ast::ExprKind::Vec(vec![
                builder.expr().i8(1),
                builder.expr().i8(2),
                builder.expr().i8(3),
            ]),
            span: DUMMY_SP,
            attrs: None,
        })
    );
}

#[test]
fn test_vec() {
    let builder = AstBuilder::new();

    let expr = builder.expr().vec()
        .expr().i8(1)
        .expr().i8(2)
        .expr().i8(3)
        .build();

    assert_eq!(
        expr,
        builder.expr().call()
            .qpath().ty().slice().infer().id("into_vec")
            .arg().box_().slice()
                .expr().i8(1)
                .expr().i8(2)
                .expr().i8(3)
                .build()
            .build()
    );
}

#[test]
fn test_break() {
    let builder = AstBuilder::new();

    let expr = builder.expr().break_();

    assert_eq!(
        expr,
        builder.expr().build_expr_kind(ast::ExprKind::Break(None))
    );

    let expr = builder.expr().break_to("'a");
    let id = respan(DUMMY_SP, builder.id("'a"));

    assert_eq!(
        expr,
        builder.expr().build_expr_kind(ast::ExprKind::Break(Some(id)))
    );
}

#[test]
fn test_continue() {
    let builder = AstBuilder::new();

    let expr = builder.expr().continue_();

    assert_eq!(
        expr,
        builder.expr().build_expr_kind(ast::ExprKind::Again(None))
    );

    let expr = builder.expr().continue_to("'a");
    let id = respan(DUMMY_SP, builder.id("'a"));

    assert_eq!(
        expr,
        builder.expr().build_expr_kind(ast::ExprKind::Again(Some(id)))
    );
}

#[test]
fn test_return() {
    let builder = AstBuilder::new();

    let expr = builder.expr().return_();

    assert_eq!(
        expr,
        builder.expr().build_expr_kind(ast::ExprKind::Ret(None))
    );

    let expr = builder.expr().return_expr().unit();

    assert_eq!(
        expr,
        builder.expr().build_expr_kind(ast::ExprKind::Ret(Some(builder.expr().unit())))
    );
}

#[test]
fn test_loop() {
    let builder = AstBuilder::new();

    let expr = builder.expr().loop_()
        .block()
            .build();

    assert_eq!(
        expr,
        builder.expr().build_expr_kind(
            ast::ExprKind::Loop(
                builder.block().build(),
                None,
            )
        )
    );

    let expr = builder.expr().loop_()
        .label("'a")
        .block()
            .build();

    assert_eq!(
        expr,
        builder.expr().build_expr_kind(
            ast::ExprKind::Loop(
                builder.block().build(),
                Some(respan(DUMMY_SP, builder.id("'a"))),
            )
        )
    );
}

#[test]
fn test_if() {
    let builder = AstBuilder::new();

    let expr = builder.expr().if_()
        .true_()
        .then().expr().u32(1)
        .build();

    assert_eq!(
        expr,
        builder.expr().build_expr_kind(
            ast::ExprKind::If(
                builder.expr().true_(),
                builder.block().expr().u32(1),
                None,
            )
        )
    );

    let expr = builder.expr().if_()
        .true_()
        .then().expr().u32(1)
        .else_().expr().u32(2);

    assert_eq!(
        expr,
        builder.expr().build_expr_kind(
            ast::ExprKind::If(
                builder.expr().true_(),
                builder.block().expr().u32(1),
                Some(builder.expr().block().expr().u32(2))
            )
        )
    );

    let expr = builder.expr()
        .if_()
            .eq().id("x").u32(1)
        .then()
            .expr().u32(1)
        .else_if()
            .eq().id("x").u32(2)
        .then()
            .expr().u32(2)
        .else_if()
            .eq().id("x").u32(3)
        .then()
            .expr().u32(3)
        .else_()
            .expr().u32(4);

    assert_eq!(
        expr,
        builder.expr().build_expr_kind(
            ast::ExprKind::If(
                builder.expr()
                    .eq().id("x").u32(1),
                builder.block()
                    .expr().u32(1),
                Some(
                    builder.expr().build_expr_kind(
                        ast::ExprKind::If(
                            builder.expr()
                                .eq().id("x").u32(2),
                            builder.block()
                                .expr().u32(2),
                            Some(
                                builder.expr()
                                    .if_()
                                        .eq().id("x").u32(3)
                                    .then()
                                        .expr().u32(3)
                                    .else_()
                                        .expr().u32(4)
                            )
                        )
                    )
                )
            )
        )
    );
}

#[test]
fn test_match() {
    let builder = AstBuilder::new();

    let expr = builder.expr().match_().u32(0)
        .arm()
            .pat().expr().u32(0)
            .body().unit()
        .arm()
            .pat().expr().u32(1)
            .body().unit()
        .arm()
            .pat().wild()
            .body().unit()
        .build();

    assert_eq!(
        expr,
        builder.expr().build_expr_kind(
            ast::ExprKind::Match(
                builder.expr().u32(0),
                vec![
                    builder.arm()
                        .pat().expr().u32(0)
                        .body().unit(),
                    builder.arm()
                        .pat().expr().u32(1)
                        .body().unit(),
                    builder.arm()
                        .pat().wild()
                        .body().unit(),
                ]
            )
        )
    );
}

#[test]
fn test_index() {
    let builder = AstBuilder::new();

    let expr = builder.expr().index()
        .id("x")
        .usize(2);

    assert_eq!(
        expr,
        builder.expr().build_expr_kind(
            ast::ExprKind::Index(
                builder.expr().id("x"),
                builder.expr().usize(2)
            )
        )
    );
}

#[test]
fn test_repeat() {
    let builder = AstBuilder::new();

    let expr = builder.expr().repeat()
        .u16(1024)
        .usize(16);

    assert_eq!(
        expr,
        builder.expr().build_expr_kind(
            ast::ExprKind::Repeat(
                builder.expr().u16(1024),
                builder.expr().usize(16)
            )
        )
    );
}
