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
                node: ast::ExprLit(lit),
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
            node: ast::ExprPath(
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
            node: ast::ExprPath(
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
            node: ast::ExprPath(
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
            node: ast::ExprBinary(
                Spanned {
                    span: DUMMY_SP,
                    node: ast::BiAdd,
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
            node: ast::ExprTup(vec![]),
            span: DUMMY_SP,
            attrs: None,
        })
    );

    assert_eq!(
        builder.expr().tuple().build(),
        P(ast::Expr {
            id: ast::DUMMY_NODE_ID,
            node: ast::ExprTup(vec![]),
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
            node: ast::ExprTup(vec![
                builder.expr().i8(1),
                P(ast::Expr {
                    id: ast::DUMMY_NODE_ID,
                    node: ast::ExprTup(vec![
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
            node: ast::ExprVec(vec![
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
        builder.expr().build_expr_(ast::ExprBreak(None))
    );

    let expr = builder.expr().break_to("'a");
    let id = respan(DUMMY_SP, builder.id("'a"));

    assert_eq!(
        expr,
        builder.expr().build_expr_(ast::ExprBreak(Some(id)))
    );
}

#[test]
fn test_continue() {
    let builder = AstBuilder::new();

    let expr = builder.expr().continue_();

    assert_eq!(
        expr,
        builder.expr().build_expr_(ast::ExprAgain(None))
    );

    let expr = builder.expr().continue_to("'a");
    let id = respan(DUMMY_SP, builder.id("'a"));

    assert_eq!(
        expr,
        builder.expr().build_expr_(ast::ExprAgain(Some(id)))
    );
}

#[test]
fn test_return() {
    let builder = AstBuilder::new();

    let expr = builder.expr().return_();

    assert_eq!(
        expr,
        builder.expr().build_expr_(ast::ExprRet(None))
    );

    let expr = builder.expr().return_expr().unit();

    assert_eq!(
        expr,
        builder.expr().build_expr_(ast::ExprRet(Some(builder.expr().unit())))
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
        builder.expr().build_expr_(
            ast::ExprLoop(
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
        builder.expr().build_expr_(
            ast::ExprLoop(
                builder.block().build(),
                Some(builder.id("'a")),
            )
        )
    );
}
