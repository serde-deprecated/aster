use syntax::ast::{self, DUMMY_NODE_ID};
use syntax::codemap::DUMMY_SP;
use syntax::ptr::P;
use syntax::util::ThinVec;

use aster::AstBuilder;

#[test]
fn test_let() {
    let builder = AstBuilder::new();

    assert_eq!(
        builder.stmt()
            .let_().id("x").build(),
        ast::Stmt {
            id: DUMMY_NODE_ID,
            node: ast::StmtKind::Local(
                P(ast::Local {
                    pat: builder.pat().id("x"),
                    ty: None,
                    init: None,
                    id: DUMMY_NODE_ID,
                    span: DUMMY_SP,
                    attrs: ThinVec::new(),
                }),
            ),
            span: DUMMY_SP,
        }
    );

    assert_eq!(
        builder.stmt()
            .let_().id("x").ty().i8().build(),
        ast::Stmt {
            id: DUMMY_NODE_ID,
            node: ast::StmtKind::Local(
                P(ast::Local {
                    pat: builder.pat().id("x"),
                    ty: Some(builder.ty().i8()),
                    init: None,
                    id: DUMMY_NODE_ID,
                    span: DUMMY_SP,
                    attrs: ThinVec::new(),
                }),
            ),
            span: DUMMY_SP,
        }
    );

    assert_eq!(
        builder.stmt()
            .let_().id("x").expr().i8(5),
        ast::Stmt {
            id: DUMMY_NODE_ID,
            node: ast::StmtKind::Local(
                P(ast::Local {
                    pat: builder.pat().id("x"),
                    ty: None,
                    init: Some(builder.expr().i8(5)),
                    id: DUMMY_NODE_ID,
                    span: DUMMY_SP,
                    attrs: ThinVec::new(),
                }),
            ),
            span: DUMMY_SP,
        }
    );

    assert_eq!(
        builder.stmt()
            .let_().id("x").ty().i8().expr().i8(5),
        ast::Stmt {
            id: DUMMY_NODE_ID,
            node: ast::StmtKind::Local(
                P(ast::Local {
                    pat: builder.pat().id("x"),
                    ty: Some(builder.ty().i8()),
                    init: Some(builder.expr().i8(5)),
                    id: DUMMY_NODE_ID,
                    span: DUMMY_SP,
                    attrs: ThinVec::new(),
                }),
            ),
            span: DUMMY_SP,
        }
    );

    assert_eq!(
        builder.stmt().let_()
            .tuple()
                .pat().id("x")
                .pat().id("y")
                .build()
            .expr().tuple()
                .expr().u8(0)
                .expr().u16(1)
                .build(),
        ast::Stmt {
            id: DUMMY_NODE_ID,
            node: ast::StmtKind::Local(
                P(ast::Local {
                    pat: builder.pat().tuple()
                        .pat().id("x")
                        .pat().id("y")
                        .build(),
                    ty: None,
                    init: Some(
                        builder.expr().tuple()
                            .expr().u8(0)
                            .expr().u16(1)
                            .build()
                    ),
                    id: DUMMY_NODE_ID,
                    span: DUMMY_SP,
                    attrs: ThinVec::new(),
                }),
            ),
            span: DUMMY_SP,
        }
    );
}
