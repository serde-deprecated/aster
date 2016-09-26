use syntax::ast;
use syntax::codemap::DUMMY_SP;
use syntax::ptr::P;

use aster::AstBuilder;

const DUMMY_PAT: ast::Pat = ast::Pat {
    id: ast::DUMMY_NODE_ID,
    node: ast::PatKind::Wild,
    span: DUMMY_SP,
};

#[test]
fn test_pat_ref() {
    let builder = AstBuilder::new();

    let pat = builder.pat().ref_().expr().unit();

    assert_eq!(
        pat,
        P(ast::Pat { node: ast::PatKind::Ref(
            P(ast::Pat { node: ast::PatKind::Lit(
                builder.expr().unit(),
            ), ..DUMMY_PAT }),
            ast::Mutability::Immutable,
        ), ..DUMMY_PAT })
    );
}

#[test]
fn test_pat_ref_mut() {
    let builder = AstBuilder::new();

    let pat = builder.pat().ref_mut().expr().unit();

    assert_eq!(
        pat,
        P(ast::Pat { node: ast::PatKind::Ref(
            P(ast::Pat { node: ast::PatKind::Lit(
                builder.expr().unit(),
            ), ..DUMMY_PAT }),
            ast::Mutability::Mutable,
        ), ..DUMMY_PAT })
    );
}

#[test]
fn test_pat_some() {
    let builder = AstBuilder::new();

    let pat = builder.pat().some().id("value");

    assert_eq!(
        pat,
        P(ast::Pat {
            node: ast::PatKind::TupleStruct(
                builder.path()
                    .global()
                    .ids(&["std", "option", "Option", "Some"])
                    .build(),
                vec![builder.pat().id("value")],
                None
            ), ..DUMMY_PAT })
    );
}

#[test]
fn test_pat_none() {
    let builder = AstBuilder::new();

    let pat = builder.pat().none();

    assert_eq!(
        pat,
        P(ast::Pat {
            node: ast::PatKind::TupleStruct(
                builder.path()
                    .global()
                    .ids(&["std", "option", "Option", "None"])
                    .build(),
                vec![],
                None
            ), ..DUMMY_PAT })
    );
}

#[test]
fn test_pat_ok() {
    let builder = AstBuilder::new();

    let pat = builder.pat().ok().id("value");

    assert_eq!(
        pat,
        P(ast::Pat {
            node: ast::PatKind::TupleStruct(
                builder.path()
                    .global()
                    .ids(&["std", "result", "Result", "Ok"])
                    .build(),
                vec![builder.pat().id("value")],
                None
            ), ..DUMMY_PAT })
    );
}

#[test]
fn test_pat_err() {
    let builder = AstBuilder::new();

    let pat = builder.pat().err().id("value");

    assert_eq!(
        pat,
        P(ast::Pat {
            node: ast::PatKind::TupleStruct(
                builder.path()
                    .global()
                    .ids(&["std", "result", "Result", "Err"])
                    .build(),
                vec![builder.pat().id("value")],
                None
            ), ..DUMMY_PAT })
    );
}
