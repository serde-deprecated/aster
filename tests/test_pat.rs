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
