use syntax::ast;
use syntax::ptr::P;

use aster::AstBuilder;

#[test]
fn test_empty() {
    let builder = AstBuilder::new();
    let generics = builder.generics().build();

    assert_eq!(
        generics,
        ast::Generics {
            lifetimes: vec![],
            ty_params: P::new(),
            where_clause: ast::WhereClause {
                id: ast::DUMMY_NODE_ID,
                predicates: vec![],
            },
        }
    );
}

#[test]
fn test_with_ty_params_and_lifetimes() {
    let builder = AstBuilder::new();
    let generics = builder.generics()
        .lifetime("'a").build()
        .lifetime("'b").bound("'a").build()
        .ty_param("T").lifetime_bound("'a").build()
        .build();

    assert_eq!(
        generics,
        ast::Generics {
            lifetimes: vec![
                builder.lifetime_def("'a").build(),
                builder.lifetime_def("'b").bound("'a").build(),
            ],
            ty_params: P::from_vec(vec![
                builder.ty_param("T").lifetime_bound("'a").build(),
            ]),
            where_clause: ast::WhereClause {
                id: ast::DUMMY_NODE_ID,
                predicates: vec![],
            },
        }
    );
}
