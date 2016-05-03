use syntax::ast;
use syntax::codemap::DUMMY_SP;
use syntax::ptr::P;

use aster::AstBuilder;
use aster::path::IntoPath;
use aster::lifetime::{IntoLifetime, IntoLifetimeDef};

#[test]
fn test_ty_param_empty() {
    let builder = AstBuilder::new();
    let ty_param = builder.ty_param("T").build();

    assert_eq!(
        ty_param,
        ast::TyParam {
            ident: builder.id("T"),
            id: ast::DUMMY_NODE_ID,
            bounds: P::new(),
            default: None,
            span: DUMMY_SP,
        }
    );
}

#[test]
fn test_ty_param_default() {
    let builder = AstBuilder::new();
    let ty_param = builder.ty_param("T")
        .default()
        .usize()
        .build();

    assert_eq!(
        ty_param,
        ast::TyParam {
            ident: builder.id("T"),
            id: ast::DUMMY_NODE_ID,
            bounds: P::new(),
            default: Some(builder.ty().usize()),
            span: DUMMY_SP,
        }
    );
}

#[test]
fn test_ty_param_bounds() {
    let builder = AstBuilder::new();
    let ty_param = builder.ty_param("T")
        .bound().trait_("Trait")
            .lifetime("'a").build()
            .build()
        .bound().maybe_trait("Sized").build()
        .bound().lifetime("'b")
        .build();

    assert_eq!(
        ty_param,
        ast::TyParam {
            ident: builder.id("T"),
            id: ast::DUMMY_NODE_ID,
            bounds: P::from_vec(vec![
                ast::TyParamBound::TraitTyParamBound(
                    ast::PolyTraitRef {
                        bound_lifetimes: vec![
                            "'a".into_lifetime_def(),
                        ],
                        trait_ref: ast::TraitRef {
                            path: "Trait".into_path(),
                            ref_id: ast::DUMMY_NODE_ID,
                        },
                        span: DUMMY_SP,
                    },
                    ast::TraitBoundModifier::None,
                ),
                ast::TyParamBound::TraitTyParamBound(
                    ast::PolyTraitRef {
                        bound_lifetimes: Vec::new(),
                        trait_ref: ast::TraitRef {
                            path: "Sized".into_path(),
                            ref_id: ast::DUMMY_NODE_ID,
                        },
                        span: DUMMY_SP,
                    },
                    ast::TraitBoundModifier::Maybe,
                ),
                ast::TyParamBound::RegionTyParamBound(
                    "'b".into_lifetime()
                ),
            ]),
            default: None,
            span: DUMMY_SP,
        }
    );
}
