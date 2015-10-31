use syntax::ast;
use syntax::codemap::{DUMMY_SP, Spanned};
use syntax::ptr::P;

use aster::AstBuilder;

#[test]
fn test_unit_variant() {
    let builder = AstBuilder::new();
    let variant = builder.variant("A").unit();

    assert_eq!(
        variant,
        P(Spanned {
            span: DUMMY_SP,
            node: ast::Variant_ {
                name: builder.id("A"),
                attrs: vec![],
                data: builder.variant_data().unit().and_then(|x| x),
                disr_expr: None,
            },
        })
    )
}

#[test]
fn test_tuple_variant() {
    let builder = AstBuilder::new();
    let variant = builder.variant("A").tuple()
        .ty().isize()
        .ty().isize()
        .build();

    assert_eq!(
        variant,
        P(Spanned {
            span: DUMMY_SP,
            node: ast::Variant_ {
                name: builder.id("A"),
                attrs: vec![],
                data: builder.variant_data().tuple()
                    .ty().isize()
                    .ty().isize()
                    .build().and_then(|x| x),
                disr_expr: None,
            },
        })
    )
}

#[test]
fn test_struct_variant() {
    let builder = AstBuilder::new();
    let variant = builder.variant("A").struct_()
        .field("a").ty().isize()
        .field("b").ty().isize()
        .build();

    assert_eq!(
        variant,
        P(Spanned {
            span: DUMMY_SP,
            node: ast::Variant_ {
                name: builder.id("A"),
                attrs: vec![],
                data: builder.variant_data().struct_()
                    .field("a").ty().isize()
                    .field("b").ty().isize()
                    .build().and_then(|x| x),
                disr_expr: None,
            },
        })
    )
}
