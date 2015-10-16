use syntax::ast;
use syntax::codemap::{DUMMY_SP, Spanned};
use syntax::ptr::P;

use aster::AstBuilder;

#[test]
fn test_empty_tuple_variant() {
    let builder = AstBuilder::new();
    let variant = builder.variant("A").tuple().build();

    assert_eq!(
        variant,
        P(Spanned {
            span: DUMMY_SP,
            node: ast::Variant_ {
                name: builder.id("A"),
                attrs: vec![],
                data: P(ast::VariantData::Tuple(vec![], ast::DUMMY_NODE_ID)),
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
                data: P(ast::VariantData::Tuple(
                    vec![
                        Spanned { span: DUMMY_SP, node: ast::StructField_ {
                            ty: builder.ty().isize(),
                            kind: ast::UnnamedField(ast::Inherited),
                            attrs: Vec::new(),
                            id: ast::DUMMY_NODE_ID,
                        }},
                        Spanned { span: DUMMY_SP, node: ast::StructField_ {
                            ty: builder.ty().isize(),
                            kind: ast::UnnamedField(ast::Inherited),
                            attrs: Vec::new(),
                            id: ast::DUMMY_NODE_ID,
                        }},
                    ],
                    ast::DUMMY_NODE_ID,
                )),
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
                data: builder.struct_def()
                             .field("a").ty().isize()
                             .field("b").ty().isize()
                             .build(),
                disr_expr: None,
            },
        })
    )
}
