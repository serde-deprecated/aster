#![cfg_attr(not(feature = "syntex"), feature(rustc_private))]

#[cfg(feature = "syntex")]
extern crate syntex_syntax as syntax;

#[cfg(not(feature = "syntex"))]
extern crate syntax;

extern crate aster;

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
                kind: ast::TupleVariantKind(vec![]),
                id: ast::DUMMY_NODE_ID,
                disr_expr: None,
                vis: ast::Inherited,
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
                kind: ast::TupleVariantKind(vec![
                    ast::VariantArg {
                        ty: builder.ty().isize(),
                        id: ast::DUMMY_NODE_ID,
                    },
                    ast::VariantArg {
                        ty: builder.ty().isize(),
                        id: ast::DUMMY_NODE_ID,
                    },
                ]),
                id: ast::DUMMY_NODE_ID,
                disr_expr: None,
                vis: ast::Inherited,
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
                kind: ast::StructVariantKind(
                    builder.struct_def()
                        .field("a").ty().isize()
                        .field("b").ty().isize()
                        .build()
                ),
                id: ast::DUMMY_NODE_ID,
                disr_expr: None,
                vis: ast::Inherited,
            },
        })
    )
}
