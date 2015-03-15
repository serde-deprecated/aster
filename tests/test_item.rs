#![feature(rustc_private)]

extern crate aster;
extern crate syntax;

use syntax::abi::Abi;
use syntax::ast;
use syntax::codemap::{DUMMY_SP, Spanned};
use syntax::ptr::P;

use aster::AstBuilder;

#[test]
fn test_fn() {
    let builder = AstBuilder::new();

    let block = builder.block()
        .stmt().let_id("x").isize(1)
        .stmt().let_id("y").isize(2)
        .expr().add().id("x").id("y");
        
    let fn_ = builder.item().fn_("foo")
        .return_().isize()
        .build(block.clone());

    assert_eq!(
        fn_,
        P(ast::Item {
            ident: builder.id("foo"),
            attrs: vec![],
            id: ast::DUMMY_NODE_ID,
            node: ast::ItemFn(
                builder.fn_decl().return_().isize(),
                ast::Unsafety::Normal,
                Abi::Rust,
                builder.generics().build(),
                block
            ),
            vis: ast::Inherited,
            span: DUMMY_SP,
        })
    );
}

#[test]
fn test_generic_fn() {
    let builder = AstBuilder::new();

    let block = builder.block()
        .stmt().let_id("x").isize(1)
        .stmt().let_id("y").isize(2)
        .expr().add().id("x").id("y");
        
    let fn_ = builder.item().fn_("foo")
        .return_().isize()
        .generics()
            .lifetime("'a").build()
            .lifetime("'b").bound("'a").build()
            .ty_param("T").build()
            .build()
        .build(block.clone());

    assert_eq!(
        fn_,
        P(ast::Item {
            ident: builder.id("foo"),
            attrs: vec![],
            id: ast::DUMMY_NODE_ID,
            node: ast::ItemFn(
                builder.fn_decl().return_().isize(),
                ast::Unsafety::Normal,
                Abi::Rust,
                builder.generics()
                    .lifetime("'a").build()
                    .lifetime("'b").bound("'a").build()
                    .ty_param("T").build()
                    .build(),
                block
            ),
            vis: ast::Inherited,
            span: DUMMY_SP,
        })
    );
}

#[test]
fn test_empty_struct() {
    let builder = AstBuilder::new();
    let struct_ = builder.item().struct_("Struct").build();

    assert_eq!(
        struct_,
        P(ast::Item {
            ident: builder.id("Struct"),
            attrs: vec![],
            id: ast::DUMMY_NODE_ID,
            node: ast::ItemStruct(
                builder.struct_def().build(),
                builder.generics().build(),
            ),
            vis: ast::Inherited,
            span: DUMMY_SP,
        })
    );
}

#[test]
fn test_struct() {
    let builder = AstBuilder::new();
    let struct_ = builder.item().struct_("Struct")
        .field("x").isize()
        .field("y").isize()
        .build();

    assert_eq!(
        struct_,
        P(ast::Item {
            ident: builder.id("Struct"),
            attrs: vec![],
            id: ast::DUMMY_NODE_ID,
            node: ast::ItemStruct(
                builder.struct_def()
                    .field("x").isize()
                    .field("y").isize()
                    .build(),
                builder.generics().build(),
            ),
            vis: ast::Inherited,
            span: DUMMY_SP,
        })
    );
}

#[test]
fn test_tuple_struct() {
    let builder = AstBuilder::new();
    let struct_ = builder.item().tuple_struct("Struct")
        .ty().isize()
        .ty().isize()
        .build();

    assert_eq!(
        struct_,
        P(ast::Item {
            ident: builder.id("Struct"),
            attrs: vec![],
            id: ast::DUMMY_NODE_ID,
            node: ast::ItemStruct(
                P(ast::StructDef {
                    fields: vec![
                        Spanned {
                            span: DUMMY_SP,
                            node: ast::StructField_ {
                                kind: ast::UnnamedField(
                                    ast::Inherited,
                                ),
                                id: ast::DUMMY_NODE_ID,
                                ty: builder.ty().isize(),
                                attrs: vec![],
                            },
                        },
                        Spanned {
                            span: DUMMY_SP,
                            node: ast::StructField_ {
                                kind: ast::UnnamedField(
                                    ast::Inherited,
                                ),
                                id: ast::DUMMY_NODE_ID,
                                ty: builder.ty().isize(),
                                attrs: vec![],
                            },
                        },
                    ],
                    ctor_id: Some(ast::DUMMY_NODE_ID),
                }),
                builder.generics().build(),
            ),
            vis: ast::Inherited,
            span: DUMMY_SP,
        })
    );
}

#[test]
fn test_empty_enum() {
    let builder = AstBuilder::new();
    let enum_= builder.item().enum_("Enum").build();

    assert_eq!(
        enum_,
        P(ast::Item {
            ident: builder.id("Enum"),
            attrs: vec![],
            id: ast::DUMMY_NODE_ID,
            node: ast::ItemEnum(
                ast::EnumDef {
                    variants: vec![],
                },
                builder.generics().build(),
            ),
            vis: ast::Inherited,
            span: DUMMY_SP,
        })
    );
}

#[test]
fn test_enum() {
    let builder = AstBuilder::new();
    let enum_= builder.item().enum_("Enum")
        .id("A")
        .tuple("B").build()
        .tuple("C")
            .ty().isize()
            .build()
        .tuple("D")
            .ty().isize()
            .ty().isize()
            .build()
        .struct_("E")
            .field("a").isize()
            .build()
        .struct_("F")
            .field("a").isize()
            .field("b").isize()
            .build()
        .build();

    assert_eq!(
        enum_,
        P(ast::Item {
            ident: builder.id("Enum"),
            attrs: vec![],
            id: ast::DUMMY_NODE_ID,
            node: ast::ItemEnum(
                ast::EnumDef {
                    variants: vec![
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
                        }),
                        P(Spanned {
                            span: DUMMY_SP,
                            node: ast::Variant_ {
                                name: builder.id("B"),
                                attrs: vec![],
                                kind: ast::TupleVariantKind(vec![]),
                                id: ast::DUMMY_NODE_ID,
                                disr_expr: None,
                                vis: ast::Inherited,
                            },
                        }),
                        P(Spanned {
                            span: DUMMY_SP,
                            node: ast::Variant_ {
                                name: builder.id("C"),
                                attrs: vec![],
                                kind: ast::TupleVariantKind(vec![
                                    ast::VariantArg {
                                        ty: builder.ty().isize(),
                                        id: ast::DUMMY_NODE_ID,
                                    },
                                ]),
                                id: ast::DUMMY_NODE_ID,
                                disr_expr: None,
                                vis: ast::Inherited,
                            },
                        }),
                        P(Spanned {
                            span: DUMMY_SP,
                            node: ast::Variant_ {
                                name: builder.id("D"),
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
                        }),
                        P(Spanned {
                            span: DUMMY_SP,
                            node: ast::Variant_ {
                                name: builder.id("E"),
                                attrs: vec![],
                                kind: ast::StructVariantKind(
                                    builder.struct_def()
                                        .field("a").isize()
                                        .build()
                                ),
                                id: ast::DUMMY_NODE_ID,
                                disr_expr: None,
                                vis: ast::Inherited,
                            },
                        }),
                        P(Spanned {
                            span: DUMMY_SP,
                            node: ast::Variant_ {
                                name: builder.id("F"),
                                attrs: vec![],
                                kind: ast::StructVariantKind(
                                    builder.struct_def()
                                        .field("a").isize()
                                        .field("b").isize()
                                        .build()
                                ),
                                id: ast::DUMMY_NODE_ID,
                                disr_expr: None,
                                vis: ast::Inherited,
                            },
                        }),
                    ],
                },
                builder.generics().build(),
            ),
            vis: ast::Inherited,
            span: DUMMY_SP,
        })
    );
}
