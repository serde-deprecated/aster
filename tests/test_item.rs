#![cfg_attr(not(feature = "syntex"), feature(rustc_private))]

#[cfg(feature = "syntex")]
extern crate syntex_syntax as syntax;

#[cfg(not(feature = "syntex"))]
extern crate syntax;

extern crate aster;

use syntax::abi::Abi;
use syntax::ast;
use syntax::codemap::{DUMMY_SP, Spanned, respan};
use syntax::parse::token;
use syntax::print::pprust;
use syntax::ptr::P;

use aster::AstBuilder;
use aster::ident::ToIdent;

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
        .field("x").ty().isize()
        .field("y").ty().isize()
        .build();

    assert_eq!(
        struct_,
        P(ast::Item {
            ident: builder.id("Struct"),
            attrs: vec![],
            id: ast::DUMMY_NODE_ID,
            node: ast::ItemStruct(
                builder.struct_def()
                    .field("x").ty().isize()
                    .field("y").ty().isize()
                    .build(),
                builder.generics().build(),
            ),
            vis: ast::Inherited,
            span: DUMMY_SP,
        })
    );
}

#[test]
fn test_struct_with_fields() {
    let builder = AstBuilder::new();
    let struct_ = builder.item().struct_("Struct")
        .field("x").ty().isize()
        .field("y").ty().isize()
        .build();

    let struct_2 = builder.item().struct_("Struct")
        .with_fields(
            vec!["x","y"].iter()
                .map(|f| builder.field(f).ty().isize())
                )
        .build();

    assert_eq!(
        struct_,
        struct_2
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
            .field("a").ty().isize()
            .build()
        .struct_("F")
            .field("a").ty().isize()
            .field("b").ty().isize()
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
                        builder.variant("A").tuple().build(),
                        builder.variant("B").tuple().build(),
                        builder.variant("C").tuple()
                            .ty().isize()
                            .build(),
                        builder.variant("D").tuple()
                            .ty().isize()
                            .ty().isize()
                            .build(),
                        builder.variant("E").struct_()
                            .field("a").ty().isize()
                            .build(),
                        builder.variant("F").struct_()
                            .field("a").ty().isize()
                            .field("b").ty().isize()
                            .build(),
                    ],
                },
                builder.generics().build(),
            ),
            vis: ast::Inherited,
            span: DUMMY_SP,
        })
    );
}

#[test]
fn test_use() {
    fn check(item: P<ast::Item>, view_path: ast::ViewPath_) {
        assert_eq!(
            item,
            P(ast::Item {
                ident: token::special_idents::invalid,
                attrs: vec![],
                id: ast::DUMMY_NODE_ID,
                node: ast::ItemUse(
                    P(respan(DUMMY_SP, view_path))
                ),
                vis: ast::Inherited,
                span: DUMMY_SP,
            })
        );
    }

    let builder = AstBuilder::new();

    let item = builder.item().use_()
        .ids(&["std", "vec", "Vec"]).build()
        .build();

    check(
        item,
        ast::ViewPathSimple(
            "Vec".to_ident(),
            builder.path().ids(&["std", "vec", "Vec"]).build()
        )
    );

    let item = builder.item().use_()
        .ids(&["std", "vec", "Vec"]).build()
        .as_("MyVec");

    check(
        item,
        ast::ViewPathSimple(
            "MyVec".to_ident(),
            builder.path().ids(&["std", "vec", "Vec"]).build()
        )
    );

    let item = builder.item().use_()
        .ids(&["std", "vec"]).build()
        .glob();

    check(
        item,
        ast::ViewPathGlob(
            builder.path().ids(&["std", "vec"]).build()
        )
    );

    let item = builder.item().use_()
        .ids(&["std", "vec"]).build()
        .list()
        .build();

    check(
        item,
        ast::ViewPathList(
            builder.path().ids(&["std", "vec"]).build(),
            vec![],
        )
    );

    let item = builder.item().use_()
        .ids(&["std", "vec"]).build()
        .list()
        .self_()
        .id("Vec")
        .id("IntoIter")
        .build();

    check(
        item,
        ast::ViewPathList(
            builder.path().ids(&["std", "vec"]).build(),
            vec![
                respan(DUMMY_SP, ast::PathListMod {
                    id: ast::DUMMY_NODE_ID,
                }),
                respan(DUMMY_SP, ast::PathListIdent {
                    name: "Vec".to_ident(),
                    id: ast::DUMMY_NODE_ID,
                }),
                respan(DUMMY_SP, ast::PathListIdent {
                    name: "IntoIter".to_ident(),
                    id: ast::DUMMY_NODE_ID,
                }),
            ],
        )
    );
}

#[test]
fn test_attr() {
    let builder = AstBuilder::new();
    let struct_ = builder.item()
        .attr().doc("/// doc string")
        .struct_("Struct")
        .field("x").ty().isize()
        .field("y").ty().isize()
        .build();

    assert_eq!(
        struct_,
        P(ast::Item {
            ident: builder.id("Struct"),
            attrs: vec![
                respan(
                    DUMMY_SP,
                    ast::Attribute_ {
                        id: ast::AttrId(0),
                        style: ast::AttrOuter,
                        value: P(respan(
                            DUMMY_SP,
                            ast::MetaNameValue(
                                builder.interned_string("doc"),
                                (*builder.lit().str("/// doc string")).clone(),
                            ),
                        )),
                        is_sugared_doc: true,
                    }
                ),
            ],
            id: ast::DUMMY_NODE_ID,
            node: ast::ItemStruct(
                builder.struct_def()
                    .field("x").ty().isize()
                    .field("y").ty().isize()
                    .build(),
                builder.generics().build(),
            ),
            vis: ast::Inherited,
            span: DUMMY_SP,
        })
    );
}

#[test]
fn test_extern_crate() {
    use aster::name::ToName;

    let builder = AstBuilder::new();
    let item = builder.item()
        .extern_crate("aster")
        .build();

    assert_eq!(
        item,
        P(ast::Item {
            ident: builder.id("aster"),
            attrs: vec![],
            id: ast::DUMMY_NODE_ID,
            node: ast::ItemExternCrate(None),
            vis: ast::Inherited,
            span: DUMMY_SP,
        })
    );

    let item = builder.item()
        .extern_crate("aster")
        .with_name("aster1".to_name());

    assert_eq!(
        item,
        P(ast::Item {
            ident: builder.id("aster"),
            attrs: vec![],
            id: ast::DUMMY_NODE_ID,
            node: ast::ItemExternCrate(Some("aster1".to_name())),
            vis: ast::Inherited,
            span: DUMMY_SP,
        })
    );
}

#[test]
fn test_mac() {
    let builder = AstBuilder::new();
    let mac = builder.item().mac()
        .path().id("my_macro").build()
        .expr().str("abc")
        .expr().id(",")
        .expr().build_add(builder.expr().u32(0), builder.expr().u32(1))
        .build();

    assert_eq!(
        &pprust::item_to_string(&mac)[..],
        "my_macro! (\"abc\" , 0u32 + 1u32);"
        );

    let mac = builder.item().mac()
        .path().id("my_macro").build()
        .with_args(
            vec![
                builder.expr().str("abc"),
                builder.expr().id(","),
                builder.expr().build_add(builder.expr().u32(0), builder.expr().u32(1))
                ]
            )
        .build();

    assert_eq!(
        &pprust::item_to_string(&mac)[..],
        "my_macro! (\"abc\" , 0u32 + 1u32);"
        );
}

#[test]
fn test_type() {
    let builder = AstBuilder::new();
    let enum_= builder.item().type_("MyT")
        .ty().isize();

    assert_eq!(
        enum_,
        P(ast::Item {
            ident: builder.id("MyT"),
            id: ast::DUMMY_NODE_ID,
            attrs: vec![],
            node: ast::ItemTy(
                builder.ty().isize(),
                builder.generics().build(),
            ),
            vis: ast::Inherited,
            span: DUMMY_SP,
        })
    );
}
