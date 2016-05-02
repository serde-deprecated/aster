use syntax::abi::Abi;
use syntax::ast;
use syntax::codemap::{DUMMY_SP, respan};
use syntax::parse::token;
use syntax::print::pprust;
use syntax::ptr::P;

use aster::AstBuilder;
use aster::ident::ToIdent;
use aster::name::ToName;

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
            node: ast::ItemKind::Fn(
                builder.fn_decl().return_().isize(),
                ast::Unsafety::Normal,
                ast::Constness::NotConst,
                Abi::Rust,
                builder.generics().build(),
                block
            ),
            vis: ast::Visibility::Inherited,
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
            node: ast::ItemKind::Fn(
                builder.fn_decl().return_().isize(),
                ast::Unsafety::Normal,
                ast::Constness::NotConst,
                Abi::Rust,
                builder.generics()
                    .lifetime("'a").build()
                    .lifetime("'b").bound("'a").build()
                    .ty_param("T").build()
                    .build(),
                block
            ),
            vis: ast::Visibility::Inherited,
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
            node: ast::ItemKind::Struct(
                builder.variant_data().struct_().build(),
                builder.generics().build(),
            ),
            vis: ast::Visibility::Inherited,
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
            node: ast::ItemKind::Struct(
                builder.variant_data().struct_()
                    .field("x").ty().isize()
                    .field("y").ty().isize()
                    .build(),
                builder.generics().build(),
            ),
            vis: ast::Visibility::Inherited,
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
                .map(|f| builder.struct_field(f).ty().isize())
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
            node: ast::ItemKind::Struct(
                builder.variant_data().tuple()
                    .ty().isize()
                    .ty().isize()
                    .build(),
                builder.generics().build(),
            ),
            vis: ast::Visibility::Inherited,
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
            node: ast::ItemKind::Enum(
                ast::EnumDef {
                    variants: vec![],
                },
                builder.generics().build(),
            ),
            vis: ast::Visibility::Inherited,
            span: DUMMY_SP,
        })
    );
}

#[test]
fn test_enum() {
    let builder = AstBuilder::new();
    let enum_= builder.item().enum_("Enum")
        .id("A")
        .tuple("B")
            .ty().isize()
            .build()
        .tuple("C")
            .ty().isize()
            .ty().isize()
            .build()
        .struct_("D")
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
            node: ast::ItemKind::Enum(
                ast::EnumDef {
                    variants: vec![
                        builder.variant("A").unit(),
                        builder.variant("B").tuple()
                            .ty().isize()
                            .build(),
                        builder.variant("C").tuple()
                            .ty().isize()
                            .ty().isize()
                            .build(),
                        builder.variant("D").struct_()
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
            vis: ast::Visibility::Inherited,
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
                ident: token::keywords::Invalid.ident(),
                attrs: vec![],
                id: ast::DUMMY_NODE_ID,
                node: ast::ItemKind::Use(
                    P(respan(DUMMY_SP, view_path))
                ),
                vis: ast::Visibility::Inherited,
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
                respan(DUMMY_SP, ast::PathListItemKind::Mod {
                    id: ast::DUMMY_NODE_ID,
                    rename: None
                }),
                respan(DUMMY_SP, ast::PathListItemKind::Ident {
                    name: "Vec".to_ident(),
                    id: ast::DUMMY_NODE_ID,
                    rename: None
                }),
                respan(DUMMY_SP, ast::PathListItemKind::Ident {
                    name: "IntoIter".to_ident(),
                    id: ast::DUMMY_NODE_ID,
                    rename: None
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
                        style: ast::AttrStyle::Outer,
                        value: P(respan(
                            DUMMY_SP,
                            ast::MetaItemKind::NameValue(
                                builder.interned_string("doc"),
                                (*builder.lit().str("/// doc string")).clone(),
                            ),
                        )),
                        is_sugared_doc: true,
                    }
                ),
            ],
            id: ast::DUMMY_NODE_ID,
            node: ast::ItemKind::Struct(
                builder.variant_data().struct_()
                    .field("x").ty().isize()
                    .field("y").ty().isize()
                    .build(),
                builder.generics().build(),
            ),
            vis: ast::Visibility::Inherited,
            span: DUMMY_SP,
        })
    );
}

#[test]
fn test_extern_crate() {
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
            node: ast::ItemKind::ExternCrate(None),
            vis: ast::Visibility::Inherited,
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
            node: ast::ItemKind::ExternCrate(Some("aster1".to_name())),
            vis: ast::Visibility::Inherited,
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
            node: ast::ItemKind::Ty(
                builder.ty().isize(),
                builder.generics().build(),
            ),
            vis: ast::Visibility::Inherited,
            span: DUMMY_SP,
        })
    );
}

#[test]
fn test_trait() {
    let builder = AstBuilder::new();
    let trait_ = builder.item().trait_("Serialize")
        // Type
        .type_("MyFloat").build()

        // Const
        .const_("PI").ty().f64()

        // Method
        .method("serialize")
            .fn_decl()
                .default_return()
                .build()


        .build();

    assert_eq!(
        trait_,
        P(ast::Item {
            ident: builder.id("Serialize"),
            id: ast::DUMMY_NODE_ID,
            attrs: vec![],
            node: ast::ItemKind::Trait(
                ast::Unsafety::Normal,
                builder.generics().build(),
                P::from_vec(vec![
                ]),
                vec![
                    ast::TraitItem {
                        id: ast::DUMMY_NODE_ID,
                        ident: builder.id("MyFloat"),
                        attrs: vec![],
                        node: ast::TraitItemKind::Type(
                            P::from_vec(vec![]),
                            None,
                        ),
                        span: DUMMY_SP,
                    },

                    ast::TraitItem {
                        id: ast::DUMMY_NODE_ID,
                        ident: builder.id("PI"),
                        attrs: vec![],
                        node: ast::TraitItemKind::Const(
                            builder.ty().f64(),
                            None,
                        ),
                        span: DUMMY_SP,
                    },

                    ast::TraitItem {
                        id: ast::DUMMY_NODE_ID,
                        ident: builder.id("serialize"),
                        attrs: vec![],
                        node: ast::TraitItemKind::Method(
                            ast::MethodSig {
                                unsafety: ast::Unsafety::Normal,
                                constness: ast::Constness::NotConst,
                                abi: Abi::Rust,
                                decl: builder.fn_decl().default_return(),
                                generics: builder.generics().build(),
                                explicit_self: respan(DUMMY_SP, ast::SelfKind::Static),
                            },
                            None
                        ),
                        span: DUMMY_SP,
                    }
                ]
            ),
            vis: ast::Visibility::Inherited,
            span: DUMMY_SP,
        })
    );
}

#[test]
fn test_impl() {
    let builder = AstBuilder::new();
    let impl_ = builder.item().impl_()
        .trait_()
            .ids(&["ser", "Serialize"])
            .build()

        // Type
        .type_("MyFloat").f64()

        // Const
        .const_("PI")
            .expr().f64("3.14159265358979323846264338327950288")
            .ty().f64()

        // Method
        .method("serialize")
            .fn_decl().default_return()
            .block().build() // empty method block

        .ty().id("MySerializer");

    assert_eq!(
        impl_,
        P(ast::Item {
            ident: builder.id(""),
            id: ast::DUMMY_NODE_ID,
            attrs: vec![],
            node: ast::ItemKind::Impl(
                ast::Unsafety::Normal,
                ast::ImplPolarity::Positive,
                builder.generics().build(),
                Some(ast::TraitRef {
                    path: builder.path().id("ser").id("Serialize").build(),
                    ref_id: ast::DUMMY_NODE_ID,
                }),
                builder.ty().id("MySerializer"),
                vec![
                    ast::ImplItem {
                        id: ast::DUMMY_NODE_ID,
                        ident: builder.id("MyFloat"),
                        vis: ast::Visibility::Inherited,
                        defaultness: ast::Defaultness::Final,
                        attrs: vec![],
                        node: ast::ImplItemKind::Type(builder.ty().f64()),
                        span: DUMMY_SP,
                    },

                    ast::ImplItem {
                        id: ast::DUMMY_NODE_ID,
                        ident: builder.id("PI"),
                        vis: ast::Visibility::Inherited,
                        defaultness: ast::Defaultness::Final,
                        attrs: vec![],
                        node: ast::ImplItemKind::Const(
                            builder.ty().f64(),
                            builder.expr().f64("3.14159265358979323846264338327950288"),
                        ),
                        span: DUMMY_SP,
                    },

                    ast::ImplItem {
                        id: ast::DUMMY_NODE_ID,
                        ident: builder.id("serialize"),
                        vis: ast::Visibility::Inherited,
                        defaultness: ast::Defaultness::Final,
                        attrs: vec![],
                        node: ast::ImplItemKind::Method(
                            ast::MethodSig {
                                unsafety: ast::Unsafety::Normal,
                                constness: ast::Constness::NotConst,
                                abi: Abi::Rust,
                                decl: builder.fn_decl().default_return(),
                                generics: builder.generics().build(),
                                explicit_self: respan(DUMMY_SP, ast::SelfKind::Static),
                            },
                            builder.block().build()
                        ),
                        span: DUMMY_SP,
                    }
                ]
            ),
            vis: ast::Visibility::Inherited,
            span: DUMMY_SP,
        })
    );
}

#[test]
fn test_const() {
    let builder = AstBuilder::new();
    let const_ = builder.item().const_("PI")
        .expr().f64("3.14159265358979323846264338327950288")
        .ty().f64();

    assert_eq!(
        const_,
        P(ast::Item {
            ident: builder.id("PI"),
            id: ast::DUMMY_NODE_ID,
            attrs: vec![],
            node: ast::ItemKind::Const(
                builder.ty().f64(),
                builder.expr().f64("3.14159265358979323846264338327950288")
            ),
            vis: ast::Visibility::Inherited,
            span: DUMMY_SP,
        })
    );
}
