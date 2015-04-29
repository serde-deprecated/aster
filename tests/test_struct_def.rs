#![cfg_attr(not(feature = "syntex"), feature(rustc_private))]

#[cfg(feature = "syntex")]
extern crate syntex_syntax as syntax;

#[cfg(not(feature = "syntex"))]
extern crate syntax;

extern crate aster;

use syntax::ast;
use syntax::codemap::{DUMMY_SP, Spanned, respan};
use syntax::ptr::P;

use aster::AstBuilder;

#[test]
fn test_empty() {
    let builder = AstBuilder::new();
    let struct_def = builder.struct_def().build();

    assert_eq!(
        struct_def,
        P(ast::StructDef {
            fields: vec![],
            ctor_id: None,
        })
    );
}

#[test]
fn test_fields() {
    let builder = AstBuilder::new();
    let struct_def = builder.struct_def()
        .field("x").ty().isize()
        .field("y").ty().isize()
        .build();

    assert_eq!(
        struct_def,
        P(ast::StructDef {
            fields: vec![
                Spanned {
                    span: DUMMY_SP,
                    node: ast::StructField_ {
                        kind: ast::NamedField(
                            builder.id("x"),
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
                        kind: ast::NamedField(
                            builder.id("y"),
                            ast::Inherited,
                        ),
                        id: ast::DUMMY_NODE_ID,
                        ty: builder.ty().isize(),
                        attrs: vec![],
                    },
                },
            ],
            ctor_id: None,
        })
    );
}

#[test]
fn test_attrs() {
    let builder = AstBuilder::new();
    let struct_def = builder.struct_def()
        .field("x")
            .attr().doc("/// doc string")
            .attr().automatically_derived()
            .ty().isize()
        .build();

    assert_eq!(
        struct_def,
        P(ast::StructDef {
            fields: vec![
                Spanned {
                    span: DUMMY_SP,
                    node: ast::StructField_ {
                        kind: ast::NamedField(
                            builder.id("x"),
                            ast::Inherited,
                        ),
                        id: ast::DUMMY_NODE_ID,
                        ty: builder.ty().isize(),
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
                            respan(
                                DUMMY_SP,
                                ast::Attribute_ {
                                    id: ast::AttrId(1),
                                    style: ast::AttrOuter,
                                    value: P(respan(
                                        DUMMY_SP,
                                        ast::MetaWord(builder.interned_string("automatically_derived")),
                                    )),
                                    is_sugared_doc: false,
                                }
                            ),
                        ],
                    },
                },
            ],
            ctor_id: None,
        })
    );
}
