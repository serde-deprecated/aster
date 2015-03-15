#![feature(rustc_private)]

extern crate aster;
extern crate syntax;

use syntax::ast;
use syntax::codemap::{DUMMY_SP, Spanned};
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
        .field("x").isize()
        .field("y").isize()
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
