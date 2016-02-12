use syntax::ast;
use syntax::codemap::{DUMMY_SP, respan};
use syntax::ptr::P;

use aster::AstBuilder;

#[test]
fn test_named() {
    let builder = AstBuilder::new();

    assert_eq!(
        builder.struct_field("x").ty().isize(),
        respan(
            DUMMY_SP,
            ast::StructField_ {
                kind: ast::NamedField(
                    builder.id("x"),
                    ast::Visibility::Inherited,
                ),
                id: ast::DUMMY_NODE_ID,
                ty: builder.ty().isize(),
                attrs: vec![],
            },
        )
    );
}

#[test]
fn test_unnamed() {
    let builder = AstBuilder::new();

    assert_eq!(
        builder.tuple_field().ty().isize(),
        respan(
            DUMMY_SP,
            ast::StructField_ {
                kind: ast::UnnamedField(ast::Visibility::Inherited),
                id: ast::DUMMY_NODE_ID,
                ty: builder.ty().isize(),
                attrs: vec![],
            },
        )
    );
}

#[test]
fn test_attrs() {
    let builder = AstBuilder::new();

    assert_eq!(
        builder.struct_field("x")
            .attr().doc("/// doc string")
            .attr().automatically_derived()
            .ty().isize(),
        respan(
            DUMMY_SP,
            ast::StructField_ {
                kind: ast::NamedField(
                    builder.id("x"),
                    ast::Visibility::Inherited,
                ),
                id: ast::DUMMY_NODE_ID,
                ty: builder.ty().isize(),
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
                    respan(
                        DUMMY_SP,
                        ast::Attribute_ {
                            id: ast::AttrId(1),
                            style: ast::AttrStyle::Outer,
                            value: P(respan(
                                DUMMY_SP,
                                ast::MetaItemKind::Word(builder.interned_string("automatically_derived")),
                            )),
                            is_sugared_doc: false,
                        }
                    ),
                ],
            },
        )
    );
}
