use syntax::ast;
use syntax::codemap::DUMMY_SP;

use aster::AstBuilder;

#[test]
fn test_named() {
    let builder = AstBuilder::new();

    assert_eq!(
        builder.struct_field("x").ty().isize(),
        ast::StructField {
            ident: Some(builder.id("x")),
            vis: ast::Visibility::Inherited,
            id: ast::DUMMY_NODE_ID,
            ty: builder.ty().isize(),
            attrs: vec![],
            span: DUMMY_SP
        }
    );
}

#[test]
fn test_unnamed() {
    let builder = AstBuilder::new();

    assert_eq!(
        builder.tuple_field().ty().isize(),
        ast::StructField {
            ident: None,
            vis: ast::Visibility::Inherited,
            id: ast::DUMMY_NODE_ID,
            ty: builder.ty().isize(),
            attrs: vec![],
            span: DUMMY_SP
        }
    );
}

#[test]
fn test_attrs() {
    let builder = AstBuilder::new();

    let item0 = ast::MetaItem {
        name: builder.symbol("doc"),
        node: ast::MetaItemKind::NameValue(
            (*builder.lit().str("/// doc string")).clone(),
        ),
        span: DUMMY_SP,
    };

    let item1 = ast::MetaItem {
        name: builder.symbol("automatically_derived"),
        node: ast::MetaItemKind::Word,
        span: DUMMY_SP,
    };

    assert_eq!(
        builder.struct_field("x")
            .attr().doc("/// doc string")
            .attr().automatically_derived()
            .ty().isize(),
        ast::StructField {
            ident: Some(builder.id("x")),
            vis: ast::Visibility::Inherited,
            id: ast::DUMMY_NODE_ID,
            ty: builder.ty().isize(),
            span: DUMMY_SP,
            attrs: vec![
                ast::Attribute {
                    id: ast::AttrId(0),
                    style: ast::AttrStyle::Outer,
                    path: ast::Path::from_ident(item0.span, ast::Ident::with_empty_ctxt(item0.name)),
                    tokens: item0.node.tokens(item0.span),
                    is_sugared_doc: true,
                    span: DUMMY_SP,
                },
                ast::Attribute {
                    id: ast::AttrId(1),
                    style: ast::AttrStyle::Outer,
                    path: ast::Path::from_ident(item1.span, ast::Ident::with_empty_ctxt(item1.name)),
                    tokens: item1.node.tokens(item1.span),
                    is_sugared_doc: false,
                    span: DUMMY_SP,
                },
            ],
        }
    );
}
