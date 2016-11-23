use syntax::ast;
use syntax::codemap::DUMMY_SP;

use aster::AstBuilder;

#[test]
fn test_doc() {
    let builder = AstBuilder::new();
    assert_eq!(
        builder.attr().doc("/// doc string"),
        ast::Attribute {
            id: ast::AttrId(0),
            style: ast::AttrStyle::Outer,
            value: ast::MetaItem {
                name: builder.symbol("doc"),
                node: ast::MetaItemKind::NameValue(
                    (*builder.lit().str("/// doc string")).clone(),
                ),
                span: DUMMY_SP,
            },
            is_sugared_doc: true,
            span: DUMMY_SP,
        }
    );
}
