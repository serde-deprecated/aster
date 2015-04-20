#![feature(rustc_private)]

extern crate aster;
extern crate syntax;

use syntax::ast;
use syntax::codemap::{DUMMY_SP, respan};
use syntax::ptr::P;

use aster::AstBuilder;

#[test]
fn test_doc() {
    let builder = AstBuilder::new();
    assert_eq!(
        builder.attr().doc("/// doc string"),
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
        )
    );
}
