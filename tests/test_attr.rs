#![cfg_attr(not(feature = "syntex"), feature(rustc_private))]

#[cfg(feature = "syntex")]
extern crate syntex_syntax as syntax;

#[cfg(not(feature = "syntex"))]
extern crate syntax;

extern crate aster;

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
