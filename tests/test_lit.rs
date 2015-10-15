use std::rc::Rc;

use syntax::ast;
use syntax::codemap::{DUMMY_SP, Spanned};
use syntax::ptr::P;

use aster::AstBuilder;

#[test]
fn test_int() {
    let builder = AstBuilder::new();

    fn check(lit: P<ast::Lit>, value: u64, lit_int_ty: ast::LitIntType) {
        assert_eq!(
            lit,
            P(Spanned {
                span: DUMMY_SP,
                node: ast::LitInt(value, lit_int_ty),
            })
        );
    }

    check(builder.lit().i8(1), 1, ast::SignedIntLit(ast::TyI8, ast::Plus));
    check(builder.lit().i16(1), 1, ast::SignedIntLit(ast::TyI16, ast::Plus));
    check(builder.lit().i32(1), 1, ast::SignedIntLit(ast::TyI32, ast::Plus));
    check(builder.lit().i64(1), 1, ast::SignedIntLit(ast::TyI64, ast::Plus));
    check(builder.lit().isize(1), 1, ast::SignedIntLit(ast::TyIs, ast::Plus));

    check(builder.lit().i8(-1), !0, ast::SignedIntLit(ast::TyI8, ast::Minus));
    check(builder.lit().i16(-1), !0, ast::SignedIntLit(ast::TyI16, ast::Minus));
    check(builder.lit().i32(-1), !0, ast::SignedIntLit(ast::TyI32, ast::Minus));
    check(builder.lit().i64(-1), !0, ast::SignedIntLit(ast::TyI64, ast::Minus));
    check(builder.lit().isize(-1), !0, ast::SignedIntLit(ast::TyIs, ast::Minus));

    check(builder.lit().u8(1), 1, ast::UnsignedIntLit(ast::TyU8));
    check(builder.lit().u16(1), 1, ast::UnsignedIntLit(ast::TyU16));
    check(builder.lit().u32(1), 1, ast::UnsignedIntLit(ast::TyU32));
    check(builder.lit().u64(1), 1, ast::UnsignedIntLit(ast::TyU64));
    check(builder.lit().usize(1), 1, ast::UnsignedIntLit(ast::TyUs));

    check(builder.lit().int(1), 1, ast::UnsuffixedIntLit(ast::Plus));
    check(builder.lit().int(-1), !0, ast::UnsuffixedIntLit(ast::Minus));
}

#[test]
fn test_bool() {
    let builder = AstBuilder::new();

    assert_eq!(builder.lit().bool(true),
        P(Spanned {
            span: DUMMY_SP,
            node: ast::LitBool(true)
        })
    );

    assert_eq!(builder.lit().bool(false),
        P(Spanned {
            span: DUMMY_SP,
            node: ast::LitBool(false)
        })
    );
}

#[test]
fn test_char() {
    let builder = AstBuilder::new();

    assert_eq!(builder.lit().char('a'),
        P(Spanned {
            span: DUMMY_SP,
            node: ast::LitChar('a')
        })
    );
}

#[test]
fn test_byte() {
    let builder = AstBuilder::new();

    assert_eq!(builder.lit().byte(b'a'),
        P(Spanned {
            span: DUMMY_SP,
            node: ast::LitByte(b'a')
        })
    );
}

#[test]
fn test_str() {
    let builder = AstBuilder::new();

    assert_eq!(builder.lit().str("string"),
        P(Spanned {
            span: DUMMY_SP,
            node: ast::LitStr(
                builder.interned_string("string"),
                ast::CookedStr,
            ),
        })
    );
}

#[test]
fn test_byte_str() {
    let builder = AstBuilder::new();

    assert_eq!(builder.lit().byte_str(&b"string"[..]),
        P(Spanned {
            span: DUMMY_SP,
            node: ast::LitByteStr(Rc::new(Vec::from(&b"string"[..]))),
        })
    );
}
