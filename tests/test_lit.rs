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
                node: ast::LitKind::Int(value, lit_int_ty),
            })
        );
    }
    check(builder.lit().u8(1), 1, ast::LitIntType::Unsigned(ast::UintTy::U8));
    check(builder.lit().u16(1), 1, ast::LitIntType::Unsigned(ast::UintTy::U16));
    check(builder.lit().u32(1), 1, ast::LitIntType::Unsigned(ast::UintTy::U32));
    check(builder.lit().u64(1), 1, ast::LitIntType::Unsigned(ast::UintTy::U64));
    check(builder.lit().usize(1), 1, ast::LitIntType::Unsigned(ast::UintTy::Us));
    check(builder.lit().uint(1), 1, ast::LitIntType::Unsuffixed);

    check(builder.lit().int(1), 1, ast::LitIntType::Unsuffixed);
}

#[test]
fn test_bool() {
    let builder = AstBuilder::new();

    assert_eq!(builder.lit().bool(true),
        P(Spanned {
            span: DUMMY_SP,
            node: ast::LitKind::Bool(true)
        })
    );

    assert_eq!(builder.lit().bool(false),
        P(Spanned {
            span: DUMMY_SP,
            node: ast::LitKind::Bool(false)
        })
    );

    assert_eq!(builder.lit().true_(),
        P(Spanned {
            span: DUMMY_SP,
            node: ast::LitKind::Bool(true)
        })
    );

    assert_eq!(builder.lit().false_(),
        P(Spanned {
            span: DUMMY_SP,
            node: ast::LitKind::Bool(false)
        })
    );
}

#[test]
fn test_char() {
    let builder = AstBuilder::new();

    assert_eq!(builder.lit().char('a'),
        P(Spanned {
            span: DUMMY_SP,
            node: ast::LitKind::Char('a')
        })
    );
}

#[test]
fn test_byte() {
    let builder = AstBuilder::new();

    assert_eq!(builder.lit().byte(b'a'),
        P(Spanned {
            span: DUMMY_SP,
            node: ast::LitKind::Byte(b'a')
        })
    );
}

#[test]
fn test_str() {
    let builder = AstBuilder::new();

    assert_eq!(builder.lit().str("string"),
        P(Spanned {
            span: DUMMY_SP,
            node: ast::LitKind::Str(
                builder.symbol("string"),
                ast::StrStyle::Cooked,
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
            node: ast::LitKind::ByteStr(Rc::new(Vec::from(&b"string"[..]))),
        })
    );
}
