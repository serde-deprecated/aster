use syntax::ast;
use syntax::codemap::DUMMY_SP;
use syntax::ptr::P;

use aster::AstBuilder;

#[test]
fn test_id() {
    let builder = AstBuilder::new();
    let path = builder.path().id("isize").build();

    assert_eq!(
        path,
        ast::Path {
            span: DUMMY_SP,
            segments: vec![
                ast::PathSegment {
                    identifier: builder.id("isize"),
                    span: DUMMY_SP,
                    parameters: None,
                },
            ]
        }
    );
}

#[test]
fn test_single_segment() {
    let builder = AstBuilder::new();
    let path = builder.path()
        .segment("isize").build()
        .build();

    assert_eq!(
        path,
        ast::Path {
            span: DUMMY_SP,
            segments: vec![
                ast::PathSegment {
                    identifier: builder.id("isize"),
                    span: DUMMY_SP,
                    parameters: None,
                },
            ]
        }
    );
}

#[test]
fn test_multiple_segments() {
    let builder = AstBuilder::new();
    let path = builder.path().global()
        .id("std")
        .id("thread")
        .id("Thread")
        .build();

    assert_eq!(
        path,
        ast::Path {
            span: DUMMY_SP,
            segments: vec![
                ast::PathSegment::crate_root(),
                ast::PathSegment {
                    identifier: builder.id("std"),
                    span: DUMMY_SP,
                    parameters: None,
                },
                ast::PathSegment {
                    identifier: builder.id("thread"),
                    span: DUMMY_SP,
                    parameters: None,
                },
                ast::PathSegment {
                    identifier: builder.id("Thread"),
                    span: DUMMY_SP,
                    parameters: None,
                },
            ]
        }
    );
}

#[test]
fn test_option() {
    let builder = AstBuilder::new();
    let path = builder.path().global()
        .id("std")
        .id("option")
        .segment("Option")
            .with_ty(builder.ty().id("isize"))
            .build()
        .build();

    assert_eq!(
        path,
        ast::Path {
            span: DUMMY_SP,
            segments: vec![
                ast::PathSegment::crate_root(),
                ast::PathSegment {
                    identifier: builder.id("std"),
                    span: DUMMY_SP,
                    parameters: None,
                },
                ast::PathSegment {
                    identifier: builder.id("option"),
                    span: DUMMY_SP,
                    parameters: None,
                },
                ast::PathSegment {
                    identifier: builder.id("Option"),
                    span: DUMMY_SP,
                    parameters: Some(P(ast::AngleBracketed(ast::AngleBracketedParameterData {
                        lifetimes: vec![],
                        types: vec![
                            builder.ty().isize(),
                        ],
                        bindings: vec![],
                    }))),
                },
            ]
        }
    );
}

#[test]
fn test_lifetimes() {
    let builder = AstBuilder::new();
    let path = builder.path()
        .segment("Foo")
            .lifetime("'a")
            .build()
        .build();

    assert_eq!(
        path,
        ast::Path {
            span: DUMMY_SP,
            segments: vec![
                ast::PathSegment {
                    identifier: builder.id("Foo"),
                    span: DUMMY_SP,
                    parameters: Some(P(ast::AngleBracketed(ast::AngleBracketedParameterData {
                        lifetimes: vec![
                            builder.lifetime("'a"),
                        ],
                        types: vec![],
                        bindings: vec![],
                    }))),
                },
            ]
        }
    );
}

#[test]
fn test_parenthesized_no_return() {
    let builder = AstBuilder::new();
    let path = builder.path().segment("Fn").ty().u8().no_return().build();

    assert_eq!(
        path,
        ast::Path {
            span: DUMMY_SP,
            segments: vec![
                ast::PathSegment {
                    identifier: builder.id("Fn"),
                    span: DUMMY_SP,
                    parameters: Some(P(ast::PathParameters::Parenthesized(
                        ast::ParenthesizedParameterData {
                            span: DUMMY_SP,
                            inputs: vec![builder.ty().u8()],
                            output: None,
                        }
                    ))),
                },
            ]
        }
    );
}

#[test]
fn test_parenthesized_with_return() {
    let builder = AstBuilder::new();
    let path = builder.path().segment("FnMut").ty().u16().return_().u32().build();

    assert_eq!(
        path,
        ast::Path {
            span: DUMMY_SP,
            segments: vec![
                ast::PathSegment {
                    identifier: builder.id("FnMut"),
                    span: DUMMY_SP,
                    parameters: Some(P(ast::PathParameters::Parenthesized(
                        ast::ParenthesizedParameterData {
                            span: DUMMY_SP,
                            inputs: vec![builder.ty().u16()],
                            output: Some(builder.ty().u32()),
                        }
                    ))),
                },
            ]
        }
    );
}
