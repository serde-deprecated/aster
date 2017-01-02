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
                    parameters: None,
                },
                ast::PathSegment {
                    identifier: builder.id("thread"),
                    parameters: None,
                },
                ast::PathSegment {
                    identifier: builder.id("Thread"),
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
                    parameters: None,
                },
                ast::PathSegment {
                    identifier: builder.id("option"),
                    parameters: None,
                },
                ast::PathSegment {
                    identifier: builder.id("Option"),
                    parameters: Some(P(ast::AngleBracketed(ast::AngleBracketedParameterData {
                        lifetimes: vec![],
                        types: P::from_vec(vec![
                            builder.ty().isize(),
                        ]),
                        bindings: P::new(),
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
                    parameters: Some(P(ast::AngleBracketed(ast::AngleBracketedParameterData {
                        lifetimes: vec![
                            builder.lifetime("'a"),
                        ],
                        types: P::new(),
                        bindings: P::new(),
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
