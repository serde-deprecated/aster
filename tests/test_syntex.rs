#![cfg_attr(not(feature = "syntex"), feature(rustc_private))]

#[cfg(feature = "syntex")]
extern crate syntex_syntax as syntax;

#[cfg(not(feature = "syntex"))]
extern crate syntax;

#[cfg(feature = "syntex")]
#[test]
fn test_with_syntex() {
    assert!(true);
}
#[cfg(not (feature = "syntex"))]
#[test]
fn test_without_syntex() {
    assert!(true);
}
