#![cfg_attr(not(feature = "with-syntex"), feature(rustc_private))]

extern crate aster;

#[cfg(feature = "with-syntex")]
extern crate syntex_syntax as syntax;

#[cfg(not(feature = "with-syntex"))]
extern crate syntax;

fn main() {
    let builder = aster::AstBuilder::new();

    let expr = builder.expr()
        .add().u32(1).u32(2);

    // prints `1 + 2`.
    println!("{}", syntax::print::pprust::expr_to_string(&expr));
}
