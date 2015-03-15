#![feature(rustc_private)]

extern crate aster;
extern crate syntax;

use syntax::print::pprust;
use aster::AstBuilder;

#[test]
fn test() {
    let builder = AstBuilder::new();

    // stmt: `let x;`
    let stmt = builder.stmt().let_().id("x").build();
    println!("stmt: {}", pprust::stmt_to_string(&*stmt));

    // stmt: `let x: i8;`
    let stmt = builder.stmt().let_()
        .id("x")
        .ty().i8()
        .build();
    println!("stmt: {}", pprust::stmt_to_string(&*stmt));

    // stmt: `let x = 0u8;`
    let stmt = builder.stmt().let_()
        .id("x").expr().i8(0);
    println!("stmt: {}", pprust::stmt_to_string(&*stmt));

    // stmt: `let x: i8 = 0u8;`
    let stmt = builder.stmt().let_()
        .id("x")
        .ty().i8()
        .expr().i8(0);
    println!("stmt: {}", pprust::stmt_to_string(&*stmt));

    // stmt: `let (x, y) = (0u8, 1u16);`
    let stmt = builder.stmt().let_()
        .tuple()
            .pat().ref_id("x")
            .pat().id("x")
            .build()
        .expr().tuple()
            .expr().u8(0)
            .expr().u16(1)
            .build();
    println!("stmt: {}", pprust::stmt_to_string(&*stmt));
}
