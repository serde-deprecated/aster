#![feature(rustc_private)]

extern crate syntax_ast_builder;
extern crate syntax;

use syntax::print::pprust;
use syntax_ast_builder::{Ctx, AstBuilder};

#[test]
fn test() {
    let ctx = Ctx::new();
    let builder = AstBuilder::new(&ctx);

    // path: `isize`
    let path = builder.path().id("isize").build();
    println!("path: {}", pprust::path_to_string(&path));

    // path: `isize`
    let path = builder.path()
        .segment("isize").build()
        .build();
    println!("path: {}", pprust::path_to_string(&path));

    // path: `::std::thread::Thread`
    let path = builder.path().global()
        .id("std")
        .id("thread")
        .id("Thread")
        .build();
    println!("path: {}", pprust::path_to_string(&path));

    // type: `isize`
    let ty = builder.ty().isize();
    println!("ty: {}", pprust::ty_to_string(&*ty));

    // type: `Option<isize>`
    let ty = builder.ty().option().isize();
    println!("ty: {}", pprust::ty_to_string(&*ty));

    // type: `Result<isize, isize>`
    let ty = builder.ty().result().isize().isize();
    println!("ty: {}", pprust::ty_to_string(&*ty));

    // type: `()`
    let ty = builder.ty().tuple().build();
    println!("ty: {}", pprust::ty_to_string(&*ty));

    // type: `(isize, ((), isize))`
    let ty = builder.ty()
        .tuple()
            .ty().isize()
            .ty().tuple()
                .ty().unit()
                .ty().isize()
            .build()
        .build();
    println!("ty: {}", pprust::ty_to_string(&*ty));

    // expr: `5i8`
    let expr = builder.expr().i8(5);
    println!("expr: {}", pprust::expr_to_string(&*expr));

    // expr: `5is`
    let expr = builder.expr().isize(5);
    println!("expr: {}", pprust::expr_to_string(&*expr));

    // expr: `1i8 + 2i8`
    let expr = builder.expr().add().i8(1).i8(2);
    println!("expr: {}", pprust::expr_to_string(&*expr));

    // expr: `(1i8, ((), 2is))`
    let expr = builder.expr()
        .tuple()
            .expr().i8(1)
            .expr().tuple()
                .expr().unit()
                .expr().isize(2)
                .build()
            .build();
    println!("expr: {}", pprust::expr_to_string(&*expr));

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
        
    let fn_ = builder.item("foo").fn_()
        .output().isize()
        .block()
            .stmt().let_id("x").isize(1)
            .stmt().let_id("y").isize(2)
            .expr().add().id("x").id("y");
    println!("fn: {}", pprust::item_to_string(&*fn_));

    let fn_ = builder.item("bar").fn_()
        .output().isize()
        .generics()
            .lifetime("'a").build()
            .lifetime("'b").bound("'a").build()
            .ty_param("T").build()
            .build()
        .block()
            .stmt().let_id("x").isize(1)
            .stmt().let_id("y").isize(2)
            .expr().add().id("x").id("y");
    println!("fn: {}", pprust::item_to_string(&*fn_));
}
