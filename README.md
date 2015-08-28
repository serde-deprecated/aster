Rust Syntax Ast Builder
=======================

[![Build Status](https://api.travis-ci.org/serde-rs/aster.png?branch=master)](https://travis-ci.org/erickt/rust-aster)
[![Latest Version](https://img.shields.io/crates/v/aster.svg)](https://crates.io/crates/aster)

Aster is a framework that simplifies generating Rust AST. It supports using
[syntex](https://github.com/erickt/rust-syntex) with stable Rust, and the builtin
libsyntax with the nightly Rust.

Example
-------

This example demonstrates how to use Aster to create a simple compound
expression. We will take advantage of
[Cargo features](http://doc.crates.io/manifest.html#the-[features]-section) to
optionally switch between the two different backends. Let's start with the
`Cargo.toml` file:

```toml
[package]
name = "hello_world"
version = "0.3.0"
authors = ["Erick Tryzelaar <erick.tryzelaar@gmail.com>"]

[features]
default = ["aster/default", "syntex_syntax"]
nightly = ["aster/nightly"]

[dependencies]
aster = { version = "*", default_features = false }
syntex_syntax = { version = "*", optional = true }
```

Here is the actual script:

```rust
#![cfg_attr(feature = "nightly", feature(rustc_private))]

extern crate aster as aster_lib;

#[cfg(feature = "nightly")]
use aster_lib::syntax as aster;

#[cfg(not(feature = "nightly"))]
use aster_lib::syntex as aster;

fn main() {
    let builder = aster::AstBuilder::new();

    let expr = builder.expr()
        .add().u32(1).u32(2);

    // prints `1 + 2`.
    println!(
        "{}",
        // Aster re-exports the syntax library to simplify it's use.
        aster::syntax::print::pprust::expr_to_string(&expr));
}
```

When run with `cargo run`, the example will use syntex. With
`cargo run --no-default-features --features nightly`, it will use libsyntax.
