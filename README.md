Rust libsyntax ast builder.

Example
-------

Here is a simple example to build the `syntax::ast::Expr` that
represents adding two numbers together:

```rust
extern crate aster;
extern crate syntax;

fn main() {
    let builder = AstBuilder::new();

    let expr = builder.expr()
        .add().u32(1).u32(2);

    // prints `1 + 2`.
    println!("{}", syntax::pprint::expr_to_string(&expr));
}
```
