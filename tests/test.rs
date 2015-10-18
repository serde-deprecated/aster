#![cfg_attr(not(feature = "with-syntex"), feature(rustc_private))]

extern crate aster;

#[cfg(feature = "with-syntex")]
extern crate syntex_syntax as syntax;

#[cfg(not(feature = "with-syntex"))]
extern crate syntax;

#[cfg(feature = "unstable")]
extern crate compiletest_rs as compiletest;

mod test_attr;
mod test_expr;
mod test_fn_decl;
mod test_generics;
mod test_item;
mod test_lit;
mod test_path;
mod test_stmt;
mod test_struct_field;
mod test_ty;
mod test_variant;
mod test_variant_data;

#[cfg(feature = "unstable")]
mod compile_tests;
