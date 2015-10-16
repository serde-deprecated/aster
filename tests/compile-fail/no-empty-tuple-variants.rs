extern crate aster;

fn main() {
    let builder = aster::AstBuilder::new();
    let tuple = builder.variant("a").tuple().build(); //~ ERROR no method named `build` found for type
}
