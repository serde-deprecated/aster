use syntax::ast;
use aster::AstBuilder;

#[test]
fn test_unit_variant() {
    let builder = AstBuilder::new();

    assert_eq!(
        builder.variant_data().unit(),
        ast::VariantData::Unit(ast::DUMMY_NODE_ID)
    );
}

#[test]
fn test_empty_struct() {
    let builder = AstBuilder::new();

    assert_eq!(
        builder.variant_data().struct_().build(),
        ast::VariantData::Struct(
            vec![],
            ast::DUMMY_NODE_ID,
        )
    );
}

#[test]
fn test_struct_fields() {
    let builder = AstBuilder::new();

    assert_eq!(
        builder.variant_data().struct_()
            .field("x").ty().isize()
            .field("y").ty().isize()
            .build(),
        ast::VariantData::Struct(
            vec![
                builder.struct_field("x").ty().isize(),
                builder.struct_field("y").ty().isize(),
            ],
            ast::DUMMY_NODE_ID,
        )
    );
}

#[test]
fn test_tuple_fields() {
    let builder = AstBuilder::new();

    assert_eq!(
        builder.variant_data().tuple()
            .ty().isize()
            .ty().isize()
            .build(),
        ast::VariantData::Tuple(
            vec![
                builder.tuple_field().ty().isize(),
                builder.tuple_field().ty().isize(),
            ],
            ast::DUMMY_NODE_ID,
        )
    );
}

#[test]
fn test_with_fields() {
    let builder = AstBuilder::new();
    let variant_data = builder.variant_data().struct_()
        .field("x").ty().isize()
        .field("y").ty().isize()
        .build();

    let variant_data2 = builder.variant_data().struct_()
        .with_fields(
            ["x", "y"].iter()
                .map(|f| builder.struct_field(f).ty().isize())
            )
        .build();

    assert_eq!(
        variant_data,
        variant_data2
    );
}
