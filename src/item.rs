use std::iter::IntoIterator;

use syntax::abi::Abi;
use syntax::ast;
use syntax::codemap::{DUMMY_SP, Span, respan};
use syntax::parse::token;
use syntax::ptr::P;

use attr::AttrBuilder;
use block::BlockBuilder;
use fn_decl::FnDeclBuilder;
use generics::GenericsBuilder;
use ident::ToIdent;
use invoke::{Invoke, Identity};
use mac::MacBuilder;
use path::PathBuilder;
use struct_def::{StructDefBuilder, StructFieldBuilder};
use ty::TyBuilder;
use variant::{VariantBuilder, VariantTupleBuilder, VariantStructBuilder};

//////////////////////////////////////////////////////////////////////////////

pub struct ItemBuilder<F=Identity> {
    callback: F,
    span: Span,
    attrs: Vec<ast::Attribute>,
    vis: ast::Visibility,
}

impl ItemBuilder {
    pub fn new() -> Self {
        ItemBuilder::new_with_callback(Identity)
    }
}

impl<F> ItemBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    pub fn new_with_callback(callback: F) -> Self {
        ItemBuilder {
            callback: callback,
            span: DUMMY_SP,
            attrs: vec![],
            vis: ast::Visibility::Inherited,
        }
    }

    pub fn build(self, item: P<ast::Item>) -> F::Result {
        self.callback.invoke(item)
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn with_attr(mut self, attr: ast::Attribute) -> Self {
        self.attrs.push(attr);
        self
    }

    pub fn attr(self) -> AttrBuilder<Self> {
        AttrBuilder::new_with_callback(self)
    }

    pub fn pub_(mut self) -> Self {
        self.vis = ast::Visibility::Public;
        self
    }

    pub fn build_item_<T>(self, id: T, item_: ast::Item_) -> F::Result
        where T: ToIdent,
    {
        let item = ast::Item {
            ident: id.to_ident(),
            attrs: self.attrs,
            id: ast::DUMMY_NODE_ID,
            node: item_,
            vis: self.vis,
            span: self.span,
        };
        self.callback.invoke(P(item))
    }

    pub fn fn_<T>(self, id: T) -> FnDeclBuilder<ItemFnDeclBuilder<F>>
        where T: ToIdent,
    {
        let id = id.to_ident();
        FnDeclBuilder::new_with_callback(ItemFnDeclBuilder {
            builder: self,
            id: id,
        })
    }

    pub fn build_use(self, view_path: ast::ViewPath_) -> F::Result {
        let item = ast::ItemUse(P(respan(self.span, view_path)));
        self.build_item_(token::special_idents::invalid, item)
    }

    pub fn use_(self) -> PathBuilder<ItemUseBuilder<F>> {
        PathBuilder::new_with_callback(ItemUseBuilder {
            builder: self,
        })
    }

    pub fn struct_<T>(self, id: T) -> ItemStructBuilder<F>
        where T: ToIdent,
    {
        let id = id.to_ident();
        let generics = GenericsBuilder::new().build();

        ItemStructBuilder {
            builder: self,
            id: id,
            generics: generics,
        }
    }

    pub fn tuple_struct<T>(self, id: T) -> ItemTupleStructBuilder<F>
        where T: ToIdent,
    {
        let id = id.to_ident();
        let generics = GenericsBuilder::new().build();

        ItemTupleStructBuilder {
            builder: self,
            id: id,
            generics: generics,
            fields: vec![],
        }
    }

    pub fn enum_<T>(self, id: T) -> ItemEnumBuilder<F>
        where T: ToIdent,
    {
        let id = id.to_ident();
        let generics = GenericsBuilder::new().build();

        ItemEnumBuilder {
            builder: self,
            id: id,
            generics: generics,
            variants: vec![],
        }

    }

    pub fn mac(self) -> ItemMacBuilder<F>
    {
        ItemMacBuilder {
            builder: self,
        }
    }
}

impl<F> Invoke<ast::Attribute> for ItemBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(self, attr: ast::Attribute) -> Self {
        self.with_attr(attr)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemFnDeclBuilder<F> {
    builder: ItemBuilder<F>,
    id: ast::Ident,
}

impl<F> Invoke<P<ast::FnDecl>> for ItemFnDeclBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = ItemFnBuilder<F>;

    fn invoke(self, fn_decl: P<ast::FnDecl>) -> ItemFnBuilder<F> {
        let generics = GenericsBuilder::new().build();

        ItemFnBuilder {
            builder: self.builder,
            id: self.id,
            fn_decl: fn_decl,
            unsafety: ast::Unsafety::Normal,
            abi: Abi::Rust,
            generics: generics,
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemFnBuilder<F> {
    builder: ItemBuilder<F>,
    id: ast::Ident,
    fn_decl: P<ast::FnDecl>,
    unsafety: ast::Unsafety,
    abi: Abi,
    generics: ast::Generics,
}

impl<F> ItemFnBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    pub fn unsafe_(mut self) -> Self {
        self.unsafety = ast::Unsafety::Normal;
        self
    }

    pub fn abi(mut self, abi: Abi) -> Self {
        self.abi = abi;
        self
    }

    pub fn generics(self) -> GenericsBuilder<Self> {
        GenericsBuilder::new_with_callback(self)
    }

    pub fn build(self, block: P<ast::Block>) -> F::Result {
        self.builder.build_item_(self.id, ast::Item_::ItemFn(
            self.fn_decl,
            self.unsafety,
            self.abi,
            self.generics,
            block,
        ))
    }

    pub fn block(self) -> BlockBuilder<Self> {
        BlockBuilder::new_with_callback(self)
    }
}

impl<F> Invoke<ast::Generics> for ItemFnBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(mut self, generics: ast::Generics) -> Self {
        self.generics = generics;
        self
    }
}

impl<F> Invoke<P<ast::Block>> for ItemFnBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = F::Result;

    fn invoke(self, block: P<ast::Block>) -> F::Result {
        self.build(block)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemUseBuilder<F> {
    builder: ItemBuilder<F>,
}

impl<F> Invoke<ast::Path> for ItemUseBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = ItemUsePathBuilder<F>;

    fn invoke(self, path: ast::Path) -> ItemUsePathBuilder<F> {
        ItemUsePathBuilder {
            builder: self.builder,
            path: path,
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemUsePathBuilder<F> {
    builder: ItemBuilder<F>,
    path: ast::Path,
}

impl<F> ItemUsePathBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    pub fn as_<T>(self, id: T) -> F::Result
        where T: ToIdent,
    {
        self.builder.build_use(ast::ViewPathSimple(id.to_ident(), self.path))
    }

    pub fn build(self) -> F::Result {
        let id = {
            let segment = self.path.segments.last().expect("path with no segments!");
            segment.identifier
        };
        self.as_(id)
    }

    pub fn glob(self) -> F::Result {
        self.builder.build_use(ast::ViewPathGlob(self.path))
    }

    pub fn list(self) -> ItemUsePathListBuilder<F> {
        let span =  self.builder.span;
        ItemUsePathListBuilder {
            builder: self.builder,
            span: span,
            path: self.path,
            idents: Vec::new(),
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemUsePathListBuilder<F> {
    builder: ItemBuilder<F>,
    span: Span,
    path: ast::Path,
    idents: Vec<ast::PathListItem>,
}

impl<F> ItemUsePathListBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn self_(mut self) -> Self {
        self.idents.push(respan(self.span, ast::PathListMod {
            id: ast::DUMMY_NODE_ID,
        }));
        self
    }

    pub fn id<T>(mut self, id: T) -> Self
        where T: ToIdent,
    {
        self.idents.push(respan(self.span, ast::PathListIdent {
            name: id.to_ident(),
            id: ast::DUMMY_NODE_ID,
        }));
        self
    }

    pub fn build(self) -> F::Result {
        self.builder.build_use(ast::ViewPathList(self.path, self.idents))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemStructBuilder<F> {
    builder: ItemBuilder<F>,
    id: ast::Ident,
    generics: ast::Generics,
}

impl<F> ItemStructBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    pub fn generics(self) -> GenericsBuilder<Self> {
        GenericsBuilder::new_with_callback(self)
    }

    pub fn with_field(self, field: ast::StructField) -> StructDefBuilder<Self> {
        let span = self.builder.span;
        StructDefBuilder::new_with_callback(self).span(span).with_field(field)
    }

    pub fn field<T>(self, id: T) -> StructFieldBuilder<StructDefBuilder<Self>>
        where T: ToIdent,
    {
        let span = self.builder.span;
        StructDefBuilder::new_with_callback(self).span(span).field(id)
    }

    pub fn build(self) -> F::Result {
        StructDefBuilder::new_with_callback(self).build()
    }
}

impl<F> Invoke<ast::Generics> for ItemStructBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(mut self, generics: ast::Generics) -> Self {
        self.generics = generics;
        self
    }
}

impl<F> Invoke<P<ast::StructDef>> for ItemStructBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = F::Result;

    fn invoke(self, struct_def: P<ast::StructDef>) -> F::Result {
        let struct_ = ast::ItemStruct(struct_def, self.generics);
        self.builder.build_item_(self.id, struct_)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemTupleStructBuilder<F> {
    builder: ItemBuilder<F>,
    id: ast::Ident,
    generics: ast::Generics,
    fields: Vec<ast::StructField>,
}

impl<F> ItemTupleStructBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    pub fn generics(self) -> GenericsBuilder<Self> {
        GenericsBuilder::new_with_callback(self)
    }

    pub fn with_tys<I>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=P<ast::Ty>>,
    {
        for ty in iter {
            self = self.ty().build(ty);
        }
        self
    }

    pub fn ty(self) -> TyBuilder<Self> {
        TyBuilder::new_with_callback(self)
    }

    pub fn field(self) -> TyBuilder<StructFieldBuilder<Self>> {
        let span = self.builder.span;
        let builder = StructFieldBuilder::unnamed_with_callback(self).span(span);
        TyBuilder::new_with_callback(builder)
    }

    pub fn build(self) -> F::Result {
        let struct_def = ast::StructDef {
            fields: self.fields,
            ctor_id: Some(ast::DUMMY_NODE_ID),
        };
        let struct_ = ast::ItemStruct(P(struct_def), self.generics);
        self.builder.build_item_(self.id, struct_)
    }
}

impl<F> Invoke<ast::Generics> for ItemTupleStructBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(mut self, generics: ast::Generics) -> Self {
        self.generics = generics;
        self
    }
}

impl<F> Invoke<P<ast::Ty>> for ItemTupleStructBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(self, ty: P<ast::Ty>) -> Self {
        self.field().build(ty)
    }
}

impl<F> Invoke<ast::StructField> for ItemTupleStructBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(mut self, field: ast::StructField) -> Self {
        self.fields.push(field);
        self
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemEnumBuilder<F> {
    builder: ItemBuilder<F>,
    id: ast::Ident,
    generics: ast::Generics,
    variants: Vec<P<ast::Variant>>,
}

impl<F> ItemEnumBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    pub fn generics(self) -> GenericsBuilder<Self> {
        GenericsBuilder::new_with_callback(self)
    }

    pub fn with_variants<I>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=P<ast::Variant>>,
    {
        self.variants.extend(iter);
        self
    }

    pub fn with_variant(mut self, variant: P<ast::Variant>) -> Self {
        self.variants.push(variant);
        self
    }

    pub fn with_variant_(self, variant: ast::Variant_) -> Self {
        let variant = P(respan(self.builder.span, variant));
        self.with_variant(variant)
    }

    pub fn ids<I, T>(mut self, ids: I) -> Self
        where I: IntoIterator<Item=T>,
              T: ToIdent,
    {
        for id in ids.into_iter() {
            self = self.id(id);
        }
        self
    }

    pub fn id<T>(self, id: T) -> Self
        where T: ToIdent,
    {
        self.variant(id).tuple().build()
    }

    pub fn tuple<T>(self, id: T) -> VariantTupleBuilder<Self>
        where T: ToIdent,
    {
        self.variant(id).tuple()
    }

    pub fn struct_<T>(self, id: T) -> StructDefBuilder<VariantStructBuilder<Self>>
        where T: ToIdent,
    {
        self.variant(id).struct_()
    }

    pub fn variant<T>(self, id: T) -> VariantBuilder<Self>
        where T: ToIdent,
    {
        VariantBuilder::new_with_callback(id, self)
    }

    pub fn build(self) -> F::Result {
        let enum_def = ast::EnumDef {
            variants: self.variants,
        };
        let enum_ = ast::ItemEnum(enum_def, self.generics);
        self.builder.build_item_(self.id, enum_)
    }
}

impl<F> Invoke<ast::Generics> for ItemEnumBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(mut self, generics: ast::Generics) -> Self {
        self.generics = generics;
        self
    }
}

impl<F> Invoke<P<ast::Variant>> for ItemEnumBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(self, variant: P<ast::Variant>) -> Self {
        self.with_variant(variant)
    }
}

//////////////////////////////////////////////////////////////////////////////

/// A builder for macro invocation items.
///
/// Specifying the macro path returns a `MacBuilder`, which is used to
/// add expressions to the macro invocation.
pub struct ItemMacBuilder<F> {
    builder: ItemBuilder<F>,
}

impl<F> ItemMacBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    pub fn path(self) -> PathBuilder<Self> {
        PathBuilder::new_with_callback(self)
    }

    pub fn build(self, mac: ast::Mac) -> F::Result {
        let item_mac = ast::ItemMac(mac);
        self.builder.build_item_(ast::Ident::new(ast::Name(0)), item_mac)
    }
}

impl<F> Invoke<ast::Path> for ItemMacBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = MacBuilder<ItemMacBuilder<F>>;

    fn invoke(self, path: ast::Path) -> MacBuilder<Self> {
        MacBuilder::new_with_callback(self).path(path)
    }
}

impl<F> Invoke<ast::Mac> for ItemMacBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = F::Result;

    fn invoke(self, mac: ast::Mac) -> F::Result {
        self.build(mac)
    }
}
