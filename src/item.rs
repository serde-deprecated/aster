#![cfg_attr(feature = "unstable", allow(wrong_self_convention))]

use std::iter::IntoIterator;

use syntax::abi::Abi;
use syntax::ast;
use syntax::codemap::{DUMMY_SP, Span, respan};
use syntax::parse::token;
use syntax::ptr::P;

use attr::AttrBuilder;
use block::BlockBuilder;
use constant::{Const, ConstBuilder};
use fn_decl::FnDeclBuilder;
use generics::GenericsBuilder;
use ident::ToIdent;
use invoke::{Invoke, Identity};
use mac::MacBuilder;
use method::{Method, MethodBuilder};
use path::PathBuilder;
use struct_field::StructFieldBuilder;
use ty::TyBuilder;
use ty_param::TyParamBoundBuilder;
use variant::VariantBuilder;
use variant_data::{
    VariantDataBuilder,
    VariantDataStructBuilder,
    VariantDataTupleBuilder,
};

//////////////////////////////////////////////////////////////////////////////

pub struct ItemBuilder<F=Identity> {
    callback: F,
    span: Span,
    attrs: Vec<ast::Attribute>,
    vis: ast::Visibility,
}

impl ItemBuilder {
    pub fn new() -> Self {
        ItemBuilder::with_callback(Identity)
    }
}

impl<F> ItemBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    pub fn with_callback(callback: F) -> Self {
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
        AttrBuilder::with_callback(self)
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
        FnDeclBuilder::with_callback(ItemFnDeclBuilder {
            builder: self,
            id: id,
        })
    }

    pub fn build_use(self, view_path: ast::ViewPath_) -> F::Result {
        let item = ast::ItemUse(P(respan(self.span, view_path)));
        self.build_item_(token::special_idents::invalid, item)
    }

    pub fn use_(self) -> PathBuilder<ItemUseBuilder<F>> {
        PathBuilder::with_callback(ItemUseBuilder {
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

    pub fn unit_struct<T>(self, id: T) -> F::Result
        where T: ToIdent,
    {
        let id = id.to_ident();
        let data = VariantDataBuilder::new().unit();
        let generics = GenericsBuilder::new().build();

        let struct_ = ast::ItemStruct(data, generics);
        self.build_item_(id, struct_)
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

    pub fn extern_crate<T>(self, id: T) -> ItemExternCrateBuilder<F>
        where T: ToIdent,
    {
        let id = id.to_ident();

        ItemExternCrateBuilder {
            builder: self,
            id: id,
        }
    }

    pub fn mac(self) -> ItemMacBuilder<F> {
        ItemMacBuilder {
            builder: self,
        }
    }

    pub fn type_<T>(self, id: T) -> ItemTyBuilder<F>
        where T: ToIdent,
    {
        let id = id.to_ident();
        let generics = GenericsBuilder::new().build();

        ItemTyBuilder {
            builder: self,
            id: id,
            generics: generics,
        }
    }

    pub fn trait_<T>(self, id: T) -> ItemTraitBuilder<F>
        where T: ToIdent,
    {
        ItemTraitBuilder {
            builder: self,
            id: id.to_ident(),
            unsafety: ast::Unsafety::Normal,
            generics: GenericsBuilder::new().build(),
            bounds: vec![],
            items: vec![],
        }
    }

    pub fn impl_(self) -> ItemImplBuilder<F> {
        let generics = GenericsBuilder::new().build();

        ItemImplBuilder {
            builder: self,
            unsafety: ast::Unsafety::Normal,
            polarity: ast::ImplPolarity::Positive,
            generics: generics,
            trait_ref: None,
            items: vec![],
        }
    }

    pub fn const_<T>(self, id: T) -> ConstBuilder<ItemConstBuilder<F>>
        where T: ToIdent,
    {
        ConstBuilder::with_callback(ItemConstBuilder {
            builder: self,
            id: id.to_ident(),
        })
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
            constness: ast::Constness::NotConst,
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
    constness: ast::Constness,
    abi: Abi,
    generics: ast::Generics,
}

impl<F> ItemFnBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    pub fn unsafe_(mut self) -> Self {
        self.unsafety = ast::Unsafety::Unsafe;
        self
    }

    pub fn const_(mut self) -> Self {
        self.constness = ast::Constness::Const;
        self
    }

    pub fn abi(mut self, abi: Abi) -> Self {
        self.abi = abi;
        self
    }

    pub fn generics(self) -> GenericsBuilder<Self> {
        GenericsBuilder::with_callback(self)
    }

    pub fn build(self, block: P<ast::Block>) -> F::Result {
        self.builder.build_item_(self.id, ast::Item_::ItemFn(
            self.fn_decl,
            self.unsafety,
            self.constness,
            self.abi,
            self.generics,
            block,
        ))
    }

    pub fn block(self) -> BlockBuilder<Self> {
        BlockBuilder::with_callback(self)
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
            rename: None,
        }));
        self
    }

    pub fn id<T>(mut self, id: T) -> Self
        where T: ToIdent,
    {
        self.idents.push(respan(self.span, ast::PathListIdent {
            name: id.to_ident(),
            rename: None,
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
        GenericsBuilder::with_callback(self)
    }

    pub fn with_fields<I>(self, iter: I) -> VariantDataStructBuilder<Self>
        where I: IntoIterator<Item=ast::StructField>,
    {
        let span = self.builder.span;
        VariantDataBuilder::with_callback(self).span(span).struct_().with_fields(iter)
    }

    pub fn with_field(self, field: ast::StructField) -> VariantDataStructBuilder<Self> {
        let span = self.builder.span;
        VariantDataBuilder::with_callback(self).span(span).struct_().with_field(field)
    }

    pub fn field<T>(self, id: T) -> StructFieldBuilder<VariantDataStructBuilder<Self>>
        where T: ToIdent,
    {
        let span = self.builder.span;
        VariantDataBuilder::with_callback(self).span(span).struct_().field(id)
    }

    pub fn build(self) -> F::Result {
        VariantDataBuilder::with_callback(self).struct_().build()
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

impl<F> Invoke<ast::VariantData> for ItemStructBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = F::Result;

    fn invoke(self, data: ast::VariantData) -> F::Result {
        let struct_ = ast::ItemStruct(data, self.generics);
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
        GenericsBuilder::with_callback(self)
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
        TyBuilder::with_callback(self)
    }

    pub fn field(self) -> StructFieldBuilder<Self> {
        let span = self.builder.span;
        StructFieldBuilder::unnamed_with_callback(self).span(span)
    }

    pub fn build(self) -> F::Result {
        let data = ast::VariantData::Tuple(self.fields, ast::DUMMY_NODE_ID);
        let struct_ = ast::ItemStruct(data, self.generics);
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
        self.field().build_ty(ty)
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
        GenericsBuilder::with_callback(self)
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
        self.variant(id).unit()
    }

    pub fn tuple<T>(self, id: T) -> StructFieldBuilder<VariantDataTupleBuilder<VariantBuilder<Self>>>
        where T: ToIdent,
    {
        self.variant(id).tuple()
    }

    pub fn struct_<T>(self, id: T) -> VariantDataStructBuilder<VariantBuilder<Self>>
        where T: ToIdent,
    {
        self.variant(id).struct_()
    }

    pub fn variant<T>(self, id: T) -> VariantBuilder<Self>
        where T: ToIdent,
    {
        VariantBuilder::with_callback(id, self)
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

/// A builder for extern crate items
pub struct ItemExternCrateBuilder<F> {
    builder: ItemBuilder<F>,
    id: ast::Ident,
}

impl<F> ItemExternCrateBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    pub fn with_name(self, name: ast::Name) -> F::Result {
        let extern_ = ast::ItemExternCrate(Some(name));
        self.builder.build_item_(self.id, extern_)
    }

    pub fn build(self) -> F::Result {
        let extern_ = ast::ItemExternCrate(None);
        self.builder.build_item_(self.id, extern_)
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
        PathBuilder::with_callback(self)
    }

    pub fn build(self, mac: ast::Mac) -> F::Result {
        let item_mac = ast::ItemMac(mac);
        self.builder.build_item_(ast::Name(0).to_ident(), item_mac)
    }
}

impl<F> Invoke<ast::Path> for ItemMacBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = MacBuilder<ItemMacBuilder<F>>;

    fn invoke(self, path: ast::Path) -> MacBuilder<Self> {
        MacBuilder::with_callback(self).path(path)
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

//////////////////////////////////////////////////////////////////////////////

pub struct ItemTyBuilder<F> {
    builder: ItemBuilder<F>,
    id: ast::Ident,
    generics: ast::Generics,
}

impl<F> ItemTyBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    pub fn generics(self) -> GenericsBuilder<Self> {
        GenericsBuilder::with_callback(self)
    }

    pub fn ty(self) -> TyBuilder<Self> {
        TyBuilder::with_callback(self)
    }

    pub fn build_ty(self, ty: P<ast::Ty>) -> F::Result {
        let ty_ = ast::ItemTy(ty, self.generics);
        self.builder.build_item_(self.id, ty_)
    }
}

impl<F> Invoke<ast::Generics> for ItemTyBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(mut self, generics: ast::Generics) -> Self {
        self.generics = generics;
        self
    }
}

impl<F> Invoke<P<ast::Ty>> for ItemTyBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = F::Result;

    fn invoke(self, ty: P<ast::Ty>) -> F::Result {
        self.build_ty(ty)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemTraitBuilder<F> {
    builder: ItemBuilder<F>,
    id: ast::Ident,
    unsafety: ast::Unsafety,
    generics: ast::Generics,
    bounds: Vec<ast::TyParamBound>,
    items: Vec<P<ast::TraitItem>>,
}

impl<F> ItemTraitBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    pub fn unsafe_(mut self) -> Self {
        self.unsafety = ast::Unsafety::Unsafe;
        self
    }

    pub fn with_generics(mut self, generics: ast::Generics) -> Self {
        self.generics = generics;
        self
    }

    pub fn generics(self) -> GenericsBuilder<Self> {
        GenericsBuilder::with_callback(self)
    }

    pub fn with_bounds<I>(mut self, iter: I) -> Self
        where I: Iterator<Item=ast::TyParamBound>,
    {
        self.bounds.extend(iter);
        self
    }

    pub fn with_bound(mut self, bound: ast::TyParamBound) -> Self {
        self.bounds.push(bound);
        self
    }

    pub fn bound(self) -> TyParamBoundBuilder<Self> {
        TyParamBoundBuilder::with_callback(self)
    }

    pub fn with_items<I>(mut self, items: I) -> Self
        where I: IntoIterator<Item=P<ast::TraitItem>>,
    {
        self.items.extend(items);
        self
    }

    pub fn with_item(mut self, item: P<ast::TraitItem>) -> Self {
        self.items.push(item);
        self
    }

    pub fn item<T>(self, id: T) -> ItemTraitItemBuilder<Self>
        where T: ToIdent,
    {
        ItemTraitItemBuilder::with_callback(id, self)
    }

    pub fn build(self) -> F::Result {
        self.builder.build_item_(self.id, ast::ItemTrait(
            self.unsafety,
            self.generics,
            P::from_vec(self.bounds),
            self.items,
        ))
    }
}

impl<F> Invoke<ast::Generics> for ItemTraitBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(self, generics: ast::Generics) -> Self {
        self.with_generics(generics)
    }
}

impl<F> Invoke<ast::TyParamBound> for ItemTraitBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(self, bound: ast::TyParamBound) -> Self {
        self.with_bound(bound)
    }
}

impl<F> Invoke<P<ast::TraitItem>> for ItemTraitBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(self, item: P<ast::TraitItem>) -> Self {
        self.with_item(item)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemTraitItemBuilder<F> {
    callback: F,
    id: ast::Ident,
    attrs: Vec<ast::Attribute>,
    span: Span,
}

impl<F> ItemTraitItemBuilder<F>
    where F: Invoke<P<ast::TraitItem>>,
{
    pub fn with_callback<T>(id: T, callback: F) -> Self
        where F: Invoke<P<ast::TraitItem>>,
              T: ToIdent,
    {
        ItemTraitItemBuilder {
            callback: callback,
            id: id.to_ident(),
            attrs: vec![],
            span: DUMMY_SP,
        }
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
        AttrBuilder::with_callback(self)
    }

    pub fn const_(self) -> ConstBuilder<Self> {
        ConstBuilder::with_callback(self)
    }

    pub fn method(self) -> MethodBuilder<Self> {
        MethodBuilder::with_callback(self)
    }

    pub fn type_(self) -> ItemTraitTypeBuilder<F> {
        ItemTraitTypeBuilder {
            builder: self,
            bounds: vec![],
        }
    }

    pub fn build_item(self, node: ast::TraitItem_) -> F::Result {
        let item = ast::TraitItem {
            id: ast::DUMMY_NODE_ID,
            ident: self.id,
            attrs: self.attrs,
            node: node,
            span: self.span,
        };
        self.callback.invoke(P(item))
    }
}

impl<F> Invoke<ast::Attribute> for ItemTraitItemBuilder<F>
    where F: Invoke<P<ast::TraitItem>>,
{
    type Result = Self;

    fn invoke(self, attr: ast::Attribute) -> Self {
        self.with_attr(attr)
    }
}

impl<F> Invoke<Const> for ItemTraitItemBuilder<F>
    where F: Invoke<P<ast::TraitItem>>,
{
    type Result = F::Result;

    fn invoke(self, const_: Const) -> F::Result {
        let node = ast::TraitItem_::ConstTraitItem(
            const_.ty,
            const_.expr);
        self.build_item(node)
    }
}

impl<F> Invoke<Method> for ItemTraitItemBuilder<F>
    where F: Invoke<P<ast::TraitItem>>,
{
    type Result = F::Result;

    fn invoke(self, method: Method) -> F::Result {
        let node = ast::TraitItem_::MethodTraitItem(
            method.sig,
            method.block);
        self.build_item(node)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemTraitTypeBuilder<F> {
    builder: ItemTraitItemBuilder<F>,
    bounds: Vec<ast::TyParamBound>,
}

impl<F> ItemTraitTypeBuilder<F>
    where F: Invoke<P<ast::TraitItem>>,
{
    pub fn with_bounds<I>(mut self, iter: I) -> Self
        where I: Iterator<Item=ast::TyParamBound>,
    {
        self.bounds.extend(iter);
        self
    }

    pub fn with_bound(mut self, bound: ast::TyParamBound) -> Self {
        self.bounds.push(bound);
        self
    }

    pub fn bound(self) -> TyParamBoundBuilder<Self> {
        TyParamBoundBuilder::with_callback(self)
    }

    pub fn build_option_ty(self, ty: Option<P<ast::Ty>>) -> F::Result {
        let bounds = P::from_vec(self.bounds);
        let node = ast::TraitItem_::TypeTraitItem(bounds, ty);
        self.builder.build_item(node)
    }

    pub fn build_ty(self, ty: P<ast::Ty>) -> F::Result {
        self.build_option_ty(Some(ty))
    }

    pub fn ty(self) -> TyBuilder<Self> {
        TyBuilder::with_callback(self)
    }

    pub fn build(self) -> F::Result {
        self.build_option_ty(None)
    }
}

impl<F> Invoke<ast::TyParamBound> for ItemTraitTypeBuilder<F>
    where F: Invoke<P<ast::TraitItem>>,
{
    type Result = ItemTraitTypeBuilder<F>;

    fn invoke(self, bound: ast::TyParamBound) -> Self::Result {
        self.with_bound(bound)
    }
}

impl<F> Invoke<P<ast::Ty>> for ItemTraitTypeBuilder<F>
    where F: Invoke<P<ast::TraitItem>>,
{
    type Result = F::Result;

    fn invoke(self, ty: P<ast::Ty>) -> Self::Result {
        self.build_ty(ty)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemImplBuilder<F> {
    builder: ItemBuilder<F>,
    unsafety: ast::Unsafety,
    polarity: ast::ImplPolarity,
    generics: ast::Generics,
    trait_ref: Option<ast::TraitRef>,
    items: Vec<P<ast::ImplItem>>,
}

impl<F> ItemImplBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    pub fn unsafe_(mut self) -> Self {
        self.unsafety = ast::Unsafety::Unsafe;
        self
    }

    pub fn negative(mut self) -> Self {
        self.polarity = ast::ImplPolarity::Negative;
        self
    }

    pub fn with_generics(mut self, generics: ast::Generics) -> Self {
        self.generics = generics;
        self
    }

    pub fn generics(self) -> GenericsBuilder<Self> {
        GenericsBuilder::with_callback(self)
    }

    pub fn with_trait(mut self, trait_ref: ast::TraitRef) -> Self {
        self.trait_ref = Some(trait_ref);
        self
    }

    pub fn trait_(self) -> PathBuilder<Self> {
        PathBuilder::with_callback(self)
    }

    pub fn ty(self) -> TyBuilder<Self> {
        TyBuilder::with_callback(self)
    }

    pub fn build_ty(self, ty: P<ast::Ty>) -> F::Result {
        let ty_ = ast::ItemImpl(
            self.unsafety,
            self.polarity,
            self.generics,
            self.trait_ref,
            ty,
            self.items);
        self.builder.build_item_(token::special_idents::invalid, ty_)
    }

    pub fn with_items<I>(mut self, items: I) -> Self
        where I: IntoIterator<Item=P<ast::ImplItem>>,
    {
        self.items.extend(items);
        self
    }

    pub fn with_item(mut self, item: P<ast::ImplItem>) -> Self {
        self.items.push(item);
        self
    }

    pub fn item<T>(self, id: T) -> ItemImplItemBuilder<Self>
        where T: ToIdent,
    {
        ItemImplItemBuilder::with_callback(id, self)
    }
}

impl<F> Invoke<ast::Generics> for ItemImplBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(self, generics: ast::Generics) -> Self {
        self.with_generics(generics)
    }
}

impl<F> Invoke<ast::Path> for ItemImplBuilder<F>
    where F: Invoke<P<ast::Item>>
{
    type Result = Self;

    fn invoke(self, path: ast::Path) -> Self {
        self.with_trait(ast::TraitRef {
            path: path,
            ref_id: ast::DUMMY_NODE_ID,
        })
    }
}

impl<F> Invoke<P<ast::ImplItem>> for ItemImplBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(self, item: P<ast::ImplItem>) -> Self {
        self.with_item(item)
    }
}

impl<F> Invoke<P<ast::Ty>> for ItemImplBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = F::Result;

    fn invoke(self, ty: P<ast::Ty>) -> F::Result {
        self.build_ty(ty)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemImplItemBuilder<F> {
    callback: F,
    id: ast::Ident,
    vis: ast::Visibility,
    attrs: Vec<ast::Attribute>,
    span: Span,
}

impl<F> ItemImplItemBuilder<F>
    where F: Invoke<P<ast::ImplItem>>,
{
    pub fn with_callback<T>(id: T, callback: F) -> Self
        where F: Invoke<P<ast::ImplItem>>,
              T: ToIdent,
    {
        ItemImplItemBuilder {
            callback: callback,
            id: id.to_ident(),
            vis: ast::Visibility::Inherited,
            attrs: vec![],
            span: DUMMY_SP,
        }
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
        AttrBuilder::with_callback(self)
    }

    pub fn pub_(mut self) -> Self {
        self.vis = ast::Visibility::Public;
        self
    }

    pub fn const_(self) -> ConstBuilder<Self> {
        ConstBuilder::with_callback(self)
    }

    pub fn method(self) -> MethodBuilder<Self> {
        MethodBuilder::with_callback(self)
    }

    pub fn type_(self) -> TyBuilder<Self> {
        TyBuilder::with_callback(self)
    }

    pub fn mac(self) -> MacBuilder<Self> {
        MacBuilder::with_callback(self)
    }

    pub fn build_item(self, node: ast::ImplItemKind) -> F::Result {
        let item = ast::ImplItem {
            id: ast::DUMMY_NODE_ID,
            ident: self.id,
            vis: self.vis,
            attrs: self.attrs,
            node: node,
            span: self.span,
        };
        self.callback.invoke(P(item))
    }
}

impl<F> Invoke<ast::Attribute> for ItemImplItemBuilder<F>
    where F: Invoke<P<ast::ImplItem>>,
{
    type Result = Self;

    fn invoke(self, attr: ast::Attribute) -> Self {
        self.with_attr(attr)
    }
}

impl<F> Invoke<Const> for ItemImplItemBuilder<F>
    where F: Invoke<P<ast::ImplItem>>,
{
    type Result = F::Result;

    fn invoke(self, const_: Const) -> F::Result {
        let node = ast::ImplItemKind::Const(const_.ty, const_.expr.expect("an expr is required for a const impl item"));
        self.build_item(node)
    }
}

impl<F> Invoke<Method> for ItemImplItemBuilder<F>
    where F: Invoke<P<ast::ImplItem>>,
{
    type Result = F::Result;

    fn invoke(self, method: Method) -> F::Result {
        let node = ast::ImplItemKind::Method(method.sig, method.block.expect("a block is required for a method impl item"));
        self.build_item(node)
    }
}

impl<F> Invoke<P<ast::Ty>> for ItemImplItemBuilder<F>
    where F: Invoke<P<ast::ImplItem>>,
{
    type Result = F::Result;

    fn invoke(self, ty: P<ast::Ty>) -> F::Result {
        let node = ast::ImplItemKind::Type(ty);
        self.build_item(node)
    }
}

impl<F> Invoke<ast::Mac> for ItemImplItemBuilder<F>
    where F: Invoke<P<ast::ImplItem>>,
{
    type Result = F::Result;

    fn invoke(self, mac: ast::Mac) -> F::Result {
        let node = ast::ImplItemKind::Macro(mac);
        self.build_item(node)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemConstBuilder<F> {
    builder: ItemBuilder<F>,
    id: ast::Ident,
}

impl<F> Invoke<Const> for ItemConstBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = F::Result;

    fn invoke(self, const_: Const) -> F::Result {
        let ty = ast::ItemConst(const_.ty, const_.expr.expect("an expr is required for a const item"));
        self.builder.build_item_(self.id, ty)
    }
}
