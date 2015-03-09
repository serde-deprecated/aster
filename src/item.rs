use std::iter::IntoIterator;

use syntax::abi::Abi;
use syntax::ast;
use syntax::codemap::{DUMMY_SP, Span, respan};
use syntax::parse::token;
use syntax::ptr::P;

use attr::AttrBuilder;
use block::BlockBuilder;
use generics::GenericsBuilder;
use ident::ToIdent;
use invoke::{Invoke, Identity};
use path::PathBuilder;
use ty::TyBuilder;
use fn_decl::FnDeclBuilder;

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

    pub fn use_glob(self) -> PathBuilder<ItemUseGlobBuilder<F>> {
        PathBuilder::new_with_callback(ItemUseGlobBuilder(self))
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
            fields: vec![],
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
        self.builder.build_item_(self.id, ast::Item_::ItemFn(
            self.fn_decl,
            self.unsafety,
            self.abi,
            self.generics,
            block,
        ))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemUseGlobBuilder<F>(ItemBuilder<F>);

impl<F> Invoke<ast::Path> for ItemUseGlobBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = F::Result;

    fn invoke(self, path: ast::Path) -> F::Result {
        self.0.build_use(ast::ViewPathGlob(path))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemStructBuilder<F> {
    builder: ItemBuilder<F>,
    id: ast::Ident,
    generics: ast::Generics,
    fields: Vec<ast::StructField>,
}

impl<F> ItemStructBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    pub fn generics(self) -> GenericsBuilder<Self> {
        GenericsBuilder::new_with_callback(self)
    }

    pub fn with_field(mut self, field: ast::StructField) -> Self {
        self.fields.push(field);
        self
    }

    pub fn field<T>(self, id: T) -> TyBuilder<ItemStructFieldBuilder<Self>>
        where T: ToIdent,
    {
        let span = self.builder.span;
        let builder = ItemStructFieldBuilder::named_with_callback(id, self).span(span);
        TyBuilder::new_with_callback(builder)
    }

    pub fn build(self) -> F::Result {
        let struct_def = ast::StructDef {
            fields: self.fields,
            ctor_id: None,
        };
        let struct_ = ast::ItemStruct(P(struct_def), self.generics);
        self.builder.build_item_(self.id, struct_)
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

impl<F> Invoke<ast::StructField> for ItemStructBuilder<F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(self, field: ast::StructField) -> Self {
        self.with_field(field)
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

    pub fn field(self) -> TyBuilder<ItemStructFieldBuilder<Self>> {
        let span = self.builder.span;
        let builder = ItemStructFieldBuilder::unnamed_with_callback(self).span(span);
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

pub struct ItemStructFieldBuilder<F=Identity> {
    callback: F,
    span: Span,
    kind: ast::StructFieldKind,
    attrs: Vec<ast::Attribute>,
}

impl ItemStructFieldBuilder {
    pub fn named<T>(name: T) -> Self
        where T: ToIdent,
    {
        ItemStructFieldBuilder::named_with_callback(name, Identity)
    }

    pub fn unnamed() -> Self {
        ItemStructFieldBuilder::unnamed_with_callback(Identity)
    }
}

impl<F> ItemStructFieldBuilder<F>
    where F: Invoke<ast::StructField>,
{
    pub fn named_with_callback<T>(id: T, callback: F) -> Self
        where T: ToIdent,
    {
        let id = id.to_ident();
        ItemStructFieldBuilder {
            callback: callback,
            span: DUMMY_SP,
            kind: ast::StructFieldKind::NamedField(id, ast::Inherited),
            attrs: vec![],
        }
    }

    pub fn unnamed_with_callback(callback: F) -> Self {
        ItemStructFieldBuilder {
            callback: callback,
            span: DUMMY_SP,
            kind: ast::StructFieldKind::UnnamedField(ast::Inherited),
            attrs: vec![],
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn pub_(mut self) -> Self {
        match self.kind {
            ast::StructFieldKind::NamedField(_, ref mut vis) => { *vis = ast::Public; }
            ast::StructFieldKind::UnnamedField(ref mut vis) => { *vis = ast::Public; }
        }
        self
    }

    pub fn attr(self) -> AttrBuilder<Self> {
        let span = self.span;
        AttrBuilder::new_with_callback(self).span(span)
    }

    pub fn build_ty(self, ty: P<ast::Ty>) -> F::Result {
        let field = ast::StructField_ {
            kind: self.kind,
            id: ast::DUMMY_NODE_ID,
            ty: ty,
            attrs: self.attrs,
        };
        self.callback.invoke(respan(self.span, field))
    }

    pub fn ty(self) -> TyBuilder<Self> {
        let span = self.span;
        TyBuilder::new_with_callback(self).span(span)
    }
}

impl<F> Invoke<ast::Attribute> for ItemStructFieldBuilder<F> {
    type Result = Self;

    fn invoke(mut self, attr: ast::Attribute) -> Self {
        self.attrs.push(attr);
        self
    }
}

impl<F> Invoke<P<ast::Ty>> for ItemStructFieldBuilder<F>
    where F: Invoke<ast::StructField>,
{
    type Result = F::Result;

    fn invoke(self, ty: P<ast::Ty>) -> F::Result {
        self.build_ty(ty)
    }
}

