use std::iter::IntoIterator;

use syntax::abi::Abi;
use syntax::ast;
use syntax::codemap::{DUMMY_SP, Span, respan};
use syntax::parse::token;
use syntax::ptr::P;

use attr::AttrBuilder;
use block::BlockBuilder;
use ctx::Ctx;
use generics::GenericsBuilder;
use ident::ToIdent;
use invoke::{Invoke, Identity};
use path::PathBuilder;
use ty::TyBuilder;
use fn_decl::FnDeclBuilder;

//////////////////////////////////////////////////////////////////////////////

pub struct ItemBuilder<'a, F=Identity> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
    attrs: Vec<ast::Attribute>,
    vis: ast::Visibility,
}

impl<'a> ItemBuilder<'a> {
    pub fn new(ctx: &'a Ctx) -> Self {
        ItemBuilder::new_with_callback(ctx, Identity)
    }
}

impl<'a, F> ItemBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    pub fn new_with_callback(ctx: &'a Ctx, callback: F) -> Self {
        ItemBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
            attrs: vec![],
            vis: ast::Visibility::Inherited,
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

    pub fn attr(self) -> AttrBuilder<'a, Self> {
        AttrBuilder::new_with_callback(self.ctx, self)
    }

    pub fn pub_(mut self) -> Self {
        self.vis = ast::Visibility::Public;
        self
    }

    pub fn build_item(self, item: P<ast::Item>) -> F::Result {
        self.callback.invoke(item)
    }

    pub fn build_item_<T>(self, id: T, item_: ast::Item_) -> F::Result
        where T: ToIdent,
    {
        let item = ast::Item {
            ident: id.to_ident(self.ctx),
            attrs: self.attrs,
            id: ast::DUMMY_NODE_ID,
            node: item_,
            vis: self.vis,
            span: self.span,
        };
        self.callback.invoke(P(item))
    }

    pub fn fn_<T>(self, id: T) -> FnDeclBuilder<'a, ItemFnDeclBuilder<'a, F>>
        where T: ToIdent,
    {
        let id = id.to_ident(self.ctx);
        FnDeclBuilder::new_with_callback(self.ctx, ItemFnDeclBuilder {
            builder: self,
            id: id,
        })
    }

    pub fn build_use(self, view_path: ast::ViewPath_) -> F::Result {
        let item = ast::ItemUse(P(respan(self.span, view_path)));
        self.build_item_(token::special_idents::invalid, item)
    }

    pub fn use_glob(self) -> PathBuilder<'a, ItemUseGlobBuilder<'a, F>> {
        PathBuilder::new_with_callback(self.ctx, ItemUseGlobBuilder(self))
    }

    pub fn struct_<T>(self, id: T) -> ItemStructBuilder<'a, F>
        where T: ToIdent,
    {
        let id = id.to_ident(self.ctx);
        let generics = GenericsBuilder::new(self.ctx).build();

        ItemStructBuilder {
            builder: self,
            id: id,
            generics: generics,
            fields: vec![],
        }
    }

    pub fn tuple_struct<T>(self, id: T) -> ItemTupleStructBuilder<'a, F>
        where T: ToIdent,
    {
        let id = id.to_ident(self.ctx);
        let generics = GenericsBuilder::new(self.ctx).build();

        ItemTupleStructBuilder {
            builder: self,
            id: id,
            generics: generics,
            fields: vec![],
        }
    }
}

impl<'a, F> Invoke<ast::Attribute> for ItemBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(self, attr: ast::Attribute) -> Self {
        self.with_attr(attr)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemFnDeclBuilder<'a, F> {
    builder: ItemBuilder<'a, F>,
    id: ast::Ident,
}

impl<'a, F> Invoke<P<ast::FnDecl>> for ItemFnDeclBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = ItemFnBuilder<'a, F>;

    fn invoke(self, fn_decl: P<ast::FnDecl>) -> ItemFnBuilder<'a, F> {
        let generics = GenericsBuilder::new(self.builder.ctx)
            .build();

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

pub struct ItemFnBuilder<'a, F> {
    builder: ItemBuilder<'a, F>,
    id: ast::Ident,
    fn_decl: P<ast::FnDecl>,
    unsafety: ast::Unsafety,
    abi: Abi,
    generics: ast::Generics,
}

impl<'a, F> ItemFnBuilder<'a, F>
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

    pub fn with_generics(mut self, generics: ast::Generics) -> Self {
        self.generics = generics;
        self
    }

    pub fn generics(self) -> GenericsBuilder<'a, Self> {
        GenericsBuilder::new_with_callback(self.builder.ctx, self)
    }

    pub fn block(self) -> BlockBuilder<'a, Self> {
        BlockBuilder::new_with_callback(self.builder.ctx, self)
    }
}

impl<'a, F> Invoke<ast::Generics> for ItemFnBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(self, generics: ast::Generics) -> Self {
        self.with_generics(generics)
    }
}

impl<'a, F> Invoke<P<ast::Block>> for ItemFnBuilder<'a, F>
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

pub struct ItemUseGlobBuilder<'a, F>(ItemBuilder<'a, F>);

impl<'a, F> Invoke<ast::Path> for ItemUseGlobBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = F::Result;

    fn invoke(self, path: ast::Path) -> F::Result {
        self.0.build_use(ast::ViewPathGlob(path))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemStructBuilder<'a, F> {
    builder: ItemBuilder<'a, F>,
    id: ast::Ident,
    generics: ast::Generics,
    fields: Vec<ast::StructField>,
}

impl<'a, F> ItemStructBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    pub fn with_generics(mut self, generics: ast::Generics) -> Self {
        self.generics = generics;
        self
    }

    pub fn generics(self) -> GenericsBuilder<'a, Self> {
        GenericsBuilder::new_with_callback(self.builder.ctx, self)
    }

    pub fn with_field(mut self, field: ast::StructField) -> Self {
        self.fields.push(field);
        self
    }

    pub fn field<T>(self, id: T) -> TyBuilder<'a, ItemStructFieldBuilder<'a, Self>>
        where T: ToIdent,
    {
        let id = id.to_ident(self.builder.ctx);
        let span = self.builder.span;

        TyBuilder::new_with_callback(self.builder.ctx, ItemStructFieldBuilder {
            ctx: self.builder.ctx,
            callback: self,
            span: span,
            kind: ast::StructFieldKind::NamedField(id, ast::Inherited),
            attrs: vec![],
        })
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

impl<'a, F> Invoke<ast::Generics> for ItemStructBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(self, generics: ast::Generics) -> Self {
        self.with_generics(generics)
    }
}

impl<'a, F> Invoke<ast::StructField> for ItemStructBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(self, field: ast::StructField) -> Self {
        self.with_field(field)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemTupleStructBuilder<'a, F> {
    builder: ItemBuilder<'a, F>,
    id: ast::Ident,
    generics: ast::Generics,
    fields: Vec<ast::StructField>,
}

impl<'a, F> ItemTupleStructBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    pub fn with_generics(mut self, generics: ast::Generics) -> Self {
        self.generics = generics;
        self
    }

    pub fn generics(self) -> GenericsBuilder<'a, Self> {
        GenericsBuilder::new_with_callback(self.builder.ctx, self)
    }

    pub fn with_field(mut self, field: ast::StructField) -> Self {
        self.fields.push(field);
        self
    }

    pub fn with_tys<I>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=P<ast::Ty>>,
    {
        for ty in iter {
            self = self.with_ty(ty);
        }
        self
    }

    pub fn with_ty(self, ty: P<ast::Ty>) -> Self {
        self.field().build_ty(ty)
    }

    pub fn ty(self) -> TyBuilder<'a, Self> {
        TyBuilder::new_with_callback(self.builder.ctx, self)
    }

    pub fn field(self) -> TyBuilder<'a, ItemStructFieldBuilder<'a, Self>> {
        let span = self.builder.span;

        TyBuilder::new_with_callback(self.builder.ctx, ItemStructFieldBuilder {
            ctx: self.builder.ctx,
            callback: self,
            span: span,
            kind: ast::StructFieldKind::UnnamedField(ast::Inherited),
            attrs: vec![],
        })
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

impl<'a, F> Invoke<ast::Generics> for ItemTupleStructBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(self, generics: ast::Generics) -> Self {
        self.with_generics(generics)
    }
}

impl<'a, F> Invoke<P<ast::Ty>> for ItemTupleStructBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(self, ty: P<ast::Ty>) -> Self {
        self.with_ty(ty)
    }
}

impl<'a, F> Invoke<ast::StructField> for ItemTupleStructBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(self, field: ast::StructField) -> Self {
        self.with_field(field)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemStructFieldBuilder<'a, F> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
    kind: ast::StructFieldKind,
    attrs: Vec<ast::Attribute>,
}

impl<'a, F> ItemStructFieldBuilder<'a, F>
    where F: Invoke<ast::StructField>,
{
    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn attr(self) -> AttrBuilder<'a, Self> {
        let span = self.span;
        AttrBuilder::new_with_callback(self.ctx, self).span(span)
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

    pub fn ty(self) -> TyBuilder<'a, Self> {
        let span = self.span;
        TyBuilder::new_with_callback(self.ctx, self).span(span)
    }
}

impl<'a, F> Invoke<ast::Attribute> for ItemStructFieldBuilder<'a, F> {
    type Result = Self;

    fn invoke(mut self, attr: ast::Attribute) -> Self {
        self.attrs.push(attr);
        self
    }
}

impl<'a, F> Invoke<P<ast::Ty>> for ItemStructFieldBuilder<'a, F>
    where F: Invoke<ast::StructField>,
{
    type Result = F::Result;

    fn invoke(self, ty: P<ast::Ty>) -> F::Result {
        self.build_ty(ty)
    }
}

