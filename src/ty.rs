use std::iter::IntoIterator;

use syntax::ast;
use syntax::codemap::{DUMMY_SP, Span};
use syntax::ptr::P;

use invoke::{Invoke, Identity};

use ctx::Ctx;
use ident::ToIdent;
use name::ToName;
use path::PathBuilder;

//////////////////////////////////////////////////////////////////////////////

pub struct TyBuilder<'a, F=Identity> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
}

impl<'a> TyBuilder<'a> {
    pub fn new(ctx: &'a Ctx) -> Self {
        TyBuilder::new_with_callback(ctx, Identity)
    }
}

impl<'a, F> TyBuilder<'a, F>
    where F: Invoke<P<ast::Ty>>,
{
    pub fn new_with_callback(ctx: &'a Ctx, callback: F) -> Self {
        TyBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn build_ty(self, ty: P<ast::Ty>) -> F::Result {
        self.callback.invoke(ty)
    }


    pub fn build_ty_(self, ty_: ast::Ty_) -> F::Result {
        let span = self.span;
        self.build_ty(P(ast::Ty {
            id: ast::DUMMY_NODE_ID,
            node: ty_,
            span: span,
        }))
    }

    pub fn id<I>(self, id: I) -> F::Result
        where I: ToIdent,
    {
        self.path().id(id).build()
    }

    pub fn build_path(self, path: ast::Path) -> F::Result {
        self.build_ty_(ast::Ty_::TyPath(None, path))
    }

    pub fn build_qpath(self, qself: ast::QSelf, path: ast::Path) -> F::Result {
        self.build_ty_(ast::Ty_::TyPath(Some(qself), path))
    }

    pub fn path(self) -> PathBuilder<'a, TyPathBuilder<'a, F>> {
        PathBuilder::new_with_callback(self.ctx, TyPathBuilder(self))
    }

    pub fn isize(self) -> F::Result {
        self.id("isize")
    }

    pub fn i8(self) -> F::Result {
        self.id("i8")
    }

    pub fn i16(self) -> F::Result {
        self.id("i16")
    }

    pub fn i32(self) -> F::Result {
        self.id("i32")
    }

    pub fn i64(self) -> F::Result {
        self.id("i64")
    }

    pub fn usize(self) -> F::Result {
        self.id("usize")
    }

    pub fn u8(self) -> F::Result {
        self.id("u8")
    }

    pub fn u16(self) -> F::Result {
        self.id("u16")
    }

    pub fn u32(self) -> F::Result {
        self.id("u32")
    }

    pub fn u64(self) -> F::Result {
        self.id("u64")
    }

    pub fn option(self) -> TyBuilder<'a, TyOptionBuilder<'a, F>> {
        TyBuilder::new_with_callback(self.ctx, TyOptionBuilder(self))
    }

    pub fn result(self) -> TyBuilder<'a, TyResultOkBuilder<'a, F>> {
        TyBuilder::new_with_callback(self.ctx, TyResultOkBuilder(self))
    }

    pub fn phantom_data(self) -> TyBuilder<'a, TyPhantomDataBuilder<'a, F>> {
        TyBuilder::new_with_callback(self.ctx, TyPhantomDataBuilder(self))
    }

    pub fn unit(self) -> F::Result {
        self.tuple().build()
    }

    pub fn tuple(self) -> TyTupleBuilder<'a, F> {
        TyTupleBuilder {
            builder: self,
            tys: vec![],
        }
    }

    pub fn ref_(self) -> TyRefBuilder<'a, F> {
        TyRefBuilder {
            builder: self,
            lifetime: None,
            mutability: ast::MutImmutable,
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct TyPathBuilder<'a, F>(TyBuilder<'a, F>);

impl<'a, F> Invoke<ast::Path> for TyPathBuilder<'a, F>
    where F: Invoke<P<ast::Ty>>,
{
    type Result = F::Result;

    fn invoke(self, path: ast::Path) -> F::Result {
        self.0.build_path(path)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct TyRefBuilder<'a, F> {
    builder: TyBuilder<'a, F>,
    lifetime: Option<ast::Lifetime>,
    mutability: ast::Mutability,
}

impl<'a, F> TyRefBuilder<'a, F>
    where F: Invoke<P<ast::Ty>>,
{
    pub fn mut_(mut self) -> Self {
        self.mutability = ast::MutMutable;
        self
    }

    pub fn lifetime<N>(mut self, name: N) -> Self
        where N: ToName,
    {
        self.lifetime = Some(ast::Lifetime {
            id: ast::DUMMY_NODE_ID,
            span: self.builder.span,
            name: name.to_name(self.builder.ctx),
        });
        self
    }

    pub fn build_ty(self, ty: P<ast::Ty>) -> F::Result {
        let ty = ast::MutTy {
            ty: ty,
            mutbl: self.mutability,
        };
        self.builder.build_ty_(ast::TyRptr(self.lifetime, ty))
    }

    pub fn ty(self) -> TyBuilder<'a, Self> {
        TyBuilder::new_with_callback(self.builder.ctx, self)
    }
}

impl<'a, F> Invoke<P<ast::Ty>> for TyRefBuilder<'a, F>
    where F: Invoke<P<ast::Ty>>,
{
    type Result = F::Result;

    fn invoke(self, ty: P<ast::Ty>) -> F::Result {
        self.build_ty(ty)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct TyOptionBuilder<'a, F>(TyBuilder<'a, F>);

impl<'a, F> Invoke<P<ast::Ty>> for TyOptionBuilder<'a, F>
    where F: Invoke<P<ast::Ty>>,
{
    type Result = F::Result;

    fn invoke(self, ty: P<ast::Ty>) -> F::Result {
        let path = PathBuilder::new(self.0.ctx)
            .global()
            .id("std")
            .id("option")
            .segment("Option")
                .with_ty(ty)
                .build()
            .build();

        self.0.build_path(path)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct TyResultOkBuilder<'a, F>(TyBuilder<'a, F>);

impl<'a, F> Invoke<P<ast::Ty>> for TyResultOkBuilder<'a, F>
    where F: Invoke<P<ast::Ty>>,
{
    type Result = TyBuilder<'a, TyResultErrBuilder<'a, F>>;

    fn invoke(self, ty: P<ast::Ty>) -> TyBuilder<'a, TyResultErrBuilder<'a, F>> {
        TyBuilder::new_with_callback(self.0.ctx, TyResultErrBuilder(self.0, ty))
    }
}

pub struct TyResultErrBuilder<'a, F>(TyBuilder<'a, F>, P<ast::Ty>);

impl<'a, F> Invoke<P<ast::Ty>> for TyResultErrBuilder<'a, F>
    where F: Invoke<P<ast::Ty>>,
{
    type Result = F::Result;

    fn invoke(self, ty: P<ast::Ty>) -> F::Result {
        let path = PathBuilder::new(self.0.ctx)
            .global()
            .id("std")
            .id("result")
            .segment("Result")
                .with_ty(self.1)
                .with_ty(ty)
                .build()
            .build();

        self.0.build_path(path)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct TyPhantomDataBuilder<'a, F>(TyBuilder<'a, F>);

impl<'a, F> Invoke<P<ast::Ty>> for TyPhantomDataBuilder<'a, F>
    where F: Invoke<P<ast::Ty>>,
{
    type Result = F::Result;

    fn invoke(self, ty: P<ast::Ty>) -> F::Result {
        let path = PathBuilder::new(self.0.ctx)
            .global()
            .id("std")
            .id("marker")
            .segment("PhantomData")
                .with_ty(ty)
                .build()
            .build();

        self.0.build_path(path)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct TyTupleBuilder<'a, F> {
    builder: TyBuilder<'a, F>,
    tys: Vec<P<ast::Ty>>,
}

impl<'a, F> TyTupleBuilder<'a, F>
    where F: Invoke<P<ast::Ty>>,
{
    pub fn with_tys<I>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=P<ast::Ty>>,
    {
        self.tys.extend(iter);
        self
    }

    pub fn with_ty(mut self, ty: P<ast::Ty>) -> Self {
        self.tys.push(ty);
        self
    }

    pub fn ty(self) -> TyBuilder<'a, Self> {
        TyBuilder::new_with_callback(self.builder.ctx, self)
    }

    pub fn build(self) -> F::Result {
        self.builder.build_ty_(ast::TyTup(self.tys))
    }
}

impl<'a, F> Invoke<P<ast::Ty>> for TyTupleBuilder<'a, F>
    where F: Invoke<P<ast::Ty>>,
{
    type Result = Self;

    fn invoke(self, ty: P<ast::Ty>) -> Self {
        self.with_ty(ty)
    }
}

