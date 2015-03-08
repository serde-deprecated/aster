use std::iter::IntoIterator;

use syntax::ast;
use syntax::codemap::{DUMMY_SP, Span};
use syntax::owned_slice::OwnedSlice;
use syntax::ptr::P;

use invoke::{Invoke, Identity};

use ctx::Ctx;
use ident::ToIdent;
use name::ToName;
use ty::TyBuilder;

use lifetime::IntoLifetime;

//////////////////////////////////////////////////////////////////////////////

pub trait IntoPath {
    fn into_path(self, ctx: &Ctx) -> ast::Path;
}

impl IntoPath for ast::Path {
    fn into_path(self, _ctx: &Ctx) -> ast::Path {
        self
    }
}

impl<'a> IntoPath for ast::Ident {
    fn into_path(self, ctx: &Ctx) -> ast::Path {
        PathBuilder::new(ctx).id(self).build()
    }
}

impl<'a> IntoPath for &'a str {
    fn into_path(self, ctx: &Ctx) -> ast::Path {
        PathBuilder::new(ctx).id(self).build()
    }
}

impl IntoPath for String {
    fn into_path(self, ctx: &Ctx) -> ast::Path {
        (&*self).into_path(ctx)
    }
}

impl<'a, I, T> IntoPath for I where I: IntoIterator<Item=T>, T: ToIdent {
    fn into_path(self, ctx: &Ctx) -> ast::Path {
        PathBuilder::new(ctx).ids(self).build()
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct PathBuilder<'a, F=Identity> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
    global: bool,
}

impl<'a> PathBuilder<'a> {
    pub fn new(ctx: &'a Ctx) -> Self {
        PathBuilder::new_with_callback(ctx, Identity)
    }
}

impl<'a, F> PathBuilder<'a, F>
    where F: Invoke<ast::Path>,
{
    pub fn new_with_callback(ctx: &'a Ctx, callback: F) -> Self {
        PathBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
            global: false,
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn global(mut self) -> Self {
        self.global = true;
        self
    }

    pub fn ids<I, T>(self, ids: I) -> PathSegmentsBuilder<'a, F>
        where I: IntoIterator<Item=T>,
              T: ToIdent,
    {
        let mut ids = ids.into_iter();
        let id = ids.next().expect("passed path with no id");

        self.id(id).ids(ids)
    }

    pub fn id<I>(self, id: I) -> PathSegmentsBuilder<'a, F>
        where I: ToIdent,
    {
        self.segment(id).build()
    }

    pub fn segment<I>(self, id: I)
        -> PathSegmentBuilder<'a, PathSegmentsBuilder<'a, F>>
        where I: ToIdent,
    {
        PathSegmentBuilder::new_with_callback(self.ctx, id, PathSegmentsBuilder {
            ctx: self.ctx,
            callback: self.callback,
            span: self.span,
            global: self.global,
            segments: Vec::new(),
        })
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct PathSegmentsBuilder<'a, F=Identity> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
    global: bool,
    segments: Vec<ast::PathSegment>,
}

impl<'a, F> PathSegmentsBuilder<'a, F>
    where F: Invoke<ast::Path>,
{
    pub fn ids<I, T>(mut self, ids: I) -> PathSegmentsBuilder<'a, F>
        where I: IntoIterator<Item=T>,
              T: ToIdent,
    {
        for id in ids {
            self = self.id(id);
        }

        self 
    }

    pub fn id<T>(self, id: T) -> PathSegmentsBuilder<'a, F>
        where T: ToIdent,
    {
        self.segment(id).build()
    }

    pub fn segment<T>(self, id: T) -> PathSegmentBuilder<'a, Self>
        where T: ToIdent,
    {
        PathSegmentBuilder::new_with_callback(self.ctx, id, self)
    }

    pub fn build(self) -> F::Result {
        self.callback.invoke(ast::Path {
            span: self.span,
            global: self.global,
            segments: self.segments,
        })
    }
}

impl<'a, F> Invoke<ast::PathSegment> for PathSegmentsBuilder<'a, F> {
    type Result = Self;

    fn invoke(mut self, segment: ast::PathSegment) -> Self {
        self.segments.push(segment);
        self
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct PathSegmentBuilder<'a, F=Identity> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
    id: ast::Ident,
    lifetimes: Vec<ast::Lifetime>,
    tys: Vec<P<ast::Ty>>,
    bindings: Vec<P<ast::TypeBinding>>,
}

impl<'a, F> PathSegmentBuilder<'a, F>
    where F: Invoke<ast::PathSegment>,
{
    pub fn new_with_callback<I>(ctx: &'a Ctx, id: I, callback: F) -> Self
        where I: ToIdent,
    {
        PathSegmentBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
            id: id.to_ident(ctx),
            lifetimes: Vec::new(),
            tys: Vec::new(),
            bindings: Vec::new(),
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn with_generics(self, generics: ast::Generics) -> Self {
        let ctx = self.ctx;

        // Strip off the bounds.
        let lifetimes = generics.lifetimes.iter()
            .map(|lifetime_def| lifetime_def.lifetime);

        let tys = generics.ty_params.iter()
            .map(|ty_param| TyBuilder::new(ctx).id(ty_param.ident));

        self.with_lifetimes(lifetimes)
            .with_tys(tys)
    }


    pub fn with_lifetimes<I, L>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=L>,
              L: IntoLifetime,
    {
        let ctx = self.ctx;
        let iter = iter.into_iter().map(|lifetime| lifetime.into_lifetime(ctx));
        self.lifetimes.extend(iter);
        self
    }

    pub fn with_lifetime<L>(mut self, lifetime: L) -> Self
        where L: IntoLifetime,
    {
        self.lifetimes.push(lifetime.into_lifetime(self.ctx));
        self
    }

    pub fn lifetime<N>(self, name: N) -> Self
        where N: ToName,
    {
        let lifetime = ast::Lifetime {
            id: ast::DUMMY_NODE_ID,
            span: self.span,
            name: name.to_name(self.ctx.clone()),
        };
        self.with_lifetime(lifetime)
    }

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
        TyBuilder::new_with_callback(self.ctx, self)
    }

    pub fn build(self) -> F::Result {
        let data = ast::AngleBracketedParameterData {
            lifetimes: self.lifetimes,
            types: OwnedSlice::from_vec(self.tys),
            bindings: OwnedSlice::from_vec(self.bindings),
        };

        let parameters = ast::PathParameters::AngleBracketedParameters(data);

        self.callback.invoke(ast::PathSegment {
            identifier: self.id,
            parameters: parameters,
        })
    }
}

impl<'a, F> Invoke<P<ast::Ty>> for PathSegmentBuilder<'a, F>
    where F: Invoke<ast::PathSegment>
{
    type Result = Self;

    fn invoke(self, ty: P<ast::Ty>) -> Self {
        self.with_ty(ty)
    }
}
