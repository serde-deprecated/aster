use syntax::ast;
use syntax::codemap::{DUMMY_SP, Span};
use syntax::owned_slice::OwnedSlice;
use syntax::ptr::P;

use ctx::Ctx;
use ident::ToIdent;
use invoke::{Invoke, Identity};
use lifetime::{IntoLifetime, IntoLifetimeDef, LifetimeDefBuilder};
use name::ToName;
use path::IntoPath;

//////////////////////////////////////////////////////////////////////////////

pub struct TyParamBuilder<'a, F=Identity> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
    id: ast::Ident,
    bounds: Vec<ast::TyParamBound>,
    default: Option<P<ast::Ty>>,
}

impl<'a> TyParamBuilder<'a> {
    pub fn new<I>(ctx: &'a Ctx, id: I) -> Self
        where I: ToIdent,
    {
        TyParamBuilder::new_with_callback(ctx, id, Identity)
    }

    pub fn from_ty_param(ctx: &'a Ctx, ty_param: ast::TyParam) -> Self {
        TyParamBuilder::from_ty_param_with_callback(ctx, Identity, ty_param)
    }
}

impl<'a, F> TyParamBuilder<'a, F>
    where F: Invoke<ast::TyParam>,
{
    pub fn new_with_callback<I>(ctx: &'a Ctx, id: I, callback: F) -> Self
        where I: ToIdent
    {
        TyParamBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
            id: id.to_ident(ctx),
            bounds: Vec::new(),
            default: None,
        }
    }

    pub fn from_ty_param_with_callback(ctx: &'a Ctx, callback: F, ty_param: ast::TyParam) -> Self {
        TyParamBuilder {
            ctx: ctx,
            callback: callback,
            span: ty_param.span,
            id: ty_param.ident,
            bounds: ty_param.bounds.into_vec(),
            default: ty_param.default,
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn with_default(mut self, ty: P<ast::Ty>) -> Self {
        self.default = Some(ty);
        self
    }

    pub fn with_trait_bound(mut self, trait_ref: ast::PolyTraitRef) -> Self {
        self.bounds.push(ast::TyParamBound::TraitTyParamBound(
            trait_ref,
            ast::TraitBoundModifier::None,
        ));
        self
    }

    pub fn trait_bound<P>(self, path: P) -> PolyTraitRefBuilder<'a, Self>
        where P: IntoPath,
    {
        PolyTraitRefBuilder::new_with_callback(self.ctx, path, self)
    }

    pub fn lifetime_bound<L>(mut self, lifetime: L) -> Self
        where L: IntoLifetime,
    {
        let lifetime = lifetime.into_lifetime(self.ctx);

        self.bounds.push(ast::TyParamBound::RegionTyParamBound(lifetime));
        self
    }

    pub fn build(self) -> F::Result {
        self.callback.invoke(ast::TyParam {
            ident: self.id,
            id: ast::DUMMY_NODE_ID,
            bounds: OwnedSlice::from_vec(self.bounds),
            default: self.default,
            span: self.span,
        })
    }
}

impl<'a, F> Invoke<ast::PolyTraitRef> for TyParamBuilder<'a, F>
    where F: Invoke<ast::TyParam>,
{
    type Result = Self;

    fn invoke(self, trait_ref: ast::PolyTraitRef) -> Self {
        self.with_trait_bound(trait_ref)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct PolyTraitRefBuilder<'a, F> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
    trait_ref: ast::TraitRef,
    lifetimes: Vec<ast::LifetimeDef>,
}

impl<'a, F> PolyTraitRefBuilder<'a, F>
    where F: Invoke<ast::PolyTraitRef>,
{
    pub fn new_with_callback<P>(ctx: &'a Ctx, path: P, callback: F) -> Self
        where P: IntoPath,
    {
        let trait_ref = ast::TraitRef {
            path: path.into_path(ctx),
            ref_id: ast::DUMMY_NODE_ID,
        };

        PolyTraitRefBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
            trait_ref: trait_ref,
            lifetimes: Vec::new(),
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn with_lifetime<L>(mut self, lifetime: L) -> Self
        where L: IntoLifetimeDef,
    {
        self.lifetimes.push(lifetime.into_lifetime_def(self.ctx));
        self
    }

    pub fn lifetime<N>(self, name: N) -> LifetimeDefBuilder<'a, Self>
        where N: ToName,
    {
        LifetimeDefBuilder::new_with_callback(self.ctx, name, self)
    }

    pub fn build(self) -> F::Result {
        self.callback.invoke(ast::PolyTraitRef {
            bound_lifetimes: self.lifetimes,
            trait_ref: self.trait_ref,
            span: self.span,
        })
    }
}

impl<'a, F> Invoke<ast::LifetimeDef> for PolyTraitRefBuilder<'a, F>
    where F: Invoke<ast::PolyTraitRef>,
{
    type Result = Self;

    fn invoke(self, lifetime: ast::LifetimeDef) -> Self {
        self.with_lifetime(lifetime)
    }
}
