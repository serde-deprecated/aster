use std::iter::IntoIterator;

use syntax::ast;
use syntax::codemap::{DUMMY_SP, Span};
use syntax::owned_slice::OwnedSlice;

use ctx::Ctx;
use ident::ToIdent;
use invoke::{Invoke, Identity};
use lifetime::{IntoLifetime, IntoLifetimeDef, LifetimeDefBuilder};
use name::ToName;
use path::IntoPath;
use ty_param::TyParamBuilder;

//////////////////////////////////////////////////////////////////////////////

pub struct GenericsBuilder<'a, F=Identity> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
    lifetimes: Vec<ast::LifetimeDef>,
    ty_params: Vec<ast::TyParam>,
    predicates: Vec<ast::WherePredicate>,
}

impl<'a> GenericsBuilder<'a> {
    pub fn new(ctx: &'a Ctx) -> Self {
        GenericsBuilder::new_with_callback(ctx, Identity)
    }

    pub fn from_generics(ctx: &'a Ctx, generics: ast::Generics) -> Self {
        GenericsBuilder::from_generics_with_callback(ctx, Identity, generics)
    }
}

impl<'a, F> GenericsBuilder<'a, F>
    where F: Invoke<ast::Generics>,
{
    pub fn new_with_callback(ctx: &'a Ctx, callback: F) -> Self {
        GenericsBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
            lifetimes: Vec::new(),
            ty_params: Vec::new(),
            predicates: Vec::new(),
        }
    }

    pub fn from_generics_with_callback(ctx: &'a Ctx, callback: F, generics: ast::Generics) -> Self {
        GenericsBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
            lifetimes: generics.lifetimes,
            ty_params: generics.ty_params.into_vec(),
            predicates: generics.where_clause.predicates,
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn with_lifetimes<I, L>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=L>,
              L: IntoLifetimeDef,
    {
        let ctx = self.ctx;
        let iter = iter.into_iter().map(|lifetime_def| lifetime_def.into_lifetime_def(ctx));
        self.lifetimes.extend(iter);
        self
    }

    pub fn with_lifetime_names<I, N>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=N>,
              N: ToName,
    {
        for name in iter {
            self = self.lifetime_name(name);
        }
        self
    }

    pub fn with_lifetime(mut self, lifetime: ast::LifetimeDef) -> Self {
        self.lifetimes.push(lifetime);
        self
    }

    pub fn lifetime_name<N>(self, name: N) -> Self
        where N: ToName,
    {
        self.lifetime(name).build()
    }

    pub fn lifetime<N>(self, name: N) -> LifetimeDefBuilder<'a, Self>
        where N: ToName,
    {
        LifetimeDefBuilder::new_with_callback(self.ctx, name, self)
    }

    pub fn with_ty_params<I>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=ast::TyParam>,
    {
        self.ty_params.extend(iter);
        self
    }

    pub fn with_ty_param_ids<I, T>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=T>,
              T: ToIdent,
    {
        for id in iter {
            self = self.ty_param_id(id);
        }
        self
    }

    pub fn with_ty_param(mut self, ty_param: ast::TyParam) -> Self {
        self.ty_params.push(ty_param);
        self
    }

    pub fn ty_param_id<I>(self, id: I) -> Self
        where I: ToIdent,
    {
        self.ty_param(id).build()
    }

    pub fn ty_param<I>(self, id: I) -> TyParamBuilder<'a, Self>
        where I: ToIdent,
    {
        let span = self.span;
        TyParamBuilder::new_with_callback(self.ctx, id, self).span(span)
    }

    pub fn with_predicates<I>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=ast::WherePredicate>,
    {
        self.predicates.extend(iter);
        self
    }

    pub fn with_predicate(mut self, predicate: ast::WherePredicate) -> Self {
        self.predicates.push(predicate);
        self
    }

    pub fn add_lifetime_bound<L>(mut self, lifetime: L) -> Self
        where L: IntoLifetime,
    {
        let lifetime = lifetime.into_lifetime(self.ctx);

        for lifetime_def in self.lifetimes.iter_mut() {
            lifetime_def.bounds.push(lifetime.clone());
        }

        for ty_param in self.ty_params.iter_mut() {
            *ty_param = TyParamBuilder::from_ty_param(self.ctx, ty_param.clone())
                .lifetime_bound(lifetime.clone())
                .build();
        }

        self 
    }

    pub fn add_ty_param_bound<P>(mut self, path: P) -> Self
        where P: IntoPath,
    {
        let path = path.into_path(self.ctx);

        for ty_param in self.ty_params.iter_mut() {
            *ty_param = TyParamBuilder::from_ty_param(self.ctx, ty_param.clone())
                .trait_bound(path.clone()).build()
                .build();
        }

        self 
    }

    pub fn strip_bounds(mut self) -> Self {
        for lifetime in self.lifetimes.iter_mut() {
            lifetime.bounds = vec![];
        }

        for ty_param in self.ty_params.iter_mut() {
            ty_param.bounds = OwnedSlice::empty();
        }

        self.predicates = vec![];

        self
    }

    pub fn build(self) -> F::Result {
        self.callback.invoke(ast::Generics {
            lifetimes: self.lifetimes,
            ty_params: OwnedSlice::from_vec(self.ty_params),
            where_clause: ast::WhereClause {
                id: ast::DUMMY_NODE_ID,
                predicates: self.predicates,
            },
        })
    }
}

impl<'a, F> Invoke<ast::LifetimeDef> for GenericsBuilder<'a, F>
    where F: Invoke<ast::Generics>,
{
    type Result = Self;

    fn invoke(self, lifetime: ast::LifetimeDef) -> Self {
        self.with_lifetime(lifetime)
    }
}

impl<'a, F> Invoke<ast::TyParam> for GenericsBuilder<'a, F>
    where F: Invoke<ast::Generics>,
{
    type Result = Self;

    fn invoke(self, ty_param: ast::TyParam) -> Self {
        self.with_ty_param(ty_param)
    }
}
