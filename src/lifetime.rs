use syntax::ast;
use syntax::codemap::{DUMMY_SP};

use invoke::{Invoke, Identity};

use ctx::Ctx;
use name::ToName;

//////////////////////////////////////////////////////////////////////////////

pub trait IntoLifetime {
    fn into_lifetime(self, ctx: &Ctx) -> ast::Lifetime;
}

impl IntoLifetime for ast::Lifetime {
    fn into_lifetime(self, _ctx: &Ctx) -> ast::Lifetime {
        self
    }
}

impl<'a> IntoLifetime for &'a str {
    fn into_lifetime(self, ctx: &Ctx) -> ast::Lifetime {
        ast::Lifetime {
            id: ast::DUMMY_NODE_ID,
            span: DUMMY_SP,
            name: self.to_name(ctx),
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

pub trait IntoLifetimeDef {
    fn into_lifetime_def(self, ctx: &Ctx) -> ast::LifetimeDef;
}

impl IntoLifetimeDef for ast::LifetimeDef {
    fn into_lifetime_def(self, _ctx: &Ctx) -> ast::LifetimeDef {
        self
    }
}

impl IntoLifetimeDef for ast::Lifetime {
    fn into_lifetime_def(self, _ctx: &Ctx) -> ast::LifetimeDef {
        ast::LifetimeDef {
            lifetime: self,
            bounds: vec![],
        }
    }
}

impl<'a> IntoLifetimeDef for &'a str {
    fn into_lifetime_def(self, ctx: &Ctx) -> ast::LifetimeDef {
        self.into_lifetime(ctx).into_lifetime_def(ctx)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct LifetimeDefBuilder<'a, F=Identity> {
    ctx: &'a Ctx,
    callback: F,
    lifetime: ast::Lifetime,
    bounds: Vec<ast::Lifetime>,
}

impl<'a> LifetimeDefBuilder<'a> {
    pub fn new<N>(ctx: &'a Ctx, name: N) -> Self
        where N: ToName,
    {
        LifetimeDefBuilder::new_with_callback(ctx, name, Identity)
    }
}

impl<'a, F> LifetimeDefBuilder<'a, F>
    where F: Invoke<ast::LifetimeDef>,
{
    pub fn new_with_callback<N>(ctx: &'a Ctx, name: N, callback: F) -> Self
        where N: ToName,
    {
        let lifetime = ast::Lifetime {
            id: ast::DUMMY_NODE_ID,
            span: DUMMY_SP,
            name: name.to_name(ctx),
        };

        LifetimeDefBuilder {
            ctx: ctx,
            callback: callback,
            lifetime: lifetime,
            bounds: Vec::new(),
        }
    }

    pub fn bound<N>(mut self, name: N) -> Self
        where N: ToName,
    {
        let lifetime = ast::Lifetime {
            id: ast::DUMMY_NODE_ID,
            span: DUMMY_SP,
            name: name.to_name(self.ctx),
        };

        self.bounds.push(lifetime);
        self
    }

    pub fn build(self) -> F::Result {
        self.callback.invoke(ast::LifetimeDef {
            lifetime: self.lifetime,
            bounds: self.bounds,
        })
    }
}
