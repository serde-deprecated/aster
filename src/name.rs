use syntax::ast;

use ctx::Ctx;

//////////////////////////////////////////////////////////////////////////////

pub trait ToName {
    fn to_name(&self, ctx: &Ctx) -> ast::Name;
}

impl ToName for ast::Name {
    fn to_name(&self, _ctx: &Ctx) -> ast::Name {
        *self
    }
}

impl<'a> ToName for &'a str {
    fn to_name(&self, ctx: &Ctx) -> ast::Name {
        ctx.intern(*self)
    }
}

impl<'a, T> ToName for &'a T where T: ToName {
    fn to_name(&self, ctx: &Ctx) -> ast::Name {
        (**self).to_name(ctx)
    }
}
