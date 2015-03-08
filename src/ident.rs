use syntax::ast;

use ctx::Ctx;

//////////////////////////////////////////////////////////////////////////////

pub trait ToIdent {
    fn to_ident(&self, ctx: &Ctx) -> ast::Ident;
}

impl ToIdent for ast::Ident {
    fn to_ident(&self, _ctx: &Ctx) -> ast::Ident {
        *self
    }
}

impl<'a> ToIdent for &'a str {
    fn to_ident(&self, ctx: &Ctx) -> ast::Ident {
        ast::Ident::new(ctx.intern(*self))
    }
}

impl ToIdent for String {
    fn to_ident(&self, ctx: &Ctx) -> ast::Ident {
        (&**self).to_ident(ctx)
    }
}

impl<'a, T> ToIdent for &'a T where T: ToIdent {
    fn to_ident(&self, ctx: &Ctx) -> ast::Ident {
        (**self).to_ident(ctx)
    }
}

