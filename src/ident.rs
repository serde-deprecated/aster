use syntax::ast;

use name::ToName;

//////////////////////////////////////////////////////////////////////////////

pub trait ToIdent {
    fn to_ident(&self) -> ast::Ident;
}

impl ToIdent for ast::Ident {
    fn to_ident(&self) -> ast::Ident {
        *self
    }
}

impl ToIdent for ast::Name {
    fn to_ident(&self) -> ast::Ident {
        ast::Ident::new(*self)
    }
}

impl<'a> ToIdent for &'a str {
    fn to_ident(&self) -> ast::Ident {
        self.to_name().to_ident()
    }
}

impl ToIdent for String {
    fn to_ident(&self) -> ast::Ident {
        (&**self).to_ident()
    }
}

impl<'a, T> ToIdent for &'a T where T: ToIdent {
    fn to_ident(&self) -> ast::Ident {
        (**self).to_ident()
    }
}

impl<'a, T> ToIdent for &'a mut T where T: ToIdent {
    fn to_ident(&self) -> ast::Ident {
        (**self).to_ident()
    }
}
