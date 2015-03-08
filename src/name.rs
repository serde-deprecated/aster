use syntax::ast;
use syntax::parse::token;

//////////////////////////////////////////////////////////////////////////////

pub trait ToName {
    fn to_name(&self) -> ast::Name;
}

impl ToName for ast::Name {
    fn to_name(&self) -> ast::Name {
        *self
    }
}

impl<'a> ToName for &'a str {
    fn to_name(&self) -> ast::Name {
        token::intern(*self)
    }
}

impl<'a, T> ToName for &'a T where T: ToName {
    fn to_name(&self) -> ast::Name {
        (**self).to_name()
    }
}

impl<'a, T> ToName for &'a mut T where T: ToName {
    fn to_name(&self) -> ast::Name {
        (**self).to_name()
    }
}
