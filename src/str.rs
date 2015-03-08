use syntax::ast;
use syntax::parse::token;

pub use expr::ExprBuilder;
pub use ident::ToIdent;
pub use name::ToName;
pub use path::PathBuilder;

//////////////////////////////////////////////////////////////////////////////

pub trait ToInternedString {
    fn into_interned_string(&self) -> token::InternedString;
}

impl ToInternedString for token::InternedString {
    fn into_interned_string(&self) -> token::InternedString {
        self.clone()
    }
}

impl<'a> ToInternedString for &'a str {
    fn into_interned_string(&self) -> token::InternedString {
        token::intern_and_get_ident(self)
    }
}

impl ToInternedString for ast::Ident {
    fn into_interned_string(&self) -> token::InternedString {
        token::get_ident(*self)
    }
}

impl ToInternedString for ast::Name {
    fn into_interned_string(&self) -> token::InternedString {
        token::get_name(*self)
    }
}

impl<'a, T> ToInternedString for &'a T where T: ToInternedString {
    fn into_interned_string(&self) -> token::InternedString {
        (**self).into_interned_string()
    }
}
