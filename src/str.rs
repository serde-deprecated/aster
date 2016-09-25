use syntax::ast;
use syntax::parse::token;

//////////////////////////////////////////////////////////////////////////////

pub trait ToInternedString {
    fn to_interned_string(&self) -> token::InternedString;
}

impl ToInternedString for token::InternedString {
    fn to_interned_string(&self) -> token::InternedString {
        self.clone()
    }
}

impl<'a> ToInternedString for &'a str {
    fn to_interned_string(&self) -> token::InternedString {
        token::intern_and_get_ident(self)
    }
}

impl ToInternedString for ast::Ident {
    fn to_interned_string(&self) -> token::InternedString {
        self.name.as_str()
    }
}

impl ToInternedString for ast::Name {
    fn to_interned_string(&self) -> token::InternedString {
        self.as_str()
    }
}

impl<'a, T> ToInternedString for &'a T where T: ToInternedString {
    fn to_interned_string(&self) -> token::InternedString {
        (**self).to_interned_string()
    }
}
