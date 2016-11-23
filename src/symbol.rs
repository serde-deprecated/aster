use syntax::ast;
use syntax::symbol::Symbol;

//////////////////////////////////////////////////////////////////////////////

pub trait ToSymbol {
    fn to_symbol(&self) -> Symbol;
}

impl ToSymbol for Symbol {
    fn to_symbol(&self) -> Symbol {
        self.clone()
    }
}

impl<'a> ToSymbol for &'a str {
    fn to_symbol(&self) -> Symbol {
        Symbol::intern(self)
    }
}

impl ToSymbol for ast::Ident {
    fn to_symbol(&self) -> Symbol {
        self.name
    }
}

impl<'a, T> ToSymbol for &'a T where T: ToSymbol {
    fn to_symbol(&self) -> Symbol {
        (**self).to_symbol()
    }
}

impl<'a, T> ToSymbol for &'a mut T where T: ToSymbol {
    fn to_symbol(&self) -> Symbol {
        (**self).to_symbol()
    }
}
