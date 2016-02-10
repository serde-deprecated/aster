#![cfg_attr(not(feature = "with-syntex"), feature(rustc_private))]
#![cfg_attr(feature = "unstable-testing", feature(plugin))]
#![cfg_attr(feature = "unstable-testing", plugin(clippy))]
#![cfg_attr(feature = "unstable-testing", allow(wrong_self_convention))]

#[cfg(feature = "with-syntex")]
extern crate syntex_syntax as syntax;

#[cfg(not(feature = "with-syntex"))]
extern crate syntax;

use syntax::ast;
use syntax::codemap::{DUMMY_SP, Span};
use syntax::parse::token;

pub mod arm;
pub mod attr;
pub mod block;
pub mod constant;
pub mod expr;
pub mod fn_decl;
pub mod generics;
pub mod ident;
pub mod invoke;
pub mod item;
pub mod lifetime;
pub mod lit;
pub mod mac;
pub mod method;
pub mod name;
pub mod pat;
pub mod path;
pub mod qpath;
pub mod stmt;
pub mod str;
pub mod struct_field;
pub mod ty;
pub mod ty_param;
pub mod variant;
pub mod variant_data;
pub mod where_predicate;

//////////////////////////////////////////////////////////////////////////////

#[derive(Copy, Clone)]
pub struct AstBuilder {
    span: Span,
}

impl AstBuilder {
    pub fn new() -> AstBuilder {
        AstBuilder {
            span: DUMMY_SP,
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn interned_string<S>(&self, s: S) -> token::InternedString
        where S: str::ToInternedString
    {
        s.to_interned_string()
    }

    pub fn id<I>(&self, id: I) -> ast::Ident
        where I: ident::ToIdent
    {
        id.to_ident()
    }

    pub fn name<N>(&self, name: N) -> ast::Name
        where N: name::ToName
    {
        name.to_name()
    }

    pub fn lifetime<L>(&self, lifetime: L) -> ast::Lifetime
        where L: lifetime::IntoLifetime
    {
        lifetime.into_lifetime()
    }

    pub fn arm(&self) -> arm::ArmBuilder {
        arm::ArmBuilder::new()
    }

    pub fn attr(&self) -> attr::AttrBuilder {
        attr::AttrBuilder::new()
    }

    pub fn path(&self) -> path::PathBuilder {
        path::PathBuilder::new()
    }

    pub fn qpath(&self) -> qpath::QPathBuilder {
        qpath::QPathBuilder::new()
    }

    pub fn ty(&self) -> ty::TyBuilder {
        ty::TyBuilder::new().span(self.span)
    }

    pub fn lifetime_def<N>(&self, name: N) -> lifetime::LifetimeDefBuilder
        where N: name::ToName,
    {
        lifetime::LifetimeDefBuilder::new(name)
    }

    pub fn ty_param<I>(&self, id: I) -> ty_param::TyParamBuilder
        where I: ident::ToIdent,
    {
        ty_param::TyParamBuilder::new(id).span(self.span)
    }

    pub fn ty_param_bound(&self) -> ty_param::TyParamBoundBuilder {
        ty_param::TyParamBoundBuilder::new().span(self.span)
    }

    pub fn from_ty_param(&self, ty_param: ast::TyParam) -> ty_param::TyParamBuilder {
        ty_param::TyParamBuilder::from_ty_param(ty_param)
    }

    pub fn generics(&self) -> generics::GenericsBuilder {
        generics::GenericsBuilder::new().span(self.span)
    }

    pub fn where_predicate(&self) -> where_predicate::WherePredicateBuilder {
        where_predicate::WherePredicateBuilder::new().span(self.span)
    }

    pub fn from_generics(&self, generics: ast::Generics) -> generics::GenericsBuilder {
        generics::GenericsBuilder::from_generics(generics).span(self.span)
    }

    pub fn lit(&self) -> lit::LitBuilder {
        lit::LitBuilder::new().span(self.span)
    }

    pub fn expr(&self) -> expr::ExprBuilder {
        expr::ExprBuilder::new().span(self.span)
    }

    pub fn stmt(&self) -> stmt::StmtBuilder {
        stmt::StmtBuilder::new().span(self.span)
    }

    pub fn block(&self) -> block::BlockBuilder {
        block::BlockBuilder::new().span(self.span)
    }

    pub fn pat(&self) -> pat::PatBuilder {
        pat::PatBuilder::new().span(self.span)
    }

    pub fn fn_decl(&self) -> fn_decl::FnDeclBuilder {
        fn_decl::FnDeclBuilder::new().span(self.span)
    }

    pub fn method(&self) -> method::MethodBuilder {
        method::MethodBuilder::new().span(self.span)
    }

    pub fn self_(&self) -> method::SelfBuilder {
        method::SelfBuilder::new().span(self.span)
    }

    pub fn arg<I>(&self, id: I) -> fn_decl::ArgBuilder
        where I: ident::ToIdent,
    {
        fn_decl::ArgBuilder::new(id).span(self.span)
    }

    pub fn variant_data(&self) -> variant_data::VariantDataBuilder {
        variant_data::VariantDataBuilder::new().span(self.span)
    }

    pub fn variant<T>(&self, id: T) -> variant::VariantBuilder
        where T: ident::ToIdent,
    {
        variant::VariantBuilder::new(id).span(self.span)
    }

    pub fn struct_field<T>(&self, id: T) -> struct_field::StructFieldBuilder
        where T: ident::ToIdent,
    {
        struct_field::StructFieldBuilder::named(id).span(self.span)
    }

    pub fn tuple_field(&self) -> struct_field::StructFieldBuilder {
        struct_field::StructFieldBuilder::unnamed().span(self.span)
    }

    pub fn item(&self) -> item::ItemBuilder {
        item::ItemBuilder::new().span(self.span)
    }

    pub fn const_(&self) -> constant::ConstBuilder {
        constant::ConstBuilder::new().span(self.span)
    }
}
