use syntax::abi::Abi;
use syntax::ast;
use syntax::codemap::{DUMMY_SP, Span, respan};
use syntax::ptr::P;

use fn_decl::FnDeclBuilder;
use generics::GenericsBuilder;
use ident::ToIdent;
use invoke::{Invoke, Identity};
use lifetime::IntoLifetime;
use ty::TyBuilder;

//////////////////////////////////////////////////////////////////////////////

pub struct MethodSigBuilder<F=Identity> {
    callback: F,
    span: Span,
    abi: Abi,
    generics: ast::Generics,
    unsafety: ast::Unsafety,
    constness: ast::Constness,
    explicit_self: ast::ExplicitSelf,
    self_mutable: ast::Mutability,
}

impl MethodSigBuilder {
    pub fn new() -> Self {
        MethodSigBuilder::with_callback(Identity)
    }
}

impl<F> MethodSigBuilder<F>
    where F: Invoke<ast::MethodSig>,
{
    pub fn with_callback(callback: F) -> Self {
        MethodSigBuilder {
            callback: callback,
            span: DUMMY_SP,
            abi: Abi::Rust,
            generics: GenericsBuilder::new().build(),
            unsafety: ast::Unsafety::Normal,
            constness: ast::Constness::NotConst,
            explicit_self: respan(DUMMY_SP, ast::ExplicitSelf_::SelfStatic),
            self_mutable: ast::MutImmutable,
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn unsafe_(mut self) -> Self {
        self.unsafety = ast::Unsafety::Normal;
        self
    }

    pub fn const_(mut self) -> Self {
        self.constness = ast::Constness::Const;
        self
    }

    pub fn abi(mut self, abi: Abi) -> Self {
        self.abi = abi;
        self
    }

    pub fn with_generics(mut self, generics: ast::Generics) -> Self {
        self.generics = generics;
        self
    }

    pub fn generics(self) -> GenericsBuilder<Self> {
        GenericsBuilder::with_callback(self)
    }

    pub fn with_self(mut self, mutable: ast::Mutability, explicit_self: ast::ExplicitSelf) -> Self {
        self.self_mutable = mutable;
        self.explicit_self = explicit_self;
        self
    }

    pub fn self_(self) -> SelfBuilder<Self> {
        SelfBuilder::with_callback(self)
    }

    pub fn build_fn_decl(self, mut fn_decl: P<ast::FnDecl>) -> F::Result {
        // Add `self` to the decl.
        match self.explicit_self.node {
            ast::SelfStatic => { }
            ast::SelfValue(id)
            | ast::SelfRegion(_, _, id)
            | ast::SelfExplicit(_, id) => {
                fn_decl = fn_decl.map(|mut fn_decl| {
                    let arg = ast::Arg::new_self(self.span, self.self_mutable, id);
                    fn_decl.inputs.insert(0, arg);
                    fn_decl
                });
            }
        }

        self.callback.invoke(ast::MethodSig {
            unsafety: self.unsafety,
            constness: self.constness,
            abi: self.abi,
            decl: fn_decl,
            generics: self.generics,
            explicit_self: self.explicit_self,
        })
    }

    pub fn fn_decl(self) -> FnDeclBuilder<Self> {
        FnDeclBuilder::with_callback(self)
    }
}

impl<F> Invoke<ast::Generics> for MethodSigBuilder<F>
    where F: Invoke<ast::MethodSig>,
{
    type Result = Self;

    fn invoke(self, generics: ast::Generics) -> Self {
        self.with_generics(generics)
    }
}

impl<F> Invoke<(ast::Mutability, ast::ExplicitSelf)> for MethodSigBuilder<F>
    where F: Invoke<ast::MethodSig>,
{
    type Result = Self;

    fn invoke(self, (mutable, explicit_self): (ast::Mutability, ast::ExplicitSelf)) -> Self {
        self.with_self(mutable, explicit_self)
    }
}

impl<F> Invoke<P<ast::FnDecl>> for MethodSigBuilder<F>
    where F: Invoke<ast::MethodSig>,
{
    type Result = F::Result;

    fn invoke(self, fn_decl: P<ast::FnDecl>) -> Self::Result {
        self.build_fn_decl(fn_decl)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct SelfBuilder<F=Identity> {
    callback: F,
    span: Span,
    mutable: ast::Mutability,
}

impl SelfBuilder {
    pub fn new() -> Self {
        SelfBuilder::with_callback(Identity)
    }
}

impl<F> SelfBuilder<F>
    where F: Invoke<(ast::Mutability, ast::ExplicitSelf)>,
{
    pub fn with_callback(callback: F) -> Self {
        SelfBuilder {
            callback: callback,
            span: DUMMY_SP,
            mutable: ast::MutImmutable,
        }
    }

    pub fn build(self, self_: ast::ExplicitSelf) -> F::Result {
        self.callback.invoke((self.mutable, self_))
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn build_self_(self, self_: ast::ExplicitSelf_) -> F::Result {
        let self_ = respan(self.span, self_);
        self.build(self_)
    }

    pub fn mut_(mut self) -> Self {
        self.mutable = ast::MutMutable;
        self
    }

    pub fn static_(self) -> F::Result {
        self.build_self_(ast::ExplicitSelf_::SelfStatic)
    }

    pub fn mut_static(self) -> F::Result {
        self.build_self_(ast::ExplicitSelf_::SelfStatic)
    }

    pub fn value(self) -> F::Result {
        self.build_self_(ast::ExplicitSelf_::SelfValue("self".to_ident()))
    }

    pub fn ref_(self) -> F::Result {
        self.build_self_(ast::ExplicitSelf_::SelfRegion(
            None,
            ast::Mutability::MutImmutable,
            "self".to_ident(),
        ))
    }

    pub fn ref_lifetime<L>(self, lifetime: L) -> F::Result
        where L: IntoLifetime,
    {
        self.build_self_(ast::ExplicitSelf_::SelfRegion(
            Some(lifetime.into_lifetime()),
            ast::Mutability::MutImmutable,
            "self".to_ident(),
        ))
    }

    pub fn ref_mut(self) -> F::Result {
        self.build_self_(ast::ExplicitSelf_::SelfRegion(
            None,
            ast::Mutability::MutMutable,
            "self".to_ident(),
        ))
    }

    pub fn ref_mut_lifetime<L>(self, lifetime: L) -> F::Result
        where L: IntoLifetime,
    {
        self.build_self_(ast::ExplicitSelf_::SelfRegion(
            Some(lifetime.into_lifetime()),
            ast::Mutability::MutMutable,
            "self".to_ident(),
        ))
    }

    pub fn ty(self) -> TyBuilder<Self> {
        TyBuilder::with_callback(self)
    }
}

impl<F> Invoke<P<ast::Ty>> for SelfBuilder<F>
    where F: Invoke<(ast::Mutability, ast::ExplicitSelf)>,
{
    type Result = F::Result;

    fn invoke(self, ty: P<ast::Ty>) -> F::Result {
        self.build_self_(ast::ExplicitSelf_::SelfExplicit(ty, "self".to_ident()))
    }
}
