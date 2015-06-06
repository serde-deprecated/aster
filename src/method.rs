use syntax::abi::Abi;
use syntax::ast;
use syntax::codemap::{DUMMY_SP, Span, respan};
use syntax::ptr::P;

use attr::AttrBuilder;
use block::BlockBuilder;
use generics::GenericsBuilder;
use ident::ToIdent;
use invoke::{Invoke, Identity};
use lifetime::IntoLifetime;

//////////////////////////////////////////////////////////////////////////////

pub struct MethodBuilder<F=Identity> {
    callback: F,
    span: Span,
    attrs: Vec<ast::Attribute>,
    abi: Abi,
    generics: ast::Generics,
    unsafety: ast::Unsafety,
    constness: ast::Constness,
    id: ast::Ident,
    vis: ast::Visibility,
}

impl MethodBuilder {
    pub fn new<I: ToIdent>(id: I) -> Self {
        MethodBuilder::new_with_callback(id, Identity)
    }
}

impl<F> MethodBuilder<F>
    where F: Invoke<P<ast::ImplItem>>,
{
    pub fn new_with_callback<I>(id: I, callback: F) -> Self
        where I: ToIdent,
    {
        MethodBuilder {
            callback: callback,
            span: DUMMY_SP,
            attrs: vec![],
            abi: Abi::Rust,
            generics: GenericsBuilder::new().build(),
            unsafety: ast::Unsafety::Normal,
            constness: ast::Constness::NotConst,
            id: id.to_ident(),
            vis: ast::Visibility::Inherited,
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn with_attr(mut self, attr: ast::Attribute) -> Self {
        self.attrs.push(attr);
        self
    }

    pub fn attr(self) -> AttrBuilder<Self> {
        AttrBuilder::new_with_callback(self)
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

    pub fn vis(mut self, vis: ast::Visibility) -> Self {
        self.vis = vis;
        self
    }

    pub fn with_generics(mut self, generics: ast::Generics) -> Self {
        self.generics = generics;
        self
    }

    pub fn generics(self) -> GenericsBuilder<Self> {
        GenericsBuilder::new_with_callback(self)
    }

    pub fn self_(self) -> SelfBuilder<Self> {
        SelfBuilder::new_with_callback(self)
    }
}

impl<F> Invoke<ast::Attribute> for MethodBuilder<F>
    where F: Invoke<P<ast::ImplItem>>,
{
    type Result = Self;

    fn invoke(self, attr: ast::Attribute) -> Self {
        self.with_attr(attr)
    }
}

impl<F> Invoke<ast::Generics> for MethodBuilder<F>
    where F: Invoke<P<ast::ImplItem>>,
{
    type Result = Self;

    fn invoke(self, generics: ast::Generics) -> Self {
        self.with_generics(generics)
    }
}

impl<F> Invoke<ast::ExplicitSelf> for MethodBuilder<F>
    where F: Invoke<P<ast::ImplItem>>,
{
    type Result = MethodSelfBuilder<F>;

    fn invoke(self, explicit_self: ast::ExplicitSelf) -> MethodSelfBuilder<F> {
        MethodSelfBuilder {
            builder: self,
            explicit_self: explicit_self,
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct MethodSelfBuilder<F> {
    builder: MethodBuilder<F>,
    explicit_self: ast::ExplicitSelf,
}

impl<F> Invoke<P<ast::FnDecl>> for MethodSelfBuilder<F>
    where F: Invoke<P<ast::ImplItem>>,
{
    type Result = BlockBuilder<MethodSelfFnDeclBuilder<F>>;

    fn invoke(self, fn_decl: P<ast::FnDecl>) -> BlockBuilder<MethodSelfFnDeclBuilder<F>> {
        BlockBuilder::new_with_callback(MethodSelfFnDeclBuilder {
            builder: self.builder,
            explicit_self: self.explicit_self,
            fn_decl: fn_decl,
        })
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct MethodSelfFnDeclBuilder<F> {
    builder: MethodBuilder<F>,
    explicit_self: ast::ExplicitSelf,
    fn_decl: P<ast::FnDecl>,
}

impl<F> Invoke<P<ast::Block>> for MethodSelfFnDeclBuilder<F>
    where F: Invoke<P<ast::ImplItem>>,
{
    type Result = F::Result;

    fn invoke(self, block: P<ast::Block>) -> F::Result {
        let method_sig = ast::MethodSig {
            unsafety: self.builder.unsafety,
            constness: self.builder.constness,
            abi: self.builder.abi,
            decl: self.fn_decl,
            generics: self.builder.generics,
            explicit_self: self.explicit_self,
        };

        self.builder.callback.invoke(P(ast::ImplItem {
            id: ast::DUMMY_NODE_ID,
            ident: self.builder.id,
            vis: self.builder.vis,
            attrs: self.builder.attrs,
            node: ast::MethodImplItem(method_sig, block),
            span: self.builder.span,
        }))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct SelfBuilder<F> {
    callback: F,
    span: Span,
}

impl<F> SelfBuilder<F>
    where F: Invoke<ast::ExplicitSelf>,
{
    pub fn new_with_callback(callback: F) -> Self {
        SelfBuilder {
            callback: callback,
            span: DUMMY_SP,
        }
    }

    pub fn build(self, self_: ast::ExplicitSelf) -> F::Result {
        self.callback.invoke(self_)
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn build_self_(self, self_: ast::ExplicitSelf_) -> F::Result {
        let self_ = respan(self.span, self_);
        self.build(self_)
    }

    pub fn static_(self) -> F::Result {
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

    /*
    pub fn ty(self) -> TyBuilder<F::Result> {
        TyBuilder::new_with_callback(self)
    }
    */
}

impl<F> Invoke<P<ast::Ty>> for SelfBuilder<F>
    where F: Invoke<ast::ExplicitSelf>,
{
    type Result = F::Result;

    fn invoke(self, ty: P<ast::Ty>) -> F::Result {
        self.build_self_(ast::ExplicitSelf_::SelfExplicit(ty, "self".to_ident()))
    }
}
