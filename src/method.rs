use syntax::abi::Abi;
use syntax::ast;
use syntax::codemap::{DUMMY_SP, Span, respan};
use syntax::ptr::P;

use attr::AttrBuilder;
use block::BlockBuilder;
use fn_decl::FnDeclBuilder;
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
    explicit_self: ast::ExplicitSelf,
    fn_decl: P<ast::FnDecl>,
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
            explicit_self: respan(DUMMY_SP, ast::ExplicitSelf_::SelfStatic),
            fn_decl: P(ast::FnDecl {
                inputs: vec![],
                output: ast::FunctionRetTy::NoReturn(DUMMY_SP),
                variadic: false
            }),
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

    pub fn pub_(mut self) -> Self {
        self.vis = ast::Visibility::Public;
        self
    }

    pub fn with_generics(mut self, generics: ast::Generics) -> Self {
        self.generics = generics;
        self
    }

    pub fn generics(self) -> GenericsBuilder<Self> {
        GenericsBuilder::new_with_callback(self)
    }

    pub fn with_self(mut self, explicit_self: ast::ExplicitSelf) -> Self {
        self.explicit_self = explicit_self;
        self
    }

    pub fn self_(self) -> SelfBuilder<Self> {
        SelfBuilder::new_with_callback(self)
    }

    pub fn with_fn_decl(mut self, fn_decl: P<ast::FnDecl>) -> Self {
        self.fn_decl = fn_decl;
        self
    }

    pub fn fn_decl(self) -> FnDeclBuilder<Self> {
        FnDeclBuilder::new_with_callback(self)
    }

    pub fn build(self, block: P<ast::Block>) -> F::Result {
        let method_sig = ast::MethodSig {
            unsafety: self.unsafety,
            constness: self.constness,
            abi: self.abi,
            decl: self.fn_decl,
            generics: self.generics,
            explicit_self: self.explicit_self,
        };

        self.callback.invoke(P(ast::ImplItem {
            id: ast::DUMMY_NODE_ID,
            ident: self.id,
            vis: self.vis,
            attrs: self.attrs,
            node: ast::MethodImplItem(method_sig, block),
            span: self.span,
        }))
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
    type Result = Self;

    fn invoke(self, explicit_self: ast::ExplicitSelf) -> Self {
        self.with_self(explicit_self)
    }
}

impl<F> Invoke<P<ast::FnDecl>> for MethodBuilder<F>
    where F: Invoke<P<ast::ImplItem>>,
{
    type Result = BlockBuilder<Self>;

    fn invoke(self, fn_decl: P<ast::FnDecl>) -> BlockBuilder<Self> {
        BlockBuilder::new_with_callback(self.with_fn_decl(fn_decl))
    }
}

impl<F> Invoke<P<ast::Block>> for MethodBuilder<F>
    where F: Invoke<P<ast::ImplItem>>,
{
    type Result = F::Result;

    fn invoke(self, block: P<ast::Block>) -> F::Result {
        self.build(block)
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
