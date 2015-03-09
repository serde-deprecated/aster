use syntax::abi::Abi;
use syntax::ast;
use syntax::codemap::{DUMMY_SP, Span, respan};
use syntax::ptr::P;

use generics::GenericsBuilder;
use ident::ToIdent;
use invoke::{Invoke, Identity};

//////////////////////////////////////////////////////////////////////////////

pub struct MethodBuilder<F=Identity> {
    callback: F,
    span: Span,
    attrs: Vec<ast::Attribute>,
    abi: Abi,
    generics: ast::Generics,
    unsafety: ast::Unsafety,
    id: ast::Ident,
    vis: ast::Visibility,
}

impl MethodBuilder {
    pub fn new<I: ToIdent>(id: I) -> Self {
        MethodBuilder::new_with_callback(id, Identity)
    }
}

impl<F> MethodBuilder<F>
    where F: Invoke<P<ast::Method>>,
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
            id: id.to_ident(),
            vis: ast::Visibility::Inherited,
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

    pub fn abi(mut self, abi: Abi) -> Self {
        self.abi = abi;
        self
    }

    pub fn vis(mut self, vis: ast::Visibility) -> Self {
        self.vis = vis;
        self
    }

    pub fn generics(self) -> GenericsBuilder<Self> {
        GenericsBuilder::new_with_callback(self)
    }

    pub fn self_(self) -> SelfBuilder<Self> {
        SelfBuilder::new_with_callback(self)
    }

    /*
    pub fn block(self) -> BlockBuilder<Self> {
        BlockBuilder::new_with_callback(self)
    }
    */
}

impl<F> Invoke<ast::Generics> for MethodBuilder<F>
    where F: Invoke<P<ast::Method>>,
{
    type Result = Self;

    fn invoke(mut self, generics: ast::Generics) -> Self {
        self.generics = generics;
        self
    }
}

impl<F> Invoke<ast::ExplicitSelf> for MethodBuilder<F>
    where F: Invoke<P<ast::Method>>,
{
    type Result = MethodSelfBuilder<F>;

    fn invoke(self, self_: ast::ExplicitSelf) -> MethodSelfBuilder<F> {
        MethodSelfBuilder {
            builder: self,
            self_: self_,
        }
    }
}

pub struct MethodSelfBuilder<F> {
    builder: MethodBuilder<F>,
    self_: ast::ExplicitSelf,
}

impl<F> Invoke<P<ast::FnDecl>> for MethodSelfBuilder<F>
    where F: Invoke<P<ast::Method>>,
{
    type Result = MethodSelfFnDeclBuilder<F>;

    fn invoke(self, fn_decl: P<ast::FnDecl>) -> MethodSelfFnDeclBuilder<F> {
        MethodSelfFnDeclBuilder {
            builder: self.builder,
            self_: self.self_,
            fn_decl: fn_decl,
        }
    }
}

pub struct MethodSelfFnDeclBuilder<F> {
    builder: MethodBuilder<F>,
    self_: ast::ExplicitSelf,
    fn_decl: P<ast::FnDecl>,
}

impl<F> Invoke<P<ast::Block>> for MethodSelfFnDeclBuilder<F>
    where F: Invoke<P<ast::Method>>,
{
    type Result = F::Result;

    fn invoke(self, block: P<ast::Block>) -> F::Result {
        let method = ast::Method {
            attrs: self.builder.attrs,
            id: ast::DUMMY_NODE_ID,
            span: self.builder.span,
            node: ast::MethDecl(
                self.builder.id,
                self.builder.generics,
                self.builder.abi,
                self.self_,
                self.builder.unsafety,
                self.fn_decl,
                block,
                self.builder.vis,
            ),
        };

        self.builder.callback.invoke(P(method))
    }
}

/*
impl<F> Invoke<ast::Block> for MethodBuilder<F>
    where F: Invoke<P<ast::Method>>,
{
    type Result = F::Result;

    fn invoke(self, block: P<ast::Block>) -> F::Result {
        let method = ast::Method {
            attrs: self.attrs,
            id: ast::DUMMY_NODE_ID,
            span: self.span,
            node: ast::MethDecl(
                self.id,
                self.generics,
                self.abi,
                respan(self.span, self.self_),
                ast::Unsafety::Normal,
                self.fn_decl,
                block,
                self.vis,
            ),
        };

        self.callback.invoke(P(method))
    }
}
*/

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

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn build_self(self, self_: ast::ExplicitSelf_) -> F::Result {
        self.callback.invoke(respan(self.span, self_))
    }

    pub fn static_(self) -> F::Result {
        self.build_self(ast::ExplicitSelf_::SelfStatic)
    }

    pub fn value(self) -> F::Result {
        let ident = "self".to_ident();
        self.build_self(ast::ExplicitSelf_::SelfValue(ident))
    }

    /*
    pub fn ref_(self) -> F::Result {
        self.self_(ast::ExplicitSelf_::SelfValue(ident))
    }

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
        let ident = "self".to_ident();
        self.build_self(ast::ExplicitSelf_::SelfExplicit(ty, ident))
    }
}
