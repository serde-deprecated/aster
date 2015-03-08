use syntax::abi::Abi;
use syntax::ast;
use syntax::codemap::{DUMMY_SP, Span, respan};
use syntax::ptr::P;

use ctx::Ctx;
use generics::GenericsBuilder;
use ident::ToIdent;
use invoke::{Invoke, Identity};

//////////////////////////////////////////////////////////////////////////////

pub struct MethodBuilder<'a, F=Identity> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
    attrs: Vec<ast::Attribute>,
    abi: Abi,
    generics: ast::Generics,
    unsafety: ast::Unsafety,
    id: ast::Ident,
    vis: ast::Visibility,
}

impl<'a> MethodBuilder<'a> {
    pub fn new<I: ToIdent>(ctx: &'a Ctx, id: I) -> Self {
        MethodBuilder::new_with_callback(ctx, id, Identity)
    }
}

impl<'a, F> MethodBuilder<'a, F>
    where F: Invoke<P<ast::Method>>,
{
    pub fn new_with_callback<I>(ctx: &'a Ctx, id: I, callback: F) -> Self
        where I: ToIdent,
    {
        MethodBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
            attrs: vec![],
            abi: Abi::Rust,
            generics: GenericsBuilder::new(ctx).build(),
            unsafety: ast::Unsafety::Normal,
            id: id.to_ident(ctx),
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

    pub fn with_generics(mut self, generics: ast::Generics) -> Self {
        self.generics = generics;
        self
    }

    pub fn generics(self) -> GenericsBuilder<'a, Self> {
        GenericsBuilder::new_with_callback(self.ctx, self)
    }

    pub fn self_(self) -> SelfBuilder<'a, Self> {
        SelfBuilder::new_with_callback(self.ctx, self)
    }

    /*
    pub fn block(self) -> BlockBuilder<'a, Self> {
        BlockBuilder::new_with_callback(self.ctx, self)
    }
    */
}

impl<'a, F> Invoke<ast::Generics> for MethodBuilder<'a, F>
    where F: Invoke<P<ast::Method>>,
{
    type Result = Self;

    fn invoke(self, generics: ast::Generics) -> Self {
        self.with_generics(generics)
    }
}

impl<'a, F> Invoke<ast::ExplicitSelf> for MethodBuilder<'a, F>
    where F: Invoke<P<ast::Method>>,
{
    type Result = MethodSelfBuilder<'a, F>;

    fn invoke(self, self_: ast::ExplicitSelf) -> MethodSelfBuilder<'a, F> {
        MethodSelfBuilder {
            builder: self,
            self_: self_,
        }
    }
}

pub struct MethodSelfBuilder<'a, F> {
    builder: MethodBuilder<'a, F>,
    self_: ast::ExplicitSelf,
}

impl<'a, F> Invoke<P<ast::FnDecl>> for MethodSelfBuilder<'a, F>
    where F: Invoke<P<ast::Method>>,
{
    type Result = MethodSelfFnDeclBuilder<'a, F>;

    fn invoke(self, fn_decl: P<ast::FnDecl>) -> MethodSelfFnDeclBuilder<'a, F> {
        MethodSelfFnDeclBuilder {
            builder: self.builder,
            self_: self.self_,
            fn_decl: fn_decl,
        }
    }
}

pub struct MethodSelfFnDeclBuilder<'a, F> {
    builder: MethodBuilder<'a, F>,
    self_: ast::ExplicitSelf,
    fn_decl: P<ast::FnDecl>,
}

impl<'a, F> Invoke<P<ast::Block>> for MethodSelfFnDeclBuilder<'a, F>
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
impl<'a, F> Invoke<ast::Block> for MethodBuilder<'a, F>
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

pub struct SelfBuilder<'a, F> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
}

impl<'a, F> SelfBuilder<'a, F>
    where F: Invoke<ast::ExplicitSelf>,
{
    pub fn new_with_callback(ctx: &'a Ctx, callback: F) -> Self {
        SelfBuilder {
            ctx: ctx,
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
        let ident = "self".to_ident(self.ctx);
        self.build_self(ast::ExplicitSelf_::SelfValue(ident))
    }

    /*
    pub fn ref_(self) -> F::Result {
        self.self_(ast::ExplicitSelf_::SelfValue(ident))
    }

    pub fn ty(self) -> TyBuilder<'a, F::Result> {
        TyBuilder::new_with_callback(self.ctx, self)
    }
    */
}

impl<'a, F> Invoke<P<ast::Ty>> for SelfBuilder<'a, F>
    where F: Invoke<ast::ExplicitSelf>,
{
    type Result = F::Result;

    fn invoke(self, ty: P<ast::Ty>) -> F::Result {
        let ident = "self".to_ident(self.ctx);
        self.build_self(ast::ExplicitSelf_::SelfExplicit(ty, ident))
    }
}
