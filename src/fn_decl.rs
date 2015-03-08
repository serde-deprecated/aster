use syntax::ast;
use syntax::codemap::{DUMMY_SP, Span, respan};
use syntax::ptr::P;

use ctx::Ctx;
use ident::ToIdent;
use invoke::{Invoke, Identity};
use ty::TyBuilder;

//////////////////////////////////////////////////////////////////////////////

pub struct FnDeclBuilder<'a, F=Identity> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
    args: Vec<ast::Arg>,
    variadic: bool,
}

impl<'a> FnDeclBuilder<'a> {
    pub fn new(ctx: &'a Ctx) -> FnDeclBuilder<'a> {
        FnDeclBuilder::new_with_callback(ctx, Identity)
    }
}

impl<'a, F> FnDeclBuilder<'a, F>
    where F: Invoke<P<ast::FnDecl>>,
{
    pub fn new_with_callback(ctx: &'a Ctx, callback: F) -> Self {
        FnDeclBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
            args: Vec::new(),
            variadic: false,
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn variadic(mut self) -> Self {
        self.variadic = true;
        self
    }

    pub fn with_arg(mut self, arg: ast::Arg) -> Self {
        self.args.push(arg);
        self
    }

    pub fn arg<I>(self, id: I) -> ArgBuilder<'a, Self>
        where I: ToIdent,
    {
        ArgBuilder::new_with_callback(self.ctx, id, self)
    }

    pub fn no_return(self) -> F::Result {
        let ret_ty = ast::FunctionRetTy::NoReturn(self.span);
        self.build(ret_ty)
    }

    pub fn build_output(self, ty: P<ast::Ty>) -> F::Result {
        self.build(ast::FunctionRetTy::Return(ty))
    }

    pub fn output(self) -> TyBuilder<'a, Self> {
        TyBuilder::new_with_callback(self.ctx, self)
    }

    pub fn build(self, output: ast::FunctionRetTy) -> F::Result {
        self.callback.invoke(P(ast::FnDecl {
            inputs: self.args,
            output: output,
            variadic: self.variadic,
        }))
    }
}

impl<'a, F> Invoke<ast::Arg> for FnDeclBuilder<'a, F>
    where F: Invoke<P<ast::FnDecl>>
{
    type Result = Self;

    fn invoke(self, arg: ast::Arg) -> Self {
        self.with_arg(arg)
    }
}

impl<'a, F> Invoke<P<ast::Ty>> for FnDeclBuilder<'a, F>
    where F: Invoke<P<ast::FnDecl>>,
{
    type Result = F::Result;

    fn invoke(self, ty: P<ast::Ty>) -> F::Result {
        self.build_output(ty)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ArgBuilder<'a, F=Identity> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
    id: ast::Ident,
}

impl<'a> ArgBuilder<'a> {
    pub fn new<I>(ctx: &'a Ctx, id: I) -> Self where I: ToIdent {
        ArgBuilder::new_with_callback(ctx, id, Identity)
    }
}

impl<'a, F> ArgBuilder<'a, F>
    where F: Invoke<ast::Arg>,
{
    pub fn new_with_callback<I>(ctx: &'a Ctx, id: I, callback: F) -> ArgBuilder<'a, F>
        where I: ToIdent,
    {
        ArgBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
            id: id.to_ident(ctx),
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn build_ty(self, ty: P<ast::Ty>) -> F::Result {
        let path = respan(self.span, self.id);

        self.callback.invoke(ast::Arg {
            id: ast::DUMMY_NODE_ID,
            ty: ty,
            pat: P(ast::Pat {
                id: ast::DUMMY_NODE_ID,
                node: ast::PatIdent(
                    ast::BindByValue(ast::Mutability::MutImmutable),
                    path,
                    None,
                ),
                span: self.span,
            }),
        })
    }

    pub fn ty(self) -> TyBuilder<'a, ArgTyBuilder<'a, F>> {
        TyBuilder::new_with_callback(self.ctx, ArgTyBuilder(self))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ArgTyBuilder<'a, F>(ArgBuilder<'a, F>);

impl<'a, F: Invoke<ast::Arg>> Invoke<P<ast::Ty>> for ArgTyBuilder<'a, F>
{
    type Result = F::Result;

    fn invoke(self, ty: P<ast::Ty>) -> F::Result {
        self.0.build_ty(ty)
    }
}
