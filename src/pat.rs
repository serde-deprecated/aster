use syntax::ast;
use syntax::codemap::{DUMMY_SP, Span, respan};
use syntax::ptr::P;

use invoke::{Invoke, Identity};

use ctx::Ctx;
use expr::ExprBuilder;
use ident::ToIdent;
use path::PathBuilder;

//////////////////////////////////////////////////////////////////////////////

pub struct PatBuilder<'a, F=Identity> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
}

impl<'a> PatBuilder<'a> {
    pub fn new(ctx: &'a Ctx) -> Self {
        PatBuilder::new_with_callback(ctx, Identity)
    }
}


impl<'a, F> PatBuilder<'a, F>
    where F: Invoke<P<ast::Pat>>,
{
    pub fn new_with_callback(ctx: &'a Ctx, callback: F) -> Self {
        PatBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn build_pat(self, pat_: ast::Pat_) -> F::Result {
        self.callback.invoke(P(ast::Pat {
            id: ast::DUMMY_NODE_ID,
            node: pat_,
            span: self.span,
        }))
    }

    pub fn wild(self) -> F::Result {
        self.build_pat(ast::Pat_::PatWild(ast::PatWildKind::PatWildSingle))
    }

    pub fn wild_multi(self) -> F::Result {
        self.build_pat(ast::Pat_::PatWild(ast::PatWildKind::PatWildMulti))
    }

    pub fn build_id<I>(self, mode: ast::BindingMode, id: I, sub: Option<P<ast::Pat>>) -> F::Result
        where I: ToIdent,
    {
        let id = respan(self.span, id.to_ident(self.ctx));

        self.build_pat(ast::Pat_::PatIdent(mode, id, sub))
    }

    pub fn id<I>(self, id: I) -> F::Result
        where I: ToIdent
    {
        let mode = ast::BindingMode::BindByValue(ast::Mutability::MutImmutable);
        self.build_id(mode, id, None)
    }

    pub fn mut_id<I>(self, id: I) -> F::Result
        where I: ToIdent
    {
        let mode = ast::BindingMode::BindByValue(ast::Mutability::MutMutable);
        self.build_id(mode, id, None)
    }

    pub fn ref_id<I>(self, id: I) -> F::Result
        where I: ToIdent
    {
        let mode = ast::BindingMode::BindByRef(ast::Mutability::MutImmutable);
        self.build_id(mode, id, None)
    }

    pub fn ref_mut_id<I>(self, id: I) -> F::Result
        where I: ToIdent
    {
        let mode = ast::BindingMode::BindByRef(ast::Mutability::MutMutable);
        self.build_id(mode, id, None)
    }

    pub fn enum_(self) -> PathBuilder<'a, PatPathBuilder<'a, F>> {
        PathBuilder::new_with_callback(self.ctx, PatPathBuilder(self))
    }

    pub fn expr(self) -> ExprBuilder<'a, PatExprBuilder<'a, F>> {
        ExprBuilder::new_with_callback(self.ctx, PatExprBuilder(self))
    }

    pub fn tuple(self) -> PatTupleBuilder<'a, F> {
        PatTupleBuilder {
            builder: self,
            pats: Vec::new(),
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct PatPathBuilder<'a, F>(PatBuilder<'a, F>);

impl<'a, F> Invoke<ast::Path> for PatPathBuilder<'a, F> {
    type Result = PatEnumBuilder<'a, F>;

    fn invoke(self, path: ast::Path) -> PatEnumBuilder<'a, F> {
        PatEnumBuilder {
            builder: self.0,
            path: path,
            pats: Vec::new(),
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct PatEnumBuilder<'a, F> {
    builder: PatBuilder<'a, F>,
    path: ast::Path,
    pats: Vec<P<ast::Pat>>,
}

impl<'a, F> PatEnumBuilder<'a, F>
    where F: Invoke<P<ast::Pat>>,
{
    pub fn pat(self) -> PatBuilder<'a, Self> {
        PatBuilder::new_with_callback(self.builder.ctx, self)
    }

    pub fn build(self) -> F::Result {
        let pats = if self.pats.is_empty() { None } else { Some(self.pats) };

        self.builder.build_pat(ast::Pat_::PatEnum(self.path, pats))
    }
}

impl<'a, F> Invoke<P<ast::Pat>> for PatEnumBuilder<'a, F>
    where F: Invoke<P<ast::Pat>>,
{
    type Result = Self;

    fn invoke(mut self, pat: P<ast::Pat>) -> Self {
        self.pats.push(pat);
        self
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct PatExprBuilder<'a, F>(PatBuilder<'a, F>);

impl<'a, F> Invoke<P<ast::Expr>> for PatExprBuilder<'a, F>
    where F: Invoke<P<ast::Pat>>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.0.build_pat(ast::Pat_::PatLit(expr))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct PatTupleBuilder<'a, F> {
    builder: PatBuilder<'a, F>,
    pats: Vec<P<ast::Pat>>,
}

impl<'a, F: Invoke<P<ast::Pat>>> PatTupleBuilder<'a, F>
    where F: Invoke<P<ast::Pat>>
{
    pub fn build_pat(mut self, pat: P<ast::Pat>) -> PatTupleBuilder<'a, F> {
        self.pats.push(pat);
        self
    }

    pub fn pat(self) -> PatBuilder<'a, PatTupleBuilder<'a, F>> {
        PatBuilder::new_with_callback(self.builder.ctx, self)
    }

    pub fn build(self) -> F::Result {
        self.builder.build_pat(ast::PatTup(self.pats))
    }
}

impl<'a, F> Invoke<P<ast::Pat>> for PatTupleBuilder<'a, F>
    where F: Invoke<P<ast::Pat>>
{
    type Result = PatTupleBuilder<'a, F>;

    fn invoke(self, pat: P<ast::Pat>) -> Self {
        self.build_pat(pat)
    }
}
