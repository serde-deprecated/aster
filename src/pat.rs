use syntax::ast;
use syntax::codemap::{DUMMY_SP, Span, respan};
use syntax::ptr::P;

use invoke::{Invoke, Identity};

use expr::ExprBuilder;
use ident::ToIdent;
use path::PathBuilder;

//////////////////////////////////////////////////////////////////////////////

pub struct PatBuilder<F=Identity> {
    callback: F,
    span: Span,
}

impl PatBuilder {
    pub fn new() -> Self {
        PatBuilder::new_with_callback(Identity)
    }
}


impl<F> PatBuilder<F>
    where F: Invoke<P<ast::Pat>>,
{
    pub fn new_with_callback(callback: F) -> Self {
        PatBuilder {
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
        let id = respan(self.span, id.to_ident());

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

    pub fn enum_(self) -> PathBuilder<PatPathBuilder<F>> {
        PathBuilder::new_with_callback(PatPathBuilder(self))
    }

    pub fn expr(self) -> ExprBuilder<PatExprBuilder<F>> {
        ExprBuilder::new_with_callback(PatExprBuilder(self))
    }

    pub fn tuple(self) -> PatTupleBuilder<F> {
        PatTupleBuilder {
            builder: self,
            pats: Vec::new(),
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct PatPathBuilder<F>(PatBuilder<F>);

impl<F> Invoke<ast::Path> for PatPathBuilder<F> {
    type Result = PatEnumBuilder<F>;

    fn invoke(self, path: ast::Path) -> PatEnumBuilder<F> {
        PatEnumBuilder {
            builder: self.0,
            path: path,
            pats: Vec::new(),
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct PatEnumBuilder<F> {
    builder: PatBuilder<F>,
    path: ast::Path,
    pats: Vec<P<ast::Pat>>,
}

impl<F> PatEnumBuilder<F>
    where F: Invoke<P<ast::Pat>>,
{
    pub fn pat(self) -> PatBuilder<Self> {
        PatBuilder::new_with_callback(self)
    }

    pub fn build(self) -> F::Result {
        let pats = if self.pats.is_empty() { None } else { Some(self.pats) };

        self.builder.build_pat(ast::Pat_::PatEnum(self.path, pats))
    }
}

impl<F> Invoke<P<ast::Pat>> for PatEnumBuilder<F>
    where F: Invoke<P<ast::Pat>>,
{
    type Result = Self;

    fn invoke(mut self, pat: P<ast::Pat>) -> Self {
        self.pats.push(pat);
        self
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct PatExprBuilder<F>(PatBuilder<F>);

impl<F> Invoke<P<ast::Expr>> for PatExprBuilder<F>
    where F: Invoke<P<ast::Pat>>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.0.build_pat(ast::Pat_::PatLit(expr))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct PatTupleBuilder<F> {
    builder: PatBuilder<F>,
    pats: Vec<P<ast::Pat>>,
}

impl<F: Invoke<P<ast::Pat>>> PatTupleBuilder<F>
    where F: Invoke<P<ast::Pat>>
{
    pub fn build_pat(mut self, pat: P<ast::Pat>) -> PatTupleBuilder<F> {
        self.pats.push(pat);
        self
    }

    pub fn pat(self) -> PatBuilder<PatTupleBuilder<F>> {
        PatBuilder::new_with_callback(self)
    }

    pub fn build(self) -> F::Result {
        self.builder.build_pat(ast::PatTup(self.pats))
    }
}

impl<F> Invoke<P<ast::Pat>> for PatTupleBuilder<F>
    where F: Invoke<P<ast::Pat>>
{
    type Result = PatTupleBuilder<F>;

    fn invoke(self, pat: P<ast::Pat>) -> Self {
        self.build_pat(pat)
    }
}
