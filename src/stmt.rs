use syntax::ast;
use syntax::codemap::{DUMMY_SP, Span};
use syntax::ptr::P;

use invoke::{Invoke, Identity};

use expr::ExprBuilder;
use ident::ToIdent;
use item::ItemBuilder;
use pat::PatBuilder;
use ty::TyBuilder;

//////////////////////////////////////////////////////////////////////////////

pub struct StmtBuilder<F=Identity> {
    callback: F,
    span: Span,
    attrs: Vec<ast::Attribute>,
}

impl StmtBuilder {
    pub fn new() -> StmtBuilder {
        StmtBuilder::with_callback(Identity)
    }
}

impl<F> StmtBuilder<F>
    where F: Invoke<ast::Stmt>,
{
    pub fn with_callback(callback: F) -> Self {
        StmtBuilder {
            callback: callback,
            span: DUMMY_SP,
            attrs: vec![],
        }
    }

    pub fn build(self, stmt: ast::Stmt) -> F::Result {
        self.callback.invoke(stmt)
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn build_stmt_kind(self, stmt_: ast::StmtKind) -> F::Result {
        let stmt = ast::Stmt {
            id: ast::DUMMY_NODE_ID,
            node: stmt_,
            span: self.span,
        };
        self.build(stmt)
    }

    pub fn build_let(self,
                     pat: P<ast::Pat>,
                     ty: Option<P<ast::Ty>>,
                     init: Option<P<ast::Expr>>) -> F::Result {
        let local = ast::Local {
            pat: pat,
            ty: ty,
            init: init,
            id: ast::DUMMY_NODE_ID,
            span: self.span,
            attrs: self.attrs.clone().into(),
        };

        self.build_stmt_kind(ast::StmtKind::Local(P(local)))
    }

    pub fn let_(self) -> PatBuilder<Self> {
        PatBuilder::with_callback(self)
    }

    pub fn let_id<I>(self, id: I) -> ExprBuilder<StmtLetIdBuilder<F>>
        where I: ToIdent,
    {
        let span = self.span;
        ExprBuilder::with_callback(StmtLetIdBuilder(self, id.to_ident())).span(span)
    }

    pub fn build_expr(self, expr: P<ast::Expr>) -> F::Result {
        self.build_stmt_kind(ast::StmtKind::Expr(expr))
    }

    pub fn expr(self) -> ExprBuilder<StmtExprBuilder<F>> {
        let span = self.span;
        ExprBuilder::with_callback(StmtExprBuilder(self)).span(span)
    }

    pub fn semi(self) -> ExprBuilder<StmtSemiBuilder<F>> {
        let span = self.span;
        ExprBuilder::with_callback(StmtSemiBuilder(self)).span(span)
    }

    pub fn build_item(self, item: P<ast::Item>) -> F::Result {
        self.build_stmt_kind(ast::StmtKind::Item(item))
    }

    pub fn item(self) -> ItemBuilder<StmtItemBuilder<F>> {
        let span = self.span;
        ItemBuilder::with_callback(StmtItemBuilder(self)).span(span)
    }
}

impl<F> Invoke<P<ast::Pat>> for StmtBuilder<F>
    where F: Invoke<ast::Stmt>,
{
    type Result = StmtLetBuilder<F>;

    fn invoke(self, pat: P<ast::Pat>) -> StmtLetBuilder<F> {
        StmtLetBuilder {
            builder: self,
            pat: pat,
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct StmtLetIdBuilder<F>(StmtBuilder<F>, ast::Ident);

impl<F> Invoke<P<ast::Expr>> for StmtLetIdBuilder<F>
    where F: Invoke<ast::Stmt>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.0.let_().id(self.1).build_expr(expr)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct StmtExprBuilder<F>(StmtBuilder<F>);

impl<F> Invoke<P<ast::Expr>> for StmtExprBuilder<F>
    where F: Invoke<ast::Stmt>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.0.build_expr(expr)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct StmtSemiBuilder<F>(StmtBuilder<F>);

impl<F> Invoke<P<ast::Expr>> for StmtSemiBuilder<F>
    where F: Invoke<ast::Stmt>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.0.build_stmt_kind(ast::StmtKind::Semi(expr))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct StmtLetBuilder<F> {
    builder: StmtBuilder<F>,
    pat: P<ast::Pat>,
}

impl<F> StmtLetBuilder<F>
    where F: Invoke<ast::Stmt>,
{
    fn build_ty(self, ty: P<ast::Ty>) -> StmtLetTyBuilder<F> {
        StmtLetTyBuilder {
            builder: self.builder,
            pat: self.pat,
            ty: ty,
        }
    }

    pub fn ty(self) -> TyBuilder<Self> {
        let span = self.builder.span;
        TyBuilder::with_callback(self).span(span)
    }

    pub fn build_expr(self, expr: P<ast::Expr>) -> F::Result {
        self.builder.build_let(self.pat, None, Some(expr))
    }

    pub fn expr(self) -> ExprBuilder<Self> {
        ExprBuilder::with_callback(self)
    }

    pub fn build(self) -> F::Result {
        self.builder.build_let(self.pat, None, None)
    }
}

impl<F> Invoke<P<ast::Ty>> for StmtLetBuilder<F>
    where F: Invoke<ast::Stmt>,
{
    type Result = StmtLetTyBuilder<F>;

    fn invoke(self, ty: P<ast::Ty>) -> StmtLetTyBuilder<F> {
        self.build_ty(ty)
    }
}

impl<F> Invoke<P<ast::Expr>> for StmtLetBuilder<F>
    where F: Invoke<ast::Stmt>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.build_expr(expr)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct StmtLetTyBuilder<F> {
    builder: StmtBuilder<F>,
    pat: P<ast::Pat>,
    ty: P<ast::Ty>,
}

impl<F> StmtLetTyBuilder<F>
    where F: Invoke<ast::Stmt>,
{
    pub fn expr(self) -> ExprBuilder<Self> {
        ExprBuilder::with_callback(self)
    }

    pub fn build(self) -> F::Result {
        self.builder.build_let(self.pat, Some(self.ty), None)
    }
}

impl<F> Invoke<P<ast::Expr>> for StmtLetTyBuilder<F>
    where F: Invoke<ast::Stmt>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.builder.build_let(self.pat, Some(self.ty), Some(expr))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct StmtItemBuilder<F>(StmtBuilder<F>);

impl<F> Invoke<P<ast::Item>> for StmtItemBuilder<F>
    where F: Invoke<ast::Stmt>,
{
    type Result = F::Result;

    fn invoke(self, item: P<ast::Item>) -> F::Result {
        self.0.build_item(item)
    }
}
