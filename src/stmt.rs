use syntax::ast;
use syntax::codemap::{DUMMY_SP, Span, respan};
use syntax::ptr::P;

use invoke::{Invoke, Identity};

use ctx::Ctx;
use expr::ExprBuilder;
use ident::ToIdent;
use item::ItemBuilder;
use pat::PatBuilder;
use ty::TyBuilder;

//////////////////////////////////////////////////////////////////////////////

pub struct StmtBuilder<'a, F=Identity> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
}

impl<'a> StmtBuilder<'a> {
    pub fn new(ctx: &Ctx) -> StmtBuilder {
        StmtBuilder::new_with_callback(ctx, Identity)
    }
}

impl<'a, F> StmtBuilder<'a, F>
    where F: Invoke<P<ast::Stmt>>,
{
    pub fn new_with_callback(ctx: &'a Ctx, callback: F) -> Self {
        StmtBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn build_stmt(self, stmt_: ast::Stmt_) -> F::Result {
        self.callback.invoke(P(respan(self.span, stmt_)))
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
            source: ast::LocalSource::LocalLet,
        };

        let decl = respan(self.span, ast::Decl_::DeclLocal(P(local)));

        self.build_stmt(ast::StmtDecl(P(decl), ast::DUMMY_NODE_ID))
    }

    pub fn let_(self) -> PatBuilder<'a, Self> {
        PatBuilder::new_with_callback(self.ctx, self)
    }

    pub fn let_id<I>(self, id: I) -> ExprBuilder<'a, StmtLetIdBuilder<'a, F>>
        where I: ToIdent,
    {
        let id = id.to_ident(self.ctx);
        ExprBuilder::new_with_callback(self.ctx, StmtLetIdBuilder(self, id))
    }

    pub fn build_expr(self, expr: P<ast::Expr>) -> F::Result {
        self.build_stmt(ast::Stmt_::StmtExpr(expr, ast::DUMMY_NODE_ID))
    }

    pub fn expr(self) -> ExprBuilder<'a, StmtExprBuilder<'a, F>> {
        ExprBuilder::new_with_callback(self.ctx, StmtExprBuilder(self))
    }

    pub fn semi(self) -> ExprBuilder<'a, StmtSemiBuilder<'a, F>> {
        ExprBuilder::new_with_callback(self.ctx, StmtSemiBuilder(self))
    }

    pub fn build_item(self, item: P<ast::Item>) -> F::Result {
        let decl = respan(self.span, ast::Decl_::DeclItem(item));
        self.build_stmt(ast::StmtDecl(P(decl), ast::DUMMY_NODE_ID))
    }

    pub fn item(self) -> ItemBuilder<'a, StmtItemBuilder<'a, F>> {
        ItemBuilder::new_with_callback(self.ctx, StmtItemBuilder(self))
    }
}

impl<'a, F> Invoke<P<ast::Pat>> for StmtBuilder<'a, F>
    where F: Invoke<P<ast::Stmt>>,
{
    type Result = StmtLetBuilder<'a, F>;

    fn invoke(self, pat: P<ast::Pat>) -> StmtLetBuilder<'a, F> {
        StmtLetBuilder {
            builder: self,
            pat: pat,
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct StmtLetIdBuilder<'a, F>(StmtBuilder<'a, F>, ast::Ident);

impl<'a, F> Invoke<P<ast::Expr>> for StmtLetIdBuilder<'a, F>
    where F: Invoke<P<ast::Stmt>>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.0.let_().id(self.1).build_expr(expr)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct StmtExprBuilder<'a, F>(StmtBuilder<'a, F>);

impl<'a, F> Invoke<P<ast::Expr>> for StmtExprBuilder<'a, F>
    where F: Invoke<P<ast::Stmt>>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.0.build_expr(expr)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct StmtSemiBuilder<'a, F>(StmtBuilder<'a, F>);

impl<'a, F> Invoke<P<ast::Expr>> for StmtSemiBuilder<'a, F>
    where F: Invoke<P<ast::Stmt>>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.0.build_stmt(ast::Stmt_::StmtSemi(expr, ast::DUMMY_NODE_ID))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct StmtLetBuilder<'a, F> {
    builder: StmtBuilder<'a, F>,
    pat: P<ast::Pat>,
}

impl<'a, F> StmtLetBuilder<'a, F>
    where F: Invoke<P<ast::Stmt>>,
{
    fn build_ty(self, ty: P<ast::Ty>) -> StmtLetTyBuilder<'a, F> {
        StmtLetTyBuilder {
            builder: self.builder,
            pat: self.pat,
            ty: ty,
        }
    }

    pub fn ty(self) -> TyBuilder<'a, Self> {
        TyBuilder::new_with_callback(self.builder.ctx, self)
    }

    pub fn build_expr(self, expr: P<ast::Expr>) -> F::Result {
        self.builder.build_let(self.pat, None, Some(expr))
    }

    pub fn expr(self) -> ExprBuilder<'a, Self> {
        ExprBuilder::new_with_callback(self.builder.ctx, self)
    }

    pub fn build(self) -> F::Result {
        self.builder.build_let(self.pat, None, None)
    }
}

impl<'a, F> Invoke<P<ast::Ty>> for StmtLetBuilder<'a, F>
    where F: Invoke<P<ast::Stmt>>,
{
    type Result = StmtLetTyBuilder<'a, F>;

    fn invoke(self, ty: P<ast::Ty>) -> StmtLetTyBuilder<'a, F> {
        self.build_ty(ty)
    }
}

impl<'a, F> Invoke<P<ast::Expr>> for StmtLetBuilder<'a, F>
    where F: Invoke<P<ast::Stmt>>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.build_expr(expr)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct StmtLetTyBuilder<'a, F> {
    builder: StmtBuilder<'a, F>,
    pat: P<ast::Pat>,
    ty: P<ast::Ty>,
}

impl<'a, F> StmtLetTyBuilder<'a, F>
    where F: Invoke<P<ast::Stmt>>,
{
    pub fn expr(self) -> ExprBuilder<'a, Self> {
        ExprBuilder::new_with_callback(self.builder.ctx, self)
    }

    pub fn build(self) -> F::Result {
        self.builder.build_let(self.pat, Some(self.ty), None)
    }
}

impl<'a, F> Invoke<P<ast::Expr>> for StmtLetTyBuilder<'a, F>
    where F: Invoke<P<ast::Stmt>>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.builder.build_let(self.pat, Some(self.ty), Some(expr))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct StmtItemBuilder<'a, F>(StmtBuilder<'a, F>);

impl<'a, F> Invoke<P<ast::Item>> for StmtItemBuilder<'a, F>
    where F: Invoke<P<ast::Stmt>>,
{
    type Result = F::Result;

    fn invoke(self, item: P<ast::Item>) -> F::Result {
        self.0.build_item(item)
    }
}

