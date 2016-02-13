use std::iter::IntoIterator;

use syntax::ast;
use syntax::codemap::{DUMMY_SP, Span, Spanned, respan};
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
        PatBuilder::with_callback(Identity)
    }
}


impl<F> PatBuilder<F>
    where F: Invoke<P<ast::Pat>>,
{
    pub fn with_callback(callback: F) -> Self {
        PatBuilder {
            callback: callback,
            span: DUMMY_SP,
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn build(self, pat: P<ast::Pat>) -> F::Result {
        self.callback.invoke(pat)
    }

    pub fn build_pat_(self, pat_: ast::Pat_) -> F::Result {
        let span = self.span;
        self.build(P(ast::Pat {
            id: ast::DUMMY_NODE_ID,
            node: pat_,
            span: span,
        }))
    }

    pub fn wild(self) -> F::Result {
        self.build_pat_(ast::Pat_::PatWild)
    }

    pub fn build_id<I>(self, mode: ast::BindingMode, id: I, sub: Option<P<ast::Pat>>) -> F::Result
        where I: ToIdent,
    {
        let id = respan(self.span, id.to_ident());

        self.build_pat_(ast::Pat_::PatIdent(mode, id, sub))
    }

    pub fn id<I>(self, id: I) -> F::Result
        where I: ToIdent
    {
        let mode = ast::BindingMode::ByValue(ast::Mutability::Immutable);
        self.build_id(mode, id, None)
    }

    pub fn mut_id<I>(self, id: I) -> F::Result
        where I: ToIdent
    {
        let mode = ast::BindingMode::ByValue(ast::Mutability::Mutable);
        self.build_id(mode, id, None)
    }

    pub fn ref_id<I>(self, id: I) -> F::Result
        where I: ToIdent
    {
        let mode = ast::BindingMode::ByRef(ast::Mutability::Immutable);
        self.build_id(mode, id, None)
    }

    pub fn ref_mut_id<I>(self, id: I) -> F::Result
        where I: ToIdent
    {
        let mode = ast::BindingMode::ByRef(ast::Mutability::Mutable);
        self.build_id(mode, id, None)
    }

    pub fn enum_(self) -> PathBuilder<PatEnumBuilder<F>> {
        PathBuilder::with_callback(PatEnumBuilder(self))
    }

    pub fn struct_(self) -> PathBuilder<PatStructBuilder<F>> {
        PathBuilder::with_callback(PatStructBuilder(self))
    }

    pub fn expr(self) -> ExprBuilder<PatExprBuilder<F>> {
        ExprBuilder::with_callback(PatExprBuilder(self))
    }

    pub fn tuple(self) -> PatTupleBuilder<F> {
        PatTupleBuilder {
            builder: self,
            pats: Vec::new(),
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct PatEnumBuilder<F>(PatBuilder<F>);

impl<F> Invoke<ast::Path> for PatEnumBuilder<F> {
    type Result = PatEnumPathBuilder<F>;

    fn invoke(self, path: ast::Path) -> PatEnumPathBuilder<F> {
        PatEnumPathBuilder {
            builder: self.0,
            path: path,
            pats: Vec::new(),
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct PatEnumPathBuilder<F> {
    builder: PatBuilder<F>,
    path: ast::Path,
    pats: Vec<P<ast::Pat>>,
}

impl<F> PatEnumPathBuilder<F>
    where F: Invoke<P<ast::Pat>>,
{
    pub fn with_pats<I>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=P<ast::Pat>>,
    {
        self.pats.extend(iter);
        self
    }

    pub fn pat(self) -> PatBuilder<Self> {
        PatBuilder::with_callback(self)
    }

    pub fn with_ids<I, T>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=T>,
              T: ToIdent,
    {
        for id in iter {
            self = self.id(id);
        }
        self
    }

    pub fn id<I>(self, id: I) -> Self
        where I: ToIdent
    {
        self.pat().id(id)
    }

    pub fn build(self) -> F::Result {
        self.builder.build_pat_(ast::Pat_::PatEnum(self.path, Some(self.pats)))
    }
}

impl<F> Invoke<P<ast::Pat>> for PatEnumPathBuilder<F>
    where F: Invoke<P<ast::Pat>>,
{
    type Result = Self;

    fn invoke(mut self, pat: P<ast::Pat>) -> Self {
        self.pats.push(pat);
        self
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct PatStructBuilder<F>(PatBuilder<F>);

impl<F> Invoke<ast::Path> for PatStructBuilder<F> {
    type Result = PatStructPathBuilder<F>;

    fn invoke(self, path: ast::Path) -> PatStructPathBuilder<F> {
        PatStructPathBuilder {
            builder: self.0,
            path: path,
            pats: Vec::new(),
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct PatStructPathBuilder<F> {
    builder: PatBuilder<F>,
    path: ast::Path,
    pats: Vec<Spanned<ast::FieldPat>>,
}

impl<F> PatStructPathBuilder<F>
    where F: Invoke<P<ast::Pat>>,
{
    pub fn with_field_pat(mut self, pat: ast::FieldPat) -> Self {
        self.pats.push(respan(self.builder.span, pat));
        self
    }

    pub fn with_pats<I, T>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=(T, P<ast::Pat>)>,
              T: ToIdent,
    {
        for (id, pat) in iter {
            self = self.pat(id).build(pat);
        }
        self
    }

    pub fn pat<I>(self, id: I) -> PatBuilder<PatStructFieldBuilder<F>>
        where I: ToIdent,
    {
        PatBuilder::with_callback(PatStructFieldBuilder {
            builder: self,
            id: id.to_ident(),
        })
    }

    pub fn with_ids<I, T>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=T>,
              T: ToIdent,
    {
        for id in iter {
            self = self.id(id);
        }
        self
    }

    pub fn mut_id<I>(self, id: I) -> Self
        where I: ToIdent,
    {
        let id = id.to_ident();
        let span = self.builder.span;
        let pat = PatBuilder::new().span(span).mut_id(id);

        self.with_field_pat(ast::FieldPat {
            ident: id,
            pat: pat,
            is_shorthand: true,
        })
    }

    pub fn id<I>(self, id: I) -> Self
        where I: ToIdent,
    {
        let id = id.to_ident();
        let span = self.builder.span;
        let pat = PatBuilder::new().span(span).id(id);

        self.with_field_pat(ast::FieldPat {
            ident: id,
            pat: pat,
            is_shorthand: true,
        })
    }

    pub fn etc(self) -> F::Result {
        self.builder.build_pat_(ast::Pat_::PatStruct(self.path, self.pats, true))
    }

    pub fn build(self) -> F::Result {
        self.builder.build_pat_(ast::Pat_::PatStruct(self.path, self.pats, false))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct PatStructFieldBuilder<F> {
    builder: PatStructPathBuilder<F>,
    id: ast::Ident,
}

impl<F> Invoke<P<ast::Pat>> for PatStructFieldBuilder<F>
    where F: Invoke<P<ast::Pat>>,
{
    type Result = PatStructPathBuilder<F>;

    fn invoke(self, pat: P<ast::Pat>) -> PatStructPathBuilder<F> {
        self.builder.with_field_pat(ast::FieldPat {
            ident: self.id,
            pat: pat,
            is_shorthand: false,
        })
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct PatExprBuilder<F>(PatBuilder<F>);

impl<F> Invoke<P<ast::Expr>> for PatExprBuilder<F>
    where F: Invoke<P<ast::Pat>>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.0.build_pat_(ast::Pat_::PatLit(expr))
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
    pub fn with_pats<I>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=P<ast::Pat>>,
    {
        self.pats.extend(iter);
        self
    }

    pub fn with_pat(mut self, pat: P<ast::Pat>) -> Self {
        self.pats.push(pat);
        self
    }

    pub fn pat(self) -> PatBuilder<Self> {
        PatBuilder::with_callback(self)
    }

    pub fn build(self) -> F::Result {
        self.builder.build_pat_(ast::PatTup(self.pats))
    }
}

impl<F> Invoke<P<ast::Pat>> for PatTupleBuilder<F>
    where F: Invoke<P<ast::Pat>>
{
    type Result = PatTupleBuilder<F>;

    fn invoke(self, pat: P<ast::Pat>) -> Self {
        self.with_pat(pat)
    }
}
