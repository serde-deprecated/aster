#![feature(rustc_private)]

extern crate syntax;

use std::iter::IntoIterator;

use syntax::abi::Abi;
use syntax::ast;
use syntax::attr;
use syntax::codemap::{DUMMY_SP, Span, Spanned, respan};
use syntax::owned_slice::OwnedSlice;
use syntax::parse::token;
use syntax::ptr::P;

//////////////////////////////////////////////////////////////////////////////

#[derive(Copy)]
pub struct Ctx;

impl Ctx {
    pub fn new() -> Ctx {
        Ctx
    }

    pub fn intern(&self, name: &str) -> ast::Name {
        token::intern(name)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub trait ToName {
    fn to_name(&self, ctx: &Ctx) -> ast::Name;
}

impl ToName for ast::Name {
    fn to_name(&self, _ctx: &Ctx) -> ast::Name {
        *self
    }
}

impl<'a> ToName for &'a str {
    fn to_name(&self, ctx: &Ctx) -> ast::Name {
        ctx.intern(*self)
    }
}

impl<'a, T> ToName for &'a T where T: ToName {
    fn to_name(&self, ctx: &Ctx) -> ast::Name {
        (**self).to_name(ctx)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub trait ToIdent {
    fn to_ident(&self, ctx: &Ctx) -> ast::Ident;
}

impl ToIdent for ast::Ident {
    fn to_ident(&self, _ctx: &Ctx) -> ast::Ident {
        *self
    }
}

impl<'a> ToIdent for &'a str {
    fn to_ident(&self, ctx: &Ctx) -> ast::Ident {
        ast::Ident::new(ctx.intern(*self))
    }
}

impl ToIdent for String {
    fn to_ident(&self, ctx: &Ctx) -> ast::Ident {
        (&**self).to_ident(ctx)
    }
}

impl<'a, T> ToIdent for &'a T where T: ToIdent {
    fn to_ident(&self, ctx: &Ctx) -> ast::Ident {
        (**self).to_ident(ctx)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub trait IntoPath {
    fn into_path(self, ctx: &Ctx) -> ast::Path;
}

impl IntoPath for ast::Path {
    fn into_path(self, _ctx: &Ctx) -> ast::Path {
        self
    }
}

impl<'a> IntoPath for ast::Ident {
    fn into_path(self, ctx: &Ctx) -> ast::Path {
        PathBuilder::new(ctx).id(self).build()
    }
}

impl<'a> IntoPath for &'a str {
    fn into_path(self, ctx: &Ctx) -> ast::Path {
        PathBuilder::new(ctx).id(self).build()
    }
}

impl IntoPath for String {
    fn into_path(self, ctx: &Ctx) -> ast::Path {
        (&*self).into_path(ctx)
    }
}

impl<'a, I, T> IntoPath for I where I: IntoIterator<Item=T>, T: ToIdent {
    fn into_path(self, ctx: &Ctx) -> ast::Path {
        PathBuilder::new(ctx).ids(self).build()
    }
}

//////////////////////////////////////////////////////////////////////////////

pub trait ToInternedString {
    fn into_interned_string(&self) -> token::InternedString;
}

impl ToInternedString for token::InternedString {
    fn into_interned_string(&self) -> token::InternedString {
        self.clone()
    }
}

impl<'a> ToInternedString for &'a str {
    fn into_interned_string(&self) -> token::InternedString {
        token::intern_and_get_ident(self)
    }
}

impl ToInternedString for ast::Ident {
    fn into_interned_string(&self) -> token::InternedString {
        token::get_ident(*self)
    }
}

impl ToInternedString for ast::Name {
    fn into_interned_string(&self) -> token::InternedString {
        token::get_name(*self)
    }
}

impl<'a, T> ToInternedString for &'a T where T: ToInternedString {
    fn into_interned_string(&self) -> token::InternedString {
        (**self).into_interned_string()
    }
}

//////////////////////////////////////////////////////////////////////////////

pub trait IntoLifetime {
    fn into_lifetime(self, ctx: &Ctx) -> ast::Lifetime;
}

impl IntoLifetime for ast::Lifetime {
    fn into_lifetime(self, _ctx: &Ctx) -> ast::Lifetime {
        self
    }
}

impl<'a> IntoLifetime for &'a str {
    fn into_lifetime(self, ctx: &Ctx) -> ast::Lifetime {
        ast::Lifetime {
            id: ast::DUMMY_NODE_ID,
            span: DUMMY_SP,
            name: self.to_name(ctx),
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

pub trait IntoLifetimeDef {
    fn into_lifetime_def(self, ctx: &Ctx) -> ast::LifetimeDef;
}

impl IntoLifetimeDef for ast::LifetimeDef {
    fn into_lifetime_def(self, _ctx: &Ctx) -> ast::LifetimeDef {
        self
    }
}

impl IntoLifetimeDef for ast::Lifetime {
    fn into_lifetime_def(self, _ctx: &Ctx) -> ast::LifetimeDef {
        ast::LifetimeDef {
            lifetime: self,
            bounds: vec![],
        }
    }
}

impl<'a> IntoLifetimeDef for &'a str {
    fn into_lifetime_def(self, ctx: &Ctx) -> ast::LifetimeDef {
        self.into_lifetime(ctx).into_lifetime_def(ctx)
    }
}

//////////////////////////////////////////////////////////////////////////////

trait Invoke<A> {
    type Result;

    fn invoke(self, arg: A) -> Self::Result;
}

//////////////////////////////////////////////////////////////////////////////

#[derive(Copy)]
pub struct Identity;

impl<A> Invoke<A> for Identity {
    type Result = A;

    fn invoke(self, arg: A) -> A { arg }
}

//////////////////////////////////////////////////////////////////////////////

pub struct PathBuilder<'a, F=Identity> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
    global: bool,
}

impl<'a> PathBuilder<'a> {
    pub fn new(ctx: &'a Ctx) -> Self {
        PathBuilder::new_with_callback(ctx, Identity)
    }
}

impl<'a, F> PathBuilder<'a, F>
    where F: Invoke<ast::Path>,
{
    pub fn new_with_callback(ctx: &'a Ctx, callback: F) -> Self {
        PathBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
            global: false,
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn global(mut self) -> Self {
        self.global = true;
        self
    }

    pub fn ids<I, T>(self, ids: I) -> PathSegmentsBuilder<'a, F>
        where I: IntoIterator<Item=T>,
              T: ToIdent,
    {
        let mut ids = ids.into_iter();
        let id = ids.next().expect("passed path with no id");

        self.id(id).ids(ids)
    }

    pub fn id<I>(self, id: I) -> PathSegmentsBuilder<'a, F>
        where I: ToIdent,
    {
        self.segment(id).build()
    }

    pub fn segment<I>(self, id: I)
        -> PathSegmentBuilder<'a, PathSegmentsBuilder<'a, F>>
        where I: ToIdent,
    {
        PathSegmentBuilder::new_with_callback(self.ctx, id, PathSegmentsBuilder {
            ctx: self.ctx,
            callback: self.callback,
            span: self.span,
            global: self.global,
            segments: Vec::new(),
        })
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct PathSegmentsBuilder<'a, F=Identity> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
    global: bool,
    segments: Vec<ast::PathSegment>,
}

impl<'a, F> PathSegmentsBuilder<'a, F>
    where F: Invoke<ast::Path>,
{
    pub fn ids<I, T>(mut self, ids: I) -> PathSegmentsBuilder<'a, F>
        where I: IntoIterator<Item=T>,
              T: ToIdent,
    {
        for id in ids {
            self = self.id(id);
        }

        self 
    }

    pub fn id<T>(self, id: T) -> PathSegmentsBuilder<'a, F>
        where T: ToIdent,
    {
        self.segment(id).build()
    }

    pub fn segment<T>(self, id: T) -> PathSegmentBuilder<'a, Self>
        where T: ToIdent,
    {
        PathSegmentBuilder::new_with_callback(self.ctx, id, self)
    }

    pub fn build(self) -> F::Result {
        self.callback.invoke(ast::Path {
            span: self.span,
            global: self.global,
            segments: self.segments,
        })
    }
}

impl<'a, F> Invoke<ast::PathSegment> for PathSegmentsBuilder<'a, F> {
    type Result = Self;

    fn invoke(mut self, segment: ast::PathSegment) -> Self {
        self.segments.push(segment);
        self
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct PathSegmentBuilder<'a, F=Identity> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
    id: ast::Ident,
    lifetimes: Vec<ast::Lifetime>,
    tys: Vec<P<ast::Ty>>,
    bindings: Vec<P<ast::TypeBinding>>,
}

impl<'a, F> PathSegmentBuilder<'a, F>
    where F: Invoke<ast::PathSegment>,
{
    pub fn new_with_callback<I>(ctx: &'a Ctx, id: I, callback: F) -> Self
        where I: ToIdent,
    {
        PathSegmentBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
            id: id.to_ident(ctx),
            lifetimes: Vec::new(),
            tys: Vec::new(),
            bindings: Vec::new(),
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn with_generics(self, generics: ast::Generics) -> Self {
        let ctx = self.ctx;

        // Strip off the bounds.
        let lifetimes = generics.lifetimes.iter()
            .map(|lifetime_def| lifetime_def.lifetime);

        let tys = generics.ty_params.iter()
            .map(|ty_param| TyBuilder::new(ctx).id(ty_param.ident));

        self.with_lifetimes(lifetimes)
            .with_tys(tys)
    }


    pub fn with_lifetimes<I, L>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=L>,
              L: IntoLifetime,
    {
        let ctx = self.ctx;
        let iter = iter.into_iter().map(|lifetime| lifetime.into_lifetime(ctx));
        self.lifetimes.extend(iter);
        self
    }

    pub fn with_lifetime<L>(mut self, lifetime: L) -> Self
        where L: IntoLifetime,
    {
        self.lifetimes.push(lifetime.into_lifetime(self.ctx));
        self
    }

    pub fn lifetime<N>(self, name: N) -> Self
        where N: ToName,
    {
        let lifetime = ast::Lifetime {
            id: ast::DUMMY_NODE_ID,
            span: self.span,
            name: name.to_name(self.ctx.clone()),
        };
        self.with_lifetime(lifetime)
    }

    pub fn with_tys<I>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=P<ast::Ty>>,
    {
        self.tys.extend(iter);
        self
    }

    pub fn with_ty(mut self, ty: P<ast::Ty>) -> Self {
        self.tys.push(ty);
        self
    }

    pub fn ty(self) -> TyBuilder<'a, Self> {
        TyBuilder::new_with_callback(self.ctx, self)
    }

    pub fn build(self) -> F::Result {
        let data = ast::AngleBracketedParameterData {
            lifetimes: self.lifetimes,
            types: OwnedSlice::from_vec(self.tys),
            bindings: OwnedSlice::from_vec(self.bindings),
        };

        let parameters = ast::PathParameters::AngleBracketedParameters(data);

        self.callback.invoke(ast::PathSegment {
            identifier: self.id,
            parameters: parameters,
        })
    }
}

impl<'a, F> Invoke<P<ast::Ty>> for PathSegmentBuilder<'a, F>
    where F: Invoke<ast::PathSegment>
{
    type Result = Self;

    fn invoke(self, ty: P<ast::Ty>) -> Self {
        self.with_ty(ty)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct TyBuilder<'a, F=Identity> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
}

impl<'a> TyBuilder<'a> {
    pub fn new(ctx: &'a Ctx) -> Self {
        TyBuilder::new_with_callback(ctx, Identity)
    }
}

impl<'a, F> TyBuilder<'a, F>
    where F: Invoke<P<ast::Ty>>,
{
    pub fn new_with_callback(ctx: &'a Ctx, callback: F) -> Self {
        TyBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn build_ty(self, ty: P<ast::Ty>) -> F::Result {
        self.callback.invoke(ty)
    }


    pub fn build_ty_(self, ty_: ast::Ty_) -> F::Result {
        let span = self.span;
        self.build_ty(P(ast::Ty {
            id: ast::DUMMY_NODE_ID,
            node: ty_,
            span: span,
        }))
    }

    pub fn id<I>(self, id: I) -> F::Result
        where I: ToIdent,
    {
        self.path().id(id).build()
    }

    pub fn build_path(self, path: ast::Path) -> F::Result {
        self.build_ty_(ast::Ty_::TyPath(None, path))
    }

    pub fn build_qpath(self, qself: ast::QSelf, path: ast::Path) -> F::Result {
        self.build_ty_(ast::Ty_::TyPath(Some(qself), path))
    }

    pub fn path(self) -> PathBuilder<'a, TyPathBuilder<'a, F>> {
        PathBuilder::new_with_callback(self.ctx, TyPathBuilder(self))
    }

    pub fn isize(self) -> F::Result {
        self.id("isize")
    }

    pub fn i8(self) -> F::Result {
        self.id("i8")
    }

    pub fn i16(self) -> F::Result {
        self.id("i16")
    }

    pub fn i32(self) -> F::Result {
        self.id("i32")
    }

    pub fn i64(self) -> F::Result {
        self.id("i64")
    }

    pub fn usize(self) -> F::Result {
        self.id("usize")
    }

    pub fn u8(self) -> F::Result {
        self.id("u8")
    }

    pub fn u16(self) -> F::Result {
        self.id("u16")
    }

    pub fn u32(self) -> F::Result {
        self.id("u32")
    }

    pub fn u64(self) -> F::Result {
        self.id("u64")
    }

    pub fn option(self) -> TyBuilder<'a, TyOptionBuilder<'a, F>> {
        TyBuilder::new_with_callback(self.ctx, TyOptionBuilder(self))
    }

    pub fn result(self) -> TyBuilder<'a, TyResultOkBuilder<'a, F>> {
        TyBuilder::new_with_callback(self.ctx, TyResultOkBuilder(self))
    }

    pub fn phantom_data(self) -> TyBuilder<'a, TyPhantomDataBuilder<'a, F>> {
        TyBuilder::new_with_callback(self.ctx, TyPhantomDataBuilder(self))
    }

    pub fn unit(self) -> F::Result {
        self.tuple().build()
    }

    pub fn tuple(self) -> TyTupleBuilder<'a, F> {
        TyTupleBuilder {
            builder: self,
            tys: vec![],
        }
    }

    pub fn ref_(self) -> TyRefBuilder<'a, F> {
        TyRefBuilder {
            builder: self,
            lifetime: None,
            mutability: ast::MutImmutable,
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct TyPathBuilder<'a, F>(TyBuilder<'a, F>);

impl<'a, F> Invoke<ast::Path> for TyPathBuilder<'a, F>
    where F: Invoke<P<ast::Ty>>,
{
    type Result = F::Result;

    fn invoke(self, path: ast::Path) -> F::Result {
        self.0.build_path(path)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct TyRefBuilder<'a, F> {
    builder: TyBuilder<'a, F>,
    lifetime: Option<ast::Lifetime>,
    mutability: ast::Mutability,
}

impl<'a, F> TyRefBuilder<'a, F>
    where F: Invoke<P<ast::Ty>>,
{
    pub fn mut_(mut self) -> Self {
        self.mutability = ast::MutMutable;
        self
    }

    pub fn lifetime<N>(mut self, name: N) -> Self
        where N: ToName,
    {
        self.lifetime = Some(ast::Lifetime {
            id: ast::DUMMY_NODE_ID,
            span: self.builder.span,
            name: name.to_name(self.builder.ctx),
        });
        self
    }

    pub fn build_ty(self, ty: P<ast::Ty>) -> F::Result {
        let ty = ast::MutTy {
            ty: ty,
            mutbl: self.mutability,
        };
        self.builder.build_ty_(ast::TyRptr(self.lifetime, ty))
    }

    pub fn ty(self) -> TyBuilder<'a, Self> {
        TyBuilder::new_with_callback(self.builder.ctx, self)
    }
}

impl<'a, F> Invoke<P<ast::Ty>> for TyRefBuilder<'a, F>
    where F: Invoke<P<ast::Ty>>,
{
    type Result = F::Result;

    fn invoke(self, ty: P<ast::Ty>) -> F::Result {
        self.build_ty(ty)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct TyOptionBuilder<'a, F>(TyBuilder<'a, F>);

impl<'a, F> Invoke<P<ast::Ty>> for TyOptionBuilder<'a, F>
    where F: Invoke<P<ast::Ty>>,
{
    type Result = F::Result;

    fn invoke(self, ty: P<ast::Ty>) -> F::Result {
        let path = PathBuilder::new(self.0.ctx)
            .global()
            .id("std")
            .id("option")
            .segment("Option")
                .with_ty(ty)
                .build()
            .build();

        self.0.build_path(path)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct TyResultOkBuilder<'a, F>(TyBuilder<'a, F>);

impl<'a, F> Invoke<P<ast::Ty>> for TyResultOkBuilder<'a, F>
    where F: Invoke<P<ast::Ty>>,
{
    type Result = TyBuilder<'a, TyResultErrBuilder<'a, F>>;

    fn invoke(self, ty: P<ast::Ty>) -> TyBuilder<'a, TyResultErrBuilder<'a, F>> {
        TyBuilder::new_with_callback(self.0.ctx, TyResultErrBuilder(self.0, ty))
    }
}

pub struct TyResultErrBuilder<'a, F>(TyBuilder<'a, F>, P<ast::Ty>);

impl<'a, F> Invoke<P<ast::Ty>> for TyResultErrBuilder<'a, F>
    where F: Invoke<P<ast::Ty>>,
{
    type Result = F::Result;

    fn invoke(self, ty: P<ast::Ty>) -> F::Result {
        let path = PathBuilder::new(self.0.ctx)
            .global()
            .id("std")
            .id("result")
            .segment("Result")
                .with_ty(self.1)
                .with_ty(ty)
                .build()
            .build();

        self.0.build_path(path)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct TyPhantomDataBuilder<'a, F>(TyBuilder<'a, F>);

impl<'a, F> Invoke<P<ast::Ty>> for TyPhantomDataBuilder<'a, F>
    where F: Invoke<P<ast::Ty>>,
{
    type Result = F::Result;

    fn invoke(self, ty: P<ast::Ty>) -> F::Result {
        let path = PathBuilder::new(self.0.ctx)
            .global()
            .id("std")
            .id("marker")
            .segment("PhantomData")
                .with_ty(ty)
                .build()
            .build();

        self.0.build_path(path)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct TyTupleBuilder<'a, F> {
    builder: TyBuilder<'a, F>,
    tys: Vec<P<ast::Ty>>,
}

impl<'a, F> TyTupleBuilder<'a, F>
    where F: Invoke<P<ast::Ty>>,
{
    pub fn with_tys<I>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=P<ast::Ty>>,
    {
        self.tys.extend(iter);
        self
    }

    pub fn with_ty(mut self, ty: P<ast::Ty>) -> Self {
        self.tys.push(ty);
        self
    }

    pub fn ty(self) -> TyBuilder<'a, Self> {
        TyBuilder::new_with_callback(self.builder.ctx, self)
    }

    pub fn build(self) -> F::Result {
        self.builder.build_ty_(ast::TyTup(self.tys))
    }
}

impl<'a, F> Invoke<P<ast::Ty>> for TyTupleBuilder<'a, F>
    where F: Invoke<P<ast::Ty>>,
{
    type Result = Self;

    fn invoke(self, ty: P<ast::Ty>) -> Self {
        self.with_ty(ty)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct LitBuilder<'a, F=Identity> {
    _ctx: &'a Ctx,
    callback: F,
    span: Span,
}

impl<'a> LitBuilder<'a> {
    pub fn builder(ctx: &Ctx) -> LitBuilder {
        LitBuilder::new_with_callback(ctx, Identity)
    }
}

impl<'a, F> LitBuilder<'a, F>
    where F: Invoke<P<ast::Lit>>,
{
    pub fn new_with_callback(ctx: &'a Ctx, callback: F) -> Self {
        LitBuilder {
            _ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
        }
    }

    pub fn span(mut self, span: Span) -> LitBuilder<'a, F> {
        self.span = span;
        self
    }

    pub fn build_lit(self, lit: ast::Lit_) -> F::Result {
        self.callback.invoke(P(ast::Lit {
            span: self.span,
            node: lit,
        }))
    }

    pub fn bool(self, value: bool) -> F::Result {
        self.build_lit(ast::LitBool(value))
    }

    pub fn int(self, value: i64, ty: ast::IntTy) -> F::Result {
        let sign = ast::Sign::new(value);
        self.build_lit(ast::LitInt(value as u64, ast::LitIntType::SignedIntLit(ty, sign)))
    }

    pub fn isize(self, value: isize) -> F::Result {
        self.int(value as i64, ast::IntTy::TyIs(false))
    }

    pub fn i8(self, value: i8) -> F::Result {
        self.int(value as i64, ast::IntTy::TyI8)
    }

    pub fn i16(self, value: i16) -> F::Result {
        self.int(value as i64, ast::IntTy::TyI16)
    }

    pub fn i32(self, value: i32) -> F::Result {
        self.int(value as i64, ast::IntTy::TyI32)
    }

    pub fn i64(self, value: i64) -> F::Result {
        self.int(value, ast::IntTy::TyI64)
    }

    pub fn uint(self, value: u64, ty: ast::UintTy) -> F::Result {
        self.build_lit(ast::LitInt(value, ast::LitIntType::UnsignedIntLit(ty)))
    }

    pub fn usize(self, value: usize) -> F::Result {
        self.uint(value as u64, ast::UintTy::TyUs(false))
    }

    pub fn u8(self, value: u8) -> F::Result {
        self.uint(value as u64, ast::UintTy::TyU8)
    }

    pub fn u16(self, value: u16) -> F::Result {
        self.uint(value as u64, ast::UintTy::TyU16)
    }

    pub fn u32(self, value: u32) -> F::Result {
        self.uint(value as u64, ast::UintTy::TyU32)
    }

    pub fn u64(self, value: u64) -> F::Result {
        self.uint(value, ast::UintTy::TyU64)
    }

    pub fn str<S>(self, value: S) -> F::Result
        where S: ToInternedString,
    {
        let value = value.into_interned_string();
        self.build_lit(ast::LitStr(value, ast::CookedStr))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprBuilder<'a, F=Identity> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
}

impl<'a> ExprBuilder<'a> {
    pub fn new(ctx: &'a Ctx) -> Self {
        ExprBuilder::new_with_callback(ctx, Identity)
    }
}

impl<'a, F> ExprBuilder<'a, F>
    where F: Invoke<P<ast::Expr>>,
{
    pub fn new_with_callback(ctx: &'a Ctx, callback: F) -> Self {
        ExprBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn build_expr(self, expr: P<ast::Expr>) -> F::Result {
        self.callback.invoke(expr)
    }

    pub fn build_expr_(self, expr: ast::Expr_) -> F::Result {
        let span = self.span;
        self.build_expr(P(ast::Expr {
            id: ast::DUMMY_NODE_ID,
            node: expr,
            span: span,
        }))
    }

    pub fn build_path(self, path: ast::Path) -> F::Result {
        self.build_expr_(ast::Expr_::ExprPath(None, path))
    }

    pub fn build_qpath(self, qself: ast::QSelf, path: ast::Path) -> F::Result {
        self.build_expr_(ast::Expr_::ExprPath(Some(qself), path))
    }

    pub fn path(self) -> PathBuilder<'a, Self> {
        PathBuilder::new_with_callback(self.ctx, self)
    }

    pub fn id<I>(self, id: I) -> F::Result
        where I: ToIdent
    {
        self.path().id(id).build()
    }

    pub fn build_lit(self, lit: P<ast::Lit>) -> F::Result {
        self.build_expr_(ast::Expr_::ExprLit(lit))
    }

    pub fn lit(self) -> LitBuilder<'a, Self> {
        LitBuilder::new_with_callback(self.ctx, self)
    }

    pub fn bool(self, value: bool) -> F::Result {
        self.lit().bool(value)
    }

    pub fn isize(self, value: isize) -> F::Result {
        self.lit().isize(value)
    }

    pub fn i8(self, value: i8) -> F::Result {
        self.lit().i8(value)
    }

    pub fn i16(self, value: i16) -> F::Result {
        self.lit().i16(value)
    }

    pub fn i32(self, value: i32) -> F::Result {
        self.lit().i32(value)
    }

    pub fn i64(self, value: i64) -> F::Result {
        self.lit().i64(value)
    }

    pub fn usize(self, value: usize) -> F::Result {
        self.lit().usize(value)
    }

    pub fn u8(self, value: u8) -> F::Result {
        self.lit().u8(value)
    }

    pub fn u16(self, value: u16) -> F::Result {
        self.lit().u16(value)
    }

    pub fn u32(self, value: u32) -> F::Result {
        self.lit().u32(value)
    }

    pub fn u64(self, value: u64) -> F::Result {
        self.lit().u64(value)
    }

    pub fn str<S>(self, value: S) -> F::Result
        where S: ToInternedString,
    {
        self.lit().str(value)
    }

    pub fn build_unary(self, unop: ast::UnOp, expr: P<ast::Expr>) -> F::Result {
        self.build_expr_(ast::ExprUnary(unop, expr))
    }

    pub fn build_box(self, expr: P<ast::Expr>) -> F::Result {
        self.build_unary(ast::UnUniq, expr)
    }

    pub fn build_deref(self, expr: P<ast::Expr>) -> F::Result {
        self.build_unary(ast::UnDeref, expr)
    }

    pub fn build_not(self, expr: P<ast::Expr>) -> F::Result {
        self.build_unary(ast::UnNot, expr)
    }

    pub fn build_neg(self, expr: P<ast::Expr>) -> F::Result {
        self.build_unary(ast::UnNeg, expr)
    }

    pub fn unary(self, unop: ast::UnOp) -> ExprBuilder<'a, ExprUnaryBuilder<'a, F>> {
        ExprBuilder::new_with_callback(self.ctx, ExprUnaryBuilder {
            builder: self,
            unop: unop,
        })
    }

    pub fn box_(self) -> ExprBuilder<'a, ExprUnaryBuilder<'a, F>> {
        self.unary(ast::UnUniq)
    }

    pub fn deref(self) -> ExprBuilder<'a, ExprUnaryBuilder<'a, F>> {
        self.unary(ast::UnDeref)
    }

    pub fn not(self) -> ExprBuilder<'a, ExprUnaryBuilder<'a, F>> {
        self.unary(ast::UnNot)
    }

    pub fn neg(self) -> ExprBuilder<'a, ExprUnaryBuilder<'a, F>> {
        self.unary(ast::UnNeg)
    }

    pub fn build_binary(
        self,
        binop: ast::BinOp_,
        lhs: P<ast::Expr>,
        rhs: P<ast::Expr>,
    ) -> F::Result {
        let binop = respan(self.span, binop);
        self.build_expr_(ast::Expr_::ExprBinary(binop, lhs, rhs))
    }

    pub fn build_add(self, lhs: P<ast::Expr>, rhs: P<ast::Expr>) -> F::Result {
        self.build_binary(ast::BinOp_::BiAdd, lhs, rhs)
    }

    pub fn build_sub(self, lhs: P<ast::Expr>, rhs: P<ast::Expr>) -> F::Result {
        self.build_binary(ast::BinOp_::BiSub, lhs, rhs)
    }

    pub fn build_mul(self, lhs: P<ast::Expr>, rhs: P<ast::Expr>) -> F::Result {
        self.build_binary(ast::BinOp_::BiMul, lhs, rhs)
    }

    pub fn build_div(self, lhs: P<ast::Expr>, rhs: P<ast::Expr>) -> F::Result {
        self.build_binary(ast::BinOp_::BiDiv, lhs, rhs)
    }

    pub fn build_rem(self, lhs: P<ast::Expr>, rhs: P<ast::Expr>) -> F::Result {
        self.build_binary(ast::BinOp_::BiRem, lhs, rhs)
    }

    pub fn build_and(self, lhs: P<ast::Expr>, rhs: P<ast::Expr>) -> F::Result {
        self.build_binary(ast::BinOp_::BiAnd, lhs, rhs)
    }

    pub fn build_or(self, lhs: P<ast::Expr>, rhs: P<ast::Expr>) -> F::Result {
        self.build_binary(ast::BinOp_::BiOr, lhs, rhs)
    }

    pub fn build_bit_xor(self, lhs: P<ast::Expr>, rhs: P<ast::Expr>) -> F::Result {
        self.build_binary(ast::BinOp_::BiBitXor, lhs, rhs)
    }

    pub fn build_bit_and(self, lhs: P<ast::Expr>, rhs: P<ast::Expr>) -> F::Result {
        self.build_binary(ast::BinOp_::BiBitAnd, lhs, rhs)
    }

    pub fn build_bit_or(self, lhs: P<ast::Expr>, rhs: P<ast::Expr>) -> F::Result {
        self.build_binary(ast::BinOp_::BiBitOr, lhs, rhs)
    }

    pub fn build_shl(self, lhs: P<ast::Expr>, rhs: P<ast::Expr>) -> F::Result {
        self.build_binary(ast::BinOp_::BiShl, lhs, rhs)
    }

    pub fn build_shr(self, lhs: P<ast::Expr>, rhs: P<ast::Expr>) -> F::Result {
        self.build_binary(ast::BinOp_::BiShr, lhs, rhs)
    }

    pub fn build_eq(self, lhs: P<ast::Expr>, rhs: P<ast::Expr>) -> F::Result {
        self.build_binary(ast::BinOp_::BiEq, lhs, rhs)
    }

    pub fn build_lt(self, lhs: P<ast::Expr>, rhs: P<ast::Expr>) -> F::Result {
        self.build_binary(ast::BinOp_::BiLt, lhs, rhs)
    }

    pub fn build_le(self, lhs: P<ast::Expr>, rhs: P<ast::Expr>) -> F::Result {
        self.build_binary(ast::BinOp_::BiLe, lhs, rhs)
    }

    pub fn build_ne(self, lhs: P<ast::Expr>, rhs: P<ast::Expr>) -> F::Result {
        self.build_binary(ast::BinOp_::BiNe, lhs, rhs)
    }

    pub fn build_ge(self, lhs: P<ast::Expr>, rhs: P<ast::Expr>) -> F::Result {
        self.build_binary(ast::BinOp_::BiGe, lhs, rhs)
    }

    pub fn build_gt(self, lhs: P<ast::Expr>, rhs: P<ast::Expr>) -> F::Result {
        self.build_binary(ast::BinOp_::BiGt, lhs, rhs)
    }

    pub fn binary(self, binop: ast::BinOp_) -> ExprBuilder<'a, ExprBinaryLhsBuilder<'a, F>> {
        ExprBuilder::new_with_callback(self.ctx, ExprBinaryLhsBuilder {
            builder: self,
            binop: binop,
        })
    }

    pub fn add(self) -> ExprBuilder<'a, ExprBinaryLhsBuilder<'a, F>> {
        self.binary(ast::BinOp_::BiAdd)
    }

    pub fn sub(self) -> ExprBuilder<'a, ExprBinaryLhsBuilder<'a, F>> {
        self.binary(ast::BinOp_::BiSub)
    }

    pub fn mul(self) -> ExprBuilder<'a, ExprBinaryLhsBuilder<'a, F>> {
        self.binary(ast::BinOp_::BiMul)
    }

    pub fn div(self) -> ExprBuilder<'a, ExprBinaryLhsBuilder<'a, F>> {
        self.binary(ast::BinOp_::BiDiv)
    }

    pub fn rem(self) -> ExprBuilder<'a, ExprBinaryLhsBuilder<'a, F>> {
        self.binary(ast::BinOp_::BiRem)
    }

    pub fn and(self) -> ExprBuilder<'a, ExprBinaryLhsBuilder<'a, F>> {
        self.binary(ast::BinOp_::BiAnd)
    }

    pub fn or(self) -> ExprBuilder<'a, ExprBinaryLhsBuilder<'a, F>> {
        self.binary(ast::BinOp_::BiOr)
    }

    pub fn bit_xor(self) -> ExprBuilder<'a, ExprBinaryLhsBuilder<'a, F>> {
        self.binary(ast::BinOp_::BiBitXor)
    }

    pub fn bit_and(self) -> ExprBuilder<'a, ExprBinaryLhsBuilder<'a, F>> {
        self.binary(ast::BinOp_::BiBitAnd)
    }

    pub fn bit_or(self) -> ExprBuilder<'a, ExprBinaryLhsBuilder<'a, F>> {
        self.binary(ast::BinOp_::BiBitOr)
    }

    pub fn shl(self) -> ExprBuilder<'a, ExprBinaryLhsBuilder<'a, F>> {
        self.binary(ast::BinOp_::BiShl)
    }

    pub fn shr(self) -> ExprBuilder<'a, ExprBinaryLhsBuilder<'a, F>> {
        self.binary(ast::BinOp_::BiShr)
    }

    pub fn eq(self) -> ExprBuilder<'a, ExprBinaryLhsBuilder<'a, F>> {
        self.binary(ast::BinOp_::BiEq)
    }

    pub fn lt(self) -> ExprBuilder<'a, ExprBinaryLhsBuilder<'a, F>> {
        self.binary(ast::BinOp_::BiLt)
    }

    pub fn le(self) -> ExprBuilder<'a, ExprBinaryLhsBuilder<'a, F>> {
        self.binary(ast::BinOp_::BiLe)
    }

    pub fn ne(self) -> ExprBuilder<'a, ExprBinaryLhsBuilder<'a, F>> {
        self.binary(ast::BinOp_::BiNe)
    }

    pub fn ge(self) -> ExprBuilder<'a, ExprBinaryLhsBuilder<'a, F>> {
        self.binary(ast::BinOp_::BiGe)
    }

    pub fn gt(self) -> ExprBuilder<'a, ExprBinaryLhsBuilder<'a, F>> {
        self.binary(ast::BinOp_::BiGt)
    }

    pub fn addr_of(self) -> ExprBuilder<'a, ExprAddrOfBuilder<'a, F>> {
        ExprBuilder::new_with_callback(self.ctx, ExprAddrOfBuilder {
            builder: self,
            mutability: ast::Mutability::MutImmutable,
        })
    }

    pub fn mut_addr_of(self) -> ExprBuilder<'a, ExprAddrOfBuilder<'a, F>> {
        ExprBuilder::new_with_callback(self.ctx, ExprAddrOfBuilder {
            builder: self,
            mutability: ast::Mutability::MutMutable,
        })
    }

    pub fn unit(self) -> F::Result {
        self.tuple().build()
    }

    pub fn tuple(self) -> ExprTupleBuilder<'a, F> {
        ExprTupleBuilder {
            builder: self,
            exprs: Vec::new(),
        }
    }

    pub fn struct_path<P>(self, path: P) -> ExprStructPathBuilder<'a, F>
        where P: IntoPath,
    {
        let span = self.span;
        let path = path.into_path(self.ctx);
        ExprStructPathBuilder {
            builder: self,
            span: span,
            path: path,
            fields: vec![],
        }
    }

    pub fn struct_(self) -> PathBuilder<'a, ExprStructBuilder<'a, F>> {
        PathBuilder::new_with_callback(self.ctx, ExprStructBuilder {
            builder: self,
        })
    }

    pub fn self_(self) -> F::Result {
        self.id("self")
    }

    pub fn none(self) -> F::Result {
        self.path()
            .global()
            .id("std").id("option").id("Option").id("None")
            .build()
    }

    pub fn some(self) -> ExprBuilder<'a, ExprPathBuilder<'a, F>> {
        let path = PathBuilder::new(self.ctx)
            .global()
            .id("std").id("option").id("Option").id("None")
            .build();

        ExprBuilder::new_with_callback(self.ctx, ExprPathBuilder {
            builder: self,
            path: path,
        })
    }

    pub fn ok(self) -> ExprBuilder<'a, ExprPathBuilder<'a, F>> {
        let path = PathBuilder::new(self.ctx)
            .global()
            .id("std").id("result").id("Result").id("Ok")
            .build();

        ExprBuilder::new_with_callback(self.ctx, ExprPathBuilder {
            builder: self,
            path: path,
        })
    }

    pub fn err(self) -> ExprBuilder<'a, ExprPathBuilder<'a, F>> {
        let path = PathBuilder::new(self.ctx)
            .global()
            .id("std").id("result").id("Result").id("Err")
            .build();

        ExprBuilder::new_with_callback(self.ctx, ExprPathBuilder {
            builder: self,
            path: path,
        })
    }

    pub fn phantom_data(self) -> F::Result {
        self.path()
            .global()
            .ids(&["std", "marker", "PhantomData"])
            .build()
    }

    pub fn call(self) -> ExprBuilder<'a, ExprCallBuilder<'a, F>> {
        ExprBuilder::new_with_callback(self.ctx, ExprCallBuilder {
            builder: self,
        })
    }

    pub fn method_call<I>(self, id: I) -> ExprBuilder<'a, ExprMethodCallBuilder<'a, F>>
        where I: ToIdent,
    {
        let id = respan(self.span, id.to_ident(self.ctx));
        ExprBuilder::new_with_callback(self.ctx, ExprMethodCallBuilder {
            builder: self,
            id: id,
        })
    }

    pub fn block(self) -> BlockBuilder<'a, Self> {
        BlockBuilder::new_with_callback(self.ctx, self)
    }

    pub fn paren(self) -> ExprBuilder<'a, ExprParenBuilder<'a, F>> {
        ExprBuilder::new_with_callback(self.ctx, ExprParenBuilder {
            builder: self,
        })
    }

    pub fn field<I>(self, id: I) -> ExprBuilder<'a, ExprFieldBuilder<'a, F>>
        where I: ToIdent,
    {
        let id = respan(self.span, id.to_ident(self.ctx));
        ExprBuilder::new_with_callback(self.ctx, ExprFieldBuilder {
            builder: self,
            id: id,
        })
    }

    pub fn tup_field(self, index: usize) -> ExprBuilder<'a, ExprTupFieldBuilder<'a, F>> {
        let index = respan(self.span, index);
        ExprBuilder::new_with_callback(self.ctx, ExprTupFieldBuilder {
            builder: self,
            index: index,
        })
    }
}

impl<'a, F> Invoke<P<ast::Lit>> for ExprBuilder<'a, F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, lit: P<ast::Lit>) -> F::Result {
        self.build_lit(lit)
    }
}

impl<'a, F> Invoke<ast::Path> for ExprBuilder<'a, F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, path: ast::Path) -> F::Result {
        self.build_path(path)
    }
}

impl<'a, F> Invoke<P<ast::Block>> for ExprBuilder<'a, F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, block: P<ast::Block>) -> F::Result {
        self.build_expr_(ast::ExprBlock(block))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprUnaryBuilder<'a, F> {
    builder: ExprBuilder<'a, F>,
    unop: ast::UnOp,
}

impl<'a, F> Invoke<P<ast::Expr>> for ExprUnaryBuilder<'a, F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.builder.build_unary(self.unop, expr)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprBinaryLhsBuilder<'a, F> {
    builder: ExprBuilder<'a, F>,
    binop: ast::BinOp_,
}

impl<'a, F> Invoke<P<ast::Expr>> for ExprBinaryLhsBuilder<'a, F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = ExprBuilder<'a, ExprBinaryRhsBuilder<'a, F>>;

    fn invoke(self, lhs: P<ast::Expr>) -> ExprBuilder<'a, ExprBinaryRhsBuilder<'a, F>> {
        ExprBuilder::new_with_callback(self.builder.ctx, ExprBinaryRhsBuilder {
            builder: self.builder,
            binop: self.binop,
            lhs: lhs,
        })
    }
}

pub struct ExprBinaryRhsBuilder<'a, F> {
    builder: ExprBuilder<'a, F>,
    binop: ast::BinOp_,
    lhs: P<ast::Expr>,
}

impl<'a, F> Invoke<P<ast::Expr>> for ExprBinaryRhsBuilder<'a, F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, rhs: P<ast::Expr>) -> F::Result {
        self.builder.build_binary(self.binop, self.lhs, rhs)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprTupleBuilder<'a, F> {
    builder: ExprBuilder<'a, F>,
    exprs: Vec<P<ast::Expr>>,
}

impl<'a, F: Invoke<P<ast::Expr>>> ExprTupleBuilder<'a, F>
    where F: Invoke<P<ast::Expr>>
{
    pub fn with_exprs<I>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=P<ast::Expr>>,
    {
        self.exprs.extend(iter);
        self
    }

    pub fn with_expr(mut self, expr: P<ast::Expr>) -> Self {
        self.exprs.push(expr);
        self
    }

    pub fn expr(self) -> ExprBuilder<'a, Self> {
        ExprBuilder::new_with_callback(self.builder.ctx, self)
    }

    pub fn build(self) -> F::Result {
        self.builder.build_expr_(ast::ExprTup(self.exprs))
    }
}

impl<'a, F> Invoke<P<ast::Expr>> for ExprTupleBuilder<'a, F>
    where F: Invoke<P<ast::Expr>>
{
    type Result = ExprTupleBuilder<'a, F>;

    fn invoke(self, expr: P<ast::Expr>) -> Self {
        self.with_expr(expr)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprStructBuilder<'a, F> {
    builder: ExprBuilder<'a, F>,
}

impl<'a, F> Invoke<ast::Path> for ExprStructBuilder<'a, F>
    where F: Invoke<P<ast::Expr>>
{
    type Result = ExprStructPathBuilder<'a, F>;

    fn invoke(self, path: ast::Path) -> ExprStructPathBuilder<'a, F> {
        self.builder.struct_path(path)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprStructPathBuilder<'a, F> {
    builder: ExprBuilder<'a, F>,
    span: Span,
    path: ast::Path,
    fields: Vec<ast::Field>,
}

impl<'a, F> ExprStructPathBuilder<'a, F>
    where F: Invoke<P<ast::Expr>>
{
    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn with_fields<I>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=ast::Field>,
    {
        self.fields.extend(iter);
        self
    }

    pub fn with_field<I>(mut self, id: I, expr: P<ast::Expr>) -> Self
        where I: ToIdent,
    {
        let span = self.span;
        let id = respan(span, id.to_ident(self.builder.ctx));
        self.fields.push(ast::Field {
            ident: id,
            expr: expr,
            span: span,
        });
        self
    }

    pub fn field<I>(self, id: I) -> ExprBuilder<'a, ExprStructFieldBuilder<'a, I, F>>
        where I: ToIdent,
    {
        ExprBuilder::new_with_callback(self.builder.ctx, ExprStructFieldBuilder {
            builder: self,
            id: id,
        })
    }

    pub fn build_expr(self, expr: P<ast::Expr>) -> F::Result {
        let expr_ = ast::ExprStruct(self.path, self.fields, Some(expr));
        self.builder.build_expr_(expr_)
    }

    pub fn expr(self) -> ExprBuilder<'a, Self> {
        ExprBuilder::new_with_callback(self.builder.ctx, self)
    }

    pub fn build(self) -> F::Result {
        let expr_ = ast::ExprStruct(self.path, self.fields, None);
        self.builder.build_expr_(expr_)
    }
}

impl<'a, F> Invoke<P<ast::Expr>> for ExprStructPathBuilder<'a, F>
    where F: Invoke<P<ast::Expr>>
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.build_expr(expr)
    }
}

pub struct ExprStructFieldBuilder<'a, I, F> {
    builder: ExprStructPathBuilder<'a, F>,
    id: I,
}

impl<'a, I, F> Invoke<P<ast::Expr>> for ExprStructFieldBuilder<'a, I, F>
    where I: ToIdent,
          F: Invoke<P<ast::Expr>>,
{
    type Result = ExprStructPathBuilder<'a, F>;

    fn invoke(self, expr: P<ast::Expr>) -> ExprStructPathBuilder<'a, F> {
        self.builder.with_field(self.id, expr)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprCallBuilder<'a, F> {
    builder: ExprBuilder<'a, F>,
}

impl<'a, F> Invoke<P<ast::Expr>> for ExprCallBuilder<'a, F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = ExprCallArgsBuilder<'a, F>;

    fn invoke(self, expr: P<ast::Expr>) -> ExprCallArgsBuilder<'a, F> {
        ExprCallArgsBuilder {
            builder: self.builder,
            fn_: expr,
            args: vec![],
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprCallArgsBuilder<'a, F> {
    builder: ExprBuilder<'a, F>,
    fn_: P<ast::Expr>,
    args: Vec<P<ast::Expr>>,
}

impl<'a, F> ExprCallArgsBuilder<'a, F>
    where F: Invoke<P<ast::Expr>>,
{
    pub fn with_args<I>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=P<ast::Expr>>,
    {
        self.args.extend(iter);
        self
    }

    pub fn with_arg(mut self, arg: P<ast::Expr>) -> Self {
        self.args.push(arg);
        self
    }

    pub fn arg(self) -> ExprBuilder<'a, Self> {
        ExprBuilder::new_with_callback(self.builder.ctx, self)
    }

    pub fn build(self) -> F::Result {
        self.builder.build_expr_(ast::ExprCall(self.fn_, self.args))
    }
}

impl<'a, F> Invoke<P<ast::Expr>> for ExprCallArgsBuilder<'a, F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = Self;

    fn invoke(self, arg: P<ast::Expr>) -> Self {
        self.with_arg(arg)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprMethodCallBuilder<'a, F> {
    builder: ExprBuilder<'a, F>,
    id: ast::SpannedIdent,
}

impl<'a, F> Invoke<P<ast::Expr>> for ExprMethodCallBuilder<'a, F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = ExprMethodCallArgsBuilder<'a, F>;

    fn invoke(self, expr: P<ast::Expr>) -> ExprMethodCallArgsBuilder<'a, F> {
        ExprMethodCallArgsBuilder {
            builder: self.builder,
            id: self.id,
            tys: vec![],
            args: vec![expr],
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprMethodCallArgsBuilder<'a, F> {
    builder: ExprBuilder<'a, F>,
    id: ast::SpannedIdent,
    tys: Vec<P<ast::Ty>>,
    args: Vec<P<ast::Expr>>,
}

impl<'a, F> ExprMethodCallArgsBuilder<'a, F>
    where F: Invoke<P<ast::Expr>>,
{
    pub fn with_tys<I>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=P<ast::Ty>>,
    {
        self.tys.extend(iter);
        self
    }

    pub fn with_ty(mut self, ty: P<ast::Ty>) -> Self {
        self.tys.push(ty);
        self
    }

    pub fn ty(self) -> TyBuilder<'a, Self> {
        TyBuilder::new_with_callback(self.builder.ctx, self)
    }

    pub fn with_args<I>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=P<ast::Expr>>,
    {
        self.args.extend(iter);
        self
    }

    pub fn with_arg(mut self, arg: P<ast::Expr>) -> Self {
        self.args.push(arg);
        self
    }

    pub fn arg(self) -> ExprBuilder<'a, Self> {
        ExprBuilder::new_with_callback(self.builder.ctx, self)
    }

    pub fn build(self) -> F::Result {
        self.builder.build_expr_(ast::ExprMethodCall(self.id, self.tys, self.args))
    }
}

impl<'a, F> Invoke<P<ast::Ty>> for ExprMethodCallArgsBuilder<'a, F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = Self;

    fn invoke(self, ty: P<ast::Ty>) -> Self {
        self.with_ty(ty)
    }
}

impl<'a, F> Invoke<P<ast::Expr>> for ExprMethodCallArgsBuilder<'a, F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = Self;

    fn invoke(self, arg: P<ast::Expr>) -> Self {
        self.with_arg(arg)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprAddrOfBuilder<'a, F> {
    builder: ExprBuilder<'a, F>,
    mutability: ast::Mutability,
}

impl<'a, F> Invoke<P<ast::Expr>> for ExprAddrOfBuilder<'a, F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.builder.build_expr_(ast::ExprAddrOf(self.mutability, expr))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprPathBuilder<'a, F> {
    builder: ExprBuilder<'a, F>,
    path: ast::Path,
}

impl<'a, F> Invoke<P<ast::Expr>> for ExprPathBuilder<'a, F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, arg: P<ast::Expr>) -> F::Result {
        self.builder.call()
            .build_path(self.path)
            .with_arg(arg)
            .build()
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprParenBuilder<'a, F> {
    builder: ExprBuilder<'a, F>,
}

impl<'a, F> Invoke<P<ast::Expr>> for ExprParenBuilder<'a, F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.builder.build_expr_(ast::ExprParen(expr))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprFieldBuilder<'a, F> {
    builder: ExprBuilder<'a, F>,
    id: ast::SpannedIdent,
}

impl<'a, F> Invoke<P<ast::Expr>> for ExprFieldBuilder<'a, F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.builder.build_expr_(ast::ExprField(expr, self.id))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprTupFieldBuilder<'a, F> {
    builder: ExprBuilder<'a, F>,
    index: Spanned<usize>,
}

impl<'a, F> Invoke<P<ast::Expr>> for ExprTupFieldBuilder<'a, F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.builder.build_expr_(ast::ExprTupField(expr, self.index))
    }
}

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

//////////////////////////////////////////////////////////////////////////////

pub struct BlockBuilder<'a, F=Identity> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
    stmts: Vec<P<ast::Stmt>>,
    block_check_mode: ast::BlockCheckMode,
}

impl<'a> BlockBuilder<'a> {
    pub fn new(ctx: &'a Ctx) -> Self {
        BlockBuilder::new_with_callback(ctx, Identity)
    }
}

impl<'a, F> BlockBuilder<'a, F>
    where F: Invoke<P<ast::Block>>,
{
    pub fn new_with_callback(ctx: &'a Ctx, callback: F) -> Self {
        BlockBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
            stmts: Vec::new(),
            block_check_mode: ast::BlockCheckMode::DefaultBlock,
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn unsafe_(mut self) -> Self {
        let source = ast::UnsafeSource::CompilerGenerated;
        self.block_check_mode = ast::BlockCheckMode::UnsafeBlock(source);
        self
    }

    pub fn with_stmts<I>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=P<ast::Stmt>>
    {
        self.stmts.extend(iter);
        self
    }

    pub fn with_stmt(mut self, stmt: P<ast::Stmt>) -> Self {
        self.stmts.push(stmt);
        self
    }

    pub fn stmt(self) -> StmtBuilder<'a, Self> {
        StmtBuilder::new_with_callback(self.ctx, self)
    }

    pub fn build_expr(self, expr: P<ast::Expr>) -> F::Result {
        self.build_(Some(expr))
    }

    pub fn expr(self) -> ExprBuilder<'a, Self> {
        ExprBuilder::new_with_callback(self.ctx, self)
    }

    pub fn build(self) -> F::Result {
        self.build_(None)
    }

    fn build_(self, expr: Option<P<ast::Expr>>) -> F::Result {
        self.callback.invoke(P(ast::Block {
            stmts: self.stmts,
            expr: expr,
            id: ast::DUMMY_NODE_ID,
            rules: self.block_check_mode,
            span: self.span,
        }))
    }
}

impl<'a, F> Invoke<P<ast::Stmt>> for BlockBuilder<'a, F>
    where F: Invoke<P<ast::Block>>,
{
    type Result = Self;

    fn invoke(self, stmt: P<ast::Stmt>) -> Self {
        self.with_stmt(stmt)
    }
}

impl<'a, F> Invoke<P<ast::Expr>> for BlockBuilder<'a, F>
    where F: Invoke<P<ast::Block>>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.build_expr(expr)
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

//////////////////////////////////////////////////////////////////////////////

pub struct ItemBuilder<'a, F=Identity> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
    attrs: Vec<ast::Attribute>,
    vis: ast::Visibility,
}

impl<'a> ItemBuilder<'a> {
    pub fn new(ctx: &'a Ctx) -> Self {
        ItemBuilder::new_with_callback(ctx, Identity)
    }
}

impl<'a, F> ItemBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    pub fn new_with_callback(ctx: &'a Ctx, callback: F) -> Self {
        ItemBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
            attrs: vec![],
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

    pub fn attr(self) -> AttrBuilder<'a, Self> {
        AttrBuilder::new_with_callback(self.ctx, self)
    }

    pub fn pub_(mut self) -> Self {
        self.vis = ast::Visibility::Public;
        self
    }

    pub fn build_item(self, item: P<ast::Item>) -> F::Result {
        self.callback.invoke(item)
    }

    pub fn build_item_<T>(self, id: T, item_: ast::Item_) -> F::Result
        where T: ToIdent,
    {
        let item = ast::Item {
            ident: id.to_ident(self.ctx),
            attrs: self.attrs,
            id: ast::DUMMY_NODE_ID,
            node: item_,
            vis: self.vis,
            span: self.span,
        };
        self.callback.invoke(P(item))
    }

    pub fn fn_<T>(self, id: T) -> FnDeclBuilder<'a, ItemFnDeclBuilder<'a, F>>
        where T: ToIdent,
    {
        let id = id.to_ident(self.ctx);
        FnDeclBuilder::new_with_callback(self.ctx, ItemFnDeclBuilder {
            builder: self,
            id: id,
        })
    }

    pub fn build_use(self, view_path: ast::ViewPath_) -> F::Result {
        let item = ast::ItemUse(P(respan(self.span, view_path)));
        self.build_item_(token::special_idents::invalid, item)
    }

    pub fn use_glob(self) -> PathBuilder<'a, ItemUseGlobBuilder<'a, F>> {
        PathBuilder::new_with_callback(self.ctx, ItemUseGlobBuilder(self))
    }

    pub fn struct_<T>(self, id: T) -> ItemStructBuilder<'a, F>
        where T: ToIdent,
    {
        let id = id.to_ident(self.ctx);
        let generics = GenericsBuilder::new(self.ctx).build();

        ItemStructBuilder {
            builder: self,
            id: id,
            generics: generics,
            fields: vec![],
        }
    }

    pub fn tuple_struct<T>(self, id: T) -> ItemTupleStructBuilder<'a, F>
        where T: ToIdent,
    {
        let id = id.to_ident(self.ctx);
        let generics = GenericsBuilder::new(self.ctx).build();

        ItemTupleStructBuilder {
            builder: self,
            id: id,
            generics: generics,
            fields: vec![],
        }
    }
}

impl<'a, F> Invoke<ast::Attribute> for ItemBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(self, attr: ast::Attribute) -> Self {
        self.with_attr(attr)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemFnDeclBuilder<'a, F> {
    builder: ItemBuilder<'a, F>,
    id: ast::Ident,
}

impl<'a, F> Invoke<P<ast::FnDecl>> for ItemFnDeclBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = ItemFnBuilder<'a, F>;

    fn invoke(self, fn_decl: P<ast::FnDecl>) -> ItemFnBuilder<'a, F> {
        let generics = GenericsBuilder::new(self.builder.ctx)
            .build();

        ItemFnBuilder {
            builder: self.builder,
            id: self.id,
            fn_decl: fn_decl,
            unsafety: ast::Unsafety::Normal,
            abi: Abi::Rust,
            generics: generics,
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemFnBuilder<'a, F> {
    builder: ItemBuilder<'a, F>,
    id: ast::Ident,
    fn_decl: P<ast::FnDecl>,
    unsafety: ast::Unsafety,
    abi: Abi,
    generics: ast::Generics,
}

impl<'a, F> ItemFnBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    pub fn unsafe_(mut self) -> Self {
        self.unsafety = ast::Unsafety::Normal;
        self
    }

    pub fn abi(mut self, abi: Abi) -> Self {
        self.abi = abi;
        self
    }

    pub fn with_generics(mut self, generics: ast::Generics) -> Self {
        self.generics = generics;
        self
    }

    pub fn generics(self) -> GenericsBuilder<'a, Self> {
        GenericsBuilder::new_with_callback(self.builder.ctx, self)
    }

    pub fn block(self) -> BlockBuilder<'a, Self> {
        BlockBuilder::new_with_callback(self.builder.ctx, self)
    }
}

impl<'a, F> Invoke<ast::Generics> for ItemFnBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(self, generics: ast::Generics) -> Self {
        self.with_generics(generics)
    }
}

impl<'a, F> Invoke<P<ast::Block>> for ItemFnBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = F::Result;

    fn invoke(self, block: P<ast::Block>) -> F::Result {
        self.builder.build_item_(self.id, ast::Item_::ItemFn(
            self.fn_decl,
            self.unsafety,
            self.abi,
            self.generics,
            block,
        ))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemUseGlobBuilder<'a, F>(ItemBuilder<'a, F>);

impl<'a, F> Invoke<ast::Path> for ItemUseGlobBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = F::Result;

    fn invoke(self, path: ast::Path) -> F::Result {
        self.0.build_use(ast::ViewPathGlob(path))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemStructBuilder<'a, F> {
    builder: ItemBuilder<'a, F>,
    id: ast::Ident,
    generics: ast::Generics,
    fields: Vec<ast::StructField>,
}

impl<'a, F> ItemStructBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    pub fn with_generics(mut self, generics: ast::Generics) -> Self {
        self.generics = generics;
        self
    }

    pub fn generics(self) -> GenericsBuilder<'a, Self> {
        GenericsBuilder::new_with_callback(self.builder.ctx, self)
    }

    pub fn with_field(mut self, field: ast::StructField) -> Self {
        self.fields.push(field);
        self
    }

    pub fn field<T>(self, id: T) -> TyBuilder<'a, ItemStructFieldBuilder<'a, Self>>
        where T: ToIdent,
    {
        let id = id.to_ident(self.builder.ctx);
        let span = self.builder.span;

        TyBuilder::new_with_callback(self.builder.ctx, ItemStructFieldBuilder {
            ctx: self.builder.ctx,
            callback: self,
            span: span,
            kind: ast::StructFieldKind::NamedField(id, ast::Inherited),
            attrs: vec![],
        })
    }

    pub fn build(self) -> F::Result {
        let struct_def = ast::StructDef {
            fields: self.fields,
            ctor_id: None,
        };
        let struct_ = ast::ItemStruct(P(struct_def), self.generics);
        self.builder.build_item_(self.id, struct_)
    }
}

impl<'a, F> Invoke<ast::Generics> for ItemStructBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(self, generics: ast::Generics) -> Self {
        self.with_generics(generics)
    }
}

impl<'a, F> Invoke<ast::StructField> for ItemStructBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(self, field: ast::StructField) -> Self {
        self.with_field(field)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemTupleStructBuilder<'a, F> {
    builder: ItemBuilder<'a, F>,
    id: ast::Ident,
    generics: ast::Generics,
    fields: Vec<ast::StructField>,
}

impl<'a, F> ItemTupleStructBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    pub fn with_generics(mut self, generics: ast::Generics) -> Self {
        self.generics = generics;
        self
    }

    pub fn generics(self) -> GenericsBuilder<'a, Self> {
        GenericsBuilder::new_with_callback(self.builder.ctx, self)
    }

    pub fn with_field(mut self, field: ast::StructField) -> Self {
        self.fields.push(field);
        self
    }

    pub fn with_tys<I>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=P<ast::Ty>>,
    {
        for ty in iter {
            self = self.with_ty(ty);
        }
        self
    }

    pub fn with_ty(self, ty: P<ast::Ty>) -> Self {
        self.field().build_ty(ty)
    }

    pub fn ty(self) -> TyBuilder<'a, Self> {
        TyBuilder::new_with_callback(self.builder.ctx, self)
    }

    pub fn field(self) -> TyBuilder<'a, ItemStructFieldBuilder<'a, Self>> {
        let span = self.builder.span;

        TyBuilder::new_with_callback(self.builder.ctx, ItemStructFieldBuilder {
            ctx: self.builder.ctx,
            callback: self,
            span: span,
            kind: ast::StructFieldKind::UnnamedField(ast::Inherited),
            attrs: vec![],
        })
    }

    pub fn build(self) -> F::Result {
        let struct_def = ast::StructDef {
            fields: self.fields,
            ctor_id: Some(ast::DUMMY_NODE_ID),
        };
        let struct_ = ast::ItemStruct(P(struct_def), self.generics);
        self.builder.build_item_(self.id, struct_)
    }
}

impl<'a, F> Invoke<ast::Generics> for ItemTupleStructBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(self, generics: ast::Generics) -> Self {
        self.with_generics(generics)
    }
}

impl<'a, F> Invoke<P<ast::Ty>> for ItemTupleStructBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(self, ty: P<ast::Ty>) -> Self {
        self.with_ty(ty)
    }
}

impl<'a, F> Invoke<ast::StructField> for ItemTupleStructBuilder<'a, F>
    where F: Invoke<P<ast::Item>>,
{
    type Result = Self;

    fn invoke(self, field: ast::StructField) -> Self {
        self.with_field(field)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ItemStructFieldBuilder<'a, F> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
    kind: ast::StructFieldKind,
    attrs: Vec<ast::Attribute>,
}

impl<'a, F> ItemStructFieldBuilder<'a, F>
    where F: Invoke<ast::StructField>,
{
    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn attr(self) -> AttrBuilder<'a, Self> {
        let span = self.span;
        AttrBuilder::new_with_callback(self.ctx, self).span(span)
    }

    pub fn build_ty(self, ty: P<ast::Ty>) -> F::Result {
        let field = ast::StructField_ {
            kind: self.kind,
            id: ast::DUMMY_NODE_ID,
            ty: ty,
            attrs: self.attrs,
        };
        self.callback.invoke(respan(self.span, field))
    }

    pub fn ty(self) -> TyBuilder<'a, Self> {
        let span = self.span;
        TyBuilder::new_with_callback(self.ctx, self).span(span)
    }
}

impl<'a, F> Invoke<ast::Attribute> for ItemStructFieldBuilder<'a, F> {
    type Result = Self;

    fn invoke(mut self, attr: ast::Attribute) -> Self {
        self.attrs.push(attr);
        self
    }
}

impl<'a, F> Invoke<P<ast::Ty>> for ItemStructFieldBuilder<'a, F>
    where F: Invoke<ast::StructField>,
{
    type Result = F::Result;

    fn invoke(self, ty: P<ast::Ty>) -> F::Result {
        self.build_ty(ty)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct AttrBuilder<'a, F> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
}

impl<'a, F> AttrBuilder<'a, F>
    where F: Invoke<ast::Attribute>,
{
    pub fn new_with_callback(ctx: &'a Ctx, callback: F) -> Self {
        AttrBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn build_meta_item(self, item: P<ast::MetaItem>) -> F::Result {
        let attr = respan(self.span, ast::Attribute_ {
            id: attr::mk_attr_id(),
            style: ast::AttrOuter,
            value: item,
            is_sugared_doc: false,
        });
        self.callback.invoke(attr)
    }

    pub fn build_meta_item_(self, item: ast::MetaItem_) -> F::Result {
        let item = P(respan(self.span, item));
        self.build_meta_item(item)
    }

    pub fn word<T>(self, word: T) -> F::Result
        where T: ToInternedString
    {
        self.build_meta_item_(ast::MetaWord(word.into_interned_string()))
    }

    pub fn list<T>(self, word: T) -> AttrListBuilder<'a, Self>
        where T: ToInternedString
    {
        AttrListBuilder::new_with_callback(self.ctx, word, self)
    }

    pub fn name_value<T>(self, name: T) -> LitBuilder<'a, AttrNameValueBuilder<Self>>
        where T: ToInternedString,
    {
        LitBuilder::new_with_callback(self.ctx, AttrNameValueBuilder {
            callback: self,
            name: name.into_interned_string(),
        })
    }

    pub fn inline(self) -> F::Result {
        self.word("inline")
    }

    pub fn test(self) -> F::Result {
        self.word("test")
    }

    pub fn build_allow<I, T>(self, iter: I) -> F::Result
        where I: IntoIterator<Item=T>,
              T: ToInternedString,
    {
        self.list("allow").words(iter).build()
    }

    pub fn build_warn<I, T>(self, iter: I) -> F::Result
        where I: IntoIterator<Item=T>,
              T: ToInternedString,
    {
        self.list("warn").words(iter).build()
    }

    pub fn build_deny<I, T>(self, iter: I) -> F::Result
        where I: IntoIterator<Item=T>,
              T: ToInternedString,
    {
        self.list("deny").words(iter).build()
    }

    pub fn build_features<I, T>(self, iter: I) -> F::Result
        where I: IntoIterator<Item=T>,
              T: ToInternedString,
    {
        self.list("feature").words(iter).build()
    }

    pub fn build_plugins<I, T>(self, iter: I) -> F::Result
        where I: IntoIterator<Item=T>,
              T: ToInternedString,
    {
        self.list("plugin").words(iter).build()
    }
}

impl<'a, F> Invoke<P<ast::MetaItem>> for AttrBuilder<'a, F>
    where F: Invoke<ast::Attribute>,
{
    type Result = F::Result;

    fn invoke(self, item: P<ast::MetaItem>) -> F::Result {
        self.build_meta_item(item)
    }
}

impl<'a, F> Invoke<ast::MetaItem_> for AttrBuilder<'a, F>
    where F: Invoke<ast::Attribute>,
{
    type Result = F::Result;

    fn invoke(self, item: ast::MetaItem_) -> F::Result {
        self.build_meta_item_(item)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct AttrListBuilder<'a, F> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
    name: token::InternedString,
    items: Vec<P<ast::MetaItem>>,
}

impl<'a, F> AttrListBuilder<'a, F>
    where F: Invoke<P<ast::MetaItem>>,
{
    pub fn new_with_callback<T>(ctx: &'a Ctx, name: T, callback: F) -> Self
        where T: ToInternedString,
    {
        AttrListBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
            name: name.into_interned_string(),
            items: vec![],
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn with_meta_items<I>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=P<ast::MetaItem>>,
    {
        self.items.extend(iter);
        self
    }

    pub fn with_meta_items_<I>(self, iter: I) -> Self
        where I: IntoIterator<Item=ast::MetaItem_>,
    {
        let iter = iter.into_iter();
        let span = self.span;
        self.with_meta_items(iter.map(|item| P(respan(span, item))))
    }

    pub fn with_meta_item(mut self, item: P<ast::MetaItem>) -> Self {
        self.items.push(item);
        self
    }

    pub fn with_meta_item_(self, item: ast::MetaItem_) -> Self {
        let span = self.span;
        self.with_meta_item(P(respan(span, item)))
    }

    pub fn words<I, T>(self, iter: I) -> Self
        where I: IntoIterator<Item=T>,
              T: ToInternedString,
    {
        let iter = iter.into_iter();
        self.with_meta_items_(iter.map(|word| ast::MetaWord(word.into_interned_string())))
    }

    pub fn word<T>(self, word: T) -> Self
        where T: ToInternedString,
    {
        self.with_meta_item_(ast::MetaWord(word.into_interned_string()))
    }

    pub fn list<T>(self, name: T) -> AttrListBuilder<'a, Self>
        where T: ToInternedString,
    {
        AttrListBuilder::new_with_callback(self.ctx, name, self)
    }

    pub fn name_value<T>(self, name: T) -> LitBuilder<'a, AttrNameValueBuilder<Self>>
        where T: ToInternedString,
    {
        LitBuilder::new_with_callback(self.ctx, AttrNameValueBuilder {
            callback: self,
            name: name.into_interned_string(),
        })
    }

    pub fn build(self) -> F::Result {
        let item = respan(self.span, ast::MetaList(self.name, self.items));
        self.callback.invoke(P(item))
    }
}

impl<'a, F> Invoke<P<ast::MetaItem>> for AttrListBuilder<'a, F>
    where F: Invoke<P<ast::MetaItem>>,
{
    type Result = Self;

    fn invoke(self, item: P<ast::MetaItem>) -> Self {
        self.with_meta_item(item)
    }
}

impl<'a, F> Invoke<ast::MetaItem_> for AttrListBuilder<'a, F>
    where F: Invoke<P<ast::MetaItem>>,
{
    type Result = Self;

    fn invoke(self, item: ast::MetaItem_) -> Self {
        self.with_meta_item_(item)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct AttrNameValueBuilder<F> {
    callback: F,
    name: token::InternedString,
}

impl<F: Invoke<ast::MetaItem_>> Invoke<P<ast::Lit>> for AttrNameValueBuilder<F> {
    type Result = F::Result;

    fn invoke(self, value: P<ast::Lit>) -> F::Result {
        let item = ast::MetaNameValue(self.name, (*value).clone());
        self.callback.invoke(item)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct GenericsBuilder<'a, F=Identity> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
    lifetimes: Vec<ast::LifetimeDef>,
    ty_params: Vec<ast::TyParam>,
    predicates: Vec<ast::WherePredicate>,
}

impl<'a> GenericsBuilder<'a> {
    pub fn new(ctx: &'a Ctx) -> Self {
        GenericsBuilder::new_with_callback(ctx, Identity)
    }

    pub fn from_generics(ctx: &'a Ctx, generics: ast::Generics) -> Self {
        GenericsBuilder::from_generics_with_callback(ctx, Identity, generics)
    }
}

impl<'a, F> GenericsBuilder<'a, F>
    where F: Invoke<ast::Generics>,
{
    pub fn new_with_callback(ctx: &'a Ctx, callback: F) -> Self {
        GenericsBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
            lifetimes: Vec::new(),
            ty_params: Vec::new(),
            predicates: Vec::new(),
        }
    }

    pub fn from_generics_with_callback(ctx: &'a Ctx, callback: F, generics: ast::Generics) -> Self {
        GenericsBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
            lifetimes: generics.lifetimes,
            ty_params: generics.ty_params.into_vec(),
            predicates: generics.where_clause.predicates,
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn with_lifetimes<I, L>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=L>,
              L: IntoLifetimeDef,
    {
        let ctx = self.ctx;
        let iter = iter.into_iter().map(|lifetime_def| lifetime_def.into_lifetime_def(ctx));
        self.lifetimes.extend(iter);
        self
    }

    pub fn with_lifetime_names<I, N>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=N>,
              N: ToName,
    {
        for name in iter {
            self = self.lifetime_name(name);
        }
        self
    }

    pub fn with_lifetime(mut self, lifetime: ast::LifetimeDef) -> Self {
        self.lifetimes.push(lifetime);
        self
    }

    pub fn lifetime_name<N>(self, name: N) -> Self
        where N: ToName,
    {
        self.lifetime(name).build()
    }

    pub fn lifetime<N>(self, name: N) -> LifetimeDefBuilder<'a, Self>
        where N: ToName,
    {
        LifetimeDefBuilder::new_with_callback(self.ctx, name, self)
    }

    pub fn with_ty_params<I>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=ast::TyParam>,
    {
        self.ty_params.extend(iter);
        self
    }

    pub fn with_ty_param_ids<I, T>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=T>,
              T: ToIdent,
    {
        for id in iter {
            self = self.ty_param_id(id);
        }
        self
    }

    pub fn with_ty_param(mut self, ty_param: ast::TyParam) -> Self {
        self.ty_params.push(ty_param);
        self
    }

    pub fn ty_param_id<I>(self, id: I) -> Self
        where I: ToIdent,
    {
        self.ty_param(id).build()
    }

    pub fn ty_param<I>(self, id: I) -> TyParamBuilder<'a, Self>
        where I: ToIdent,
    {
        let span = self.span;
        TyParamBuilder::new_with_callback(self.ctx, id, self).span(span)
    }

    pub fn with_predicates<I>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=ast::WherePredicate>,
    {
        self.predicates.extend(iter);
        self
    }

    pub fn with_predicate(mut self, predicate: ast::WherePredicate) -> Self {
        self.predicates.push(predicate);
        self
    }

    pub fn add_lifetime_bound<L>(mut self, lifetime: L) -> Self
        where L: IntoLifetime,
    {
        let lifetime = lifetime.into_lifetime(self.ctx);

        for lifetime_def in self.lifetimes.iter_mut() {
            lifetime_def.bounds.push(lifetime.clone());
        }

        for ty_param in self.ty_params.iter_mut() {
            *ty_param = TyParamBuilder::from_ty_param(self.ctx, ty_param.clone())
                .lifetime_bound(lifetime.clone())
                .build();
        }

        self 
    }

    pub fn add_ty_param_bound<P>(mut self, path: P) -> Self
        where P: IntoPath,
    {
        let path = path.into_path(self.ctx);

        for ty_param in self.ty_params.iter_mut() {
            *ty_param = TyParamBuilder::from_ty_param(self.ctx, ty_param.clone())
                .trait_bound(path.clone()).build()
                .build();
        }

        self 
    }

    pub fn strip_bounds(mut self) -> Self {
        for lifetime in self.lifetimes.iter_mut() {
            lifetime.bounds = vec![];
        }

        for ty_param in self.ty_params.iter_mut() {
            ty_param.bounds = OwnedSlice::empty();
        }

        self.predicates = vec![];

        self
    }

    pub fn build(self) -> F::Result {
        self.callback.invoke(ast::Generics {
            lifetimes: self.lifetimes,
            ty_params: OwnedSlice::from_vec(self.ty_params),
            where_clause: ast::WhereClause {
                id: ast::DUMMY_NODE_ID,
                predicates: self.predicates,
            },
        })
    }
}

impl<'a, F> Invoke<ast::LifetimeDef> for GenericsBuilder<'a, F>
    where F: Invoke<ast::Generics>,
{
    type Result = Self;

    fn invoke(self, lifetime: ast::LifetimeDef) -> Self {
        self.with_lifetime(lifetime)
    }
}

impl<'a, F> Invoke<ast::TyParam> for GenericsBuilder<'a, F>
    where F: Invoke<ast::Generics>,
{
    type Result = Self;

    fn invoke(self, ty_param: ast::TyParam) -> Self {
        self.with_ty_param(ty_param)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct LifetimeDefBuilder<'a, F=Identity> {
    ctx: &'a Ctx,
    callback: F,
    lifetime: ast::Lifetime,
    bounds: Vec<ast::Lifetime>,
}

impl<'a> LifetimeDefBuilder<'a> {
    pub fn new<N>(ctx: &'a Ctx, name: N) -> Self
        where N: ToName,
    {
        LifetimeDefBuilder::new_with_callback(ctx, name, Identity)
    }
}

impl<'a, F> LifetimeDefBuilder<'a, F>
    where F: Invoke<ast::LifetimeDef>,
{
    pub fn new_with_callback<N>(ctx: &'a Ctx, name: N, callback: F) -> Self
        where N: ToName,
    {
        let lifetime = ast::Lifetime {
            id: ast::DUMMY_NODE_ID,
            span: DUMMY_SP,
            name: name.to_name(ctx),
        };

        LifetimeDefBuilder {
            ctx: ctx,
            callback: callback,
            lifetime: lifetime,
            bounds: Vec::new(),
        }
    }

    pub fn bound<N>(mut self, name: N) -> Self
        where N: ToName,
    {
        let lifetime = ast::Lifetime {
            id: ast::DUMMY_NODE_ID,
            span: DUMMY_SP,
            name: name.to_name(self.ctx),
        };

        self.bounds.push(lifetime);
        self
    }

    pub fn build(self) -> F::Result {
        self.callback.invoke(ast::LifetimeDef {
            lifetime: self.lifetime,
            bounds: self.bounds,
        })
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct TyParamBuilder<'a, F=Identity> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
    id: ast::Ident,
    bounds: Vec<ast::TyParamBound>,
    default: Option<P<ast::Ty>>,
}

impl<'a> TyParamBuilder<'a> {
    pub fn new<I>(ctx: &'a Ctx, id: I) -> Self
        where I: ToIdent,
    {
        TyParamBuilder::new_with_callback(ctx, id, Identity)
    }

    pub fn from_ty_param(ctx: &'a Ctx, ty_param: ast::TyParam) -> Self {
        TyParamBuilder::from_ty_param_with_callback(ctx, Identity, ty_param)
    }
}

impl<'a, F> TyParamBuilder<'a, F>
    where F: Invoke<ast::TyParam>,
{
    pub fn new_with_callback<I>(ctx: &'a Ctx, id: I, callback: F) -> Self
        where I: ToIdent
    {
        TyParamBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
            id: id.to_ident(ctx),
            bounds: Vec::new(),
            default: None,
        }
    }

    pub fn from_ty_param_with_callback(ctx: &'a Ctx, callback: F, ty_param: ast::TyParam) -> Self {
        TyParamBuilder {
            ctx: ctx,
            callback: callback,
            span: ty_param.span,
            id: ty_param.ident,
            bounds: ty_param.bounds.into_vec(),
            default: ty_param.default,
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn with_default(mut self, ty: P<ast::Ty>) -> Self {
        self.default = Some(ty);
        self
    }

    pub fn with_trait_bound(mut self, trait_ref: ast::PolyTraitRef) -> Self {
        self.bounds.push(ast::TyParamBound::TraitTyParamBound(
            trait_ref,
            ast::TraitBoundModifier::None,
        ));
        self
    }

    pub fn trait_bound<P>(self, path: P) -> PolyTraitRefBuilder<'a, Self>
        where P: IntoPath,
    {
        PolyTraitRefBuilder::new_with_callback(self.ctx, path, self)
    }

    pub fn lifetime_bound<L>(mut self, lifetime: L) -> Self
        where L: IntoLifetime,
    {
        let lifetime = lifetime.into_lifetime(self.ctx);

        self.bounds.push(ast::TyParamBound::RegionTyParamBound(lifetime));
        self
    }

    pub fn build(self) -> F::Result {
        self.callback.invoke(ast::TyParam {
            ident: self.id,
            id: ast::DUMMY_NODE_ID,
            bounds: OwnedSlice::from_vec(self.bounds),
            default: self.default,
            span: self.span,
        })
    }
}

impl<'a, F> Invoke<ast::PolyTraitRef> for TyParamBuilder<'a, F>
    where F: Invoke<ast::TyParam>,
{
    type Result = Self;

    fn invoke(self, trait_ref: ast::PolyTraitRef) -> Self {
        self.with_trait_bound(trait_ref)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct PolyTraitRefBuilder<'a, F> {
    ctx: &'a Ctx,
    callback: F,
    span: Span,
    trait_ref: ast::TraitRef,
    lifetimes: Vec<ast::LifetimeDef>,
}

impl<'a, F> PolyTraitRefBuilder<'a, F>
    where F: Invoke<ast::PolyTraitRef>,
{
    pub fn new_with_callback<P>(ctx: &'a Ctx, path: P, callback: F) -> Self
        where P: IntoPath,
    {
        let trait_ref = ast::TraitRef {
            path: path.into_path(ctx),
            ref_id: ast::DUMMY_NODE_ID,
        };

        PolyTraitRefBuilder {
            ctx: ctx,
            callback: callback,
            span: DUMMY_SP,
            trait_ref: trait_ref,
            lifetimes: Vec::new(),
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn with_lifetime<L>(mut self, lifetime: L) -> Self
        where L: IntoLifetimeDef,
    {
        self.lifetimes.push(lifetime.into_lifetime_def(self.ctx));
        self
    }

    pub fn lifetime<N>(self, name: N) -> LifetimeDefBuilder<'a, Self>
        where N: ToName,
    {
        LifetimeDefBuilder::new_with_callback(self.ctx, name, self)
    }

    pub fn build(self) -> F::Result {
        self.callback.invoke(ast::PolyTraitRef {
            bound_lifetimes: self.lifetimes,
            trait_ref: self.trait_ref,
            span: self.span,
        })
    }
}

impl<'a, F> Invoke<ast::LifetimeDef> for PolyTraitRefBuilder<'a, F>
    where F: Invoke<ast::PolyTraitRef>,
{
    type Result = Self;

    fn invoke(self, lifetime: ast::LifetimeDef) -> Self {
        self.with_lifetime(lifetime)
    }
}

//////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct AstBuilder<'a> {
    ctx: &'a Ctx,
    span: Span,
}

impl<'a> AstBuilder<'a> {
    pub fn new(ctx: &Ctx) -> AstBuilder {
        AstBuilder {
            ctx: ctx,
            span: DUMMY_SP,
        }
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn id<I>(&self, id: I) -> ast::Ident
        where I: ToIdent
    {
        id.to_ident(self.ctx)
    }

    pub fn name<N>(&self, name: N) -> ast::Name
        where N: ToName
    {
        name.to_name(self.ctx)
    }

    pub fn path(&self) -> PathBuilder {
        PathBuilder::new(self.ctx)
    }

    pub fn ty(&self) -> TyBuilder {
        TyBuilder::new(self.ctx).span(self.span)
    }

    pub fn ty_param<I>(&self, id: I) -> TyParamBuilder
        where I: ToIdent,
    {
        TyParamBuilder::new(self.ctx, id).span(self.span)
    }

    pub fn from_ty_param(&self, ty_param: ast::TyParam) -> TyParamBuilder {
        TyParamBuilder::from_ty_param(self.ctx, ty_param)
    }

    pub fn generics(&self) -> GenericsBuilder {
        GenericsBuilder::new(self.ctx).span(self.span)
    }

    pub fn from_generics(&self, generics: ast::Generics) -> GenericsBuilder {
        GenericsBuilder::from_generics(self.ctx, generics).span(self.span)
    }

    pub fn expr(&self) -> ExprBuilder {
        ExprBuilder::new(self.ctx).span(self.span)
    }

    pub fn stmt(&self) -> StmtBuilder {
        StmtBuilder::new(self.ctx).span(self.span)
    }

    pub fn arg<I>(&self, id: I) -> ArgBuilder
        where I: ToIdent,
    {
        ArgBuilder::new(self.ctx, id).span(self.span)
    }

    pub fn block(&self) -> BlockBuilder {
        BlockBuilder::new(self.ctx).span(self.span)
    }

    pub fn fn_decl(&self) -> FnDeclBuilder {
        FnDeclBuilder::new(self.ctx).span(self.span)
    }

    pub fn method<I>(&self, id: I) -> MethodBuilder
        where I: ToIdent,
    {
        MethodBuilder::new(self.ctx, id).span(self.span)
    }

    pub fn item(&self) -> ItemBuilder {
        ItemBuilder::new(self.ctx).span(self.span)
    }
}
