use std::iter::IntoIterator;

use syntax::ast;
use syntax::codemap::{DUMMY_SP, Span, Spanned, respan};
use syntax::ptr::P;

use block::BlockBuilder;
use ctx::Ctx;
use ident::ToIdent;
use invoke::{Invoke, Identity};
use lit::LitBuilder;
use path::{IntoPath, PathBuilder};
use str::ToInternedString;
use ty::TyBuilder;

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
