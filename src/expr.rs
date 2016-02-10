#![cfg_attr(feature = "unstable-testing", allow(should_implement_trait))]

use std::iter::IntoIterator;

use syntax::ast;
use syntax::attr::AttributesExt;
use syntax::codemap::{DUMMY_SP, Span, Spanned, respan};
use syntax::ptr::P;

use arm::ArmBuilder;
use attr::AttrBuilder;
use block::BlockBuilder;
use ident::ToIdent;
use invoke::{Invoke, Identity};
use lit::LitBuilder;
use pat::PatBuilder;
use path::{IntoPath, PathBuilder};
use qpath::QPathBuilder;
use str::ToInternedString;
use ty::TyBuilder;

//////////////////////////////////////////////////////////////////////////////

pub struct ExprBuilder<F=Identity> {
    callback: F,
    span: Span,
    attrs: Vec<ast::Attribute>,
}

impl ExprBuilder {
    pub fn new() -> Self {
        ExprBuilder::with_callback(Identity)
    }
}

impl<F> ExprBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    pub fn with_callback(callback: F) -> Self {
        ExprBuilder {
            callback: callback,
            span: DUMMY_SP,
            attrs: vec![],
        }
    }

    pub fn build(self, expr: P<ast::Expr>) -> F::Result {
        self.callback.invoke(expr)
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn with_attr(mut self, attr: ast::Attribute) -> Self {
        self.attrs.push(attr);
        self
    }

    pub fn attr(self) -> AttrBuilder<Self> {
        AttrBuilder::with_callback(self)
    }

    pub fn build_expr_(self, expr: ast::Expr_) -> F::Result {
        let expr = P(ast::Expr {
            id: ast::DUMMY_NODE_ID,
            node: expr,
            span: self.span,
            attrs: self.attrs.clone().into_thin_attrs(),
        });
        self.build(expr)
    }

    pub fn build_path(self, path: ast::Path) -> F::Result {
        self.build_expr_(ast::Expr_::ExprPath(None, path))
    }

    pub fn build_qpath(self, qself: ast::QSelf, path: ast::Path) -> F::Result {
        self.build_expr_(ast::Expr_::ExprPath(Some(qself), path))
    }

    pub fn path(self) -> PathBuilder<Self> {
        PathBuilder::with_callback(self)
    }

    pub fn qpath(self) -> QPathBuilder<Self> {
        QPathBuilder::with_callback(self)
    }

    pub fn id<I>(self, id: I) -> F::Result
        where I: ToIdent
    {
        self.path().id(id).build()
    }

    pub fn build_lit(self, lit: P<ast::Lit>) -> F::Result {
        self.build_expr_(ast::Expr_::ExprLit(lit))
    }

    pub fn lit(self) -> LitBuilder<Self> {
        LitBuilder::with_callback(self)
    }

    pub fn bool(self, value: bool) -> F::Result {
        self.lit().bool(value)
    }

    pub fn true_(self) -> F::Result {
        self.bool(true)
    }

    pub fn false_(self) -> F::Result {
        self.bool(false)
    }

    pub fn int(self, value: i64) -> F::Result {
        self.lit().int(value)
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

    pub fn f32<S>(self, value: S) -> F::Result
        where S: ToInternedString,
    {
        self.lit().f32(value)
    }

    pub fn f64<S>(self, value: S) -> F::Result
        where S: ToInternedString,
    {
        self.lit().f64(value)
    }

    pub fn str<S>(self, value: S) -> F::Result
        where S: ToInternedString,
    {
        self.lit().str(value)
    }

    pub fn build_unary(self, unop: ast::UnOp, expr: P<ast::Expr>) -> F::Result {
        self.build_expr_(ast::ExprUnary(unop, expr))
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

    pub fn unary(self, unop: ast::UnOp) -> ExprBuilder<ExprUnaryBuilder<F>> {
        ExprBuilder::with_callback(ExprUnaryBuilder {
            builder: self,
            unop: unop,
        })
    }

    pub fn deref(self) -> ExprBuilder<ExprUnaryBuilder<F>> {
        self.unary(ast::UnDeref)
    }

    pub fn not(self) -> ExprBuilder<ExprUnaryBuilder<F>> {
        self.unary(ast::UnNot)
    }

    pub fn neg(self) -> ExprBuilder<ExprUnaryBuilder<F>> {
        self.unary(ast::UnNeg)
    }

    pub fn build_binary(self,
                        binop: ast::BinOp_,
                        lhs: P<ast::Expr>,
                        rhs: P<ast::Expr>) -> F::Result {
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

    pub fn binary(self, binop: ast::BinOp_) -> ExprBuilder<ExprBinaryLhsBuilder<F>> {
        ExprBuilder::with_callback(ExprBinaryLhsBuilder {
            builder: self,
            binop: binop,
        })
    }

    pub fn add(self) -> ExprBuilder<ExprBinaryLhsBuilder<F>> {
        self.binary(ast::BinOp_::BiAdd)
    }

    pub fn sub(self) -> ExprBuilder<ExprBinaryLhsBuilder<F>> {
        self.binary(ast::BinOp_::BiSub)
    }

    pub fn mul(self) -> ExprBuilder<ExprBinaryLhsBuilder<F>> {
        self.binary(ast::BinOp_::BiMul)
    }

    pub fn div(self) -> ExprBuilder<ExprBinaryLhsBuilder<F>> {
        self.binary(ast::BinOp_::BiDiv)
    }

    pub fn rem(self) -> ExprBuilder<ExprBinaryLhsBuilder<F>> {
        self.binary(ast::BinOp_::BiRem)
    }

    pub fn and(self) -> ExprBuilder<ExprBinaryLhsBuilder<F>> {
        self.binary(ast::BinOp_::BiAnd)
    }

    pub fn or(self) -> ExprBuilder<ExprBinaryLhsBuilder<F>> {
        self.binary(ast::BinOp_::BiOr)
    }

    pub fn bit_xor(self) -> ExprBuilder<ExprBinaryLhsBuilder<F>> {
        self.binary(ast::BinOp_::BiBitXor)
    }

    pub fn bit_and(self) -> ExprBuilder<ExprBinaryLhsBuilder<F>> {
        self.binary(ast::BinOp_::BiBitAnd)
    }

    pub fn bit_or(self) -> ExprBuilder<ExprBinaryLhsBuilder<F>> {
        self.binary(ast::BinOp_::BiBitOr)
    }

    pub fn shl(self) -> ExprBuilder<ExprBinaryLhsBuilder<F>> {
        self.binary(ast::BinOp_::BiShl)
    }

    pub fn shr(self) -> ExprBuilder<ExprBinaryLhsBuilder<F>> {
        self.binary(ast::BinOp_::BiShr)
    }

    pub fn eq(self) -> ExprBuilder<ExprBinaryLhsBuilder<F>> {
        self.binary(ast::BinOp_::BiEq)
    }

    pub fn lt(self) -> ExprBuilder<ExprBinaryLhsBuilder<F>> {
        self.binary(ast::BinOp_::BiLt)
    }

    pub fn le(self) -> ExprBuilder<ExprBinaryLhsBuilder<F>> {
        self.binary(ast::BinOp_::BiLe)
    }

    pub fn ne(self) -> ExprBuilder<ExprBinaryLhsBuilder<F>> {
        self.binary(ast::BinOp_::BiNe)
    }

    pub fn ge(self) -> ExprBuilder<ExprBinaryLhsBuilder<F>> {
        self.binary(ast::BinOp_::BiGe)
    }

    pub fn gt(self) -> ExprBuilder<ExprBinaryLhsBuilder<F>> {
        self.binary(ast::BinOp_::BiGt)
    }

    pub fn addr_of(self) -> ExprBuilder<ExprAddrOfBuilder<F>> {
        ExprBuilder::with_callback(ExprAddrOfBuilder {
            builder: self,
            mutability: ast::Mutability::MutImmutable,
        })
    }

    pub fn mut_addr_of(self) -> ExprBuilder<ExprAddrOfBuilder<F>> {
        ExprBuilder::with_callback(ExprAddrOfBuilder {
            builder: self,
            mutability: ast::Mutability::MutMutable,
        })
    }

    pub fn break_(self) -> F::Result {
        self.build_expr_(ast::Expr_::ExprBreak(None))
    }

    pub fn break_to<I>(self, label: I) -> F::Result
        where I: ToIdent,
    {
        let label = respan(self.span, label.to_ident());
        self.build_expr_(ast::Expr_::ExprBreak(Some(label)))
    }

    pub fn continue_(self) -> F::Result {
        self.build_expr_(ast::Expr_::ExprAgain(None))
    }

    pub fn continue_to<I>(self, label: I) -> F::Result
        where I: ToIdent,
    {
        let label = respan(self.span, label.to_ident());
        self.build_expr_(ast::Expr_::ExprAgain(Some(label)))
    }

    pub fn return_(self) -> F::Result {
        self.build_expr_(ast::Expr_::ExprRet(None))
    }

    pub fn return_expr(self) -> ExprBuilder<ExprReturnBuilder<F>> {
        ExprBuilder::with_callback(ExprReturnBuilder {
            builder: self,
        })
    }

    pub fn unit(self) -> F::Result {
        self.tuple().build()
    }

    pub fn tuple(self) -> ExprTupleBuilder<F> {
        ExprTupleBuilder {
            builder: self,
            exprs: Vec::new(),
        }
    }

    pub fn struct_path<P>(self, path: P) -> ExprStructPathBuilder<F>
        where P: IntoPath,
    {
        let span = self.span;
        let path = path.into_path();
        ExprStructPathBuilder {
            builder: self,
            span: span,
            path: path,
            fields: vec![],
        }
    }

    pub fn struct_(self) -> PathBuilder<ExprStructBuilder<F>> {
        PathBuilder::with_callback(ExprStructBuilder {
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

    pub fn some(self) -> ExprBuilder<ExprPathBuilder<F>> {
        let path = PathBuilder::new()
            .global()
            .id("std").id("option").id("Option").id("Some")
            .build();

        ExprBuilder::with_callback(ExprPathBuilder {
            builder: self,
            path: path,
        })
    }

    pub fn ok(self) -> ExprBuilder<ExprPathBuilder<F>> {
        let path = PathBuilder::new()
            .global()
            .id("std").id("result").id("Result").id("Ok")
            .build();

        ExprBuilder::with_callback(ExprPathBuilder {
            builder: self,
            path: path,
        })
    }

    pub fn err(self) -> ExprBuilder<ExprPathBuilder<F>> {
        let path = PathBuilder::new()
            .global()
            .id("std").id("result").id("Result").id("Err")
            .build();

        ExprBuilder::with_callback(ExprPathBuilder {
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

    pub fn call(self) -> ExprBuilder<ExprCallBuilder<F>> {
        ExprBuilder::with_callback(ExprCallBuilder {
            builder: self,
        })
    }

    pub fn method_call<I>(self, id: I) -> ExprBuilder<ExprMethodCallBuilder<F>>
        where I: ToIdent,
    {
        let id = respan(self.span, id.to_ident());
        ExprBuilder::with_callback(ExprMethodCallBuilder {
            builder: self,
            id: id,
        })
    }

    pub fn build_block(self, block: P<ast::Block>) -> F::Result {
        self.build_expr_(ast::ExprBlock(block))
    }

    pub fn block(self) -> BlockBuilder<Self> {
        BlockBuilder::with_callback(self)
    }

    pub fn build_assign(self, lhs: P<ast::Expr>, rhs: P<ast::Expr>) -> F::Result {
        self.build_expr_(ast::ExprAssign(lhs, rhs))
    }

    pub fn assign(self) -> ExprBuilder<ExprAssignBuilder<F>> {
        ExprBuilder::with_callback(ExprAssignBuilder {
            builder: self,
        })
    }

    pub fn build_assign_op(self,
                           binop: ast::BinOp_,
                           lhs: P<ast::Expr>,
                           rhs: P<ast::Expr>) -> F::Result {
        let binop = respan(self.span, binop);
        self.build_expr_(ast::ExprAssignOp(binop, lhs, rhs))
    }

    pub fn assign_op(self, binop: ast::BinOp_) -> ExprBuilder<ExprAssignOpBuilder<F>> {
        ExprBuilder::with_callback(ExprAssignOpBuilder {
            builder: self,
            binop: binop,
        })
    }

    pub fn add_assign(self) -> ExprBuilder<ExprAssignOpBuilder<F>> {
        self.assign_op(ast::BinOp_::BiAdd)
    }

    pub fn sub_assign(self) -> ExprBuilder<ExprAssignOpBuilder<F>> {
        self.assign_op(ast::BinOp_::BiSub)
    }

    pub fn mul_assign(self) -> ExprBuilder<ExprAssignOpBuilder<F>> {
        self.assign_op(ast::BinOp_::BiMul)
    }

    pub fn rem_assign(self) -> ExprBuilder<ExprAssignOpBuilder<F>> {
        self.assign_op(ast::BinOp_::BiRem)
    }

    pub fn and_assign(self) -> ExprBuilder<ExprAssignOpBuilder<F>> {
        self.assign_op(ast::BinOp_::BiAnd)
    }

    pub fn or_assign(self) -> ExprBuilder<ExprAssignOpBuilder<F>> {
        self.assign_op(ast::BinOp_::BiOr)
    }

    pub fn bit_xor_assign(self) -> ExprBuilder<ExprAssignOpBuilder<F>> {
        self.assign_op(ast::BinOp_::BiBitXor)
    }

    pub fn bit_and_assign(self) -> ExprBuilder<ExprAssignOpBuilder<F>> {
        self.assign_op(ast::BinOp_::BiBitAnd)
    }

    pub fn bit_or_assign(self) -> ExprBuilder<ExprAssignOpBuilder<F>> {
        self.assign_op(ast::BinOp_::BiBitOr)
    }

    pub fn bit_shl_assign(self) -> ExprBuilder<ExprAssignOpBuilder<F>> {
        self.assign_op(ast::BinOp_::BiShl)
    }

    pub fn bit_shr_assign(self) -> ExprBuilder<ExprAssignOpBuilder<F>> {
        self.assign_op(ast::BinOp_::BiShr)
    }

    pub fn build_index(self, lhs: P<ast::Expr>, rhs: P<ast::Expr>) -> F::Result {
        self.build_expr_(ast::ExprIndex(lhs, rhs))
    }

    pub fn index(self) -> ExprBuilder<ExprIndexBuilder<F>> {
        ExprBuilder::with_callback(ExprIndexBuilder {
            builder: self,
        })
    }

    pub fn build_repeat(self, lhs: P<ast::Expr>, rhs: P<ast::Expr>) -> F::Result {
        self.build_expr_(ast::ExprRepeat(lhs, rhs))
    }

    pub fn repeat(self) -> ExprBuilder<ExprRepeatBuilder<F>> {
        ExprBuilder::with_callback(ExprRepeatBuilder {
            builder: self,
        })
    }

    pub fn loop_(self) -> ExprLoopBuilder<F> {
        ExprLoopBuilder {
            builder: self,
            label: None,
        }
    }

    pub fn if_(self) -> ExprBuilder<ExprIfBuilder<F>> {
        ExprBuilder::with_callback(ExprIfBuilder {
            builder: self,
        })
    }

    pub fn match_(self) -> ExprBuilder<ExprMatchBuilder<F>> {
        ExprBuilder::with_callback(ExprMatchBuilder {
            builder: self,
        })
    }

    pub fn paren(self) -> ExprBuilder<ExprParenBuilder<F>> {
        ExprBuilder::with_callback(ExprParenBuilder {
            builder: self,
        })
    }

    pub fn field<I>(self, id: I) -> ExprBuilder<ExprFieldBuilder<F>>
        where I: ToIdent,
    {
        let id = respan(self.span, id.to_ident());
        ExprBuilder::with_callback(ExprFieldBuilder {
            builder: self,
            id: id,
        })
    }

    pub fn tup_field(self, index: usize) -> ExprBuilder<ExprTupFieldBuilder<F>> {
        let index = respan(self.span, index);
        ExprBuilder::with_callback(ExprTupFieldBuilder {
            builder: self,
            index: index,
        })
    }

    pub fn box_(self) -> ExprBuilder<ExprPathBuilder<F>> {
        let path = PathBuilder::new()
            .global()
            .id("std").id("boxed").id("Box").id("new")
            .build();

        ExprBuilder::with_callback(ExprPathBuilder {
            builder: self,
            path: path,
        })
    }

    pub fn rc(self) -> ExprBuilder<ExprPathBuilder<F>> {
        let path = PathBuilder::new()
            .global()
            .id("std").id("rc").id("Rc").id("new")
            .build();

        ExprBuilder::with_callback(ExprPathBuilder {
            builder: self,
            path: path,
        })
    }

    pub fn arc(self) -> ExprBuilder<ExprPathBuilder<F>> {
        let path = PathBuilder::new()
            .global()
            .id("std").id("arc").id("Arc").id("new")
            .build();

        ExprBuilder::with_callback(ExprPathBuilder {
            builder: self,
            path: path,
        })
    }

    pub fn default(self) -> F::Result {
        let path = PathBuilder::new()
            .global()
            .ids(&["std", "default", "Default", "default"])
            .build();

        self.call()
            .build_path(path)
            .build()
    }

    pub fn slice(self) -> ExprSliceBuilder<F> {
        ExprSliceBuilder {
            builder: self,
            exprs: Vec::new(),
        }
    }

    pub fn vec(self) -> ExprSliceBuilder<ExprVecBuilder<F>> {
        ExprBuilder::with_callback(ExprVecBuilder {
            builder: self,
        }).slice()
    }

    /// Represents an equivalent to `try!(...)`.
    pub fn try(self) -> ExprBuilder<ExprTryBuilder<F>> {
        let span = self.span;

        ExprBuilder::with_callback(ExprTryBuilder {
            builder: self,
        }).span(span)
    }
}

impl<F> Invoke<ast::Attribute> for ExprBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = Self;

    fn invoke(self, attr: ast::Attribute) -> Self {
        self.with_attr(attr)
    }
}

impl<F> Invoke<P<ast::Lit>> for ExprBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, lit: P<ast::Lit>) -> F::Result {
        self.build_lit(lit)
    }
}

impl<F> Invoke<ast::Path> for ExprBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, path: ast::Path) -> F::Result {
        self.build_path(path)
    }
}

impl<F> Invoke<(ast::QSelf, ast::Path)> for ExprBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, (qself, path): (ast::QSelf, ast::Path)) -> F::Result {
        self.build_qpath(qself, path)
    }
}

impl<F> Invoke<P<ast::Block>> for ExprBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, block: P<ast::Block>) -> F::Result {
        self.build_block(block)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprUnaryBuilder<F> {
    builder: ExprBuilder<F>,
    unop: ast::UnOp,
}

impl<F> Invoke<P<ast::Expr>> for ExprUnaryBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.builder.build_unary(self.unop, expr)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprBinaryLhsBuilder<F> {
    builder: ExprBuilder<F>,
    binop: ast::BinOp_,
}

impl<F> Invoke<P<ast::Expr>> for ExprBinaryLhsBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = ExprBuilder<ExprBinaryRhsBuilder<F>>;

    fn invoke(self, lhs: P<ast::Expr>) -> ExprBuilder<ExprBinaryRhsBuilder<F>> {
        ExprBuilder::with_callback(ExprBinaryRhsBuilder {
            builder: self.builder,
            binop: self.binop,
            lhs: lhs,
        })
    }
}

pub struct ExprBinaryRhsBuilder<F> {
    builder: ExprBuilder<F>,
    binop: ast::BinOp_,
    lhs: P<ast::Expr>,
}

impl<F> Invoke<P<ast::Expr>> for ExprBinaryRhsBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, rhs: P<ast::Expr>) -> F::Result {
        self.builder.build_binary(self.binop, self.lhs, rhs)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprReturnBuilder<F> {
    builder: ExprBuilder<F>,
}

impl<F> Invoke<P<ast::Expr>> for ExprReturnBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.builder.build_expr_(ast::Expr_::ExprRet(Some(expr)))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprTupleBuilder<F> {
    builder: ExprBuilder<F>,
    exprs: Vec<P<ast::Expr>>,
}

impl<F: Invoke<P<ast::Expr>>> ExprTupleBuilder<F>
    where F: Invoke<P<ast::Expr>>
{
    pub fn with_exprs<I>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=P<ast::Expr>>,
    {
        self.exprs.extend(iter);
        self
    }

    pub fn expr(self) -> ExprBuilder<Self> {
        ExprBuilder::with_callback(self)
    }

    pub fn build(self) -> F::Result {
        self.builder.build_expr_(ast::ExprTup(self.exprs))
    }
}

impl<F> Invoke<P<ast::Expr>> for ExprTupleBuilder<F>
    where F: Invoke<P<ast::Expr>>
{
    type Result = ExprTupleBuilder<F>;

    fn invoke(mut self, expr: P<ast::Expr>) -> Self {
        self.exprs.push(expr);
        self
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprStructBuilder<F> {
    builder: ExprBuilder<F>,
}

impl<F> Invoke<ast::Path> for ExprStructBuilder<F>
    where F: Invoke<P<ast::Expr>>
{
    type Result = ExprStructPathBuilder<F>;

    fn invoke(self, path: ast::Path) -> ExprStructPathBuilder<F> {
        self.builder.struct_path(path)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprStructPathBuilder<F> {
    builder: ExprBuilder<F>,
    span: Span,
    path: ast::Path,
    fields: Vec<ast::Field>,
}

impl<F> ExprStructPathBuilder<F>
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

    pub fn with_id_exprs<I>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=(ast::Ident, P<ast::Expr>)>,
    {
        for (id, expr) in iter {
            self = self.field(id).build(expr);
        }

        self
    }

    pub fn field<I>(self, id: I) -> ExprBuilder<ExprStructFieldBuilder<I, F>>
        where I: ToIdent,
    {
        ExprBuilder::with_callback(ExprStructFieldBuilder {
            builder: self,
            id: id,
        })
    }

    pub fn build_with(self) -> ExprBuilder<Self> {
        ExprBuilder::with_callback(self)
    }

    pub fn build(self) -> F::Result {
        let expr_ = ast::ExprStruct(self.path, self.fields, None);
        self.builder.build_expr_(expr_)
    }
}

impl<F> Invoke<P<ast::Expr>> for ExprStructPathBuilder<F>
    where F: Invoke<P<ast::Expr>>
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        let expr_ = ast::ExprStruct(self.path, self.fields, Some(expr));
        self.builder.build_expr_(expr_)
    }
}

pub struct ExprStructFieldBuilder<I, F> {
    builder: ExprStructPathBuilder<F>,
    id: I,
}

impl<I, F> Invoke<P<ast::Expr>> for ExprStructFieldBuilder<I, F>
    where I: ToIdent,
          F: Invoke<P<ast::Expr>>,
{
    type Result = ExprStructPathBuilder<F>;

    fn invoke(mut self, expr: P<ast::Expr>) -> ExprStructPathBuilder<F> {
        let field = ast::Field {
            ident: respan(self.builder.span, self.id.to_ident()),
            expr: expr,
            span: self.builder.span,
        };
        self.builder.fields.push(field);
        self.builder
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprCallBuilder<F> {
    builder: ExprBuilder<F>,
}

impl<F> Invoke<P<ast::Expr>> for ExprCallBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = ExprCallArgsBuilder<F>;

    fn invoke(self, expr: P<ast::Expr>) -> ExprCallArgsBuilder<F> {
        ExprCallArgsBuilder {
            builder: self.builder,
            fn_: expr,
            args: vec![],
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprCallArgsBuilder<F> {
    builder: ExprBuilder<F>,
    fn_: P<ast::Expr>,
    args: Vec<P<ast::Expr>>,
}

impl<F> ExprCallArgsBuilder<F>
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

    pub fn arg(self) -> ExprBuilder<Self> {
        ExprBuilder::with_callback(self)
    }

    pub fn build(self) -> F::Result {
        self.builder.build_expr_(ast::ExprCall(self.fn_, self.args))
    }
}

impl<F> Invoke<P<ast::Expr>> for ExprCallArgsBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = Self;

    fn invoke(self, arg: P<ast::Expr>) -> Self {
        self.with_arg(arg)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprMethodCallBuilder<F> {
    builder: ExprBuilder<F>,
    id: ast::SpannedIdent,
}

impl<F> Invoke<P<ast::Expr>> for ExprMethodCallBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = ExprMethodCallArgsBuilder<F>;

    fn invoke(self, expr: P<ast::Expr>) -> ExprMethodCallArgsBuilder<F> {
        ExprMethodCallArgsBuilder {
            builder: self.builder,
            id: self.id,
            tys: vec![],
            args: vec![expr],
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprMethodCallArgsBuilder<F> {
    builder: ExprBuilder<F>,
    id: ast::SpannedIdent,
    tys: Vec<P<ast::Ty>>,
    args: Vec<P<ast::Expr>>,
}

impl<F> ExprMethodCallArgsBuilder<F>
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

    pub fn ty(self) -> TyBuilder<Self> {
        TyBuilder::with_callback(self)
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

    pub fn arg(self) -> ExprBuilder<Self> {
        ExprBuilder::with_callback(self)
    }

    pub fn build(self) -> F::Result {
        self.builder.build_expr_(ast::ExprMethodCall(self.id, self.tys, self.args))
    }
}

impl<F> Invoke<P<ast::Ty>> for ExprMethodCallArgsBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = Self;

    fn invoke(self, ty: P<ast::Ty>) -> Self {
        self.with_ty(ty)
    }
}

impl<F> Invoke<P<ast::Expr>> for ExprMethodCallArgsBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = Self;

    fn invoke(self, arg: P<ast::Expr>) -> Self {
        self.with_arg(arg)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprAddrOfBuilder<F> {
    builder: ExprBuilder<F>,
    mutability: ast::Mutability,
}

impl<F> Invoke<P<ast::Expr>> for ExprAddrOfBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.builder.build_expr_(ast::ExprAddrOf(self.mutability, expr))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprPathBuilder<F> {
    builder: ExprBuilder<F>,
    path: ast::Path,
}

impl<F> Invoke<P<ast::Expr>> for ExprPathBuilder<F>
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

pub struct ExprAssignBuilder<F> {
    builder: ExprBuilder<F>,
}

impl<F> Invoke<P<ast::Expr>> for ExprAssignBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = ExprBuilder<ExprAssignLhsBuilder<F>>;

    fn invoke(self, lhs: P<ast::Expr>) -> ExprBuilder<ExprAssignLhsBuilder<F>> {
        ExprBuilder::with_callback(ExprAssignLhsBuilder {
            builder: self.builder,
            lhs: lhs,
        })
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprAssignLhsBuilder<F> {
    builder: ExprBuilder<F>,
    lhs: P<ast::Expr>,
}

impl<F> Invoke<P<ast::Expr>> for ExprAssignLhsBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, rhs: P<ast::Expr>) -> F::Result {
        self.builder.build_assign(self.lhs, rhs)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprAssignOpBuilder<F> {
    builder: ExprBuilder<F>,
    binop: ast::BinOp_,
}

impl<F> Invoke<P<ast::Expr>> for ExprAssignOpBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = ExprBuilder<ExprAssignOpLhsBuilder<F>>;

    fn invoke(self, lhs: P<ast::Expr>) -> ExprBuilder<ExprAssignOpLhsBuilder<F>> {
        ExprBuilder::with_callback(ExprAssignOpLhsBuilder {
            builder: self.builder,
            binop: self.binop,
            lhs: lhs,
        })
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprAssignOpLhsBuilder<F> {
    builder: ExprBuilder<F>,
    binop: ast::BinOp_,
    lhs: P<ast::Expr>,
}

impl<F> Invoke<P<ast::Expr>> for ExprAssignOpLhsBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, rhs: P<ast::Expr>) -> F::Result {
        self.builder.build_assign_op(self.binop, self.lhs, rhs)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprIndexBuilder<F> {
    builder: ExprBuilder<F>,
}

impl<F> Invoke<P<ast::Expr>> for ExprIndexBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = ExprBuilder<ExprIndexLhsBuilder<F>>;

    fn invoke(self, lhs: P<ast::Expr>) -> ExprBuilder<ExprIndexLhsBuilder<F>> {
        ExprBuilder::with_callback(ExprIndexLhsBuilder {
            builder: self.builder,
            lhs: lhs,
        })
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprIndexLhsBuilder<F> {
    builder: ExprBuilder<F>,
    lhs: P<ast::Expr>,
}

impl<F> Invoke<P<ast::Expr>> for ExprIndexLhsBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, rhs: P<ast::Expr>) -> F::Result {
        self.builder.build_index(self.lhs, rhs)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprRepeatBuilder<F> {
    builder: ExprBuilder<F>,
}

impl<F> Invoke<P<ast::Expr>> for ExprRepeatBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = ExprBuilder<ExprRepeatLhsBuilder<F>>;

    fn invoke(self, lhs: P<ast::Expr>) -> ExprBuilder<ExprRepeatLhsBuilder<F>> {
        ExprBuilder::with_callback(ExprRepeatLhsBuilder {
            builder: self.builder,
            lhs: lhs,
        })
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprRepeatLhsBuilder<F> {
    builder: ExprBuilder<F>,
    lhs: P<ast::Expr>,
}

impl<F> Invoke<P<ast::Expr>> for ExprRepeatLhsBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, rhs: P<ast::Expr>) -> F::Result {
        self.builder.build_repeat(self.lhs, rhs)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprLoopBuilder<F> {
    builder: ExprBuilder<F>,
    label: Option<ast::Ident>,
}

impl<F> ExprLoopBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    pub fn label<I>(mut self, id: I) -> Self
        where I: ToIdent,
    {
        self.label = Some(id.to_ident());
        self
    }

    pub fn block(self) -> BlockBuilder<Self> {
        BlockBuilder::with_callback(self)
    }
}

impl<F> Invoke<P<ast::Block>> for ExprLoopBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, block: P<ast::Block>) -> F::Result {
        self.builder.build_expr_(ast::ExprLoop(block, self.label))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprIfBuilder<F> {
    builder: ExprBuilder<F>,
}

impl<F> Invoke<P<ast::Expr>> for ExprIfBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = ExprIfThenBuilder<F>;

    fn invoke(self, condition: P<ast::Expr>) -> ExprIfThenBuilder<F> {
        ExprIfThenBuilder {
            builder: self.builder,
            condition: condition,
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprIfThenBuilder<F> {
    builder: ExprBuilder<F>,
    condition: P<ast::Expr>,
}

impl<F> ExprIfThenBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    pub fn build_then(self, block: P<ast::Block>) -> ExprIfThenElseBuilder<F> {
        ExprIfThenElseBuilder {
            builder: self.builder,
            condition: self.condition,
            then: block,
            else_ifs: Vec::new(),
        }
    }

    pub fn then(self) -> BlockBuilder<Self> {
        BlockBuilder::with_callback(self)
    }
}

impl<F> Invoke<P<ast::Block>> for ExprIfThenBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = ExprIfThenElseBuilder<F>;

    fn invoke(self, block: P<ast::Block>) -> ExprIfThenElseBuilder<F> {
        self.build_then(block)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprIfThenElseBuilder<F> {
    builder: ExprBuilder<F>,
    condition: P<ast::Expr>,
    then: P<ast::Block>,
    else_ifs: Vec<(P<ast::Expr>, P<ast::Block>)>,
}

impl<F> ExprIfThenElseBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    pub fn else_if(self) -> ExprBuilder<ExprElseIfBuilder<F>> {
        ExprBuilder::with_callback(ExprElseIfBuilder {
            builder: self,
        })
    }

    fn build_else_expr(self, mut else_: P<ast::Expr>) -> F::Result {
        for (cond, block) in self.else_ifs.into_iter().rev() {
            else_ = ExprBuilder::new().if_()
                .build(cond)
                .build_then(block)
                .build_else_expr(else_);
        }

        self.builder.build_expr_(ast::ExprIf(self.condition, self.then, Some(else_)))
    }

    pub fn build_else(self, block: P<ast::Block>) -> F::Result {
        let else_ = ExprBuilder::new().build_block(block);
        self.build_else_expr(else_)
    }

    pub fn else_(self) -> BlockBuilder<Self> {
        BlockBuilder::with_callback(self)
    }

    pub fn build(self) -> F::Result {
        let mut else_ifs = self.else_ifs.into_iter().rev();

        let else_ = match else_ifs.next() {
            Some((cond, block)) => {
                let mut else_ = ExprBuilder::new().if_()
                    .build(cond)
                    .build_then(block)
                    .build();

                for (cond, block) in else_ifs.into_iter().rev() {
                    else_ = ExprBuilder::new().if_()
                        .build(cond)
                        .build_then(block)
                        .build_else_expr(else_);
                }

                Some(else_)
            }
            None => None
        };

        self.builder.build_expr_(ast::ExprIf(self.condition, self.then, else_))
    }
}

impl<F> Invoke<P<ast::Block>> for ExprIfThenElseBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, block: P<ast::Block>) -> F::Result {
        self.build_else(block)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprElseIfBuilder<F> {
    builder: ExprIfThenElseBuilder<F>,
}

impl<F> Invoke<P<ast::Expr>> for ExprElseIfBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = ExprElseIfThenBuilder<F>;

    fn invoke(self, expr: P<ast::Expr>) -> ExprElseIfThenBuilder<F> {
        ExprElseIfThenBuilder {
            builder: self.builder,
            condition: expr,
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprElseIfThenBuilder<F> {
    builder: ExprIfThenElseBuilder<F>,
    condition: P<ast::Expr>,
}

impl<F> ExprElseIfThenBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    pub fn build_then(mut self, block: P<ast::Block>) -> ExprIfThenElseBuilder<F> {
        self.builder.else_ifs.push((self.condition, block));
        self.builder
    }

    pub fn then(self) -> BlockBuilder<Self> {
        BlockBuilder::with_callback(self)
    }
}

impl<F> Invoke<P<ast::Block>> for ExprElseIfThenBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = ExprIfThenElseBuilder<F>;

    fn invoke(self, block: P<ast::Block>) -> ExprIfThenElseBuilder<F> {
        self.build_then(block)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprMatchBuilder<F> {
    builder: ExprBuilder<F>,
}

impl<F> Invoke<P<ast::Expr>> for ExprMatchBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = ExprMatchArmBuilder<F>;

    fn invoke(self, expr: P<ast::Expr>) -> ExprMatchArmBuilder<F> {
        ExprMatchArmBuilder {
            builder: self.builder,
            expr: expr,
            arms: Vec::new(),
        }
    }
}

/////////////////////////////////////////////////////////////////////////////

pub struct ExprMatchArmBuilder<F> {
    builder: ExprBuilder<F>,
    expr: P<ast::Expr>,
    arms: Vec<ast::Arm>,
}

impl<F> ExprMatchArmBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    pub fn with_arms<I>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=ast::Arm>,
    {
        self.arms.extend(iter);
        self
    }

    pub fn with_arm(mut self, arm: ast::Arm) -> Self {
        self.arms.push(arm);
        self
    }

    pub fn arm(self) -> ArmBuilder<Self> {
        ArmBuilder::with_callback(self)
    }

    pub fn build(self) -> F::Result {
        self.builder.build_expr_(ast::ExprMatch(self.expr, self.arms))
    }
}

impl<F> Invoke<ast::Arm> for ExprMatchArmBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = Self;

    fn invoke(self, arm: ast::Arm) -> Self {
        self.with_arm(arm)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprParenBuilder<F> {
    builder: ExprBuilder<F>,
}

impl<F> Invoke<P<ast::Expr>> for ExprParenBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.builder.build_expr_(ast::ExprParen(expr))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprFieldBuilder<F> {
    builder: ExprBuilder<F>,
    id: ast::SpannedIdent,
}

impl<F> Invoke<P<ast::Expr>> for ExprFieldBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.builder.build_expr_(ast::ExprField(expr, self.id))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprTupFieldBuilder<F> {
    builder: ExprBuilder<F>,
    index: Spanned<usize>,
}

impl<F> Invoke<P<ast::Expr>> for ExprTupFieldBuilder<F>
    where F: Invoke<P<ast::Expr>>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.builder.build_expr_(ast::ExprTupField(expr, self.index))
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprSliceBuilder<F> {
    builder: ExprBuilder<F>,
    exprs: Vec<P<ast::Expr>>,
}

impl<F: Invoke<P<ast::Expr>>> ExprSliceBuilder<F>
    where F: Invoke<P<ast::Expr>>
{
    pub fn with_exprs<I>(mut self, iter: I) -> Self
        where I: IntoIterator<Item=P<ast::Expr>>,
    {
        self.exprs.extend(iter);
        self
    }

    pub fn expr(self) -> ExprBuilder<Self> {
        ExprBuilder::with_callback(self)
    }

    pub fn build(self) -> F::Result {
        self.builder.build_expr_(ast::ExprVec(self.exprs))
    }
}

impl<F> Invoke<P<ast::Expr>> for ExprSliceBuilder<F>
    where F: Invoke<P<ast::Expr>>
{
    type Result = ExprSliceBuilder<F>;

    fn invoke(mut self, expr: P<ast::Expr>) -> Self {
        self.exprs.push(expr);
        self
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprVecBuilder<F> {
    builder: ExprBuilder<F>,
}

impl<F> Invoke<P<ast::Expr>> for ExprVecBuilder<F>
    where F: Invoke<P<ast::Expr>>
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        let qpath = ExprBuilder::new().qpath()
            .ty().slice().infer()
            .id("into_vec");

        self.builder.call()
            .build(qpath)
            .arg().box_().build(expr)
            .build()
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ExprTryBuilder<F> {
    builder: ExprBuilder<F>,
}

impl<F> Invoke<P<ast::Expr>> for ExprTryBuilder<F>
    where F: Invoke<P<ast::Expr>>
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        let ok_path = PathBuilder::new()
            .global()
            .ids(&["std", "result", "Result", "Ok"])
            .build();

        let ok_pat = PatBuilder::new()
            .enum_().build(ok_path)
            .id("value")
            .build();

        let ok_arm = ArmBuilder::new()
            .pat().build(ok_pat)
            .body()
                .id("value");

        let err_path = PathBuilder::new()
            .global()
            .ids(&["std", "result", "Result", "Err"])
            .build();

        let err_pat = PatBuilder::new()
            .enum_().build(err_path.clone())
            .id("value")
            .build();

        let err_arm = ArmBuilder::new()
            .pat().build(err_pat)
            .body()
                .return_expr()
                .call()
                    .build_path(err_path)
                    .arg().id("value")
                    .build();

        self.builder.match_().build(expr)
            .with_arm(ok_arm)
            .with_arm(err_arm)
            .build()
    }
}
