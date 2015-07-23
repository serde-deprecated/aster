use syntax::ast;
use syntax::ptr::P;

use expr::ExprBuilder;
use invoke::{Invoke, Identity};
use ty::TyBuilder;

pub struct Const {
    pub ty: P<ast::Ty>,
    pub expr: P<ast::Expr>,
}

//////////////////////////////////////////////////////////////////////////////

pub struct ConstBuilder<F=Identity> {
    callback: F
}

impl<F> ConstBuilder<F>
    where F: Invoke<Const>,
{
    pub fn new_with_callback(callback: F) -> Self
        where F: Invoke<Const>,
    {
        ConstBuilder {
            callback: callback,
        }
    }

    pub fn ty(self) -> TyBuilder<Self> {
        TyBuilder::new_with_callback(self)
    }
}

impl<F> Invoke<P<ast::Ty>> for ConstBuilder<F>
    where F: Invoke<Const>,
{
    type Result = ExprBuilder<ConstExprBuilder<F>>;

    fn invoke(self, ty: P<ast::Ty>) -> ExprBuilder<ConstExprBuilder<F>> {
        ExprBuilder::new_with_callback(ConstExprBuilder {
            builder: self,
            ty: ty,
        })
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct ConstExprBuilder<F> {
    builder: ConstBuilder<F>,
    ty: P<ast::Ty>,
}

impl<F> Invoke<P<ast::Expr>> for ConstExprBuilder<F>
    where F: Invoke<Const>,
{
    type Result = F::Result;

    fn invoke(self, expr: P<ast::Expr>) -> F::Result {
        self.builder.callback.invoke(Const {
            ty: self.ty,
            expr: expr,
        })
    }
}
