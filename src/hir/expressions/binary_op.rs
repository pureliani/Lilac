use crate::{
    ast::{expr::Expr, Span},
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::SemanticError,
    },
};

impl<'a> Builder<'a, InBlock> {
    fn build_binary_expr<F>(
        &mut self,
        left: Box<Expr>,
        right: Box<Expr>,
        op: F,
    ) -> Result<ValueId, SemanticError>
    where
        F: FnOnce(
            &mut Self,
            ValueId,
            Span,
            ValueId,
            Span,
        ) -> Result<ValueId, SemanticError>,
    {
        let l_span = left.span.clone();
        let r_span = right.span.clone();

        let lhs = self.build_expr(*left)?;
        let rhs = self.build_expr(*right)?;

        op(self, lhs, l_span, rhs, r_span)
    }

    pub fn build_add_expr(
        &mut self,
        left: Box<Expr>,
        right: Box<Expr>,
    ) -> Result<ValueId, SemanticError> {
        self.build_binary_expr(left, right, |builder, l, ls, r, rs| {
            builder.add(l, ls, r, rs)
        })
    }

    pub fn build_sub_expr(
        &mut self,
        left: Box<Expr>,
        right: Box<Expr>,
    ) -> Result<ValueId, SemanticError> {
        self.build_binary_expr(left, right, |b, l, ls, r, rs| b.sub(l, ls, r, rs))
    }

    pub fn build_mul_expr(
        &mut self,
        left: Box<Expr>,
        right: Box<Expr>,
    ) -> Result<ValueId, SemanticError> {
        self.build_binary_expr(left, right, |b, l, ls, r, rs| b.mul(l, ls, r, rs))
    }

    pub fn build_div_expr(
        &mut self,
        left: Box<Expr>,
        right: Box<Expr>,
    ) -> Result<ValueId, SemanticError> {
        self.build_binary_expr(left, right, |b, l, ls, r, rs| b.div(l, ls, r, rs))
    }

    pub fn build_mod_expr(
        &mut self,
        left: Box<Expr>,
        right: Box<Expr>,
    ) -> Result<ValueId, SemanticError> {
        self.build_binary_expr(left, right, |b, l, ls, r, rs| b.rem(l, ls, r, rs))
    }

    pub fn build_eq_expr(
        &mut self,
        left: Box<Expr>,
        right: Box<Expr>,
    ) -> Result<ValueId, SemanticError> {
        self.build_binary_expr(left, right, |b, l, ls, r, rs| b.eq(l, ls, r, rs))
    }

    pub fn build_neq_expr(
        &mut self,
        left: Box<Expr>,
        right: Box<Expr>,
    ) -> Result<ValueId, SemanticError> {
        self.build_binary_expr(left, right, |b, l, ls, r, rs| b.ne(l, ls, r, rs))
    }

    pub fn build_lt_expr(
        &mut self,
        left: Box<Expr>,
        right: Box<Expr>,
    ) -> Result<ValueId, SemanticError> {
        self.build_binary_expr(left, right, |b, l, ls, r, rs| b.lt(l, ls, r, rs))
    }

    pub fn build_lte_expr(
        &mut self,
        left: Box<Expr>,
        right: Box<Expr>,
    ) -> Result<ValueId, SemanticError> {
        self.build_binary_expr(left, right, |b, l, ls, r, rs| b.le(l, ls, r, rs))
    }

    pub fn build_gt_expr(
        &mut self,
        left: Box<Expr>,
        right: Box<Expr>,
    ) -> Result<ValueId, SemanticError> {
        self.build_binary_expr(left, right, |b, l, ls, r, rs| b.gt(l, ls, r, rs))
    }

    pub fn build_gte_expr(
        &mut self,
        left: Box<Expr>,
        right: Box<Expr>,
    ) -> Result<ValueId, SemanticError> {
        self.build_binary_expr(left, right, |b, l, ls, r, rs| b.ge(l, ls, r, rs))
    }
}
