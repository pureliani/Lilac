use crate::{
    ast::{expr::Expr, Span},
    hir::builders::{Builder, InBlock, ValueId},
};

impl<'a> Builder<'a, InBlock> {
    fn build_binary_op<F>(&mut self, left: Expr, right: Expr, emit_fn: F) -> ValueId
    where
        F: FnOnce(&mut Self, ValueId, Span, ValueId, Span) -> ValueId,
    {
        let left_span = left.span.clone();
        let left_value = self.build_expr(left);

        let right_span = right.span.clone();
        let right_value = self.build_expr(right);

        emit_fn(self, left_value, left_span, right_value, right_span)
    }

    pub fn build_add_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        self.build_binary_op(left, right, Self::emit_add)
    }

    pub fn build_sub_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        self.build_binary_op(left, right, Self::emit_sub)
    }

    pub fn build_mul_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        self.build_binary_op(left, right, Self::emit_mul)
    }

    pub fn build_div_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        self.build_binary_op(left, right, Self::emit_div)
    }

    pub fn build_mod_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        self.build_binary_op(left, right, Self::emit_rem)
    }

    pub fn build_eq_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        self.build_binary_op(left, right, Self::emit_eq)
    }

    pub fn build_neq_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        self.build_binary_op(left, right, Self::emit_neq)
    }

    pub fn build_lt_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        self.build_binary_op(left, right, Self::emit_lt)
    }

    pub fn build_lte_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        self.build_binary_op(left, right, Self::emit_lte)
    }

    pub fn build_gt_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        self.build_binary_op(left, right, Self::emit_gt)
    }

    pub fn build_gte_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        self.build_binary_op(left, right, Self::emit_gte)
    }
}
