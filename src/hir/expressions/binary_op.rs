use crate::{
    ast::expr::Expr,
    hir::builders::{Builder, InBlock, ValueId},
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_add_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        let left_value = self.build_expr(left);
        let right_value = self.build_expr(right);
        self.emit_add(left_value, right_value)
    }

    pub fn build_sub_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        let left_value = self.build_expr(left);
        let right_value = self.build_expr(right);
        self.emit_sub(left_value, right_value)
    }

    pub fn build_mul_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        let left_value = self.build_expr(left);
        let right_value = self.build_expr(right);
        self.emit_mul(left_value, right_value)
    }

    pub fn build_div_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        let left_value = self.build_expr(left);
        let right_value = self.build_expr(right);
        self.emit_div(left_value, right_value)
    }

    pub fn build_mod_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        let left_value = self.build_expr(left);
        let right_value = self.build_expr(right);
        self.emit_rem(left_value, right_value)
    }

    pub fn build_eq_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        let left_value = self.build_expr(left);
        let right_value = self.build_expr(right);
        self.emit_eq(left_value, right_value)
    }

    pub fn build_neq_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        let left_value = self.build_expr(left);
        let right_value = self.build_expr(right);
        self.emit_neq(left_value, right_value)
    }

    pub fn build_lt_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        let left_value = self.build_expr(left);
        let right_value = self.build_expr(right);
        self.emit_lt(left_value, right_value)
    }

    pub fn build_lte_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        let left_value = self.build_expr(left);
        let right_value = self.build_expr(right);
        self.emit_lte(left_value, right_value)
    }

    pub fn build_gt_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        let left_value = self.build_expr(left);
        let right_value = self.build_expr(right);
        self.emit_gt(left_value, right_value)
    }

    pub fn build_gte_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        let left_value = self.build_expr(left);
        let right_value = self.build_expr(right);
        self.emit_gte(left_value, right_value)
    }
}
