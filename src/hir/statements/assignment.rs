use crate::{
    ast::expr::Expr,
    hir::builders::{Builder, InBlock},
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_assignment_stmt(&mut self, target: Expr, value: Expr) {
        let target_span = target.span.clone();
        let source_val = self.build_expr(value);
    }
}
