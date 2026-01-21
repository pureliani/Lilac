use crate::{
    ast::expr::Expr,
    hir::builders::{Builder, InBlock, ValueId},
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_access_expr(&mut self, expr: Expr) -> ValueId {
        let s = expr.span.clone();
        match self.build_place(expr) {
            Ok(place) => self.read_place(place, s),
            Err(e) => self.report_error_and_get_poison(e),
        }
    }
}
