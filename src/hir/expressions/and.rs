use crate::{
    ast::expr::Expr,
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::SemanticError,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_and_expr(
        &mut self,
        left: Expr,
        right: Expr,
    ) -> Result<ValueId, SemanticError> {
        let left_span = left.span.clone();
        let left_id = self.build_expr(left)?;

        self.emit_logical_and(left_id, left_span, |builder| builder.build_expr(right))
    }
}
