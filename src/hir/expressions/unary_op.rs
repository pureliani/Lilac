use crate::{
    ast::expr::Expr,
    hir::{
        builders::{Builder, InBlock, TypePredicate, ValueId},
        errors::SemanticError,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_not_expr(&mut self, expr: Expr) -> Result<ValueId, SemanticError> {
        let span = expr.span.clone();
        let operand_id = self.build_expr(expr)?;

        let result_id = self.not(operand_id, span)?;

        if let Some(pred) = self.type_predicates.get(&operand_id).cloned() {
            self.type_predicates.insert(
                result_id,
                TypePredicate {
                    place: pred.place,
                    on_true_type: pred.on_false_type,
                    on_false_type: pred.on_true_type,
                },
            );
        }

        Ok(result_id)
    }

    pub fn build_neg_expr(&mut self, expr: Expr) -> Result<ValueId, SemanticError> {
        let span = expr.span.clone();
        let operand_id = self.build_expr(expr)?;

        self.neg(operand_id, span)
    }
}
