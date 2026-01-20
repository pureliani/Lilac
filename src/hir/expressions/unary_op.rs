use crate::{
    ast::expr::Expr,
    hir::builders::{Builder, InBlock, TypePredicate, ValueId},
    unwrap_or_poison,
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_not_expr(&mut self, expr: Box<Expr>) -> ValueId {
        let span = expr.span.clone();
        let operand_id = self.build_expr(*expr);

        let result_id = unwrap_or_poison!(self, self.not(operand_id, span));

        let predicate = self.get_fn().predicates.get(&operand_id).cloned();
        if let Some(pred) = predicate {
            self.get_fn().predicates.insert(
                result_id,
                TypePredicate {
                    source: pred.source,
                    true_id: pred.false_id,
                    false_id: pred.true_id,
                },
            );
        }

        result_id
    }

    pub fn build_neg_expr(&mut self, expr: Box<Expr>) -> ValueId {
        let span = expr.span.clone();
        let operand_id = self.build_expr(*expr);

        unwrap_or_poison!(self, self.neg(operand_id, span))
    }
}
