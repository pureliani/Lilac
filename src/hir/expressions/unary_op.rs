use crate::{
    ast::expr::Expr,
    hir::builders::{Builder, InBlock, TypePredicate, ValueId},
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_not_expr(&mut self, expr: Expr) -> ValueId {
        let span = expr.span.clone();
        let operand_id = self.build_expr(expr);

        let result_id = self.emit_not(operand_id, span);

        if let Some(preds) = self.type_predicates.get(&operand_id).cloned() {
            let flipped: Vec<TypePredicate> = preds
                .into_iter()
                .map(|pred| TypePredicate {
                    decl_id: pred.decl_id,
                    on_true_type: pred.on_false_type,
                    on_false_type: pred.on_true_type,
                })
                .collect();

            self.type_predicates.insert(result_id, flipped);
        }

        result_id
    }

    pub fn build_neg_expr(&mut self, expr: Expr) -> ValueId {
        let span = expr.span.clone();
        let operand_id = self.build_expr(expr);

        self.emit_neg(operand_id, span)
    }
}
