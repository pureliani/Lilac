use crate::{
    ast::expr::Expr,
    hir::{
        builders::{Builder, InBlock, TypePredicate, ValueId},
        types::checked_type::{SpannedType, Type},
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_not_expr(
        &mut self,
        right: Expr,
        expected_type: Option<&SpannedType>,
    ) -> ValueId {
        let right_span = right.span.clone();
        let expected_right_type = SpannedType {
            kind: Type::Bool,
            span: right_span.clone(),
        };
        let operand_id = self.build_expr(right, Some(&expected_right_type));
        let result_id = self.emit_not(operand_id, right_span.clone());

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

        self.check_expected(result_id, right_span, expected_type)
    }

    pub fn build_neg_expr(
        &mut self,
        right: Expr,
        expected_type: Option<&SpannedType>,
    ) -> ValueId {
        let span = right.span.clone();
        let operand_id = self.build_expr(right, None);
        let result = self.emit_neg(operand_id, span.clone());
        self.check_expected(result, span, expected_type)
    }
}
