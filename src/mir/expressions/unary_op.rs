use crate::{
    ast::expr::Expr,
    mir::{
        builders::{Builder, InBlock, ValueId},
        types::checked_type::SpannedType,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_not_expr(
        &mut self,
        right: Expr,
        expected_type: Option<&SpannedType>,
    ) -> ValueId {
        let span = right.span.clone();
        let expected_right_type = SpannedType {
            id: self.types.bool(None),
            span: span.clone(),
        };
        let operand_id = self.build_expr(right, Some(&expected_right_type));
        let result = self.not(operand_id);
        self.check_expected(result, span, expected_type)
    }

    pub fn build_neg_expr(
        &mut self,
        right: Expr,
        expected_type: Option<&SpannedType>,
    ) -> ValueId {
        let span = right.span.clone();
        let operand_id = self.build_expr(right, expected_type);
        let result = self.neg(operand_id);
        self.check_expected(result, span, expected_type)
    }
}
