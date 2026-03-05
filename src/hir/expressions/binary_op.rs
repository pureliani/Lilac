use crate::{
    ast::{expr::Expr, Span},
    hir::{
        builders::{Builder, InBlock, ValueId},
        types::checked_type::SpannedType,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_binary_op<F>(
        &mut self,
        left: Expr,
        right: Expr,
        emit_fn: F,
        expected_type: Option<&SpannedType>,
    ) -> ValueId
    where
        F: FnOnce(&mut Self, ValueId, Span, ValueId, Span) -> ValueId,
    {
        let left_span = left.span.clone();
        let left_value = self.build_expr(left, None);

        let right_span = right.span.clone();
        let right_value = self.build_expr(right, None);

        let result = emit_fn(
            self,
            left_value,
            left_span.clone(),
            right_value,
            right_span.clone(),
        );

        let span = Span {
            start: left_span.start,
            end: right_span.end,
            path: left_span.path,
        };

        self.check_expected(result, span, expected_type)
    }
}
