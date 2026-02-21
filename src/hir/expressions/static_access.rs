use crate::{
    ast::{expr::Expr, IdentifierNode},
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_static_access_expr(
        &mut self,
        left: Expr,
        field: IdentifierNode,
    ) -> ValueId {
        let span = field.span.clone();

        let left_id = self.build_expr(left);
        let left_type = self.get_value_type(left_id).clone();

        self.report_error_and_get_poison(SemanticError {
            kind: SemanticErrorKind::CannotStaticAccess(left_type),
            span,
        })
    }
}
