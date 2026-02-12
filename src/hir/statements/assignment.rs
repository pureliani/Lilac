use crate::{
    ast::expr::Expr,
    hir::{
        builders::{Builder, InBlock},
        errors::{SemanticError, SemanticErrorKind},
        utils::{adjustments::Adjustment, place::Place},
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_assignment_stmt(
        &mut self,
        target: Expr,
        value: Expr,
    ) -> Result<(), SemanticError> {
        let value_span = value.span.clone();
        let target_span = target.span.clone();
        let value_id = self.build_expr(value)?;

        let place = self.resolve_place(target)?;

        if matches!(&place, Place::Temporary(_)) {
            return Err(SemanticError {
                kind: SemanticErrorKind::InvalidLValue,
                span: target_span,
            });
        }

        let constraint_type = self.type_of_place(&place);

        let adjustment = self
            .compute_adjustment(value_id, &constraint_type, false)
            .map_err(|kind| SemanticError {
                kind,
                span: value_span.clone(),
            })?;

        let stored_value = if matches!(
            &adjustment,
            Adjustment::Identity
                | Adjustment::WrapInUnion { .. }
                | Adjustment::WidenUnion { .. }
        ) {
            value_id
        } else {
            self.apply_adjustment(value_id, adjustment, value_span)
        };

        self.remap_place(&place, stored_value);

        Ok(())
    }
}
