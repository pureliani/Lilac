use crate::{
    ast::{expr::Expr, IdentifierNode},
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

        match &place {
            Place::Field(base, field_name) => {
                let base_val = self.read_place(base, target_span.clone())?;
                let field_ptr = self.try_get_field_ptr(
                    base_val,
                    &IdentifierNode {
                        name: *field_name,
                        span: target_span.clone(),
                    },
                    false,
                )?;

                let field_ptr_type = self.get_value_type(&field_ptr).clone();
                let field_type = field_ptr_type
                    .try_unwrap_pointer()
                    .expect("INTERNAL COMPILER ERROR: field ptr is not a pointer");

                let adjusted =
                    self.adjust_value(value_id, value_span, field_type.clone(), false)?;

                self.emit_store(field_ptr, adjusted, target_span);
            }
            _ => {
                let constraint_type = self.type_of_place(&place);

                let adjustment = self
                    .compute_adjustment(value_id, &constraint_type, false)
                    .map_err(|kind| SemanticError {
                        kind,
                        span: value_span.clone(),
                    })?;

                // For union-typed places, store the narrow value.
                // Widening happens at use sites.
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
            }
        }

        Ok(())
    }
}
