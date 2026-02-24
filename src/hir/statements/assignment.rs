use crate::{
    ast::{
        expr::{Expr, ExprKind},
        Span,
    },
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn write_expr(
        &mut self,
        target: &Expr,
        value: ValueId,
        span: Span,
    ) -> Result<(), SemanticError> {
        match &target.kind {
            ExprKind::Identifier(ident) => {
                let decl_id = self.current_scope.lookup(ident.name).ok_or_else(|| {
                    SemanticError {
                        kind: SemanticErrorKind::UndeclaredIdentifier(ident.clone()),
                        span: span.clone(),
                    }
                })?;

                self.write_variable(decl_id, self.context.block_id, value);
                Ok(())
            }
            ExprKind::Access { left, field } => {
                let base_val = self.build_expr(*left.clone());
                let new_base_val =
                    self.emit_update_struct_field(base_val, field.clone(), value);
                self.write_expr(left, new_base_val, span)
            }
            // TODO: Add ExprKind::IndexAccess here for lists later
            _ => Err(SemanticError {
                kind: SemanticErrorKind::InvalidLValue,
                span,
            }),
        }
    }

    pub fn build_assignment_stmt(&mut self, target: Expr, value: Expr) {
        let value_span = value.span.clone();
        let target_span = target.span.clone();

        let value_id = self.build_expr(value);

        let current_target_val = self.build_expr(target.clone());
        let expected_type = self.get_value_type(current_target_val).clone();

        let final_val_id =
            match self.adjust_value(value_id, value_span, expected_type, false) {
                Ok(id) => id,
                Err(e) => {
                    self.errors.push(e);
                    return;
                }
            };

        if let Err(e) = self.write_expr(&target, final_val_id, target_span) {
            self.errors.push(e);
        }
    }
}
