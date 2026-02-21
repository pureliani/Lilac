use crate::{
    ast::expr::Expr,
    hir::{
        builders::{Builder, InBlock},
        errors::{SemanticError, SemanticErrorKind},
        utils::place::Place,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_assignment_stmt(&mut self, target: Expr, value: Expr) {
        let value_span = value.span.clone();
        let target_span = target.span.clone();

        let value_id = self.build_expr(value);

        let place = match self.resolve_place(target) {
            Ok(p) => p,
            Err(e) => {
                self.errors.push(e);
                return;
            }
        };

        if matches!(&place, Place::Temporary(_)) {
            self.errors.push(SemanticError {
                kind: SemanticErrorKind::InvalidLValue,
                span: target_span,
            });
            return;
        }

        let expected_type = self.type_of_place(&place);

        let final_val_id =
            match self.adjust_value(value_id, value_span, expected_type, false) {
                Ok(id) => id,
                Err(e) => {
                    self.errors.push(e);
                    return;
                }
            };

        self.write_place(&place, final_val_id, target_span);
    }
}
