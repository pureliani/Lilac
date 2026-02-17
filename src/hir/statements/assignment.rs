use crate::{
    ast::{expr::Expr, IdentifierNode},
    hir::{
        builders::{Builder, InBlock},
        errors::{SemanticError, SemanticErrorKind},
        utils::{adjustments::Adjustment, place::Place},
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

        todo!()
    }
}
