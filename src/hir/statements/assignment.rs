use crate::{
    ast::expr::{Expr, ExprKind},
    hir::{
        builders::{Builder, InBlock, Place, Projection},
        errors::{SemanticError, SemanticErrorKind},
        types::{checked_declaration::CheckedDeclaration, checked_type::Type},
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_assignment_stmt(&mut self, target: Expr, value: Expr) {
        let target_span = target.span.clone();
        let source_val = self.build_expr(value);

        let place = match self.build_place(target) {
            Ok(p) => p,
            Err(e) => {
                self.errors.push(e);
                return;
            }
        };

        self.write_place(place, source_val, target_span);
    }
}
