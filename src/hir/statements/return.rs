use crate::{
    ast::{expr::Expr, Span},
    hir::{
        builders::{Builder, InBlock},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_declaration::CheckedDeclaration,
        utils::check_is_assignable::check_is_assignable,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_return_stmt(&mut self, value: Expr, span: Span) {
        let func_id = self.context.func_id;
        let expected_return_type = match self.program.declarations.get(&func_id) {
            Some(CheckedDeclaration::Function(f)) => f.return_type.clone(),
            _ => {
                self.errors.push(SemanticError {
                    kind: SemanticErrorKind::ReturnKeywordOutsideFunction,
                    span: span.clone(),
                });
                return;
            }
        };

        let val_id = match self.build_expr(value) {
            Ok(id) => id,
            Err(e) => {
                self.errors.push(e);
                return;
            }
        };
        let actual_type = self.get_value_type(&val_id);

        if !check_is_assignable(actual_type, &expected_return_type) {
            self.errors.push(SemanticError {
                kind: SemanticErrorKind::ReturnTypeMismatch {
                    expected: expected_return_type.clone(),
                    received: actual_type.clone(),
                },
                span,
            });
        }

        self.emit_return_terminator(val_id);
    }
}
