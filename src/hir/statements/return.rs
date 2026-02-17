use crate::{
    ast::{expr::Expr, Span},
    hir::{
        builders::{Builder, InBlock},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_declaration::CheckedDeclaration,
        utils::adjustments::check_structural_compatibility,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_return_stmt(&mut self, value: Expr, span: Span) {
        let value_span = value.span.clone();
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

        let val_id = self.build_expr(value);
        let actual_type = self.get_value_type(&val_id);

        if !check_structural_compatibility(actual_type, &expected_return_type) {
            self.errors.push(SemanticError {
                kind: SemanticErrorKind::ReturnTypeMismatch {
                    expected: expected_return_type.clone(),
                    received: actual_type.clone(),
                },
                span: value_span,
            });
            return;
        }

        self.emit_return(val_id);
    }
}
