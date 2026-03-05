use crate::{
    ast::IdentifierNode,
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::{checked_declaration::CheckedDeclaration, checked_type::SpannedType},
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_identifier_expr(
        &mut self,
        identifier: IdentifierNode,
        expected_type: Option<&SpannedType>,
    ) -> ValueId {
        let span = identifier.span.clone();
        let decl_id = match self.current_scope.lookup(identifier.name) {
            Some(id) => id,
            None => {
                return self.report_error_and_get_poison(SemanticError {
                    span: identifier.span.clone(),
                    kind: SemanticErrorKind::UndeclaredIdentifier(identifier),
                });
            }
        };

        let decl = self.program.declarations.get(&decl_id).unwrap();

        let result = match decl {
            CheckedDeclaration::Function(_) => self.emit_const_fn(decl_id),
            CheckedDeclaration::Var(_) => {
                self.read_variable(decl_id, self.context.block_id, identifier.span)
            }
            CheckedDeclaration::TypeAlias(_) => {
                self.report_error_and_get_poison(SemanticError {
                    span: identifier.span.clone(),
                    kind: SemanticErrorKind::CannotUseTypeDeclarationAsValue,
                })
            }
        };

        self.check_expected(result, span, expected_type)
    }
}
