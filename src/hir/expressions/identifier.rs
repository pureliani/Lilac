use crate::{
    ast::IdentifierNode,
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_declaration::CheckedDeclaration,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_identifier_expr(
        &mut self,
        identifier: IdentifierNode,
    ) -> Result<ValueId, SemanticError> {
        let decl_id = match self.current_scope.lookup(identifier.name) {
            Some(id) => id,
            None => {
                return Err(SemanticError {
                    span: identifier.span.clone(),
                    kind: SemanticErrorKind::UndeclaredIdentifier(identifier),
                });
            }
        };

        let decl = self
            .program
            .declarations
            .get(&decl_id)
            .expect("INTERNAL COMPILER ERROR: Declaration not found");

        Ok(match decl {
            CheckedDeclaration::Var(var) => self.load(var.stack_ptr, identifier.span)?,
            CheckedDeclaration::Function(func) => self.emit_const_fn(func.id),
            CheckedDeclaration::TypeAlias(_) => {
                return Err(SemanticError {
                    kind: SemanticErrorKind::CannotUseTypeDeclarationAsValue,
                    span: identifier.span,
                })
            }
        })
    }
}
