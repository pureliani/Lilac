use crate::{
    ast::IdentifierNode,
    hir::{
        builders::{Builder, InBlock, LValue, ValueId},
        errors::{SemanticError, SemanticErrorKind},
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

        let lval = self
            .aliases
            .get(&decl_id)
            .cloned()
            .unwrap_or(LValue::Variable(decl_id));

        self.read_lvalue(lval, identifier.span)
    }
}
