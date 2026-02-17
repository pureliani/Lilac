use crate::{
    ast::IdentifierNode,
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        utils::place::Place,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_identifier_expr(&mut self, identifier: IdentifierNode) -> ValueId {
        let decl_id = match self.current_scope.lookup(identifier.name) {
            Some(id) => id,
            None => {
                return self.report_error_and_get_poison(SemanticError {
                    span: identifier.span.clone(),
                    kind: SemanticErrorKind::UndeclaredIdentifier(identifier),
                });
            }
        };

        let place = self
            .aliases
            .get(&decl_id)
            .cloned()
            .unwrap_or(Place::Local(decl_id));

        self.read_place(&place, identifier.span)
    }
}
