use crate::{
    ast::{
        expr::{Expr, ExprKind},
        IdentifierNode,
    },
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_declaration::CheckedDeclaration,
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

        let decl = self
            .program
            .declarations
            .get(&decl_id)
            .expect("INTERNAL COMPILER ERROR: Declaration not found");

        let expr_for_place = Expr {
            kind: ExprKind::Identifier(identifier.clone()),
            span: identifier.span.clone(),
        };

        match decl {
            CheckedDeclaration::Var(_) => match self.build_place(expr_for_place) {
                Ok(place) => self.read_place(place, identifier.span.clone()),
                Err(e) => self.report_error_and_get_poison(e),
            },
            CheckedDeclaration::UninitializedVar { .. } => self
                .report_error_and_get_poison(SemanticError {
                    span: identifier.span.clone(),
                    kind: SemanticErrorKind::UseOfUninitializedVariable(identifier),
                }),
            CheckedDeclaration::Function(func) => self.emit_const_fn(func.id),
            CheckedDeclaration::TypeAlias(alias) => {
                self.report_error_and_get_poison(SemanticError {
                    kind: SemanticErrorKind::CannotUseTypeDeclarationAsValue,
                    span: alias.identifier.span.clone(),
                })
            }
        }
    }
}
