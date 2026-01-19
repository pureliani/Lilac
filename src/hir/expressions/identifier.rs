use crate::{
    ast::{expr::Expr, expr::ExprKind},
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_declaration::CheckedDeclaration,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_identifier_expr(&mut self, expr: Expr) -> ValueId {
        let identifier = match &expr.kind {
            ExprKind::Identifier(id) => id.clone(),
            _ => panic!("INTERNAL COMPILER ERROR: Expected Identifier expression"),
        };

        let decl_id = match self.current_scope.lookup(identifier.name) {
            Some(id) => id,
            None => {
                return self.report_error_and_get_poison(SemanticError {
                    kind: SemanticErrorKind::UndeclaredIdentifier(identifier),
                    span: expr.span,
                });
            }
        };

        let decl = self
            .program
            .declarations
            .get(&decl_id)
            .expect("INTERNAL COMPILER ERROR: Declaration not found");

        match decl {
            CheckedDeclaration::Var(_) => match self.build_place(expr) {
                Ok(place) => self.read_place(place),
                Err(e) => self.report_error_and_get_poison(e),
            },
            CheckedDeclaration::UninitializedVar { .. } => self
                .report_error_and_get_poison(SemanticError {
                    kind: SemanticErrorKind::UseOfUninitializedVariable(identifier),
                    span: expr.span,
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
