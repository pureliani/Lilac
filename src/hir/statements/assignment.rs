use crate::{
    ast::{
        expr::{Expr, ExprKind},
        IdentifierNode,
    },
    hir::{
        builders::{Builder, InBlock, LValue},
        errors::{SemanticError, SemanticErrorKind},
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_assignment_stmt(
        &mut self,
        target: Expr,
        value: Expr,
    ) -> Result<(), SemanticError> {
        let target_span = target.span.clone();

        let val_id = self.build_expr(value)?;

        let lval = match target.kind {
            ExprKind::Identifier(ident) => {
                let decl_id = match self.current_scope.lookup(ident.name) {
                    Some(id) => id,
                    None => {
                        return Err(SemanticError {
                            kind: SemanticErrorKind::UndeclaredIdentifier(ident),
                            span: target_span,
                        })
                    }
                };

                self.aliases
                    .get(&decl_id)
                    .cloned()
                    .unwrap_or(LValue::Variable(decl_id))
            }
            ExprKind::Access { left, field } => {
                let base_ptr = self.build_expr(*left)?;

                LValue::Field {
                    base_ptr,
                    field: field.name,
                }
            }
            _ => {
                return Err(SemanticError {
                    kind: SemanticErrorKind::InvalidLValue,
                    span: target_span,
                });
            }
        };

        self.write_lvalue(lval.clone(), val_id);

        if let LValue::Field { base_ptr, field } = lval {
            let field_node = IdentifierNode {
                name: field,
                span: target_span.clone(),
            };

            let ptr_id = self.get_field_ptr(base_ptr, &field_node)?;
            self.emit_store(ptr_id, val_id);
        }

        Ok(())
    }
}
