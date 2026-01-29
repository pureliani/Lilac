use crate::{
    ast::{
        expr::{Expr, ExprKind},
        IdentifierNode,
    },
    hir::{
        builders::{Builder, InBlock, LValue},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_declaration::CheckedDeclaration,
        utils::adjustments::check_is_assignable,
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
        let val_type = self.get_value_type(&val_id).clone();

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

        match &lval {
            LValue::Variable(decl_id) => {
                let decl = self
                    .program
                    .declarations
                    .get(decl_id)
                    .expect("INTERNAL COMPILER ERROR: DeclId not found");

                if let CheckedDeclaration::Var(var_decl) = decl {
                    if !check_is_assignable(&val_type, &var_decl.constraint) {
                        return Err(SemanticError {
                            kind: SemanticErrorKind::TypeMismatch {
                                expected: var_decl.constraint.clone(),
                                received: val_type.clone(),
                            },
                            span: target_span.clone(),
                        });
                    }
                }

                self.remap_lvalue(lval, val_id);
            }
            LValue::Field { base_ptr, field } => {
                let field_node = IdentifierNode {
                    name: *field,
                    span: target_span.clone(),
                };

                let ptr_id =
                    self.try_get_user_struct_field_ptr(*base_ptr, &field_node)?;
                self.emit_store(ptr_id, val_id, target_span);

                self.remap_lvalue(lval, val_id);
            }
        }

        Ok(())
    }
}
