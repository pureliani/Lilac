use crate::{
    ast::decl::VarDecl,
    hir::{
        builders::{Builder, InBlock, LValue},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_declaration::{CheckedDeclaration, CheckedVarDecl},
        utils::{
            check_is_assignable::check_is_assignable,
            check_type::{check_type_annotation, TypeCheckerContext},
        },
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_var_decl(&mut self, var_decl: VarDecl) -> Result<(), SemanticError> {
        if self.current_scope.is_file_scope() {
            return Err(SemanticError {
                kind: SemanticErrorKind::CannotDeclareGlobalVariable,
                span: var_decl.identifier.span.clone(),
            });
        }

        let initial_value_span = var_decl.value.span.clone();

        let val_id = self.build_expr(var_decl.value)?;
        let val_type = self.get_value_type(&val_id).clone();

        let constraint = if let Some(annotation) = &var_decl.constraint {
            let mut type_ctx = TypeCheckerContext {
                scope: self.current_scope.clone(),
                declarations: &self.program.declarations,
                errors: self.errors,
            };

            let expected = check_type_annotation(&mut type_ctx, annotation);

            if !check_is_assignable(&val_type, &expected) {
                self.errors.push(SemanticError {
                    span: initial_value_span.clone(),
                    kind: SemanticErrorKind::TypeMismatch {
                        expected: expected.clone(),
                        received: val_type.clone(),
                    },
                });
            }
            expected
        } else {
            val_type.clone()
        };

        let lval = LValue::Variable(var_decl.id);
        self.write_lvalue(lval, val_id);
        let checked_var_decl = CheckedVarDecl {
            id: var_decl.id,
            identifier: var_decl.identifier.clone(),
            documentation: var_decl.documentation,
            constraint,
        };

        self.program
            .declarations
            .insert(var_decl.id, CheckedDeclaration::Var(checked_var_decl));

        self.current_scope
            .map_name_to_decl(var_decl.identifier.name, var_decl.id);

        Ok(())
    }
}
