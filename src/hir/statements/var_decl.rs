use crate::{
    ast::decl::VarDecl,
    hir::{
        builders::{Builder, InBlock},
        errors::{SemanticError, SemanticErrorKind},
        types::{
            checked_declaration::{CheckedDeclaration, CheckedVarDecl},
            checked_type::Type,
        },
        utils::{
            check_assignable::check_assignable,
            check_type::{check_type_annotation, TypeCheckerContext},
        },
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_var_decl(&mut self, var_decl: VarDecl) {
        if self.current_scope.is_file_scope() {
            self.errors.push(SemanticError {
                kind: SemanticErrorKind::CannotDeclareGlobalVariable,
                span: var_decl.identifier.span.clone(),
            });
            return;
        }

        let value_span = var_decl.value.span.clone();

        let val_id = self.build_expr(var_decl.value);
        let val_type = self.get_value_type(val_id).clone();

        let constraint = if let Some(annotation) = &var_decl.constraint {
            let mut type_ctx = TypeCheckerContext {
                scope: self.current_scope.clone(),
                declarations: &self.program.declarations,
                errors: self.errors,
            };

            check_type_annotation(&mut type_ctx, annotation)
        } else {
            val_type.clone()
        };

        if !check_assignable(&val_type, &constraint, false) {
            self.errors.push(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: constraint.clone(),
                    received: val_type,
                },
                span: value_span,
            });

            let poison_id = self.new_value_id(Type::Unknown);
            self.write_variable(var_decl.id, self.context.block_id, poison_id);
        } else {
            self.write_variable(var_decl.id, self.context.block_id, val_id);
        }

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
    }
}
