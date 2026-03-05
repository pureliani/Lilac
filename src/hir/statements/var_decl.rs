use crate::{
    ast::decl::VarDecl,
    hir::{
        builders::{Builder, InBlock},
        errors::{SemanticError, SemanticErrorKind},
        types::{
            checked_declaration::{CheckedDeclaration, CheckedVarDecl},
            checked_type::SpannedType,
        },
        utils::check_type::{check_type_annotation, TypeCheckerContext},
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

        let constraint = var_decl.constraint.as_ref().map(|c| {
            let mut type_ctx = TypeCheckerContext {
                scope: self.current_scope.clone(),
                declarations: &self.program.declarations,
                errors: self.errors,
            };

            check_type_annotation(&mut type_ctx, c)
        });

        let val_id = self.build_expr(var_decl.value, constraint.as_ref());
        let val_type = self.get_value_type(val_id).clone();

        self.write_variable(var_decl.id, self.context.block_id, val_id);

        let checked_var_decl = CheckedVarDecl {
            id: var_decl.id,
            identifier: var_decl.identifier.clone(),
            documentation: var_decl.documentation,
            constraint: constraint.unwrap_or(SpannedType {
                kind: val_type.clone(),
                span: value_span,
            }),
        };

        self.program
            .declarations
            .insert(var_decl.id, CheckedDeclaration::Var(checked_var_decl));

        self.current_scope
            .map_name_to_decl(var_decl.identifier.name, var_decl.id);
    }
}
