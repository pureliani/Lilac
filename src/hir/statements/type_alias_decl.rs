use crate::{
    ast::{decl::TypeAliasDecl, Span},
    hir::{
        builders::{Builder, InBlock},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_declaration::{CheckedDeclaration, CheckedTypeAliasDecl},
        utils::check_type::{check_type_annotation, TypeCheckerContext},
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_type_alias_decl(&mut self, type_alias_decl: TypeAliasDecl, span: Span) {
        if !self.current_scope.is_file_scope() {
            self.errors.push(SemanticError {
                kind: SemanticErrorKind::TypeAliasMustBeDeclaredAtTopLevel,
                span,
            });
            return;
        }

        let mut type_ctx = TypeCheckerContext {
            scope: self.current_scope.clone(),
            declarations: &self.program.declarations,
            errors: self.errors,
        };

        let resolved_type = check_type_annotation(&mut type_ctx, &type_alias_decl.value);

        let checked_type_alias_decl = CheckedTypeAliasDecl {
            id: type_alias_decl.id,
            documentation: type_alias_decl.documentation,
            identifier: type_alias_decl.identifier.clone(),
            span,
            value: Box::new(resolved_type),
            is_exported: type_alias_decl.is_exported,
        };

        self.program.declarations.insert(
            type_alias_decl.id,
            CheckedDeclaration::TypeAlias(checked_type_alias_decl),
        );

        self.current_scope
            .map_name_to_decl(type_alias_decl.identifier.name, type_alias_decl.id);
    }
}
