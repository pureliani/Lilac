use crate::{
    ast::{IdentifierNode, ModulePath, Span, StringNode},
    hir::{
        builders::{Builder, InModule},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_declaration::CheckedDeclaration,
    },
};
use std::path::PathBuf;
use std::sync::Arc;

impl<'a> Builder<'a, InModule> {
    pub fn build_from_stmt(
        &mut self,
        path: StringNode,
        identifiers: Vec<(IdentifierNode, Option<IdentifierNode>)>,
        span: Span,
    ) -> Result<(), SemanticError> {
        if !self.current_scope.is_file_scope() {
            return Err(SemanticError {
                kind: SemanticErrorKind::FromStatementMustBeDeclaredAtTopLevel,
                span,
            });
        }

        let mut target_path_buf = PathBuf::from(&*self.context.path.0);
        target_path_buf.pop();
        target_path_buf.push(&path.value);

        let canonical_path = match target_path_buf.canonicalize() {
            Ok(p) => ModulePath(Arc::new(p)),
            Err(_) => {
                return Err(SemanticError {
                    kind: SemanticErrorKind::ModuleNotFound(ModulePath(Arc::new(
                        target_path_buf,
                    ))),
                    span: path.span,
                });
            }
        };

        let target_module = match self.program.modules.get(&canonical_path) {
            Some(m) => m,
            None => {
                return Err(SemanticError {
                    kind: SemanticErrorKind::ModuleNotFound(canonical_path),
                    span: path.span,
                });
            }
        };

        for (imported_ident, alias) in identifiers {
            let not_exported_err = SemanticError {
                span: imported_ident.span.clone(),
                kind: SemanticErrorKind::SymbolNotExported {
                    module_path: canonical_path.clone(),
                    symbol: imported_ident.clone(),
                },
            };

            if let Some(decl_id) = target_module.root_scope.lookup(imported_ident.name) {
                let is_exported = match self.program.declarations.get(&decl_id) {
                    Some(CheckedDeclaration::Function(f)) => f.is_exported,
                    Some(CheckedDeclaration::TypeAlias(t)) => t.is_exported,
                    _ => false,
                };

                if is_exported {
                    let name_node = alias.unwrap_or(imported_ident);
                    self.current_scope.map_name_to_decl(name_node.name, decl_id);
                } else {
                    return Err(not_exported_err);
                }
            } else {
                return Err(not_exported_err);
            }
        }

        Ok(())
    }
}
