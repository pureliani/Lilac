use crate::{
    ast::{IdentifierNode, ModulePath, Span, StringNode},
    hir::{
        builders::{Builder, InBlock},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_declaration::CheckedDeclaration,
    },
};
use std::path::PathBuf;
use std::sync::Arc;

impl<'a> Builder<'a, InBlock> {
    pub fn build_from_stmt(
        &mut self,
        path: StringNode,
        identifiers: Vec<(IdentifierNode, Option<IdentifierNode>)>,
        span: Span,
    ) {
        if !self.current_scope.is_file_scope() {
            self.errors.push(SemanticError {
                kind: SemanticErrorKind::FromStatementMustBeDeclaredAtTopLevel,
                span,
            });
            return;
        }

        let mut target_path_buf = PathBuf::from(&*self.context.path.0);
        target_path_buf.pop();
        target_path_buf.push(&path.value);

        let canonical_path = match target_path_buf.canonicalize() {
            Ok(p) => ModulePath(Arc::new(p)),
            Err(_) => {
                self.errors.push(SemanticError {
                    kind: SemanticErrorKind::ModuleNotFound(ModulePath(Arc::new(
                        target_path_buf,
                    ))),
                    span: path.span,
                });
                return;
            }
        };

        let target_module = match self.program.modules.get(&canonical_path) {
            Some(m) => m,
            None => {
                self.errors.push(SemanticError {
                    kind: SemanticErrorKind::ModuleNotFound(canonical_path),
                    span: path.span,
                });
                return;
            }
        };

        for (imported_ident, alias) in identifiers {
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
                    self.errors.push(SemanticError {
                        span: imported_ident.span.clone(),
                        kind: SemanticErrorKind::SymbolNotExported {
                            module_path: canonical_path.clone(),
                            symbol: imported_ident,
                        },
                    });
                }
            } else {
                self.errors.push(SemanticError {
                    kind: SemanticErrorKind::SymbolNotExported {
                        module_path: canonical_path.clone(),
                        symbol: imported_ident.clone(),
                    },
                    span: imported_ident.span,
                });
            }
        }
    }
}
