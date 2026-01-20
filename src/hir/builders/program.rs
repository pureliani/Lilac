use crate::ast::decl::Declaration;
use crate::ast::expr::ExprKind;
use crate::ast::stmt::StmtKind;
use crate::ast::Position;
use crate::compile::ParallelParseResult;
use crate::hir::builders::{Builder, InBlock, InGlobal, InModule, Module};
use crate::hir::utils::scope::ScopeKind;

impl<'a> Builder<'a, InGlobal> {
    pub fn build(&mut self, mut modules: Vec<ParallelParseResult>) {
        for m in &modules {
            let module_scope = self
                .current_scope
                .enter(ScopeKind::File, Position::default());

            self.program.modules.insert(
                m.path.clone(),
                Module {
                    path: m.path.clone(),
                    root_scope: module_scope,
                },
            );
        }

        for m in &modules {
            let mut module_builder = self.as_module(m.path.clone());

            for decl in &m.declarations {
                match decl {
                    Declaration::TypeAlias(alias) => {
                        let mut dummy_builder = module_builder.as_dummy_block();
                        dummy_builder.build_type_alias_decl(
                            alias.clone(),
                            alias.identifier.span.clone(),
                        );
                    }
                    Declaration::Fn(f) => {
                        module_builder.register_fn_signature(f);
                    }
                }
            }
        }

        for m in std::mem::take(&mut modules) {
            let mut module_builder = self.as_module(m.path.clone());

            for stmt in m.statements {
                match stmt.kind {
                    StmtKind::From { path, identifiers } => {
                        let mut dummy_builder = module_builder.as_dummy_block();
                        dummy_builder.build_from_stmt(path, identifiers, stmt.span);
                    }
                    StmtKind::Expression(expr) => {
                        if let ExprKind::Fn(f) = expr.kind {
                            let mut dummy_builder = module_builder.as_dummy_block();
                            dummy_builder.build_fn_expr(*f);
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    fn as_module(&mut self, path: crate::ast::ModulePath) -> Builder<'_, InModule> {
        let scope = self.program.modules.get(&path).unwrap().root_scope.clone();
        Builder {
            context: InModule { path },
            program: self.program,
            errors: self.errors,
            current_scope: scope,
        }
    }
}

impl<'a> Builder<'a, InModule> {
    fn as_dummy_block(&mut self) -> Builder<'_, InBlock> {
        Builder {
            context: crate::hir::builders::InBlock {
                path: self.context.path.clone(),
                func_id: crate::ast::DeclarationId(0), // Not inside a real function
                block_id: crate::hir::builders::BasicBlockId(0),
            },
            program: self.program,
            errors: self.errors,
            current_scope: self.current_scope.clone(),
        }
    }

    fn register_fn_signature(&mut self, f: &crate::ast::decl::FnDecl) {
        use crate::hir::builders::Function;
        use crate::hir::types::checked_declaration::CheckedDeclaration;
        use crate::hir::utils::check_type::{
            check_params, check_type_annotation, TypeCheckerContext,
        };
        use std::collections::HashMap;

        let mut type_ctx = TypeCheckerContext {
            scope: self.current_scope.clone(),
            declarations: &self.program.declarations,
            errors: self.errors,
        };

        let checked_params = check_params(&mut type_ctx, &f.params);
        let checked_return_type = check_type_annotation(&mut type_ctx, &f.return_type);

        let function = Function {
            id: f.id,
            identifier: f.identifier.clone(),
            params: checked_params,
            return_type: checked_return_type,
            is_exported: f.is_exported,
            entry_block: crate::hir::builders::BasicBlockId(0), // Placeholder
            blocks: HashMap::new(),
            value_definitions: HashMap::new(),
            predicates: HashMap::new(),
        };

        self.program
            .declarations
            .insert(f.id, CheckedDeclaration::Function(function));
        self.current_scope.map_name_to_decl(f.identifier.name, f.id);
    }
}
