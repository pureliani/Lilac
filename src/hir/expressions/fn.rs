use std::collections::{HashMap, HashSet};

use crate::{
    ast::decl::FnDecl,
    globals::{next_block_id, next_declaration_id},
    hir::{
        builders::{BasicBlock, Builder, Function, InBlock, InModule, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_declaration::{CheckedDeclaration, CheckedVarDecl},
        utils::{
            check_is_assignable::check_is_assignable,
            check_type::{check_params, check_type_annotation, TypeCheckerContext},
            scope::ScopeKind,
        },
    },
};

impl<'a> Builder<'a, InModule> {
    pub fn build_fn_body(&mut self, fn_decl: FnDecl) {
        if !self.current_scope.is_file_scope() {
            self.errors.push(SemanticError {
                kind: SemanticErrorKind::ClosuresNotSupportedYet,
                span: fn_decl.identifier.span.clone(),
            });
            return;
        }

        let FnDecl {
            id: decl_id,
            identifier,
            params,
            return_type,
            body,
            is_exported,
            ..
        } = fn_decl;

        let mut type_ctx = TypeCheckerContext {
            scope: self.current_scope.clone(),
            declarations: &self.program.declarations,
            errors: self.errors,
        };

        let checked_params = check_params(&mut type_ctx, &params);
        let checked_return_type = check_type_annotation(&mut type_ctx, &return_type);

        let entry_block_id = next_block_id();
        let function = Function {
            id: decl_id,
            identifier: identifier.clone(),
            params: checked_params.clone(),
            return_type: checked_return_type.clone(),
            is_exported,
            entry_block: entry_block_id,
            blocks: HashMap::new(),
            value_definitions: HashMap::new(),
        };

        self.program
            .declarations
            .insert(decl_id, CheckedDeclaration::Function(function));

        let mut fn_builder = Builder {
            context: InBlock {
                path: self.context.path.clone(),
                func_id: decl_id,
                block_id: entry_block_id,
            },
            program: self.program,
            errors: self.errors,
            current_scope: self
                .current_scope
                .enter(ScopeKind::FunctionBody, body.span.start),
            definitions: self.definitions,
            incomplete_phis: self.incomplete_phis,
        };

        let entry_bb = BasicBlock {
            id: entry_block_id,
            instructions: vec![],
            terminator: None,
            predecessors: HashSet::new(),
            phis: HashMap::new(),
            sealed: true,
        };
        fn_builder.get_fn().blocks.insert(entry_block_id, entry_bb);

        for param in &checked_params {
            let identity_id = fn_builder.new_value_id(param.ty.clone());

            fn_builder
                .definitions
                .entry(entry_block_id)
                .or_default()
                .insert(identity_id, identity_id);

            let param_var_id = next_declaration_id();
            let decl = CheckedVarDecl {
                id: param_var_id,
                stack_ptr: identity_id,
                identifier: param.identifier.clone(),
                documentation: None,
                constraint: param.ty.clone(),
            };

            fn_builder
                .program
                .declarations
                .insert(param_var_id, CheckedDeclaration::Var(decl));

            fn_builder
                .current_scope
                .map_name_to_decl(param.identifier.name, param_var_id);
        }

        let body_span = body.span.clone();
        let final_value = match fn_builder.build_codeblock_expr(body) {
            Ok(id) => id,
            Err(e) => {
                self.errors.push(e);
                return;
            }
        };
        let final_value_type = fn_builder.get_value_type(&final_value).clone();

        if !check_is_assignable(&final_value_type, &checked_return_type) {
            fn_builder.errors.push(SemanticError {
                span: body_span,
                kind: SemanticErrorKind::ReturnTypeMismatch {
                    expected: checked_return_type.clone(),
                    received: final_value_type,
                },
            });
        }

        fn_builder.emit_return_terminator(final_value);
    }
}

impl<'a> Builder<'a, InBlock> {
    pub fn build_fn_expr(&mut self, fn_decl: FnDecl) -> Result<ValueId, SemanticError> {
        let id = fn_decl.id;
        self.as_module().build_fn_body(fn_decl);
        Ok(self.emit_const_fn(id))
    }
}
