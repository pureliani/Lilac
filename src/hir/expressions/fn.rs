use std::collections::{HashMap, HashSet};

use crate::{
    ast::{decl::FnDecl, DeclarationId, Span},
    globals::{next_block_id, next_declaration_id},
    hir::{
        builders::{BasicBlock, Builder, Function, InBlock, InModule, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        instructions::Terminator,
        types::{
            checked_declaration::{
                CheckedDeclaration, CheckedParam, CheckedVarDecl, FunctionEffects,
                ParamMutation,
            },
            checked_type::Type,
        },
        utils::{
            check_assignable::check_assignable,
            check_type::{check_params, check_type_annotation, TypeCheckerContext},
            points_to::PointsToGraph,
            scope::ScopeKind,
        },
    },
};

impl<'a> Builder<'a, InModule> {
    pub fn build_fn_body(&mut self, fn_decl: FnDecl) -> Result<(), SemanticError> {
        if !self.current_scope.is_file_scope() {
            return Err(SemanticError {
                kind: SemanticErrorKind::ClosuresNotSupportedYet,
                span: fn_decl.identifier.span.clone(),
            });
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
            ptg: PointsToGraph::new(),
            param_decl_ids: Vec::new(),
            effects: FunctionEffects::default(),
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
            current_defs: self.current_defs,
            incomplete_phis: self.incomplete_phis,
            type_predicates: self.type_predicates,
            ptg: self.ptg,
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

        let mut param_decl_ids = Vec::with_capacity(checked_params.len());

        for param in &checked_params {
            let identity_id = fn_builder.new_value_id(param.ty.clone());

            let param_decl_id = next_declaration_id();
            let decl = CheckedVarDecl {
                id: param_decl_id,
                identifier: param.identifier.clone(),
                documentation: None,
                constraint: param.ty.clone(),
            };

            fn_builder.write_variable(param_decl_id, entry_block_id, identity_id);

            fn_builder
                .program
                .declarations
                .insert(param_decl_id, CheckedDeclaration::Var(decl));

            fn_builder
                .current_scope
                .map_name_to_decl(param.identifier.name, param_decl_id);

            param_decl_ids.push(param_decl_id);
        }

        fn_builder.get_fn().param_decl_ids = param_decl_ids.clone();

        let (final_value, final_value_span) = fn_builder.build_codeblock_expr(body);
        let final_value_type = fn_builder.get_value_type(final_value).clone();

        if !check_assignable(&final_value_type, &checked_return_type, false) {
            return Err(SemanticError {
                span: final_value_span,
                kind: SemanticErrorKind::ReturnTypeMismatch {
                    expected: checked_return_type.clone(),
                    received: final_value_type,
                },
            });
        }

        fn_builder.emit_return(final_value);

        let effects = fn_builder.compute_effects(
            &checked_params,
            &param_decl_ids,
            &identifier.span,
        );
        fn_builder.get_fn().effects = effects;

        Ok(())
    }
}

impl<'a> Builder<'a, InBlock> {
    pub fn build_fn_expr(&mut self, fn_decl: FnDecl) -> ValueId {
        let id = fn_decl.id;
        match self.as_module().build_fn_body(fn_decl) {
            Ok(v) => v,
            Err(e) => self.errors.push(e),
        };
        self.emit_const_fn(id)
    }

    fn compute_effects(
        &mut self,
        checked_params: &[CheckedParam],
        param_decl_ids: &[DeclarationId],
        fn_span: &Span,
    ) -> FunctionEffects {
        let func = self.get_fn();
        let return_block_ids: Vec<_> = func
            .blocks
            .iter()
            .filter_map(|(id, bb)| {
                if matches!(bb.terminator, Some(Terminator::Return { .. })) {
                    Some(*id)
                } else {
                    None
                }
            })
            .collect();

        if return_block_ids.is_empty() {
            return FunctionEffects::default();
        }

        let mut mutations = Vec::new();

        for (param_index, (param, &param_decl_id)) in
            checked_params.iter().zip(param_decl_ids.iter()).enumerate()
        {
            let declared_type = &param.ty;

            let mut exit_types: Vec<Type> = Vec::new();
            let mut any_changed = false;

            for &block_id in &return_block_ids {
                let val = self.read_variable(param_decl_id, block_id, fn_span.clone());
                let ty = self.get_value_type(val).clone();

                if ty != *declared_type {
                    any_changed = true;
                }

                exit_types.push(ty);
            }

            if any_changed {
                let exit_type = Type::make_union(exit_types);
                mutations.push(ParamMutation {
                    param_index,
                    exit_type,
                });
            }
        }

        FunctionEffects { mutations }
    }
}
