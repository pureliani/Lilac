// src/hir/expressions/fn.rs

use std::collections::{HashMap, HashSet};

use crate::{
    ast::{decl::FnDecl, expr::BlockContents},
    hir::{
        cfg::{BasicBlock, BasicBlockId, ControlFlowGraph, Terminator, Value},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_declaration::{
            CheckedDeclaration, CheckedFnDecl, CheckedParam, CheckedVarDecl,
        },
        utils::{
            check_is_assignable::check_is_assignable, check_type::check_type_annotation,
        },
        FunctionBuilder, HIRContext,
    },
};

impl FunctionBuilder {
    pub fn build(ctx: &mut HIRContext, fn_decl: FnDecl) -> Value {
        if !ctx.module_builder.is_file_scope() {
            ctx.module_builder.errors.push(SemanticError {
                kind: SemanticErrorKind::ClosuresNotSupportedYet,
                span: fn_decl.identifier.span,
            });
            return Value::VoidLiteral;
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

        let checked_params: Vec<CheckedParam> = params
            .iter()
            .map(|p| CheckedParam {
                identifier: p.identifier,
                ty: check_type_annotation(ctx, &p.constraint),
            })
            .collect();
        let checked_return_type = check_type_annotation(ctx, &return_type);

        let entry_block_id = BasicBlockId(0);
        let cfg = ControlFlowGraph {
            blocks: HashMap::from([(
                entry_block_id,
                BasicBlock {
                    id: entry_block_id,
                    instructions: vec![],
                    terminator: None,
                    params: vec![],
                },
            )]),
            entry_block: entry_block_id,
        };

        let mut inner_builder = FunctionBuilder {
            cfg,
            return_type: checked_return_type.clone(),
            current_block_id: entry_block_id,
            predecessors: HashMap::new(),
            block_value_maps: HashMap::new(),
            value_definitions: HashMap::new(),
            sealed_blocks: HashSet::new(),
            incomplete_params: HashMap::new(),
            predicates: HashMap::new(),
            block_id_counter: 1,
            value_id_counter: 0,
        };
        inner_builder.sealed_blocks.insert(entry_block_id);

        ctx.module_builder
            .enter_scope(crate::hir::utils::scope::ScopeKind::FunctionBody);

        for param in &checked_params {
            let arg_ssa_val =
                inner_builder.append_block_param(ctx, entry_block_id, param.ty.clone());

            let stack_ptr = inner_builder.emit_stack_alloc(ctx, param.ty.clone(), 1);
            inner_builder.emit_store(
                ctx,
                stack_ptr,
                Value::Use(arg_ssa_val),
                param.identifier.span,
            );

            let param_decl_id = ctx.program_builder.new_declaration_id();
            let decl = CheckedVarDecl {
                id: param_decl_id,
                ptr: stack_ptr,
                identifier: param.identifier,
                documentation: None,
                constraint: param.ty.clone(),
            };

            ctx.module_builder.scope_insert(
                ctx.program_builder,
                param.identifier,
                CheckedDeclaration::Var(decl),
            );
        }

        inner_builder.build_fn_body(ctx, body);
        ctx.module_builder.exit_scope();

        let checked_fn_decl = CheckedFnDecl {
            id: decl_id,
            identifier,
            params: checked_params,
            return_type: checked_return_type,
            body: Some(inner_builder.cfg),
            is_exported,
        };

        ctx.program_builder
            .declarations
            .insert(decl_id, CheckedDeclaration::Function(checked_fn_decl));

        Value::Function(decl_id)
    }

    fn build_fn_body(&mut self, ctx: &mut HIRContext, body: BlockContents) {
        let body_span = body.span;
        let final_value = self.build_codeblock_expr(ctx, body);
        let final_value_type = ctx.program_builder.get_value_type(&final_value);

        if !check_is_assignable(&final_value_type, &self.return_type) {
            ctx.module_builder.errors.push(SemanticError {
                span: body_span,
                kind: SemanticErrorKind::ReturnTypeMismatch {
                    expected: self.return_type.clone(),
                    received: final_value_type,
                },
            });
            self.set_basic_block_terminator(Terminator::Unreachable);
        } else if self.get_current_basic_block().terminator.is_none() {
            self.set_basic_block_terminator(Terminator::Return {
                value: Some(final_value),
            });
        }
    }
}
