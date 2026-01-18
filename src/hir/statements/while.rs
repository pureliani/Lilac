// src/hir/statements/while.rs

use crate::{
    ast::expr::{BlockContents, Expr},
    hir::{
        cfg::{Terminator, Value},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_type::Type,
        utils::{check_is_assignable::check_is_assignable, scope::ScopeKind},
        FunctionBuilder, HIRContext,
    },
};

impl FunctionBuilder {
    pub fn build_while_stmt(
        &mut self,
        ctx: &mut HIRContext,
        condition: Box<Expr>,
        body: BlockContents,
    ) {
        let header_block = self.new_basic_block();
        let body_block = self.new_basic_block();
        let exit_block = self.new_basic_block();

        self.set_basic_block_terminator(Terminator::Jump {
            target: header_block,
            args: vec![],
        });

        self.use_basic_block(header_block);

        let condition_span = condition.span;
        let condition_value = self.build_expr(ctx, *condition);
        let condition_type = ctx.program_builder.get_value_type(&condition_value);

        if !check_is_assignable(&condition_type, &Type::Bool) {
            ctx.module_builder.errors.push(SemanticError {
                span: condition_span,
                kind: SemanticErrorKind::TypeMismatch {
                    expected: Type::Bool,
                    received: condition_type,
                },
            });
        }

        if let Value::Use(cond_id) = condition_value {
            if let Some(pred) = self.predicates.get(&cond_id).cloned() {
                self.map_value(body_block, pred.source, pred.true_id);
                self.map_value(exit_block, pred.source, pred.false_id);
            }
        }

        self.set_basic_block_terminator(Terminator::CondJump {
            condition: condition_value,
            true_target: body_block,
            true_args: vec![],
            false_target: exit_block,
            false_args: vec![],
        });

        self.seal_block(ctx, body_block);

        self.use_basic_block(body_block);

        ctx.module_builder.enter_scope(ScopeKind::WhileBody {
            break_target: exit_block,
            continue_target: header_block,
        });

        let _ = self.build_codeblock_expr(ctx, body);

        ctx.module_builder.exit_scope();

        if self.get_current_basic_block().terminator.is_none() {
            self.set_basic_block_terminator(Terminator::Jump {
                target: header_block,
                args: vec![],
            });
        }

        self.seal_block(ctx, header_block);
        self.use_basic_block(exit_block);
        self.seal_block(ctx, exit_block);
    }
}
