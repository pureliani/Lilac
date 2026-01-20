use crate::{
    ast::expr::{BlockContents, Expr},
    hir::{
        builders::{Builder, InBlock},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_type::Type,
        utils::{check_is_assignable::check_is_assignable, scope::ScopeKind},
    },
};
use std::collections::HashMap;

impl<'a> Builder<'a, InBlock> {
    pub fn build_while_stmt(&mut self, condition: Box<Expr>, body: BlockContents) {
        let header_block_id = self.as_fn().new_bb();
        let body_block_id = self.as_fn().new_bb();
        let exit_block_id = self.as_fn().new_bb();

        self.jmp(header_block_id, HashMap::new());
        self.use_basic_block(header_block_id);

        let condition_span = condition.span.clone();
        let cond_id = self.build_expr(*condition);
        let cond_ty = self.get_value_type(&cond_id);

        if !check_is_assignable(cond_ty, &Type::Bool) {
            self.errors.push(SemanticError {
                span: condition_span,
                kind: SemanticErrorKind::TypeMismatch {
                    expected: Type::Bool,
                    received: cond_ty.clone(),
                },
            });
        }

        let predicate = self.get_fn().predicates.get(&cond_id).cloned();
        if let Some(pred) = predicate {
            self.get_bb_mut(body_block_id)
                .original_to_local_valueid
                .insert(pred.source, pred.true_id);

            self.get_bb_mut(exit_block_id)
                .original_to_local_valueid
                .insert(pred.source, pred.false_id);
        }

        self.cond_jmp(
            cond_id,
            body_block_id,
            HashMap::new(),
            exit_block_id,
            HashMap::new(),
        );

        self.seal_block(body_block_id);
        self.use_basic_block(body_block_id);

        self.current_scope = self.current_scope.enter(
            ScopeKind::WhileBody {
                break_target: exit_block_id,
                continue_target: header_block_id,
            },
            body.span.start,
        );

        self.build_statements(body.statements);
        if let Some(final_expr) = body.final_expr {
            self.build_expr(*final_expr);
        }

        self.current_scope = self
            .current_scope
            .exit(body.span.end)
            .expect("INTERNAL COMPILER ERROR: Scope mismatch");

        if self.bb().terminator.is_none() {
            self.jmp(header_block_id, HashMap::new());
        }

        self.seal_block(header_block_id);

        self.use_basic_block(exit_block_id);
        self.seal_block(exit_block_id);
    }
}
