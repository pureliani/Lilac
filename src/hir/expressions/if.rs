use std::collections::HashMap;

use crate::{
    ast::{
        expr::{BlockContents, Expr},
        Span,
    },
    hir::{
        builders::{BasicBlockId, Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_type::Type,
        utils::{
            check_is_assignable::check_is_assignable, try_unify_types::try_unify_types,
        },
    },
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum IfContext {
    /// The `if` is used to produce a value
    Expression,
    /// The `if` is used for control flow, its value is discarded
    Statement,
}

impl<'a> Builder<'a, InBlock> {
    pub fn build_if(
        &mut self,
        branches: Vec<(Box<Expr>, BlockContents)>,
        else_branch: Option<BlockContents>,
        context: IfContext,
    ) -> ValueId {
        if context == IfContext::Expression && else_branch.is_none() {
            let span = branches.first().unwrap().0.span.clone();
            return self.report_error_and_get_poison(SemanticError {
                kind: SemanticErrorKind::IfExpressionMissingElse,
                span,
            });
        }

        let merge_block_id = self.as_fn().new_bb();
        let mut branch_results: Vec<(BasicBlockId, ValueId, Span)> = Vec::new();
        let mut current_condition_block_id = self.context.block_id;

        let get_final_expr_span = |block: &BlockContents| {
            block
                .final_expr
                .as_ref()
                .map(|f| &f.span)
                .unwrap_or(&block.span)
                .clone()
        };

        for (condition, body) in branches {
            self.use_basic_block(current_condition_block_id);

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

            let then_block_id = self.as_fn().new_bb();
            let next_cond_block_id = self.as_fn().new_bb();

            if let Some(pred) = self.get_fn().predicates.get(&cond_id).cloned() {
                self.get_bb_mut(then_block_id)
                    .original_to_local_valueid
                    .insert(pred.source, pred.true_id);

                self.get_bb_mut(next_cond_block_id)
                    .original_to_local_valueid
                    .insert(pred.source, pred.false_id);
            }

            self.cond_jmp(
                cond_id,
                then_block_id,
                HashMap::new(),
                next_cond_block_id,
                HashMap::new(),
            );

            self.seal_block(then_block_id);
            self.use_basic_block(then_block_id);
            let final_expr_span = get_final_expr_span(&body);
            let then_val = self.build_codeblock_expr(body);

            if self.bb().terminator.is_none() {
                branch_results.push((self.context.block_id, then_val, final_expr_span));
            }

            current_condition_block_id = next_cond_block_id;
        }

        self.use_basic_block(current_condition_block_id);
        if let Some(else_body) = else_branch {
            let final_expr_span = get_final_expr_span(&else_body);
            let else_val = self.build_codeblock_expr(else_body);

            if self.bb().terminator.is_none() {
                branch_results.push((self.context.block_id, else_val, final_expr_span));
            }
        } else {
            self.jmp(merge_block_id, HashMap::new());
        }

        self.seal_block(current_condition_block_id);

        let result_param_id = if context == IfContext::Expression {
            let type_entries: Vec<(Type, Span)> = branch_results
                .iter()
                .map(|(_, val, span)| (self.get_value_type(val).clone(), span.clone()))
                .collect();

            let result_type = match try_unify_types(&type_entries) {
                Ok(ty) => ty,
                Err(e) => {
                    self.errors.push(e);
                    Type::Unknown
                }
            };

            Some(self.append_param_to_block(merge_block_id, result_type))
        } else {
            None
        };

        for (block_id, val, _) in branch_results {
            self.use_basic_block(block_id);
            let mut args = HashMap::new();
            if let Some(param_id) = result_param_id {
                args.insert(param_id, val);
            }
            self.jmp(merge_block_id, args);
        }

        self.seal_block(merge_block_id);
        self.use_basic_block(merge_block_id);

        result_param_id.unwrap_or_else(|| self.emit_const_void())
    }
}
