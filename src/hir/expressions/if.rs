use std::collections::HashSet;

use crate::{
    ast::{
        expr::{BlockContents, Expr},
        Span,
    },
    hir::{
        builders::{BasicBlockId, Builder, InBlock, PhiSource, TypePredicate, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_type::Type,
        utils::adjustments::check_structural_compatibility,
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
        let expr_span = branches.first().unwrap().0.span.clone();

        if context == IfContext::Expression && else_branch.is_none() {
            return self.report_error_and_get_poison(SemanticError {
                kind: SemanticErrorKind::IfExpressionMissingElse,
                span: expr_span,
            });
        }

        let merge_block_id = self.as_fn().new_bb();
        let mut branch_results: Vec<(BasicBlockId, ValueId, Span)> = Vec::new();
        let mut current_cond_block_id = self.context.block_id;

        for (condition, body) in branches {
            self.use_basic_block(current_cond_block_id);

            let condition_span = condition.span.clone();
            let cond_id = self.build_expr(*condition);
            let cond_ty = self.get_value_type(cond_id);

            if !check_structural_compatibility(cond_ty, &Type::Bool) {
                return self.report_error_and_get_poison(SemanticError {
                    span: condition_span,
                    kind: SemanticErrorKind::TypeMismatch {
                        expected: Type::Bool,
                        received: cond_ty.clone(),
                    },
                });
            }

            let then_block_id = self.as_fn().new_bb();
            let next_cond_block_id = self.as_fn().new_bb();

            self.emit_cond_jmp(cond_id, then_block_id, next_cond_block_id);

            self.seal_block(then_block_id);
            self.use_basic_block(then_block_id);

            if let Some(preds) = self.type_predicates.get(&cond_id).cloned() {
                self.apply_predicate_list(&preds, true, &condition_span);
            }

            let (then_val, then_val_span) = self.build_codeblock_expr(body);

            if self.bb().terminator.is_none() {
                branch_results.push((self.context.block_id, then_val, then_val_span));
                self.emit_jmp(merge_block_id);
            }

            self.use_basic_block(next_cond_block_id);

            if let Some(preds) = self.type_predicates.get(&cond_id).cloned() {
                self.apply_predicate_list(&preds, false, &condition_span);
            }

            current_cond_block_id = next_cond_block_id;
        }

        self.use_basic_block(current_cond_block_id);

        if let Some(else_body) = else_branch {
            let (else_val, else_val_span) = self.build_codeblock_expr(else_body);

            if self.bb().terminator.is_none() {
                branch_results.push((self.context.block_id, else_val, else_val_span));
                self.emit_jmp(merge_block_id);
            }
        } else {
            self.emit_jmp(merge_block_id);
        }

        self.seal_block(current_cond_block_id);

        self.seal_block(merge_block_id);
        self.use_basic_block(merge_block_id);

        if context == IfContext::Expression {
            if branch_results.is_empty() {
                return self.new_value_id(Type::Never);
            }

            let type_entries: Vec<Type> = branch_results
                .iter()
                .map(|(_, val, _)| self.get_value_type(*val).clone())
                .collect();

            let result_type = Type::make_union(type_entries);

            if result_type.as_union_variants().is_some() {
                let mut coerced_sources = HashSet::new();

                for (from_block, val, span) in branch_results {
                    let (coercion_block, coerced) =
                        self.insert_on_edge(from_block, merge_block_id, |b| {
                            b.coerce_to_union(val, &result_type, span)
                        });

                    coerced_sources.insert(PhiSource {
                        from: coercion_block,
                        value: coerced,
                    });
                }

                let phi_id = self.new_value_id(result_type);
                self.insert_phi(merge_block_id, phi_id, coerced_sources);
                phi_id
            } else {
                let phi_id = self.new_value_id(result_type);
                let phi_sources: HashSet<PhiSource> = branch_results
                    .into_iter()
                    .map(|(block, value, _)| PhiSource { from: block, value })
                    .collect();
                self.insert_phi(merge_block_id, phi_id, phi_sources);
                phi_id
            }
        } else {
            self.emit_const_void()
        }
    }

    pub fn apply_type_predicate(
        &mut self,
        pred: &TypePredicate,
        new_type: Type,
        span: Span,
    ) {
        let current_val =
            self.read_variable(pred.decl_id, self.context.block_id, span.clone());
        let current_ty = self.get_value_type(current_val).clone();

        if check_structural_compatibility(&current_ty, &new_type) {
            return;
        }

        if let Ok(adjusted) = self.adjust_value(current_val, span, new_type, true) {
            self.write_variable(pred.decl_id, self.context.block_id, adjusted);
        }
    }

    pub fn apply_predicate_list(
        &mut self,
        preds: &[TypePredicate],
        use_true_side: bool,
        span: &Span,
    ) {
        for pred in preds {
            let ty = if use_true_side {
                &pred.on_true_type
            } else {
                &pred.on_false_type
            };

            if let Some(ty) = ty {
                self.apply_type_predicate(pred, ty.clone(), span.clone());
            }
        }
    }
}
