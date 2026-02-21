use std::collections::{HashMap, HashSet};

use crate::{
    ast::{
        expr::{BlockContents, Expr},
        Span,
    },
    hir::{
        builders::{BasicBlockId, Builder, InBlock, PhiSource, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_type::Type,
        utils::{adjustments::check_structural_compatibility, place::Place},
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
        let mut branch_states: Vec<(BasicBlockId, HashMap<Place, ValueId>)> = Vec::new();
        let mut current_cond_block_id = self.context.block_id;
        let mut current_narrowed_fields = self.narrowed_fields.clone();

        for (condition, body) in branches {
            self.use_basic_block(current_cond_block_id);

            *self.narrowed_fields = current_narrowed_fields.clone();

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

            *self.narrowed_fields = current_narrowed_fields.clone();

            if let Some(pred) = self.type_predicates.get(&cond_id).cloned() {
                if let Some(ty) = pred.on_true_type {
                    self.apply_narrowing(pred.place.clone(), ty, condition_span.clone());
                }
            }

            let (then_val, then_val_span) = self.build_codeblock_expr(body);

            if self.bb().terminator.is_none() {
                branch_results.push((self.context.block_id, then_val, then_val_span));
                branch_states.push((self.context.block_id, self.narrowed_fields.clone()));
                self.emit_jmp(merge_block_id);
            }

            self.use_basic_block(next_cond_block_id);

            *self.narrowed_fields = current_narrowed_fields.clone();

            if let Some(pred) = self.type_predicates.get(&cond_id).cloned() {
                if let Some(ty) = pred.on_false_type {
                    self.apply_narrowing(pred.place.clone(), ty, condition_span.clone());
                }
            }

            current_narrowed_fields = self.narrowed_fields.clone();
            current_cond_block_id = next_cond_block_id;
        }

        self.use_basic_block(current_cond_block_id);
        *self.narrowed_fields = current_narrowed_fields.clone();

        if let Some(else_body) = else_branch {
            let (else_val, else_val_span) = self.build_codeblock_expr(else_body);

            if self.bb().terminator.is_none() {
                branch_results.push((self.context.block_id, else_val, else_val_span));
                branch_states.push((self.context.block_id, self.narrowed_fields.clone()));
                self.emit_jmp(merge_block_id);
            }
        } else {
            branch_states.push((self.context.block_id, self.narrowed_fields.clone()));
            self.emit_jmp(merge_block_id);
        }

        self.seal_block(current_cond_block_id);

        self.seal_block(merge_block_id);
        self.use_basic_block(merge_block_id);

        let mut merged_fields = HashMap::new();

        if !branch_states.is_empty() {
            let (_, first_state) = &branch_states[0];

            for (place, &first_val) in first_state {
                let mut all_have_it = true;
                let mut all_same_val = true;
                let mut incoming = Vec::new();
                incoming.push((branch_states[0].0, first_val));

                for (block_id, state) in branch_states.iter().skip(1) {
                    if let Some(&val) = state.get(place) {
                        incoming.push((*block_id, val));
                        if val != first_val {
                            all_same_val = false;
                        }
                    } else {
                        all_have_it = false;
                        break;
                    }
                }

                if all_have_it {
                    if all_same_val {
                        merged_fields.insert(place.clone(), first_val);
                    } else {
                        let mut phi_sources = HashSet::new();
                        let mut incoming_types = Vec::new();

                        for (pred, val) in incoming {
                            phi_sources.insert(PhiSource {
                                from: pred,
                                value: val,
                            });
                            incoming_types.push(self.get_value_type(val).clone());
                        }

                        let unified_type = Type::make_union(incoming_types);

                        let final_sources = if unified_type.as_union_variants().is_some()
                        {
                            let current_block = self.context.block_id;
                            let mut coerced_sources = HashSet::new();
                            for source in phi_sources {
                                let coercion_block =
                                    self.get_coercion_block(source.from, merge_block_id);
                                self.use_basic_block(coercion_block);
                                let coerced = self.coerce_to_union(
                                    source.value,
                                    &unified_type,
                                    expr_span.clone(),
                                );
                                coerced_sources.insert(PhiSource {
                                    from: coercion_block,
                                    value: coerced,
                                });
                            }
                            self.use_basic_block(current_block);
                            coerced_sources
                        } else {
                            phi_sources
                        };

                        let phi_id = self.new_value_id(unified_type);
                        self.insert_phi(merge_block_id, phi_id, final_sources);

                        merged_fields.insert(place.clone(), phi_id);
                    }
                }
            }
        }

        *self.narrowed_fields = merged_fields;

        if context == IfContext::Expression {
            let type_entries: Vec<Type> = branch_results
                .iter()
                .map(|(_, val, _)| self.get_value_type(*val).clone())
                .collect();

            let result_type = Type::make_union(type_entries);

            if result_type.as_union_variants().is_some() {
                let current_block = self.context.block_id;
                let mut coerced_sources = HashSet::new();

                for (from_block, val, span) in branch_results {
                    let coercion_block =
                        self.get_coercion_block(from_block, merge_block_id);
                    self.use_basic_block(coercion_block);
                    let coerced = self.coerce_to_union(val, &result_type, span);

                    coerced_sources.insert(PhiSource {
                        from: coercion_block,
                        value: coerced,
                    });
                }

                self.use_basic_block(current_block);
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

    /// Narrows a place to a more specific type after a type predicate
    /// has confirmed the variant.
    pub fn apply_narrowing(&mut self, place: Place, new_type: Type, span: Span) {
        let current_val = self.read_place(&place, span.clone());
        let current_ty = self.get_value_type(current_val).clone();

        if check_structural_compatibility(&current_ty, &new_type) {
            return;
        }

        if new_type.as_union_variants().is_none() {
            let narrowed = self.emit_unwrap_from_union(current_val, &new_type);
            self.remap_place(&place, narrowed);
            return;
        }

        let target_variants = new_type.as_union_variants().unwrap();
        let narrowed = self.emit_narrow_union(current_val, target_variants);
        self.remap_place(&place, narrowed);
    }
}
