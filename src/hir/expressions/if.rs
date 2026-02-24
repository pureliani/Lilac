use std::collections::HashSet;

use crate::{
    ast::{
        expr::{BlockContents, Expr, ExprKind},
        Span,
    },
    hir::{
        builders::{BasicBlockId, Builder, InBlock, PhiSource, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        instructions::{CastInstr, Instruction},
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
            let cond_id = self.build_expr(*condition.clone());
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

            if let Some(pred) = self.type_predicates.get(&cond_id).cloned() {
                if let Some(ty) = pred.on_true_type {
                    self.apply_type_predicate(&pred.target, ty, condition_span.clone());
                }
            }

            let (then_val, then_val_span) = self.build_codeblock_expr(body);

            if self.bb().terminator.is_none() {
                branch_results.push((self.context.block_id, then_val, then_val_span));
                self.emit_jmp(merge_block_id);
            }

            self.use_basic_block(next_cond_block_id);

            if let Some(pred) = self.type_predicates.get(&cond_id).cloned() {
                if let Some(ty) = pred.on_false_type {
                    self.apply_type_predicate(&pred.target, ty, condition_span.clone());
                }
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
        target_expr: &Expr,
        new_type: Type,
        span: Span,
    ) {
        let current_val = self.build_expr(target_expr.clone());
        let current_ty = self.get_value_type(current_val).clone();

        if check_structural_compatibility(&current_ty, &new_type) {
            return;
        }

        if let Ok(adjusted_val) =
            self.adjust_value(current_val, span.clone(), new_type, true)
        {
            self.bubble_type_cast(target_expr, adjusted_val, span);
        }
    }

    fn bubble_type_cast(&mut self, target: &Expr, casted_val: ValueId, span: Span) {
        match &target.kind {
            ExprKind::Identifier(ident) => {
                if let Some(decl_id) = self.current_scope.lookup(ident.name) {
                    self.write_variable(decl_id, self.context.block_id, casted_val);
                }
            }
            ExprKind::Access { left, field } => {
                let base_val = self.build_expr(*left.clone());
                let mut base_ty = self.get_value_type(base_val).clone();
                let casted_ty = self.get_value_type(casted_val).clone();

                if let Type::Struct(ref mut fields) = base_ty {
                    for f in fields.iter_mut() {
                        if f.identifier.name == field.name {
                            f.ty = casted_ty.clone();
                            break;
                        }
                    }
                }

                let new_base_val = self.new_value_id(base_ty);
                self.push_instruction(Instruction::Cast(CastInstr {
                    src: base_val,
                    dest: new_base_val,
                }));

                self.bubble_type_cast(left, new_base_val, span);
            }
            _ => {}
        }
    }
}
