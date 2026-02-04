use std::collections::HashSet;

use crate::{
    ast::{
        expr::{BlockContents, Expr},
        Span,
    },
    globals::COMMON_IDENTIFIERS,
    hir::{
        builders::{BasicBlockId, Builder, InBlock, PhiEntry, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_type::{StructKind, Type},
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
    ) -> Result<ValueId, SemanticError> {
        if context == IfContext::Expression && else_branch.is_none() {
            let span = branches.first().unwrap().0.span.clone();
            return Err(SemanticError {
                kind: SemanticErrorKind::IfExpressionMissingElse,
                span,
            });
        }

        let merge_block_id = self.as_fn().new_bb();
        let mut branch_results: Vec<(BasicBlockId, ValueId, Span)> = Vec::new();
        let mut current_cond_block_id = self.context.block_id;

        for (condition, body) in branches {
            self.use_basic_block(current_cond_block_id);

            let condition_span = condition.span.clone();
            let cond_id = self.build_expr(*condition)?;
            let cond_ty = self.get_value_type(&cond_id);

            if !check_structural_compatibility(cond_ty, &Type::Bool) {
                return Err(SemanticError {
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

            self.seal_block(then_block_id)?;
            self.use_basic_block(then_block_id);

            if let Some(pred) = self.type_predicates.get(&cond_id).cloned() {
                if let Some(ty) = pred.on_true_type {
                    self.apply_narrowing(pred.place, ty, condition_span.clone())?
                }
            }

            let (then_val, then_val_span) = self.build_codeblock_expr(body)?;

            if self.bb().terminator.is_none() {
                branch_results.push((self.context.block_id, then_val, then_val_span));
                self.emit_jmp(merge_block_id);
            }

            self.use_basic_block(next_cond_block_id);

            if let Some(pred) = self.type_predicates.get(&cond_id).cloned() {
                if let Some(ty) = pred.on_false_type {
                    self.apply_narrowing(pred.place, ty, condition_span.clone())?
                }
            }
            current_cond_block_id = next_cond_block_id;
        }

        self.use_basic_block(current_cond_block_id);
        if let Some(else_body) = else_branch {
            let (else_val, else_val_span) = self.build_codeblock_expr(else_body)?;

            if self.bb().terminator.is_none() {
                branch_results.push((self.context.block_id, else_val, else_val_span));
                self.emit_jmp(merge_block_id);
            }
        } else {
            self.emit_jmp(merge_block_id);
        }

        self.seal_block(current_cond_block_id)?;

        self.seal_block(merge_block_id)?;
        self.use_basic_block(merge_block_id);

        if context == IfContext::Expression {
            let type_entries: Vec<Type> = branch_results
                .iter()
                .map(|(_, val, _)| self.get_value_type(val).clone())
                .collect();

            let result_type = Type::make_union(type_entries);

            let result_id = self.new_value_id(result_type);
            let phi_operands: HashSet<PhiEntry> = branch_results
                .into_iter()
                .map(|(block, value, _)| PhiEntry { from: block, value })
                .collect();

            self.bb_mut().phis.insert(result_id, phi_operands);
            Ok(result_id)
        } else {
            Ok(self.emit_const_void())
        }
    }

    fn apply_narrowing(
        &mut self,
        place: Place,
        new_type: Type,
        span: Span,
    ) -> Result<(), SemanticError> {
        let current_val = self.read_place(&place, span)?;
        let current_ty = self.get_value_type(&current_val).clone();

        let refined_val = if let Type::Pointer(inner) = &current_ty {
            if let Type::Struct(StructKind::Union(_)) = &**inner {
                let buffer_ptr =
                    self.get_field_ptr(current_val, COMMON_IDENTIFIERS.value);

                // unsafe bitcast is okay here because it has the
                // alignment and size of the largest variant of the union
                self.emit_bitcast_unsafe(buffer_ptr, new_type)
            } else {
                self.emit_bitcast(current_val, new_type)
            }
        } else {
            self.emit_bitcast(current_val, new_type)
        };

        self.remap_place(&place, refined_val);
        Ok(())
    }
}
