use crate::{
    ast::expr::Expr,
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_type::Type,
        utils::check_is_assignable::check_is_assignable,
    },
};
use std::collections::HashMap;

impl<'a> Builder<'a, InBlock> {
    pub fn build_and_expr(&mut self, left: Box<Expr>, right: Box<Expr>) -> ValueId {
        let right_entry_block = self.as_fn().new_bb();
        let merge_block = self.as_fn().new_bb();

        let result_param = self.append_param_to_block(merge_block, Type::Bool);

        let left_span = left.span.clone();
        let left_id = self.build_expr(*left);
        let left_type = self.get_value_type(&left_id);

        if !check_is_assignable(left_type, &Type::Bool) {
            return self.report_error_and_get_poison(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: Type::Bool,
                    received: left_type.clone(),
                },
                span: left_span,
            });
        }

        if let Some(pred) = self.get_fn().predicates.get(&left_id).cloned() {
            self.get_bb_mut(right_entry_block)
                .original_to_local_valueid
                .insert(pred.source, pred.true_id);

            self.get_bb_mut(merge_block)
                .original_to_local_valueid
                .insert(pred.source, pred.false_id);
        }

        let const_false = self.emit_const_bool(false);
        self.cond_jmp(
            left_id,
            right_entry_block,
            HashMap::new(),
            merge_block,
            HashMap::from([(result_param, const_false)]),
        );

        self.seal_block(right_entry_block);
        self.use_basic_block(right_entry_block);

        let right_span = right.span.clone();
        let right_id = self.build_expr(*right);
        let right_type = self.get_value_type(&right_id);

        if !check_is_assignable(right_type, &Type::Bool) {
            return self.report_error_and_get_poison(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: Type::Bool,
                    received: right_type.clone(),
                },
                span: right_span,
            });
        }

        if let Some(pred) = self.get_fn().predicates.get(&right_id).cloned() {
            self.get_fn().predicates.insert(result_param, pred);
        }

        self.jmp(merge_block, HashMap::from([(result_param, right_id)]));

        self.seal_block(merge_block);
        self.use_basic_block(merge_block);

        result_param
    }
}
