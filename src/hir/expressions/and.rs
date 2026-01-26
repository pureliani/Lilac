use std::collections::HashSet;

use crate::{
    ast::expr::Expr,
    hir::{
        builders::{Builder, InBlock, PhiEntry, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_type::Type,
        utils::check_is_assignable::check_is_assignable,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_and_expr(
        &mut self,
        left: Expr,
        right: Expr,
    ) -> Result<ValueId, SemanticError> {
        let right_entry_block = self.as_fn().new_bb();
        let merge_block = self.as_fn().new_bb();

        let left_span = left.span.clone();
        let left_id = self.build_expr(left)?;
        let left_block = self.context.block_id;
        let left_type = self.get_value_type(&left_id);

        if !check_is_assignable(left_type, &Type::Bool) {
            return Err(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: Type::Bool,
                    received: left_type.clone(),
                },
                span: left_span,
            });
        }

        let const_false = self.emit_const_bool(false);
        self.cond_jmp(left_id, right_entry_block, merge_block);

        self.seal_block(right_entry_block)?;
        self.use_basic_block(right_entry_block);

        let right_span = right.span.clone();
        let right_id = self.build_expr(right)?;
        let right_block = self.context.block_id;
        let right_type = self.get_value_type(&right_id);

        if !check_is_assignable(right_type, &Type::Bool) {
            return Err(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: Type::Bool,
                    received: right_type.clone(),
                },
                span: right_span,
            });
        }

        self.jmp(merge_block);

        self.seal_block(merge_block)?;
        self.use_basic_block(merge_block);

        let result_id = self.new_value_id(Type::Bool);
        let phi_operands = HashSet::from([
            PhiEntry {
                from: left_block,
                value: const_false,
            },
            PhiEntry {
                from: right_block,
                value: right_id,
            },
        ]);

        self.bb_mut().phis.insert(result_id, phi_operands);

        Ok(result_id)
    }
}
