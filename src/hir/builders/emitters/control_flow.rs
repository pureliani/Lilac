use std::collections::HashSet;

use crate::{
    ast::Span,
    hir::{
        builders::{BasicBlockId, Builder, InBlock, PhiSource, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        instructions::{Instruction, Terminator},
        types::checked_type::Type,
        utils::adjustments::check_structural_compatibility,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn push_instruction(&mut self, instruction: Instruction) {
        self.check_no_terminator();
        let bb = self.bb_mut();
        bb.instructions.push(instruction);
    }

    pub fn check_no_terminator(&mut self) {
        let bb = self.bb_mut();

        if bb.terminator.is_some() {
            panic!(
                "INTERNAL COMPILER ERROR: Tried re-set terminator or tried to add an \
                 instruction to a basic block (ID: {}) that has already been terminated",
                bb.id.0
            );
        }
    }

    pub fn emit_call(
        &mut self,
        func: ValueId,
        args: Vec<ValueId>,
        return_type: Type,
    ) -> ValueId {
        let dest = self.new_value_id(return_type);
        self.push_instruction(Instruction::Call { dest, func, args });
        dest
    }

    pub fn emit_jmp(&mut self, target: BasicBlockId) {
        self.check_no_terminator();
        let this_block_id = self.context.block_id;
        self.get_bb_mut(target).predecessors.insert(this_block_id);

        self.bb_mut().terminator = Some(Terminator::Jump { target });
    }

    pub fn emit_cond_jmp(
        &mut self,
        condition: ValueId,
        true_target: BasicBlockId,
        false_target: BasicBlockId,
    ) {
        self.check_no_terminator();
        let this_block_id = self.context.block_id;

        self.get_bb_mut(true_target)
            .predecessors
            .insert(this_block_id);
        self.get_bb_mut(false_target)
            .predecessors
            .insert(this_block_id);

        self.bb_mut().terminator = Some(Terminator::CondJump {
            condition,
            true_target,
            false_target,
        });
    }

    pub fn emit_return(&mut self, value: ValueId) {
        self.check_no_terminator();
        self.bb_mut().terminator = Some(Terminator::Return { value })
    }

    pub fn emit_logical_or<F>(
        &mut self,
        left: ValueId,
        left_span: Span,
        produce_right: F,
    ) -> Result<ValueId, SemanticError>
    where
        F: FnOnce(&mut Self) -> Result<ValueId, SemanticError>,
    {
        let left_type = self.get_value_type(left);
        if !check_structural_compatibility(left_type, &Type::Bool) {
            return Err(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: Type::Bool,
                    received: left_type.clone(),
                },
                span: left_span,
            });
        }

        let left_block = self.context.block_id;
        let right_entry_block = self.as_fn().new_bb();
        let merge_block = self.as_fn().new_bb();

        self.emit_cond_jmp(left, merge_block, right_entry_block);

        self.seal_block(right_entry_block);
        self.use_basic_block(right_entry_block);

        let right = produce_right(self)?;
        let right_block = self.context.block_id;

        let right_type = self.get_value_type(right);
        if !check_structural_compatibility(right_type, &Type::Bool) {
            return Err(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: Type::Bool,
                    received: right_type.clone(),
                },
                span: left_span,
            });
        }

        self.emit_jmp(merge_block);

        self.seal_block(merge_block);
        self.use_basic_block(merge_block);

        let const_true = self.emit_const_bool(true);

        let phi_id = self.new_value_id(Type::Bool);
        let phi_sources = HashSet::from([
            PhiSource {
                from: left_block,
                value: const_true,
            },
            PhiSource {
                from: right_block,
                value: right,
            },
        ]);

        self.insert_phi(self.context.block_id, phi_id, phi_sources);

        Ok(phi_id)
    }

    pub fn emit_logical_and<F>(
        &mut self,
        left: ValueId,
        left_span: Span,
        produce_right: F,
    ) -> ValueId
    where
        F: FnOnce(&mut Self) -> ValueId,
    {
        let left_type = self.get_value_type(left);
        if !check_structural_compatibility(left_type, &Type::Bool) {
            self.errors.push(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: Type::Bool,
                    received: left_type.clone(),
                },
                span: left_span.clone(),
            });
        }

        let left_block = self.context.block_id;
        let right_entry_block = self.as_fn().new_bb();
        let merge_block = self.as_fn().new_bb();

        self.emit_cond_jmp(left, right_entry_block, merge_block);

        self.seal_block(right_entry_block);
        self.use_basic_block(right_entry_block);

        let right = produce_right(self);
        let right_block = self.context.block_id;

        let right_type = self.get_value_type(right);
        if !check_structural_compatibility(right_type, &Type::Bool) {
            self.errors.push(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: Type::Bool,
                    received: right_type.clone(),
                },
                span: left_span,
            });
        }

        self.emit_jmp(merge_block);

        self.seal_block(merge_block);
        self.use_basic_block(merge_block);

        let const_false = self.emit_const_bool(false);

        let phi_id = self.new_value_id(Type::Bool);
        let phi_sources = HashSet::from([
            PhiSource {
                from: left_block,
                value: const_false,
            },
            PhiSource {
                from: right_block,
                value: right,
            },
        ]);

        self.insert_phi(self.context.block_id, phi_id, phi_sources);

        phi_id
    }
}
