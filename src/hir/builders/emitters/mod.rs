pub mod binary;
pub mod cast;
pub mod comp;
pub mod r#const;
pub mod memory;
pub mod unary;

use std::collections::HashSet;

use crate::{
    ast::Span,
    hir::{
        builders::{BasicBlockId, Builder, InBlock, PhiEntry, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        instructions::{Instruction, Terminator},
        types::checked_type::Type,
        utils::{
            adjustments::{arithmetic_supertype, check_is_assignable},
            numeric::{is_float, is_signed},
        },
    },
};

impl<'a> Builder<'a, InBlock> {
    fn push_instruction(&mut self, instruction: Instruction) {
        self.check_no_terminator();
        let bb = self.bb_mut();
        bb.instructions.push(instruction);
    }

    fn check_no_terminator(&mut self) {
        let bb = self.bb_mut();

        if bb.terminator.is_some() {
            panic!(
                "INTERNAL COMPILER ERROR: Tried re-set terminator or tried to add an instruction to a basic block \
                 (ID: {}) that has already been terminated",
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

    pub fn jmp(&mut self, target: BasicBlockId) {
        self.check_no_terminator();
        let this_block_id = self.context.block_id;
        self.get_bb_mut(target).predecessors.insert(this_block_id);

        self.bb_mut().terminator = Some(Terminator::Jump { target });
    }

    pub fn cond_jmp(
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

    pub fn emit_return_terminator(&mut self, value: ValueId) {
        self.check_no_terminator();
        self.bb_mut().terminator = Some(Terminator::Return { value })
    }

    pub fn emit_unreachable_terminator(&mut self) {
        self.check_no_terminator();
        self.bb_mut().terminator = Some(Terminator::Unreachable)
    }

    pub fn add(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> Result<ValueId, SemanticError> {
        let lhs_ty = self.get_value_type(&lhs);
        let rhs_ty = self.get_value_type(&rhs);

        let target_ty =
            arithmetic_supertype(lhs_ty, lhs_span.clone(), rhs_ty, rhs_span.clone())?;

        let lhs = self.adjust_value(lhs, lhs_span, &target_ty)?;
        let rhs = self.adjust_value(rhs, rhs_span, &target_ty)?;

        if is_float(&target_ty) {
            Ok(self.emit_fadd(lhs, rhs))
        } else {
            Ok(self.emit_iadd(lhs, rhs))
        }
    }

    pub fn sub(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> Result<ValueId, SemanticError> {
        let lhs_ty = self.get_value_type(&lhs);
        let rhs_ty = self.get_value_type(&rhs);

        let target_ty =
            arithmetic_supertype(lhs_ty, lhs_span.clone(), rhs_ty, rhs_span.clone())?;

        let lhs = self.adjust_value(lhs, lhs_span, &target_ty)?;
        let rhs = self.adjust_value(rhs, rhs_span, &target_ty)?;

        if is_float(&target_ty) {
            Ok(self.emit_fsub(lhs, rhs))
        } else {
            Ok(self.emit_isub(lhs, rhs))
        }
    }

    pub fn mul(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> Result<ValueId, SemanticError> {
        let lhs_ty = self.get_value_type(&lhs);
        let rhs_ty = self.get_value_type(&rhs);

        let target_ty =
            arithmetic_supertype(lhs_ty, lhs_span.clone(), rhs_ty, rhs_span.clone())?;

        let lhs = self.adjust_value(lhs, lhs_span, &target_ty)?;
        let rhs = self.adjust_value(rhs, rhs_span, &target_ty)?;

        if is_float(&target_ty) {
            Ok(self.emit_fmul(lhs, rhs))
        } else {
            Ok(self.emit_imul(lhs, rhs))
        }
    }

    pub fn div(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> Result<ValueId, SemanticError> {
        let lhs_ty = self.get_value_type(&lhs);
        let rhs_ty = self.get_value_type(&rhs);

        let target_ty =
            arithmetic_supertype(lhs_ty, lhs_span.clone(), rhs_ty, rhs_span.clone())?;

        let lhs = self.adjust_value(lhs, lhs_span, &target_ty)?;
        let rhs = self.adjust_value(rhs, rhs_span, &target_ty)?;

        if is_float(&target_ty) {
            Ok(self.emit_fdiv(lhs, rhs))
        } else if is_signed(&target_ty) {
            Ok(self.emit_sdiv(lhs, rhs))
        } else {
            Ok(self.emit_udiv(lhs, rhs))
        }
    }

    pub fn rem(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> Result<ValueId, SemanticError> {
        let lhs_ty = self.get_value_type(&lhs);
        let rhs_ty = self.get_value_type(&rhs);

        let target_ty =
            arithmetic_supertype(lhs_ty, lhs_span.clone(), rhs_ty, rhs_span.clone())?;

        let lhs = self.adjust_value(lhs, lhs_span, &target_ty)?;
        let rhs = self.adjust_value(rhs, rhs_span, &target_ty)?;

        if is_float(&target_ty) {
            Ok(self.emit_frem(lhs, rhs))
        } else if is_signed(&target_ty) {
            Ok(self.emit_srem(lhs, rhs))
        } else {
            Ok(self.emit_urem(lhs, rhs))
        }
    }

    pub fn eq(
        &mut self,
        lhs: ValueId,
        _lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> Result<ValueId, SemanticError> {
        let lhs_ty = self.get_value_type(&lhs);
        let rhs_ty = self.get_value_type(&rhs);

        if !check_is_assignable(lhs_ty, rhs_ty) && !check_is_assignable(rhs_ty, lhs_ty) {
            return Err(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: lhs_ty.clone(),
                    received: rhs_ty.clone(),
                },
                span: rhs_span,
            });
        }

        if is_float(lhs_ty) {
            Ok(self.emit_feq(lhs, rhs))
        } else {
            Ok(self.emit_ieq(lhs, rhs))
        }
    }

    pub fn ne(
        &mut self,
        lhs: ValueId,
        _lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> Result<ValueId, SemanticError> {
        let lhs_ty = self.get_value_type(&lhs);
        let rhs_ty = self.get_value_type(&rhs);

        if !check_is_assignable(lhs_ty, rhs_ty) && !check_is_assignable(rhs_ty, lhs_ty) {
            return Err(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: lhs_ty.clone(),
                    received: rhs_ty.clone(),
                },
                span: rhs_span,
            });
        }

        if is_float(lhs_ty) {
            Ok(self.emit_fne(lhs, rhs))
        } else {
            Ok(self.emit_ine(lhs, rhs))
        }
    }

    pub fn lt(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> Result<ValueId, SemanticError> {
        let lhs_ty = self.get_value_type(&lhs);
        let rhs_ty = self.get_value_type(&rhs);

        let target_ty =
            arithmetic_supertype(lhs_ty, lhs_span.clone(), rhs_ty, rhs_span.clone())?;

        let lhs = self.adjust_value(lhs, lhs_span, &target_ty)?;
        let rhs = self.adjust_value(rhs, rhs_span, &target_ty)?;

        if is_float(&target_ty) {
            Ok(self.emit_flt(lhs, rhs))
        } else if is_signed(&target_ty) {
            Ok(self.emit_slt(lhs, rhs))
        } else {
            Ok(self.emit_ult(lhs, rhs))
        }
    }

    pub fn le(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> Result<ValueId, SemanticError> {
        let lhs_ty = self.get_value_type(&lhs);
        let rhs_ty = self.get_value_type(&rhs);

        let target_ty =
            arithmetic_supertype(lhs_ty, lhs_span.clone(), rhs_ty, rhs_span.clone())?;

        let lhs = self.adjust_value(lhs, lhs_span, &target_ty)?;
        let rhs = self.adjust_value(rhs, rhs_span, &target_ty)?;

        if is_float(&target_ty) {
            Ok(self.emit_fle(lhs, rhs))
        } else if is_signed(&target_ty) {
            Ok(self.emit_sle(lhs, rhs))
        } else {
            Ok(self.emit_ule(lhs, rhs))
        }
    }

    pub fn gt(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> Result<ValueId, SemanticError> {
        let lhs_ty = self.get_value_type(&lhs);
        let rhs_ty = self.get_value_type(&rhs);

        let target_ty =
            arithmetic_supertype(lhs_ty, lhs_span.clone(), rhs_ty, rhs_span.clone())?;

        let lhs = self.adjust_value(lhs, lhs_span, &target_ty)?;
        let rhs = self.adjust_value(rhs, rhs_span, &target_ty)?;

        if is_float(&target_ty) {
            Ok(self.emit_fgt(lhs, rhs))
        } else if is_signed(&target_ty) {
            Ok(self.emit_sgt(lhs, rhs))
        } else {
            Ok(self.emit_ugt(lhs, rhs))
        }
    }

    pub fn ge(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> Result<ValueId, SemanticError> {
        let lhs_ty = self.get_value_type(&lhs);
        let rhs_ty = self.get_value_type(&rhs);

        let target_ty =
            arithmetic_supertype(lhs_ty, lhs_span.clone(), rhs_ty, rhs_span.clone())?;

        let lhs = self.adjust_value(lhs, lhs_span, &target_ty)?;
        let rhs = self.adjust_value(rhs, rhs_span, &target_ty)?;

        if is_float(&target_ty) {
            Ok(self.emit_fge(lhs, rhs))
        } else if is_signed(&target_ty) {
            Ok(self.emit_sge(lhs, rhs))
        } else {
            Ok(self.emit_uge(lhs, rhs))
        }
    }

    pub fn neg(&mut self, src: ValueId, span: Span) -> Result<ValueId, SemanticError> {
        let ty = self.get_value_type(&src);

        if is_float(ty) {
            Ok(self.emit_fneg(src))
        } else if is_signed(ty) {
            Ok(self.emit_ineg(src))
        } else {
            Err(SemanticError {
                kind: SemanticErrorKind::ExpectedANumericOperand,
                span,
            })
        }
    }

    pub fn not(&mut self, src: ValueId, span: Span) -> Result<ValueId, SemanticError> {
        let ty = self.get_value_type(&src);

        if !check_is_assignable(ty, &Type::Bool) {
            return Err(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: Type::Bool,
                    received: ty.clone(),
                },
                span,
            });
        }

        Ok(self.emit_bnot(src))
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
        let left_type = self.get_value_type(&left);
        if !check_is_assignable(left_type, &Type::Bool) {
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

        self.cond_jmp(left, merge_block, right_entry_block);

        self.seal_block(right_entry_block)?;
        self.use_basic_block(right_entry_block);

        let right = produce_right(self)?;
        let right_block = self.context.block_id;

        let right_type = self.get_value_type(&right);
        if !check_is_assignable(right_type, &Type::Bool) {
            return Err(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: Type::Bool,
                    received: right_type.clone(),
                },
                span: left_span,
            });
        }

        self.jmp(merge_block);

        self.seal_block(merge_block)?;
        self.use_basic_block(merge_block);

        let const_true = self.emit_const_bool(true);

        let result_id = self.new_value_id(Type::Bool);
        let phi_operands = HashSet::from([
            PhiEntry {
                from: left_block,
                value: const_true,
            },
            PhiEntry {
                from: right_block,
                value: right,
            },
        ]);

        self.bb_mut().phis.insert(result_id, phi_operands);

        Ok(result_id)
    }

    pub fn emit_logical_and<F>(
        &mut self,
        left: ValueId,
        left_span: Span,
        produce_right: F,
    ) -> Result<ValueId, SemanticError>
    where
        F: FnOnce(&mut Self) -> Result<ValueId, SemanticError>,
    {
        let left_type = self.get_value_type(&left);
        if !check_is_assignable(left_type, &Type::Bool) {
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

        self.cond_jmp(left, right_entry_block, merge_block);

        self.seal_block(right_entry_block)?;
        self.use_basic_block(right_entry_block);

        let right = produce_right(self)?;
        let right_block = self.context.block_id;

        let right_type = self.get_value_type(&right);
        if !check_is_assignable(right_type, &Type::Bool) {
            return Err(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: Type::Bool,
                    received: right_type.clone(),
                },
                span: left_span,
            });
        }

        self.jmp(merge_block);

        self.seal_block(merge_block)?;
        self.use_basic_block(merge_block);

        let const_false = self.emit_const_bool(false);

        let result_id = self.new_value_id(Type::Bool);
        let phi_operands = HashSet::from([
            PhiEntry {
                from: left_block,
                value: const_false,
            },
            PhiEntry {
                from: right_block,
                value: right,
            },
        ]);

        self.bb_mut().phis.insert(result_id, phi_operands);

        Ok(result_id)
    }
}
