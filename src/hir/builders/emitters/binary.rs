use crate::{
    ast::Span,
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::SemanticError,
        instructions::{BinaryInstr, Instruction},
        utils::{
            adjustments::arithmetic_supertype,
            numeric::{is_float, is_signed},
        },
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn emit_iadd(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(&lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Binary(BinaryInstr::IAdd { dest, lhs, rhs }));
        dest
    }

    pub fn emit_isub(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(&lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Binary(BinaryInstr::ISub { dest, lhs, rhs }));
        dest
    }

    pub fn emit_imul(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(&lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Binary(BinaryInstr::IMul { dest, lhs, rhs }));
        dest
    }

    pub fn emit_sdiv(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(&lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Binary(BinaryInstr::SDiv { dest, lhs, rhs }));
        dest
    }

    pub fn emit_udiv(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(&lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Binary(BinaryInstr::UDiv { dest, lhs, rhs }));
        dest
    }

    pub fn emit_srem(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(&lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Binary(BinaryInstr::SRem { dest, lhs, rhs }));
        dest
    }

    pub fn emit_urem(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(&lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Binary(BinaryInstr::URem { dest, lhs, rhs }));
        dest
    }

    pub fn emit_frem(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(&lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Binary(BinaryInstr::FRem { dest, lhs, rhs }));
        dest
    }

    pub fn emit_fadd(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(&lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Binary(BinaryInstr::FAdd { dest, lhs, rhs }));
        dest
    }

    pub fn emit_fsub(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(&lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Binary(BinaryInstr::FSub { dest, lhs, rhs }));
        dest
    }

    pub fn emit_fmul(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(&lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Binary(BinaryInstr::FMul { dest, lhs, rhs }));
        dest
    }

    pub fn emit_fdiv(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(&lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Binary(BinaryInstr::FDiv { dest, lhs, rhs }));
        dest
    }
}

impl<'a> Builder<'a, InBlock> {
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

        let lhs = self.adjust_value(lhs, lhs_span, target_ty.clone(), false)?;
        let rhs = self.adjust_value(rhs, rhs_span, target_ty.clone(), false)?;

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

        let lhs = self.adjust_value(lhs, lhs_span, target_ty.clone(), false)?;
        let rhs = self.adjust_value(rhs, rhs_span, target_ty.clone(), false)?;

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

        let lhs = self.adjust_value(lhs, lhs_span, target_ty.clone(), false)?;
        let rhs = self.adjust_value(rhs, rhs_span, target_ty.clone(), false)?;

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

        let lhs = self.adjust_value(lhs, lhs_span, target_ty.clone(), false)?;
        let rhs = self.adjust_value(rhs, rhs_span, target_ty.clone(), false)?;

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

        let lhs = self.adjust_value(lhs, lhs_span, target_ty.clone(), false)?;
        let rhs = self.adjust_value(rhs, rhs_span, target_ty.clone(), false)?;

        if is_float(&target_ty) {
            Ok(self.emit_frem(lhs, rhs))
        } else if is_signed(&target_ty) {
            Ok(self.emit_srem(lhs, rhs))
        } else {
            Ok(self.emit_urem(lhs, rhs))
        }
    }
}
