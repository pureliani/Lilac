use crate::{
    ast::Span,
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        instructions::{CompInstr, Instruction},
        types::checked_type::Type,
        utils::{
            adjustments::{
                arithmetic_supertype, check_is_assignable, check_structural_compatibility,
            },
            numeric::{is_float, is_signed},
        },
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn emit_ieq(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Comp(CompInstr::IEq { dest, lhs, rhs }));
        dest
    }

    pub fn emit_ine(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Comp(CompInstr::INe { dest, lhs, rhs }));
        dest
    }

    pub fn emit_slt(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Comp(CompInstr::SLt { dest, lhs, rhs }));
        dest
    }

    pub fn emit_sle(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Comp(CompInstr::SLe { dest, lhs, rhs }));
        dest
    }

    pub fn emit_sgt(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Comp(CompInstr::SGt { dest, lhs, rhs }));
        dest
    }

    pub fn emit_sge(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Comp(CompInstr::SGe { dest, lhs, rhs }));
        dest
    }

    pub fn emit_ult(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Comp(CompInstr::ULt { dest, lhs, rhs }));
        dest
    }

    pub fn emit_ule(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Comp(CompInstr::ULe { dest, lhs, rhs }));
        dest
    }

    pub fn emit_ugt(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Comp(CompInstr::UGt { dest, lhs, rhs }));
        dest
    }

    pub fn emit_uge(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Comp(CompInstr::UGe { dest, lhs, rhs }));
        dest
    }

    pub fn emit_feq(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Comp(CompInstr::FEq { dest, lhs, rhs }));
        dest
    }

    pub fn emit_fne(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Comp(CompInstr::FNe { dest, lhs, rhs }));
        dest
    }

    pub fn emit_flt(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Comp(CompInstr::FLt { dest, lhs, rhs }));
        dest
    }

    pub fn emit_fle(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Comp(CompInstr::FLe { dest, lhs, rhs }));
        dest
    }

    pub fn emit_fgt(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Comp(CompInstr::FGt { dest, lhs, rhs }));
        dest
    }

    pub fn emit_fge(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Comp(CompInstr::FGe { dest, lhs, rhs }));
        dest
    }
}

impl<'a> Builder<'a, InBlock> {
    pub fn emit_select(
        &mut self,
        condition: ValueId,
        true_value: ValueId,
        false_value: ValueId,
    ) -> ValueId {
        let condition_type = self.get_value_type(&condition);

        if !check_is_assignable(condition_type, &Type::Bool) {
            panic!("INTERNAL COMPILER ERROR: Select instruction expected the condition to be a boolean value");
        }

        let true_value_type = self.get_value_type(&true_value);
        let false_value_type = self.get_value_type(&false_value);

        if !check_structural_compatibility(true_value_type, false_value_type) {
            panic!("INTERNAL COMPILER ERROR: Select instruction expected both operands to have the same type");
        }

        let dest = self.new_value_id(true_value_type.clone());
        self.push_instruction(Instruction::Select {
            dest,
            cond: condition,
            true_val: true_value,
            false_val: false_value,
        });

        dest
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

    pub fn neq(
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

    pub fn lte(
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

    pub fn gte(
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
}
