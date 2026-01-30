use crate::{
    ast::Span,
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        instructions::{Instruction, UnaryInstr},
        types::checked_type::Type,
        utils::{
            adjustments::check_structural_compatibility,
            numeric::{is_float, is_signed},
        },
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn emit_ineg(&mut self, src: ValueId) -> ValueId {
        let ty = self.get_value_type(&src).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Unary(UnaryInstr::INeg { dest, src }));
        dest
    }

    pub fn emit_fneg(&mut self, src: ValueId) -> ValueId {
        let ty = self.get_value_type(&src).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Unary(UnaryInstr::FNeg { dest, src }));
        dest
    }

    pub fn emit_bnot(&mut self, src: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Unary(UnaryInstr::BNot { dest, src }));
        dest
    }
}
impl<'a> Builder<'a, InBlock> {
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

        if !check_structural_compatibility(ty, &Type::Bool) {
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
}
