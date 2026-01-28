use crate::hir::{
    builders::{Builder, InBlock, ValueId},
    instructions::{BinaryInstr, Instruction},
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
