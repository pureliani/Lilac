use crate::hir::{
    builders::{Builder, InBlock, ValueId},
    instructions::{Instruction, UnaryInstr},
    types::checked_type::Type,
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
