use crate::hir::{
    builders::{Builder, InBlock, ValueId},
    instructions::{BinaryInstr, Instruction},
};

impl<'a> Builder<'a, InBlock> {
    pub fn emit_add(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Binary(BinaryInstr::Add { dest, lhs, rhs }));
        dest
    }

    pub fn emit_sub(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Binary(BinaryInstr::Sub { dest, lhs, rhs }));
        dest
    }

    pub fn emit_mul(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Binary(BinaryInstr::Mul { dest, lhs, rhs }));
        dest
    }

    pub fn emit_div(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Binary(BinaryInstr::Div { dest, lhs, rhs }));
        dest
    }

    pub fn emit_rem(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Binary(BinaryInstr::Rem { dest, lhs, rhs }));
        dest
    }
}
