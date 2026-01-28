use crate::hir::{
    builders::{Builder, InBlock, ValueId},
    instructions::{CompInstr, Instruction},
    types::checked_type::Type,
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
