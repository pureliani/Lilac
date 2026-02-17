use crate::hir::{
    builders::{Builder, InBlock, ValueId},
    instructions::{CompInstr, Instruction},
    types::checked_type::Type,
    utils::adjustments::check_structural_compatibility,
};

impl<'a> Builder<'a, InBlock> {
    pub fn emit_eq(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Comp(CompInstr::Eq { dest, lhs, rhs }));
        dest
    }

    pub fn emit_neq(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Comp(CompInstr::Neq { dest, lhs, rhs }));
        dest
    }

    pub fn emit_lt(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Comp(CompInstr::Lt { dest, lhs, rhs }));
        dest
    }

    pub fn emit_lte(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Comp(CompInstr::Lte { dest, lhs, rhs }));
        dest
    }

    pub fn emit_gt(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Comp(CompInstr::Gt { dest, lhs, rhs }));
        dest
    }

    pub fn emit_gte(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Comp(CompInstr::Gte { dest, lhs, rhs }));
        dest
    }

    pub fn emit_select(
        &mut self,
        condition: ValueId,
        true_value: ValueId,
        false_value: ValueId,
    ) -> ValueId {
        let condition_type = self.get_value_type(&condition);

        if !check_structural_compatibility(condition_type, &Type::Bool) {
            panic!(
                "INTERNAL COMPILER ERROR: Select instruction expected the condition to \
                 be a boolean value"
            );
        }

        let true_value_type = self.get_value_type(&true_value);
        let false_value_type = self.get_value_type(&false_value);

        if !check_structural_compatibility(true_value_type, false_value_type) {
            panic!(
                "INTERNAL COMPILER ERROR: Select instruction expected both operands to \
                 have the same type"
            );
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
}
