use crate::mir::{
    builders::{Builder, InBlock, TypePredicate, ValueId},
    instructions::{Instruction, UnaryInstr},
    types::checked_type::Type,
    utils::numeric::{is_float, is_signed},
};

impl<'a> Builder<'a, InBlock> {
    fn emit_ineg(&mut self, src: ValueId) -> ValueId {
        let ty = self.get_value_type(src).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Unary(UnaryInstr::INeg { dest, src }));
        dest
    }

    fn emit_fneg(&mut self, src: ValueId) -> ValueId {
        let ty = self.get_value_type(src).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Unary(UnaryInstr::FNeg { dest, src }));
        dest
    }
}
impl<'a> Builder<'a, InBlock> {
    pub fn neg(&mut self, src: ValueId) -> ValueId {
        let ty = self.get_value_type(src);

        if is_float(ty) {
            self.emit_fneg(src)
        } else if is_signed(ty) {
            self.emit_ineg(src)
        } else {
            panic!("INTERNAL COMPILER ERROR: Cannot apply unary negation `-` operator to this type")
        }
    }

    pub fn not(&mut self, src: ValueId) -> ValueId {
        let ty = self.get_value_type(src);

        if !matches!(ty, &Type::Bool(_)) {
            panic!("INTERNAL COMPILER ERROR: Cannot apply unary not `-` operator to this type")
        }

        let dest = self.new_value_id(Type::Bool(None));

        if let Some(preds) = self.type_predicates.get(&src).cloned() {
            let flipped: Vec<TypePredicate> = preds
                .into_iter()
                .map(|pred| TypePredicate {
                    decl_id: pred.decl_id,
                    on_true_type: pred.on_false_type,
                    on_false_type: pred.on_true_type,
                })
                .collect();

            self.type_predicates.insert(dest, flipped);
        }

        self.push_instruction(Instruction::Unary(UnaryInstr::BNot { dest, src }));

        dest
    }
}
