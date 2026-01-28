use crate::hir::{
    builders::{Builder, InBlock, ValueId},
    instructions::{CastInstr, Instruction},
    types::checked_type::Type,
    utils::adjustments::check_structural_compatibility,
};

impl<'a> Builder<'a, InBlock> {
    pub fn emit_fext(&mut self, src: ValueId, target_ty: Type) -> ValueId {
        let dest = self.new_value_id(target_ty);
        self.push_instruction(Instruction::Cast(CastInstr::FExt { dest, src }));
        dest
    }

    pub fn emit_ftrunc(&mut self, src: ValueId, target_ty: Type) -> ValueId {
        let dest = self.new_value_id(target_ty);
        self.push_instruction(Instruction::Cast(CastInstr::FTrunc { dest, src }));
        dest
    }

    pub fn emit_trunc(&mut self, src: ValueId, target_ty: Type) -> ValueId {
        let dest = self.new_value_id(target_ty);
        self.push_instruction(Instruction::Cast(CastInstr::Trunc { dest, src }));
        dest
    }

    pub fn emit_itof(&mut self, src: ValueId, target_ty: Type) -> ValueId {
        let dest = self.new_value_id(target_ty);
        self.push_instruction(Instruction::Cast(CastInstr::IToF { dest, src }));
        dest
    }

    pub fn emit_ftoi(&mut self, src: ValueId, target_ty: Type) -> ValueId {
        let dest = self.new_value_id(target_ty);
        self.push_instruction(Instruction::Cast(CastInstr::FToI { dest, src }));
        dest
    }

    pub fn emit_sext(&mut self, src: ValueId, target_ty: Type) -> ValueId {
        let dest = self.new_value_id(target_ty);
        self.push_instruction(Instruction::Cast(CastInstr::SExt { dest, src }));
        dest
    }

    pub fn emit_zext(&mut self, src: ValueId, target_ty: Type) -> ValueId {
        let dest = self.new_value_id(target_ty);
        self.push_instruction(Instruction::Cast(CastInstr::ZExt { dest, src }));
        dest
    }

    pub fn emit_bitcast(&mut self, src: ValueId, target_ty: Type) -> ValueId {
        let src_ty = self.get_value_type(&src);
        if !check_structural_compatibility(src_ty, &target_ty) {
            panic!("INTERNAL COMPILER ERROR: Tried a bitcast to structurally incompatible type, from {:?} to {:?}", src_ty, &target_ty);
        }

        let dest = self.new_value_id(target_ty);
        self.push_instruction(Instruction::Cast(CastInstr::BitCast { dest, src }));
        dest
    }
}
