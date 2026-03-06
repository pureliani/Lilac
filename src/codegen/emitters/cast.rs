use crate::{
    codegen::CodeGenerator,
    hir::{instructions::CastInstr, utils::check_assignable::Adjustment},
};

impl<'ctx> CodeGenerator<'ctx> {
    pub fn emit_cast_instr(&mut self, instr: &CastInstr) {
        let CastInstr { src, dest, op } = instr;

        let src_val = self.get_val_strict(*src);

        let src_ty = self
            .program
            .value_types
            .get(src)
            .expect("Source type missing");

        let dest_ty = self
            .program
            .value_types
            .get(dest)
            .expect("Dest type missing");

        let casted_val = match op {
            Adjustment::Identity => todo!(),
            Adjustment::SExt => todo!(),
            Adjustment::ZExt => todo!(),
            Adjustment::Trunc => todo!(),
            Adjustment::FExt => todo!(),
            Adjustment::FTrunc => todo!(),
            Adjustment::SIToF => todo!(),
            Adjustment::UIToF => todo!(),
            Adjustment::FToSI => todo!(),
            Adjustment::FToUI => todo!(),
            Adjustment::WrapInUnion(_) => todo!(),
            Adjustment::UnwrapUnion => todo!(),
            Adjustment::ReTagUnion(items) => todo!(),
            Adjustment::CoerceStruct { field_adjustments } => todo!(),
        };

        self.fn_values.insert(*dest, casted_val);
    }
}
