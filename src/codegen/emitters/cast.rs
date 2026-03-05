use crate::{codegen::CodeGenerator, hir::instructions::CastInstr};

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

        let casted_val = self.emit_cast(src_val, src_ty, dest_ty);

        self.fn_values.insert(*dest, casted_val);
    }
}
