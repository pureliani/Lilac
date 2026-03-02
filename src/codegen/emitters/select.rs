use crate::{codegen::CodeGenerator, hir::instructions::SelectInstr};

impl<'ctx> CodeGenerator<'ctx> {
    pub fn emit_select(&mut self, instr: &SelectInstr) {
        let SelectInstr {
            dest,
            cond,
            true_val,
            false_val,
        } = instr;

        let cond_val = self.get_val_strict(*cond);
        let t_val = self.get_val_strict(*true_val);
        let f_val = self.get_val_strict(*false_val);

        if !cond_val.is_int_value() {
            panic!("INTERNAL COMPILER ERROR: Select condition must be integer/bool");
        }

        let res = self
            .builder
            .build_select(cond_val.into_int_value(), t_val, f_val, "select")
            .unwrap();

        self.fn_values.insert(*dest, res);
    }
}
