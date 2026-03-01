use inkwell::values::BasicValueEnum;

use crate::{codegen::CodeGenerator, hir::instructions::UnaryInstr};

impl<'ctx> CodeGenerator<'ctx> {
    pub fn emit_unary(&mut self, instr: &UnaryInstr) {
        match instr {
            UnaryInstr::Neg { dest, src } => {
                let val = self.get_val_strict(*src);

                let res: BasicValueEnum = if val.is_int_value() {
                    self.builder
                        .build_int_neg(val.into_int_value(), "neg")
                        .unwrap()
                        .into()
                } else {
                    self.builder
                        .build_float_neg(val.into_float_value(), "fneg")
                        .unwrap()
                        .into()
                };

                self.fn_values.insert(*dest, res);
            }
            UnaryInstr::Not { dest, src } => {
                let val = self.get_val_strict(*src);

                if val.is_int_value() {
                    let res = self
                        .builder
                        .build_not(val.into_int_value(), "not")
                        .unwrap()
                        .into();
                    self.fn_values.insert(*dest, res);
                } else {
                    panic!("INTERNAL COMPILER ERROR: Cannot apply 'Not' (!) to a float.");
                }
            }
        }
    }
}
