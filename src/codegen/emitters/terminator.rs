use crate::{codegen::CodeGenerator, hir::instructions::Terminator};

impl<'ctx> CodeGenerator<'ctx> {
    pub fn emit_terminator(&mut self, term: &Terminator) {
        match term {
            Terminator::Return { value } => {
                let val = self.get_val_strict(*value);
                self.builder.build_return(Some(&val)).unwrap();
            }
            Terminator::Jump { target } => {
                let bb = self.fn_blocks.get(target).unwrap_or_else(|| {
                    panic!("Jump target block {:?} not found", target)
                });
                self.builder.build_unconditional_branch(*bb).unwrap();
            }
            Terminator::CondJump {
                condition,
                true_target,
                false_target,
            } => {
                let cond = self.get_val_strict(*condition);

                if !cond.is_int_value() {
                    panic!(
                        "INTERNAL COMPILER ERROR: CondJump condition must be an integer \
                         (bool), got {:?}",
                        cond.get_type()
                    );
                }

                let t_bb = self.fn_blocks.get(true_target).unwrap();
                let f_bb = self.fn_blocks.get(false_target).unwrap();

                self.builder
                    .build_conditional_branch(cond.into_int_value(), *t_bb, *f_bb)
                    .unwrap();
            }
        }
    }
}
