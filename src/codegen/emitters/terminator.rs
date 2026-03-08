use crate::{
    codegen::CodeGenerator,
    globals::STRING_INTERNER,
    hir::{instructions::Terminator, types::checked_type::Type},
};

impl<'ctx> CodeGenerator<'ctx> {
    pub fn emit_terminator(&mut self, term: &Terminator) {
        match term {
            Terminator::Return { value } => {
                let is_main = self
                    .current_fn
                    .map(|f| STRING_INTERNER.resolve(f.identifier.name) == "main")
                    .unwrap_or(false);

                if is_main {
                    let i32_type = self.context.i32_type();
                    let ret_ty = &self.current_fn.unwrap().return_type.kind;

                    if *ret_ty == Type::I32 {
                        let val = self.get_val(*value).unwrap();
                        self.builder.build_return(Some(&val)).unwrap();
                    } else if *ret_ty == Type::Void {
                        self.builder
                            .build_return(Some(&i32_type.const_int(0, false)))
                            .unwrap();
                    } else {
                        panic!(
                            "INTERNAL COMPILER ERROR: main function should either \
                             return an i32 or void"
                        )
                    }
                } else {
                    match self.get_val(*value) {
                        Some(val) => {
                            self.builder.build_return(Some(&val)).unwrap();
                        }
                        None => {
                            self.builder.build_return(None).unwrap();
                        }
                    };
                }
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
                let cond = self.get_val(*condition).unwrap();

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
