use crate::{
    codegen::CodeGenerator,
    globals::STRING_INTERNER,
    hir::{
        instructions::ConstInstr,
        types::{
            checked_declaration::CheckedDeclaration,
            ordered_number_kind::OrderedNumberKind,
        },
    },
};

impl<'ctx> CodeGenerator<'ctx> {
    pub fn emit_const(&mut self, instr: &ConstInstr) {
        match instr {
            ConstInstr::ConstNumber { dest, val } => {
                let llvm_val = self.synth_number_const(&OrderedNumberKind(*val));
                self.fn_values.insert(*dest, llvm_val);
            }
            ConstInstr::ConstBool { dest, val } => {
                let llvm_val = self
                    .context
                    .bool_type()
                    .const_int(*val as u64, false)
                    .into();
                self.fn_values.insert(*dest, llvm_val);
            }
            ConstInstr::ConstString { dest, val } => {
                let llvm_val = self.synth_string_const(*val);
                self.fn_values.insert(*dest, llvm_val);
            }
            ConstInstr::ConstFn { dest, decl_id } => {
                let decl = self
                    .program
                    .declarations
                    .get(decl_id)
                    .expect("INTERNAL COMPILER ERROR: Function decl missing");

                let fn_name = match decl {
                    CheckedDeclaration::Function(f) => {
                        STRING_INTERNER.resolve(f.identifier.name)
                    }
                    _ => panic!(
                        "INTERNAL COMPILER ERROR: ConstFn points to non-function \
                         declaration"
                    ),
                };

                let fn_val = self.module.get_function(&fn_name).unwrap_or_else(|| {
                    panic!(
                        "INTERNAL COMPILER ERROR: LLVM function '{}' not found",
                        fn_name
                    )
                });

                self.fn_values
                    .insert(*dest, fn_val.as_global_value().as_pointer_value().into());
            }
        }
    }
}
