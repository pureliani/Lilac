use inkwell::values::{BasicMetadataValueEnum, FunctionValue};

use crate::{
    codegen::CodeGenerator,
    globals::STRING_INTERNER,
    hir::{
        builders::ValueId,
        instructions::{CallInstr, ConstInstr, Instruction},
        types::{checked_declaration::CheckedDeclaration, checked_type::Type},
    },
};

impl<'ctx> CodeGenerator<'ctx> {
    pub fn emit_call(&mut self, instr: &CallInstr) {
        let CallInstr { dest, func, args } = instr;

        let llvm_args: Vec<BasicMetadataValueEnum> = args
            .iter()
            .map(|arg_id| self.get_val_strict(*arg_id).into())
            .collect();

        let direct_fn = self.resolve_const_fn(*func);

        let call_site = if let Some(fn_val) = direct_fn {
            self.builder.build_call(fn_val, &llvm_args, "call").unwrap()
        } else {
            let func_ptr = self.get_val_strict(*func).into_pointer_value();

            let func_hir_type = self
                .program
                .value_types
                .get(func)
                .expect("Function type missing");

            let llvm_fn_type = match func_hir_type {
                Type::Fn(fn_ty) => self.lower_fn_type(fn_ty),
                _ => {
                    panic!("INTERNAL COMPILER ERROR: Call target is not a function type")
                }
            };

            self.builder
                .build_indirect_call(llvm_fn_type, func_ptr, &llvm_args, "call")
                .unwrap()
        };

        if let Some(res) = call_site.try_as_basic_value().left() {
            self.fn_values.insert(*dest, res);
        }
    }

    /// Traces a ValueId back to its definition to see if it is a static function constant.
    fn resolve_const_fn(&self, value_id: ValueId) -> Option<FunctionValue<'ctx>> {
        let func = self.current_fn?;

        let block_id = func.value_definitions.get(&value_id)?;
        let block = func.blocks.get(block_id)?;

        for instr in &block.instructions {
            if let Instruction::Const(ConstInstr::ConstFn { dest, decl_id }) = instr {
                if *dest == value_id {
                    let decl = self.program.declarations.get(decl_id)?;
                    if let CheckedDeclaration::Function(f) = decl {
                        let name = STRING_INTERNER.resolve(f.identifier.name);
                        return self.module.get_function(&name);
                    }
                }
            }
        }

        None
    }
}
