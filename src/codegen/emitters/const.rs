use inkwell::values::BasicValueEnum;

use crate::{
    codegen::CodeGenerator, hir::instructions::ConstInstr, tokenize::NumberKind,
};

impl<'ctx> CodeGenerator<'ctx> {
    pub fn emit_const(&mut self, instr: &ConstInstr) {
        match instr {
            ConstInstr::ConstNumber { dest, val } => {
                let llvm_val: BasicValueEnum = match val {
                    NumberKind::I8(v) => {
                        self.context.i8_type().const_int(*v as u64, true).into()
                    }
                    NumberKind::I16(v) => {
                        self.context.i16_type().const_int(*v as u64, true).into()
                    }
                    NumberKind::I32(v) => {
                        self.context.i32_type().const_int(*v as u64, true).into()
                    }
                    NumberKind::I64(v) => {
                        self.context.i64_type().const_int(*v as u64, true).into()
                    }

                    NumberKind::U8(v) => {
                        self.context.i8_type().const_int(*v as u64, false).into()
                    }
                    NumberKind::U16(v) => {
                        self.context.i16_type().const_int(*v as u64, false).into()
                    }
                    NumberKind::U32(v) => {
                        self.context.i32_type().const_int(*v as u64, false).into()
                    }
                    NumberKind::U64(v) => {
                        self.context.i64_type().const_int(*v, false).into()
                    }

                    NumberKind::ISize(v) => {
                        self.context.i64_type().const_int(*v as u64, true).into()
                    }
                    NumberKind::USize(v) => {
                        self.context.i64_type().const_int(*v as u64, false).into()
                    }

                    NumberKind::F32(v) => {
                        self.context.f32_type().const_float(*v as f64).into()
                    }
                    NumberKind::F64(v) => self.context.f64_type().const_float(*v).into(),
                };
                self.fn_values.insert(*dest, llvm_val);
            }
            ConstInstr::ConstBool { dest, val } => {
                let int_val = if *val { 1 } else { 0 };
                let llvm_val = self.context.bool_type().const_int(int_val, false).into();
                self.fn_values.insert(*dest, llvm_val);
            }
            ConstInstr::ConstVoid { .. } => {
                // Void values are not stored in registers
            }
            ConstInstr::ConstString { .. } => {
                unimplemented!("ConstString not implemented")
            }
            ConstInstr::ConstFn { .. } => unimplemented!("ConstFn not implemented"),
        }
    }
}
