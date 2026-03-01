use inkwell::values::BasicValueEnum;

use crate::{
    codegen::CodeGenerator,
    hir::{builders::ValueId, instructions::BinaryInstr, utils::numeric::is_signed},
};

impl<'ctx> CodeGenerator<'ctx> {
    pub fn emit_binary(&mut self, instr: &BinaryInstr) {
        match instr {
            BinaryInstr::Add { dest, lhs, rhs } => {
                self.emit_arithmetic_op(*dest, *lhs, *rhs, |builder, l, r| {
                    if l.is_int_value() {
                        builder
                            .build_int_add(l.into_int_value(), r.into_int_value(), "add")
                            .unwrap()
                            .into()
                    } else {
                        builder
                            .build_float_add(
                                l.into_float_value(),
                                r.into_float_value(),
                                "fadd",
                            )
                            .unwrap()
                            .into()
                    }
                });
            }
            BinaryInstr::Sub { dest, lhs, rhs } => {
                self.emit_arithmetic_op(*dest, *lhs, *rhs, |builder, l, r| {
                    if l.is_int_value() {
                        builder
                            .build_int_sub(l.into_int_value(), r.into_int_value(), "sub")
                            .unwrap()
                            .into()
                    } else {
                        builder
                            .build_float_sub(
                                l.into_float_value(),
                                r.into_float_value(),
                                "fsub",
                            )
                            .unwrap()
                            .into()
                    }
                });
            }
            BinaryInstr::Mul { dest, lhs, rhs } => {
                self.emit_arithmetic_op(*dest, *lhs, *rhs, |builder, l, r| {
                    if l.is_int_value() {
                        builder
                            .build_int_mul(l.into_int_value(), r.into_int_value(), "mul")
                            .unwrap()
                            .into()
                    } else {
                        builder
                            .build_float_mul(
                                l.into_float_value(),
                                r.into_float_value(),
                                "fmul",
                            )
                            .unwrap()
                            .into()
                    }
                });
            }
            BinaryInstr::Div { dest, lhs, rhs } => {
                self.emit_arithmetic_op(*dest, *lhs, *rhs, |builder, l, r| {
                    if l.is_int_value() {
                        let dest_ty = self.program.value_types.get(dest).unwrap();
                        if is_signed(dest_ty) {
                            builder
                                .build_int_signed_div(
                                    l.into_int_value(),
                                    r.into_int_value(),
                                    "sdiv",
                                )
                                .unwrap()
                                .into()
                        } else {
                            builder
                                .build_int_unsigned_div(
                                    l.into_int_value(),
                                    r.into_int_value(),
                                    "udiv",
                                )
                                .unwrap()
                                .into()
                        }
                    } else {
                        builder
                            .build_float_div(
                                l.into_float_value(),
                                r.into_float_value(),
                                "fdiv",
                            )
                            .unwrap()
                            .into()
                    }
                });
            }
            BinaryInstr::Rem { dest, lhs, rhs } => {
                self.emit_arithmetic_op(*dest, *lhs, *rhs, |builder, l, r| {
                    if l.is_int_value() {
                        let dest_ty = self.program.value_types.get(dest).unwrap();
                        if is_signed(dest_ty) {
                            builder
                                .build_int_signed_rem(
                                    l.into_int_value(),
                                    r.into_int_value(),
                                    "srem",
                                )
                                .unwrap()
                                .into()
                        } else {
                            builder
                                .build_int_unsigned_rem(
                                    l.into_int_value(),
                                    r.into_int_value(),
                                    "urem",
                                )
                                .unwrap()
                                .into()
                        }
                    } else {
                        builder
                            .build_float_rem(
                                l.into_float_value(),
                                r.into_float_value(),
                                "frem",
                            )
                            .unwrap()
                            .into()
                    }
                });
            }
        }
    }

    fn emit_arithmetic_op<F>(&mut self, dest: ValueId, lhs: ValueId, rhs: ValueId, op: F)
    where
        F: FnOnce(
            &inkwell::builder::Builder<'ctx>,
            BasicValueEnum<'ctx>,
            BasicValueEnum<'ctx>,
        ) -> BasicValueEnum<'ctx>,
    {
        let lhs_val = self.get_val_strict(lhs);
        let rhs_val = self.get_val_strict(rhs);

        let lhs_ty = self
            .program
            .value_types
            .get(&lhs)
            .expect("INTERNAL COMPILER ERROR: LLVM arithmetic op - LHS type missing");
        let rhs_ty = self
            .program
            .value_types
            .get(&rhs)
            .expect("INTERNAL COMPILER ERROR: LLVM arithmetic op - RHS type missing");
        let dest_ty =
            self.program.value_types.get(&dest).expect(
                "INTERNAL COMPILER ERROR: LLVM arithmetic op - Dest type missing",
            );

        let lhs_casted = self.emit_cast(lhs_val, lhs_ty, dest_ty);
        let rhs_casted = self.emit_cast(rhs_val, rhs_ty, dest_ty);

        let expected_dest_llvm_ty = self.lower_type(dest_ty).unwrap();
        if lhs_casted.get_type() != expected_dest_llvm_ty
            || rhs_casted.get_type() != expected_dest_llvm_ty
        {
            panic!(
                "INTERNAL COMPILER ERROR: Arithmetic operands failed to cast to \
                 destination type.\nDest HIR: {:?}\nDest LLVM: {:?}\nLHS Casted: \
                 {:?}\nRHS Casted: {:?}",
                dest_ty,
                expected_dest_llvm_ty,
                lhs_casted.get_type(),
                rhs_casted.get_type()
            );
        }

        let res = op(&self.builder, lhs_casted, rhs_casted);

        if res.get_type() != expected_dest_llvm_ty {
            panic!(
                "INTERNAL COMPILER ERROR: Arithmetic operation produced wrong LLVM \
                 type.\nExpected: {:?}\nGot: {:?}",
                expected_dest_llvm_ty,
                res.get_type()
            );
        }

        self.fn_values.insert(dest, res);
    }
}
