use inkwell::{values::BasicValueEnum, FloatPredicate, IntPredicate};

use crate::{
    codegen::CodeGenerator,
    hir::{builders::ValueId, instructions::CompInstr, utils::numeric::is_signed},
};

impl<'ctx> CodeGenerator<'ctx> {
    pub fn emit_comp(&mut self, instr: &CompInstr) {
        match instr {
            CompInstr::Eq { dest, lhs, rhs } => {
                self.emit_comparison(
                    *dest,
                    *lhs,
                    *rhs,
                    IntPredicate::EQ,
                    FloatPredicate::OEQ,
                );
            }
            CompInstr::Neq { dest, lhs, rhs } => {
                self.emit_comparison(
                    *dest,
                    *lhs,
                    *rhs,
                    IntPredicate::NE,
                    FloatPredicate::ONE,
                );
            }
            CompInstr::Lt { dest, lhs, rhs } => {
                self.emit_ordered_comparison(
                    *dest,
                    *lhs,
                    *rhs,
                    IntPredicate::SLT,
                    IntPredicate::ULT,
                    FloatPredicate::OLT,
                );
            }
            CompInstr::Lte { dest, lhs, rhs } => {
                self.emit_ordered_comparison(
                    *dest,
                    *lhs,
                    *rhs,
                    IntPredicate::SLE,
                    IntPredicate::ULE,
                    FloatPredicate::OLE,
                );
            }
            CompInstr::Gt { dest, lhs, rhs } => {
                self.emit_ordered_comparison(
                    *dest,
                    *lhs,
                    *rhs,
                    IntPredicate::SGT,
                    IntPredicate::UGT,
                    FloatPredicate::OGT,
                );
            }
            CompInstr::Gte { dest, lhs, rhs } => {
                self.emit_ordered_comparison(
                    *dest,
                    *lhs,
                    *rhs,
                    IntPredicate::SGE,
                    IntPredicate::UGE,
                    FloatPredicate::OGE,
                );
            }
        }
    }

    fn emit_comparison(
        &mut self,
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
        int_pred: IntPredicate,
        float_pred: FloatPredicate,
    ) {
        let lhs_val = self.get_val_strict(lhs);
        let rhs_val = self.get_val_strict(rhs);

        let res: BasicValueEnum = if lhs_val.is_int_value() {
            self.builder
                .build_int_compare(
                    int_pred,
                    lhs_val.into_int_value(),
                    rhs_val.into_int_value(),
                    "icmp",
                )
                .unwrap()
                .into()
        } else {
            self.builder
                .build_float_compare(
                    float_pred,
                    lhs_val.into_float_value(),
                    rhs_val.into_float_value(),
                    "fcmp",
                )
                .unwrap()
                .into()
        };

        self.fn_values.insert(dest, res);
    }

    fn emit_ordered_comparison(
        &mut self,
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
        signed_pred: IntPredicate,
        unsigned_pred: IntPredicate,
        float_pred: FloatPredicate,
    ) {
        let lhs_val = self.get_val_strict(lhs);
        let rhs_val = self.get_val_strict(rhs);

        let res: BasicValueEnum = if lhs_val.is_int_value() {
            let lhs_ty = self.program.value_types.get(&lhs).unwrap();

            let pred = if is_signed(lhs_ty) {
                signed_pred
            } else {
                unsigned_pred
            };

            self.builder
                .build_int_compare(
                    pred,
                    lhs_val.into_int_value(),
                    rhs_val.into_int_value(),
                    "icmp",
                )
                .unwrap()
                .into()
        } else {
            self.builder
                .build_float_compare(
                    float_pred,
                    lhs_val.into_float_value(),
                    rhs_val.into_float_value(),
                    "fcmp",
                )
                .unwrap()
                .into()
        };

        self.fn_values.insert(dest, res);
    }
}
