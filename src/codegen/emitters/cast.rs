use inkwell::values::IntValue;

use crate::{
    codegen::CodeGenerator,
    globals::STRING_INTERNER,
    hir::{instructions::CastInstr, utils::check_assignable::Adjustment},
};

impl<'ctx> CodeGenerator<'ctx> {
    pub fn emit_cast_instr(&mut self, instr: &CastInstr) {
        let CastInstr { src, dest, op } = instr;

        if op == &Adjustment::Identity {
            if let Some(val) = self.get_val(*src) {
                self.fn_values.insert(*dest, val);
            }
            return;
        }

        let src_val = self.get_val_strict(*src);
        let dest_ty = self.program.value_types.get(dest).unwrap();
        let dest_llvm_ty = self.lower_type(dest_ty).unwrap();

        let result = match op {
            Adjustment::Identity => unreachable!(),

            Adjustment::SExt => self
                .builder
                .build_int_cast_sign_flag(
                    src_val.into_int_value(),
                    dest_llvm_ty.into_int_type(),
                    true,
                    "sext",
                )
                .unwrap()
                .into(),

            Adjustment::ZExt => self
                .builder
                .build_int_cast_sign_flag(
                    src_val.into_int_value(),
                    dest_llvm_ty.into_int_type(),
                    false,
                    "zext",
                )
                .unwrap()
                .into(),

            Adjustment::Trunc => self
                .builder
                .build_int_truncate(
                    src_val.into_int_value(),
                    dest_llvm_ty.into_int_type(),
                    "trunc",
                )
                .unwrap()
                .into(),

            Adjustment::FExt => self
                .builder
                .build_float_ext(
                    src_val.into_float_value(),
                    dest_llvm_ty.into_float_type(),
                    "fext",
                )
                .unwrap()
                .into(),

            Adjustment::FTrunc => self
                .builder
                .build_float_trunc(
                    src_val.into_float_value(),
                    dest_llvm_ty.into_float_type(),
                    "ftrunc",
                )
                .unwrap()
                .into(),

            Adjustment::SIToF => self
                .builder
                .build_signed_int_to_float(
                    src_val.into_int_value(),
                    dest_llvm_ty.into_float_type(),
                    "sitof",
                )
                .unwrap()
                .into(),

            Adjustment::UIToF => self
                .builder
                .build_unsigned_int_to_float(
                    src_val.into_int_value(),
                    dest_llvm_ty.into_float_type(),
                    "uitof",
                )
                .unwrap()
                .into(),

            Adjustment::FToSI => self
                .builder
                .build_float_to_signed_int(
                    src_val.into_float_value(),
                    dest_llvm_ty.into_int_type(),
                    "ftosi",
                )
                .unwrap()
                .into(),

            Adjustment::FToUI => self
                .builder
                .build_float_to_unsigned_int(
                    src_val.into_float_value(),
                    dest_llvm_ty.into_int_type(),
                    "ftoui",
                )
                .unwrap()
                .into(),

            Adjustment::WrapInUnion(tag_index) => {
                let union_llvm_ty = dest_llvm_ty.into_struct_type();
                self.pack_union_variant(src_val, *tag_index as u64, union_llvm_ty)
                    .into()
            }

            Adjustment::UnwrapUnion => {
                let union_llvm_ty = self
                    .lower_type(self.program.value_types.get(src).unwrap())
                    .unwrap()
                    .into_struct_type();
                self.unpack_union_variant(
                    src_val.into_struct_value(),
                    dest_llvm_ty,
                    union_llvm_ty,
                )
            }

            Adjustment::ReTagUnion(mapping) => {
                let src_struct = src_val.into_struct_value();
                let old_tag = self.extract_union_tag(src_struct);
                let new_tag = self.emit_tag_switch(old_tag, mapping);

                let dest_struct_ty = dest_llvm_ty.into_struct_type();
                let payload = self
                    .builder
                    .build_extract_value(src_struct, 1, "payload")
                    .unwrap();

                let mut result = dest_struct_ty.get_undef();
                result = self
                    .builder
                    .build_insert_value(result, new_tag, 0, "new_tag")
                    .unwrap()
                    .into_struct_value();
                result = self
                    .builder
                    .build_insert_value(result, payload, 1, "keep_payload")
                    .unwrap()
                    .into_struct_value();
                result.into()
            }

            Adjustment::CoerceStruct { field_adjustments } => {
                // TODO: implement per-field recursive casting
                todo!("CoerceStruct not yet implemented")
            }
        };

        self.fn_values.insert(*dest, result);
    }

    pub fn emit_tag_switch(
        &mut self,
        old_tag: IntValue<'ctx>,
        mapping: &[(u64, u64)],
    ) -> IntValue<'ctx> {
        let fn_name = STRING_INTERNER.resolve(self.current_fn.unwrap().identifier.name);
        let current_fn = self.module.get_function(&fn_name).unwrap();

        let switch_bb = self.builder.get_insert_block().unwrap();
        let merge_bb = self.context.append_basic_block(current_fn, "tag_map_merge");
        let default_bb = self
            .context
            .append_basic_block(current_fn, "tag_map_default");

        self.builder.position_at_end(default_bb);
        self.builder.build_unreachable().unwrap();

        let mut cases = Vec::with_capacity(mapping.len());
        let mut incoming_phis = Vec::with_capacity(mapping.len());

        for (old_idx, new_idx) in mapping {
            let case_bb = self.context.append_basic_block(
                current_fn,
                &format!("map_{}_to_{}", old_idx, new_idx),
            );
            self.builder.position_at_end(case_bb);

            let new_tag_val = self.context.i16_type().const_int(*new_idx, false);
            self.builder.build_unconditional_branch(merge_bb).unwrap();

            cases.push((self.context.i16_type().const_int(*old_idx, false), case_bb));
            incoming_phis.push((new_tag_val, case_bb));
        }

        self.builder.position_at_end(switch_bb);
        self.builder
            .build_switch(old_tag, default_bb, &cases)
            .unwrap();

        self.builder.position_at_end(merge_bb);
        let phi = self
            .builder
            .build_phi(self.context.i16_type(), "remapped_tag")
            .unwrap();

        for (val, bb) in incoming_phis {
            phi.add_incoming(&[(&val, bb)]);
        }

        phi.as_basic_value().into_int_value()
    }
}
