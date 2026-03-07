use inkwell::values::IntValue;

use crate::{
    codegen::CodeGenerator,
    globals::STRING_INTERNER,
    hir::{instructions::CastInstr, utils::check_assignable::Adjustment},
};

impl<'ctx> CodeGenerator<'ctx> {
    pub fn emit_cast_instr(&mut self, instr: &CastInstr) {
        let CastInstr { src, dest, op } = instr;

        let src_val = self.get_val_strict(*src);

        let src_ty = self
            .program
            .value_types
            .get(src)
            .expect("Source type missing");

        let dest_ty = self
            .program
            .value_types
            .get(dest)
            .expect("Dest type missing");

        let casted_val = match op {
            Adjustment::Identity => todo!(),
            Adjustment::SExt => todo!(),
            Adjustment::ZExt => todo!(),
            Adjustment::Trunc => todo!(),
            Adjustment::FExt => todo!(),
            Adjustment::FTrunc => todo!(),
            Adjustment::SIToF => todo!(),
            Adjustment::UIToF => todo!(),
            Adjustment::FToSI => todo!(),
            Adjustment::FToUI => todo!(),
            Adjustment::WrapInUnion(_) => todo!(),
            Adjustment::UnwrapUnion => todo!(),
            Adjustment::ReTagUnion(items) => todo!(),
            Adjustment::CoerceStruct { field_adjustments } => todo!(),
        };

        self.fn_values.insert(*dest, casted_val);
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
