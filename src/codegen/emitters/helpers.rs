use std::collections::BTreeSet;

use inkwell::{
    types::StructType,
    values::{BasicValueEnum, IntValue, StructValue},
};

use crate::{
    codegen::CodeGenerator, globals::STRING_INTERNER, hir::types::checked_type::Type,
};

impl<'ctx> CodeGenerator<'ctx> {
    /// Finds the tag index of a type within a union's variant set
    pub fn find_variant_tag(&self, variants: &BTreeSet<Type>, target: &Type) -> u64 {
        variants
            .iter()
            .position(|v| v == target)
            .unwrap_or_else(|| {
                panic!(
                    "INTERNAL COMPILER ERROR: Variant {:?} not found in union",
                    target
                )
            }) as u64
    }

    /// Packs a runtime value into a union struct: `{ i16 tag, [i8 x N] payload }`
    pub fn pack_union_variant(
        &mut self,
        value: BasicValueEnum<'ctx>,
        tag: u64,
        union_llvm_ty: StructType<'ctx>,
    ) -> StructValue<'ctx> {
        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let tag_val = self.context.i16_type().const_int(tag, false);

        let mut result = union_llvm_ty.get_undef();
        result = self
            .builder
            .build_insert_value(result, tag_val, 0, "set_tag")
            .unwrap()
            .into_struct_value();

        let payload_field_ty = union_llvm_ty.get_field_type_at_index(1).unwrap();
        let payload_alloca = self
            .builder
            .build_alloca(payload_field_ty, "payload_tmp")
            .unwrap();

        let cast_ptr = self
            .builder
            .build_pointer_cast(payload_alloca, ptr_type, "payload_cast")
            .unwrap();

        self.builder.build_store(cast_ptr, value).unwrap();

        let payload = self
            .builder
            .build_load(payload_field_ty, payload_alloca, "payload")
            .unwrap();

        self.builder
            .build_insert_value(result, payload, 1, "set_payload")
            .unwrap()
            .into_struct_value()
    }

    pub fn build_zst_union(
        &mut self,
        tag: IntValue<'ctx>,
        union_llvm_ty: StructType<'ctx>,
    ) -> StructValue<'ctx> {
        let mut result = union_llvm_ty.get_undef();
        result = self
            .builder
            .build_insert_value(result, tag, 0, "set_tag")
            .unwrap()
            .into_struct_value();

        let zero_payload = union_llvm_ty
            .get_field_type_at_index(1)
            .unwrap()
            .const_zero();
        self.builder
            .build_insert_value(result, zero_payload, 1, "zero_payload")
            .unwrap()
            .into_struct_value()
    }

    /// Extracts the payload from a union struct and reinterprets it as the given type
    pub fn unpack_union_variant(
        &mut self,
        union_val: StructValue<'ctx>,
        variant_llvm_ty: inkwell::types::BasicTypeEnum<'ctx>,
        union_llvm_ty: StructType<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());

        let payload_field_ty = union_llvm_ty.get_field_type_at_index(1).unwrap();
        let payload = self
            .builder
            .build_extract_value(union_val, 1, "payload")
            .unwrap();

        let payload_alloca = self
            .builder
            .build_alloca(payload_field_ty, "unpack_tmp")
            .unwrap();
        self.builder.build_store(payload_alloca, payload).unwrap();

        let cast_ptr = self
            .builder
            .build_pointer_cast(payload_alloca, ptr_type, "unpack_cast")
            .unwrap();

        self.builder
            .build_load(variant_llvm_ty, cast_ptr, "unpacked")
            .unwrap()
    }

    /// Emits a conditional branch that produces a value
    ///
    /// Creates then/else/merge blocks, calls the provided closures
    /// to build each side, and merges the results with a phi node
    pub fn build_conditional_value<F, G>(
        &mut self,
        condition: IntValue<'ctx>,
        result_ty: inkwell::types::BasicTypeEnum<'ctx>,
        build_then: F,
        build_else: G,
        name: &str,
    ) -> BasicValueEnum<'ctx>
    where
        F: FnOnce(&mut Self) -> BasicValueEnum<'ctx>,
        G: FnOnce(&mut Self) -> BasicValueEnum<'ctx>,
    {
        let current_fn = self.get_current_llvm_fn();

        let then_bb = self
            .context
            .append_basic_block(current_fn, &format!("{}_then", name));
        let else_bb = self
            .context
            .append_basic_block(current_fn, &format!("{}_else", name));
        let merge_bb = self
            .context
            .append_basic_block(current_fn, &format!("{}_merge", name));

        self.builder
            .build_conditional_branch(condition, then_bb, else_bb)
            .unwrap();

        // Then
        self.builder.position_at_end(then_bb);
        let then_val = build_then(self);
        self.builder.build_unconditional_branch(merge_bb).unwrap();
        let then_bb_end = self.builder.get_insert_block().unwrap();

        // Else
        self.builder.position_at_end(else_bb);
        let else_val = build_else(self);
        self.builder.build_unconditional_branch(merge_bb).unwrap();
        let else_bb_end = self.builder.get_insert_block().unwrap();

        // Merge
        self.builder.position_at_end(merge_bb);
        let phi = self.builder.build_phi(result_ty, name).unwrap();
        phi.add_incoming(&[(&then_val, then_bb_end), (&else_val, else_bb_end)]);

        phi.as_basic_value()
    }

    pub fn retag_union(
        &mut self,
        src_val: StructValue<'ctx>,
        dest_llvm_ty: StructType<'ctx>,
        mapping: &[(u64, u64)],
    ) -> StructValue<'ctx> {
        let old_tag = self.extract_union_tag(src_val);
        let new_tag = self.emit_tag_switch(old_tag, mapping);

        let src_size = self
            .target_machine
            .get_target_data()
            .get_abi_size(&src_val.get_type());
        let dest_size = self
            .target_machine
            .get_target_data()
            .get_abi_size(&dest_llvm_ty);
        let max_size = src_size.max(dest_size);

        let i8_array_ty = self.context.i8_type().array_type(max_size as u32);
        let alloca = self.builder.build_alloca(i8_array_ty, "retag_buf").unwrap();

        self.builder.build_store(alloca, src_val).unwrap();
        self.builder.build_store(alloca, new_tag).unwrap();

        self.builder
            .build_load(dest_llvm_ty, alloca, "retagged")
            .unwrap()
            .into_struct_value()
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

    pub fn extract_union_tag(&self, union_val: StructValue<'ctx>) -> IntValue<'ctx> {
        self.builder
            .build_extract_value(union_val, 0, "tag")
            .unwrap()
            .into_int_value()
    }

    pub fn get_current_llvm_fn(&self) -> inkwell::values::FunctionValue<'ctx> {
        let fn_name = STRING_INTERNER.resolve(self.current_fn.unwrap().identifier.name);
        self.module.get_function(&fn_name).unwrap()
    }
}
