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
