use std::collections::BTreeSet;

use crate::{
    compile::interner::TypeId,
    globals::COMMON_IDENTIFIERS,
    mir::{
        builders::{Builder, InBlock, ValueId},
        types::checked_type::{StructKind, Type},
    },
    tokenize::NumberKind,
};

impl<'a> Builder<'a, InBlock> {
    /// Wraps a single variant value into a union. Stack-allocates the union
    /// struct, writes the discriminant and payload.
    pub fn wrap_in_union(
        &mut self,
        source: ValueId,
        variants: &BTreeSet<TypeId>,
    ) -> ValueId {
        let source_type = self.get_value_type(source);

        let variant_index = variants
            .iter()
            .position(|v| v == &source_type)
            .unwrap_or_else(|| {
                panic!(
                    "INTERNAL COMPILER ERROR: wrap_in_union - source type is not a \
                     variant of the union"
                )
            });

        let struct_type = Type::Struct(StructKind::TaggedUnion(variants.clone())).id();
        let union_ptr = self.emit_stack_alloc(struct_type, 1);

        let id_ptr = self.get_field_ptr(union_ptr, COMMON_IDENTIFIERS.id);
        let id_val = self.emit_number(NumberKind::U16(variant_index as u16));
        self.emit_store(id_ptr, id_val);

        let value_ptr = self.get_field_ptr(union_ptr, COMMON_IDENTIFIERS.val);
        let typed_ptr =
            self.emit_bitcast_unsafe(value_ptr, Type::Pointer(source_type).id());
        self.emit_store(typed_ptr, source);

        union_ptr
    }

    /// Extracts a variant value from a union pointer. Caller must ensure
    /// the active variant matches `variant_type` (via test_variant or
    /// known assignment).
    pub fn unwrap_from_union(
        &mut self,
        union_ptr: ValueId,
        variant_type: TypeId,
    ) -> ValueId {
        let union_ptr_ty = self.get_value_type(union_ptr);
        assert!(
            union_ptr_ty
                .ty()
                .get_union_variants()
                .expect(
                    "INTERNAL COMPILER ERROR: unwrap_from_union - union_ptr is not \
                     pointing to a union"
                )
                .iter()
                .any(|v| { *v == variant_type }),
            "INTERNAL COMPILER ERROR: unwrap_from_union - variant_type is not a \
             member of the union"
        );

        let value_ptr = self.get_field_ptr(union_ptr, COMMON_IDENTIFIERS.val);
        let typed_ptr =
            self.emit_bitcast_unsafe(value_ptr, Type::Pointer(variant_type).id());
        self.emit_load(typed_ptr)
    }

    /// Tests whether a union value holds a specific variant. Returns a
    /// bool ValueId.
    pub fn test_variant(&mut self, union_ptr: ValueId, variant_type: TypeId) -> ValueId {
        let union_ptr_type = self.get_value_type(union_ptr).clone();

        let variants = union_ptr_type
            .ty()
            .unwrap_ptr()
            .ty()
            .get_union_variants()
            .expect("INTERNAL COMPILER ERROR: test_variant called with non-union");

        let variant_index = variants
            .iter()
            .position(|v| *v == variant_type)
            .expect("INTERNAL COMPILER ERROR: variant not found in union");

        let id_ptr = self.get_field_ptr(union_ptr, COMMON_IDENTIFIERS.id);
        let id_val = self.emit_load(id_ptr);
        let expected = self.emit_number(NumberKind::U16(variant_index as u16));
        self.eq(id_val, expected)
    }

    pub fn retag_union(
        &mut self,
        source_ptr: ValueId,
        source_variants: &BTreeSet<TypeId>,
        target_variants: &BTreeSet<TypeId>,
    ) -> ValueId {
        let source_ptr_ty = self.get_value_type(source_ptr);

        let actual_source_variants = source_ptr_ty.ty()
            .unwrap_ptr().ty()
            .get_union_variants()
            .expect("SAFETY CHECK FAILED: retag_union called on a ValueId that is not a union pointer!");

        assert_eq!(
            actual_source_variants, *source_variants,
            "SAFETY CHECK FAILED: The variants of the source ValueId do not match \
             the source_variants provided by the Adjustment recipe!"
        );

        let is_widening = source_variants.len() <= target_variants.len();

        if is_widening {
            for sv in source_variants {
                assert!(
                    target_variants.contains(sv),
                    "INTERNAL COMPILER ERROR: retag_union (widen) - source variant not found in target union"
                );
            }
        } else {
            for tv in target_variants {
                assert!(
                    source_variants.contains(tv),
                    "INTERNAL COMPILER ERROR: retag_union (narrow) - target variant not found in source union"
                );
            }
        }

        let remap: Vec<(u16, u16)> = source_variants
            .iter()
            .enumerate()
            .filter_map(|(src_idx, variant)| {
                let tgt_idx = target_variants.iter().position(|v| v == variant);
                tgt_idx.map(|ti| (src_idx as u16, ti as u16))
            })
            .collect();

        let target_struct =
            Type::Struct(StructKind::TaggedUnion(target_variants.clone())).id();
        let target_ptr = self.emit_stack_alloc(target_struct, 1);

        let src_value_ptr = self.get_field_ptr(source_ptr, COMMON_IDENTIFIERS.val);
        let dst_value_ptr = self.get_field_ptr(target_ptr, COMMON_IDENTIFIERS.val);

        if is_widening {
            self.emit_memcopy(src_value_ptr, dst_value_ptr);
        } else {
            let src_as_target_payload = self.emit_bitcast_unsafe(
                src_value_ptr,
                Type::Pointer(Type::TaglessUnion(target_variants.clone()).id()).id(),
            );
            self.emit_memcopy(src_as_target_payload, dst_value_ptr);
        }

        self.emit_discriminant_remap(source_ptr, target_ptr, &remap);

        target_ptr
    }

    /// Emits the select chain that remaps the discriminant from source
    /// indices to target indices, then stores it in the target union.
    fn emit_discriminant_remap(
        &mut self,
        source_ptr: ValueId,
        target_ptr: ValueId,
        remap: &[(u16, u16)],
    ) {
        let src_id_ptr = self.get_field_ptr(source_ptr, COMMON_IDENTIFIERS.id);
        let src_id = self.emit_load(src_id_ptr);

        let mut remapped_id = self.emit_number(NumberKind::U16(remap.last().unwrap().1));

        for &(src_idx, tgt_idx) in remap.iter().rev().skip(1) {
            let cmp_val = self.emit_number(NumberKind::U16(src_idx));
            let is_match = self.eq(src_id, cmp_val);
            let tgt_val = self.emit_number(NumberKind::U16(tgt_idx));
            remapped_id = self.emit_select(is_match, tgt_val, remapped_id);
        }

        let dst_id_ptr = self.get_field_ptr(target_ptr, COMMON_IDENTIFIERS.id);
        self.emit_store(dst_id_ptr, remapped_id);
    }
}
