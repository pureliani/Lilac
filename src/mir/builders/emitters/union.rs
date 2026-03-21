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
}
