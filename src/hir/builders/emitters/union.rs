use std::collections::BTreeSet;

use crate::{
    ast::Span,
    globals::COMMON_IDENTIFIERS,
    hir::{
        builders::{Builder, InBlock, ValueId},
        types::checked_type::{StructKind, Type},
        utils::adjustments::check_structural_compatibility,
    },
    tokenize::NumberKind,
};

impl<'a> Builder<'a, InBlock> {
    pub fn wrap_in_union(
        &mut self,
        source: ValueId,
        variants: &BTreeSet<Type>,
        span: Span,
    ) -> ValueId {
        let source_type = self.get_value_type(&source).clone();

        let variant_index = variants
            .iter()
            .position(|v| check_structural_compatibility(v, &source_type))
            .unwrap_or_else(|| {
                panic!(
                    "INTERNAL COMPILER ERROR: wrap_in_union - source type is not a \
                     variant of the union"
                )
            });

        let struct_type = Type::Struct(StructKind::Union(variants.clone()));
        let union_ptr = self.emit_stack_alloc(struct_type, 1);

        let id_ptr = self.get_field_ptr(union_ptr, COMMON_IDENTIFIERS.id);
        let id_val = self.emit_const_number(NumberKind::U16(variant_index as u16));
        self.emit_store(id_ptr, id_val, span.clone());

        let value_ptr = self.get_field_ptr(union_ptr, COMMON_IDENTIFIERS.value);
        let typed_ptr =
            self.emit_bitcast_unsafe(value_ptr, Type::Pointer(Box::new(source_type)));
        self.emit_store(typed_ptr, source, span);

        union_ptr
    }

    pub fn unwrap_from_union(
        &mut self,
        union_ptr: ValueId,
        variant_type: &Type,
    ) -> ValueId {
        let union_ptr_ty = self.get_value_type(&union_ptr);
        assert!(union_ptr_ty
            .as_union_variants()
            .expect(
                "INTERNAL COMPILER ERROR: unwrap_from_union - union_ptr is not pointing \
                 to a union"
            )
            .iter()
            .any(|v| { check_structural_compatibility(v, variant_type) }));

        let value_ptr = self.get_field_ptr(union_ptr, COMMON_IDENTIFIERS.value);
        let typed_ptr = self.emit_bitcast_unsafe(
            value_ptr,
            Type::Pointer(Box::new(variant_type.clone())),
        );
        self.emit_load(typed_ptr)
    }

    pub fn test_variant(&mut self, union_ptr: ValueId, variant_type: &Type) -> ValueId {
        let union_type = self.get_value_type(&union_ptr).clone();
        let variants = union_type
            .as_union_variants()
            .expect("INTERNAL COMPILER ERROR: test_variant called with non-union");

        let variant_index = variants
            .iter()
            .position(|v| check_structural_compatibility(v, variant_type))
            .expect("INTERNAL COMPILER ERROR: variant not found in union");

        let id_ptr = self.get_field_ptr(union_ptr, COMMON_IDENTIFIERS.id);
        let id_val = self.emit_load(id_ptr);
        let expected = self.emit_const_number(NumberKind::U16(variant_index as u16));
        self.emit_ieq(id_val, expected)
    }

    pub fn widen_union(
        &mut self,
        source_ptr: ValueId,
        source_variants: &BTreeSet<Type>,
        target_variants: &BTreeSet<Type>,
        span: Span,
    ) -> ValueId {
        let remap: Vec<(u16, u16)> = source_variants
            .iter()
            .enumerate()
            .map(|(src_idx, variant)| {
                let tgt_idx = target_variants
                    .iter()
                    .position(|v| check_structural_compatibility(v, variant))
                    .expect(
                        "INTERNAL COMPILER ERROR: source variant not found in target \
                         union",
                    );
                (src_idx as u16, tgt_idx as u16)
            })
            .collect();

        let target_struct = Type::Struct(StructKind::Union(target_variants.clone()));
        let target_ptr = self.emit_stack_alloc(target_struct, 1);

        let src_value_ptr = self.get_field_ptr(source_ptr, COMMON_IDENTIFIERS.value);
        let dst_value_ptr = self.get_field_ptr(target_ptr, COMMON_IDENTIFIERS.value);
        self.emit_memcopy(src_value_ptr, dst_value_ptr);

        let src_id_ptr = self.get_field_ptr(source_ptr, COMMON_IDENTIFIERS.id);
        let src_id = self.emit_load(src_id_ptr);

        let mut remapped_id =
            self.emit_const_number(NumberKind::U16(remap.last().unwrap().1));

        for &(src_idx, tgt_idx) in remap.iter().rev().skip(1) {
            let cmp_val = self.emit_const_number(NumberKind::U16(src_idx));
            let is_match = self.emit_ieq(src_id, cmp_val);
            let tgt_val = self.emit_const_number(NumberKind::U16(tgt_idx));
            remapped_id = self.emit_select(is_match, tgt_val, remapped_id);
        }

        let dst_id_ptr = self.get_field_ptr(target_ptr, COMMON_IDENTIFIERS.id);
        self.emit_store(dst_id_ptr, remapped_id, span);

        target_ptr
    }
}
