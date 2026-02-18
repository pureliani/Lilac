use std::collections::BTreeSet;

use crate::hir::{
    builders::{Builder, InBlock, ValueId},
    instructions::{Instruction, UnionInstr},
    types::checked_type::Type,
    utils::adjustments::check_structural_compatibility,
};

impl<'a> Builder<'a, InBlock> {
    /// Wraps a single variant value into a union.
    pub fn emit_wrap_in_union(
        &mut self,
        source: ValueId,
        variants: &BTreeSet<Type>,
    ) -> ValueId {
        let source_type = self.get_value_type(source).clone();

        assert!(
            variants
                .iter()
                .position(|v| check_structural_compatibility(v, &source_type))
                .is_some(),
            "INTERNAL COMPILER ERROR: wrap_in_union - source type is not a \
                     variant of the union"
        );

        let new_union_value = self.new_value_id(Type::Union(variants.clone()));
        self.push_instruction(Instruction::Union(UnionInstr::WrapInUnion {
            dest: new_union_value,
            src: source,
            target_variants: variants.clone(),
        }));

        new_union_value
    }

    /// Extracts a variant value from a union pointer. Caller must ensure
    /// the active variant matches `variant_type` (via test_variant or
    /// known assignment).
    pub fn emit_unwrap_from_union(
        &mut self,
        union_value: ValueId,
        variant_type: &Type,
    ) -> ValueId {
        let union_value_ty = self.get_value_type(union_value);
        assert!(
            union_value_ty
                .as_union_variants()
                .expect("INTERNAL COMPILER ERROR: unwrap_from_union - union_value is not a union")
                .iter()
                .any(|v| { check_structural_compatibility(v, variant_type) }),
            "INTERNAL COMPILER ERROR: unwrap_from_union - variant_type is not a member of the union"
        );

        let payload_value = self.new_value_id(variant_type.clone());
        self.push_instruction(Instruction::Union(UnionInstr::UnwrapUnion {
            dest: payload_value,
            src: union_value,
            variant_type: variant_type.clone(),
        }));
        payload_value
    }

    /// Tests whether a union value holds a specific variant. Returns a
    /// bool ValueId.
    pub fn emit_test_variant(
        &mut self,
        union_value: ValueId,
        variant_type: &Type,
    ) -> ValueId {
        let union_type = self.get_value_type(union_value);
        let variants = union_type
            .as_union_variants()
            .expect("INTERNAL COMPILER ERROR: test_variant called with non-union");

        assert!(
            variants
                .iter()
                .position(|v| check_structural_compatibility(v, variant_type))
                .is_some(),
            "INTERNAL COMPILER ERROR: variant not found in union"
        );

        let bool_value = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Union(UnionInstr::TestVariant {
            dest: bool_value,
            src: union_value,
            variant_type: variant_type.clone(),
        }));
        bool_value
    }

    /// Widens a union to a larger union that contains all source variants
    /// plus additional ones. Copies the payload and remaps the discriminant.
    pub fn emit_widen_union(
        &mut self,
        union: ValueId,
        target_variants: &BTreeSet<Type>,
    ) -> ValueId {
        let source_variants = match self.get_value_type(union) {
            Type::Union(variants) => variants,
            _ => {
                panic!("INTERNAL COMPILER ERROR: narrow_union called on non-union type")
            }
        };

        assert!(
            source_variants.len() <= target_variants.len(),
            "INTERNAL COMPILER ERROR: widen_union called but source has more \
             variants than target"
        );

        for sv in source_variants {
            assert!(
                target_variants
                    .iter()
                    .any(|tv| check_structural_compatibility(sv, tv)),
                "INTERNAL COMPILER ERROR: widen_union - source variant not found \
                 in target union"
            );
        }

        let dest = self.new_value_id(Type::Union(target_variants.clone()));
        self.push_instruction(Instruction::Union(UnionInstr::WidenUnion {
            dest,
            src: union,
        }));

        dest
    }

    /// Narrows a union to a smaller union that contains a subset of the
    /// source variants. Copies the payload and remaps the discriminant.
    ///
    /// The caller must ensure that the runtime active variant is one of
    /// the target variants (via a prior type check). This method validates
    /// the structural precondition (target ⊂ source) but cannot validate
    /// the runtime invariant.
    pub fn emit_narrow_union(
        &mut self,
        union: ValueId,
        target_variants: &BTreeSet<Type>,
    ) -> ValueId {
        let source_variants = match self.get_value_type(union) {
            Type::Union(variants) => variants,
            _ => {
                panic!("INTERNAL COMPILER ERROR: narrow_union called on non-union type")
            }
        };

        assert!(
            source_variants.len() > target_variants.len(),
            "INTERNAL COMPILER ERROR: narrow_union called but target has >= \
             variants than source (use widen_union instead)"
        );

        for tv in target_variants {
            assert!(
                source_variants
                    .iter()
                    .any(|sv| check_structural_compatibility(sv, tv)),
                "INTERNAL COMPILER ERROR: narrow_union - target variant not found \
                 in source union"
            );
        }

        let dest = self.new_value_id(Type::Union(target_variants.clone()));
        self.push_instruction(Instruction::Union(UnionInstr::NarrowUnion {
            dest,
            src: union,
        }));

        dest
    }
}
