use std::collections::BTreeSet;

use crate::{
    compile::interner::{StringId, TypeId},
    mir::{
        builders::{Builder, BuilderContext},
        types::checked_type::{StructKind, Type},
    },
};

impl<'a, C: BuilderContext> Builder<'a, C> {
    fn get_type_at_path(&self, mut ty: TypeId, path: &[StringId]) -> Option<TypeId> {
        for field_name in path {
            match self.types.resolve(ty) {
                Type::Struct(StructKind::UserDefined(fields)) => {
                    let field =
                        fields.iter().find(|f| f.identifier.name == *field_name)?;
                    ty = field.ty.id;
                }
                _ => return None,
            }
        }
        Some(ty)
    }

    pub fn get_matching_variant_indices(
        &self,
        union_variants: &BTreeSet<TypeId>,
        path: &[StringId],
        target_type: &TypeId,
    ) -> Vec<u16> {
        let mut matching_indices = Vec::new();

        for (index, variant) in union_variants.iter().enumerate() {
            let index = index as u16;

            if let Some(field_type) = self.get_type_at_path(*variant, path) {
                if &field_type == target_type {
                    matching_indices.push(index);
                }
            }
        }

        matching_indices
    }

    pub fn get_non_matching_variant_indices(
        &self,
        union_variants: &BTreeSet<TypeId>,
        path: &[StringId],
        target_type: &TypeId,
    ) -> Vec<u16> {
        let matching =
            self.get_matching_variant_indices(union_variants, path, target_type);

        let total_variants = union_variants.len();

        let mut non_matching = Vec::with_capacity(total_variants - matching.len());

        let mut match_iter = matching.iter().peekable();

        for i in 0..(total_variants as u16) {
            if match_iter.peek() == Some(&&i) {
                match_iter.next();
            } else {
                non_matching.push(i);
            }
        }

        non_matching
    }
}
