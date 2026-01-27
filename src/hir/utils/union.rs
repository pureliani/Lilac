use std::collections::BTreeSet;

use crate::{
    compile::interner::StringId,
    hir::{
        types::checked_type::{StructKind, Type},
        utils::adjustments::check_is_assignable,
    },
};

fn get_type_at_path(mut ty: &Type, path: &[StringId]) -> Option<Type> {
    for field_name in path {
        match ty {
            Type::Struct(StructKind::UserDefined(fields)) => {
                let field = fields.iter().find(|f| f.identifier.name == *field_name)?;
                ty = &field.ty;
            }
            Type::Pointer(inner) => {
                return get_type_at_path(inner, path);
            }
            _ => return None,
        }
    }
    Some(ty.clone())
}

pub fn get_matching_variant_indices(
    union_variants: &BTreeSet<Type>,
    path: &[StringId],
    target_type: &Type,
) -> Vec<u16> {
    let mut matching_indices = Vec::new();

    for (index, variant) in union_variants.iter().enumerate() {
        let index = index as u16;

        if let Some(field_type) = get_type_at_path(variant, path) {
            if check_is_assignable(&field_type, target_type) {
                matching_indices.push(index);
            }
        }
    }

    matching_indices
}

pub fn get_non_matching_variant_indices(
    union_variants: &BTreeSet<Type>,
    path: &[StringId],
    target_type: &Type,
) -> Vec<u16> {
    let matching = get_matching_variant_indices(union_variants, path, target_type);

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
