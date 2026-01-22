use crate::{
    compile::interner::TagId,
    hir::{
        builders::Projection,
        types::{
            checked_declaration::TagType,
            checked_type::{StructKind, Type},
        },
    },
};

pub fn narrow_type_at_path(base: &Type, path: &[Projection], leaf_type: &Type) -> Type {
    if path.is_empty() {
        return leaf_type.clone();
    }

    match (&path[0], base) {
        (Projection::Field(field_node), Type::Pointer(ptr_to)) => {
            if let Type::Struct(StructKind::UserDefined(fields)) = &**ptr_to {
                let idx = fields
                    .iter()
                    .position(|f| f.identifier.name == field_node.name)
                    .expect("INTERNAL COMPILER ERROR: Field not found during narrowing");

                let mut new_fields = fields.clone();
                new_fields[idx].ty =
                    narrow_type_at_path(&fields[idx].ty, &path[1..], leaf_type);

                Type::Pointer(Box::new(Type::Struct(StructKind::UserDefined(new_fields))))
            } else {
                panic!("INTERNAL COMPILER ERROR: Used Projection::Field on a pointer to non-struct type")
            }
        }
        (Projection::Index(..), _) => base.clone(),
        _ => base.clone(),
    }
}

pub fn try_unify_types(entries: &[Type]) -> Type {
    if entries.is_empty() {
        return Type::Void;
    }

    let all_tags = entries
        .iter()
        .all(|t| matches!(t, Type::Tag(_) | Type::Union { .. }));

    if all_tags {
        let mut collected_tags: Vec<TagType> = Vec::new();
        for ty in entries {
            match ty {
                Type::Tag(tag) => {
                    if !collected_tags.contains(tag) {
                        collected_tags.push(tag.clone());
                    }
                }
                Type::Union(variants) => {
                    for tag in variants {
                        if !collected_tags.contains(tag) {
                            collected_tags.push(tag.clone());
                        }
                    }
                }
                _ => unreachable!(),
            }
        }

        return wrap_variants(collected_tags);
    }

    return Type::Never;
}

pub fn subtract_types(base: &Type, to_remove: &[TagId]) -> Type {
    match base {
        Type::Pointer(to) => {
            let subtracted_inner = subtract_types(to, to_remove);
            Type::Pointer(Box::new(subtracted_inner))
        }
        Type::Union(variants) => {
            let remaining: Vec<TagType> = variants
                .iter()
                .filter(|v| !to_remove.contains(&v.id))
                .cloned()
                .collect();

            wrap_variants(remaining)
        }
        Type::Tag(t) => {
            if to_remove.contains(&t.id) {
                Type::Void
            } else {
                base.clone()
            }
        }
        _ => base.clone(),
    }
}

pub fn intersect_types(base: &Type, allowed: &[TagId]) -> Type {
    match base {
        Type::Pointer(to) => {
            let intersected_inner = intersect_types(to, allowed);
            Type::Pointer(Box::new(intersected_inner))
        }
        Type::Union(variants) => {
            let kept: Vec<TagType> = variants
                .iter()
                .filter(|v| allowed.contains(&v.id))
                .cloned()
                .collect();

            wrap_variants(kept)
        }
        Type::Tag(t) => {
            if allowed.contains(&t.id) {
                base.clone()
            } else {
                Type::Void
            }
        }
        Type::Unknown => Type::Unknown,
        _ => Type::Void,
    }
}

fn wrap_variants(mut variants: Vec<TagType>) -> Type {
    if variants.is_empty() {
        return Type::Never;
    }

    if variants.len() == 1 {
        return Type::Tag(variants.pop().unwrap());
    }

    variants.sort_by(|a, b| a.id.0.cmp(&b.id.0));

    Type::Union(variants)
}
