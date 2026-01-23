use std::collections::BTreeSet;

use crate::{
    compile::interner::TagId,
    hir::types::{
        checked_declaration::TagType,
        checked_type::{StructKind, Type},
    },
};

pub fn try_unify_types(entries: &[Type]) -> Type {
    if entries.is_empty() {
        return Type::Void;
    }

    let all_tags = entries.iter().all(|t| {
        matches!(
            t,
            Type::Struct(StructKind::Tag(_)) | Type::Struct(StructKind::Union(_))
        )
    });

    if all_tags {
        let mut collected_tags: BTreeSet<TagType> = BTreeSet::new();
        for ty in entries {
            match ty {
                Type::Struct(StructKind::Tag(tag)) => {
                    collected_tags.insert(tag.clone());
                }
                Type::Struct(StructKind::Union(variants)) => {
                    for tag in variants {
                        collected_tags.insert(tag.clone());
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
        Type::Struct(StructKind::Union(variants)) => {
            let remaining: BTreeSet<TagType> = variants
                .iter()
                .filter(|v| !to_remove.contains(&v.id))
                .cloned()
                .collect();

            wrap_variants(remaining)
        }
        Type::Struct(StructKind::Tag(t)) => {
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
        Type::Struct(StructKind::Union(variants)) => {
            let kept: BTreeSet<TagType> = variants
                .iter()
                .filter(|v| allowed.contains(&v.id))
                .cloned()
                .collect();

            wrap_variants(kept)
        }
        Type::Struct(StructKind::Tag(t)) => {
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

fn wrap_variants(variants: BTreeSet<TagType>) -> Type {
    if variants.is_empty() {
        return Type::Never;
    }

    if variants.len() == 1 {
        return Type::Struct(StructKind::Tag(variants.last().cloned().unwrap()));
    }

    Type::Struct(StructKind::Union(variants))
}
