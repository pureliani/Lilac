use std::collections::HashSet;

use crate::hir::types::{
    checked_declaration::{FnType, TagType},
    checked_type::{StructKind, Type},
};

pub fn check_is_assignable<'a>(source_type: &'a Type, target_type: &'a Type) -> bool {
    let mut visited_declarations: HashSet<(&'a Type, &'a Type)> = HashSet::new();
    check_is_assignable_recursive(source_type, target_type, &mut visited_declarations)
}

fn check_is_assignable_recursive<'a>(
    source_type: &'a Type,
    target_type: &'a Type,
    visited: &mut HashSet<(&'a Type, &'a Type)>,
) -> bool {
    let pair = (source_type, target_type);
    if visited.contains(&pair) {
        return true;
    }
    visited.insert(pair);

    use Type::*;

    let result = match (&source_type, &target_type) {
        (I8, I8)
        | (I16, I16)
        | (I32, I32)
        | (I64, I64)
        | (ISize, ISize)
        | (USize, USize)
        | (U8, U8)
        | (U16, U16)
        | (U32, U32)
        | (U64, U64)
        | (F32, F32)
        | (F64, F64)
        | (Bool, Bool)
        | (Void, Void)
        | (_, Void)
        | (_, Unknown)
        | (Unknown, _) => true,

        (Type::Pointer(source_inner), Type::Pointer(target_inner)) => {
            check_is_assignable_recursive(source_inner, target_inner, visited)
        }
        (Struct(source), Struct(target)) => match (source, target) {
            (StructKind::Union(source), StructKind::Union(target)) => {
                source.iter().all(|source_item| {
                    target.iter().any(|target_item| {
                        check_is_tag_assignable(source_item, target_item, visited)
                    })
                })
            }
            (StructKind::Tag(source_item), StructKind::Union(target_union)) => {
                target_union.iter().any(|target_item| {
                    check_is_tag_assignable(source_item, target_item, visited)
                })
            }
            (StructKind::Tag(t1), StructKind::Tag(t2)) => {
                check_is_tag_assignable(t1, t2, visited)
            }
            (
                StructKind::UserDefined(source_fields),
                StructKind::UserDefined(target_fields),
            ) => {
                if source_fields.len() != target_fields.len() {
                    return false;
                }

                let is_assignable =
                    source_fields
                        .iter()
                        .zip(target_fields.iter())
                        .all(|(sp, tp)| {
                            let same_name = sp.identifier.name == tp.identifier.name;
                            let assignable =
                                check_is_assignable_recursive(&sp.ty, &tp.ty, visited);

                            same_name && assignable
                        });

                is_assignable
            }
            (StructKind::ListHeader(s_inner), StructKind::ListHeader(t_inner)) => {
                check_is_assignable_recursive(s_inner, t_inner, visited)
                    && check_is_assignable_recursive(t_inner, s_inner, visited)
            }
            (StructKind::StringHeader, StructKind::StringHeader) => true,
            _ => false,
        },
        (
            Fn(FnType {
                params: source_params,
                return_type: source_return_type,
                ..
            }),
            Fn(FnType {
                params: target_params,
                return_type: target_return_type,
                ..
            }),
        ) => {
            if source_params.len() != target_params.len() {
                return false;
            }

            let params_compatible = source_params
                .iter()
                .zip(target_params.iter())
                .all(|(sp, tp)| check_is_assignable_recursive(&tp.ty, &sp.ty, visited));

            if !params_compatible {
                return false;
            }

            check_is_assignable_recursive(source_return_type, target_return_type, visited)
        }
        _ => false,
    };

    visited.remove(&pair);

    result
}

fn check_is_tag_assignable<'a>(
    source_tag: &'a TagType,
    target_tag: &'a TagType,
    visited_declarations: &mut HashSet<(&'a Type, &'a Type)>,
) -> bool {
    match (source_tag, target_tag) {
        (
            TagType {
                id: id_a,
                value_type: Some(value_type_a),
                ..
            },
            TagType {
                id: id_b,
                value_type: Some(value_type_b),
                ..
            },
        ) => {
            if id_a != id_b {
                return false;
            }

            check_is_assignable_recursive(
                value_type_a,
                value_type_b,
                visited_declarations,
            ) && check_is_assignable_recursive(
                value_type_b,
                value_type_a,
                visited_declarations,
            )
        }
        (
            TagType {
                id: id_a,
                value_type: None,
                ..
            },
            TagType {
                id: id_b,
                value_type: None,
                ..
            },
        ) => id_a == id_b,
        _ => false,
    }
}
