use std::collections::HashSet;

use crate::hir::types::checked_type::{StructKind, Type};

pub fn check_is_assignable<'a>(source_type: &'a Type, target_type: &'a Type) -> bool {
    let mut visited_declarations: HashSet<(&'a Type, &'a Type)> = HashSet::new();
    check_is_assignable_recursive(source_type, target_type, &mut visited_declarations)
}
fn check_is_assignable_recursive<'a>(
    source_type: &'a Type,
    target_type: &'a Type,
    visited: &mut HashSet<(&'a Type, &'a Type)>,
) -> bool {
    if source_type == target_type {
        return true;
    }

    let pair = (source_type, target_type);
    if visited.contains(&pair) {
        return true;
    }
    visited.insert(pair);

    use Type::*;

    let result = match (source_type, target_type) {
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
        | (Bool, Bool) => true,

        (Never, _) => true,
        (_, Unknown) | (Unknown, _) => true,

        (Struct(StructKind::Union(variants)), target) => variants
            .iter()
            .all(|variant| check_is_assignable_recursive(variant, target, visited)),

        (source, Struct(StructKind::Union(variants))) => variants
            .iter()
            .any(|variant| check_is_assignable_recursive(source, variant, visited)),

        (Pointer(s_inner), Pointer(t_inner)) => {
            check_is_assignable_recursive(s_inner, t_inner, visited)
                && check_is_assignable_recursive(t_inner, s_inner, visited)
        }

        (Struct(s_kind), Struct(t_kind)) => match (s_kind, t_kind) {
            (StructKind::UserDefined(s_fields), StructKind::UserDefined(t_fields)) => {
                if s_fields.len() != t_fields.len() {
                    return false;
                }
                s_fields.iter().zip(t_fields.iter()).all(|(sp, tp)| {
                    sp.identifier.name == tp.identifier.name
                        && check_is_assignable_recursive(&sp.ty, &tp.ty, visited)
                })
            }

            (StructKind::ListHeader(s_inner), StructKind::ListHeader(t_inner)) => {
                check_is_assignable_recursive(s_inner, t_inner, visited)
                    && check_is_assignable_recursive(t_inner, s_inner, visited)
            }
            (StructKind::StringHeader, StructKind::StringHeader) => true,
            _ => false,
        },

        (Fn(s_fn), Fn(t_fn)) => {
            if s_fn.params.len() != t_fn.params.len() {
                return false;
            }
            let params_ok =
                s_fn.params.iter().zip(t_fn.params.iter()).all(|(sp, tp)| {
                    check_is_assignable_recursive(&tp.ty, &sp.ty, visited)
                });

            let return_ok = check_is_assignable_recursive(
                &s_fn.return_type,
                &t_fn.return_type,
                visited,
            );

            params_ok && return_ok
        }

        _ => false,
    };

    visited.remove(&pair);

    result
}
