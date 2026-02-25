use std::collections::HashSet;

use crate::{
    ast::Span,
    hir::{
        errors::{SemanticError, SemanticErrorKind},
        types::checked_type::{LiteralType, Type},
        utils::numeric::{get_numeric_type_rank, is_float, is_integer, is_signed},
    },
};

/// Checks if `source` can be assigned/cast to `target`
///
/// `is_explicit = false`: implicit conversions only (assignments, args, returns)
///   - Numeric widening (i32 → i64, f32 → f64, integer → float)
///   - Union coercion (i32 → i32 | string)
///
/// `is_explicit = true`: adds explicit conversions (typecast `as`)
///   - Numeric narrowing (i64 → i32, float → integer)
///   - Union unwrapping (i32 | string → i32)
pub fn check_assignable(source: &Type, target: &Type, is_explicit: bool) -> bool {
    let mut seen = HashSet::new();
    check_assignable_recursive(source, target, is_explicit, &mut seen)
}

fn check_invariant(
    source: &Type,
    target: &Type,
    seen: &mut HashSet<(Type, Type)>,
) -> bool {
    check_assignable_recursive(source, target, false, seen)
        && check_assignable_recursive(target, source, false, seen)
}

fn check_assignable_recursive(
    source: &Type,
    target: &Type,
    is_explicit: bool,
    seen: &mut HashSet<(Type, Type)>,
) -> bool {
    if source == target {
        return true;
    }

    if matches!(source, Type::Never) {
        return true;
    }

    if matches!(target, Type::Unknown) || matches!(source, Type::Unknown) {
        return true;
    }

    if !seen.insert((source.clone(), target.clone())) {
        return true;
    }

    let result = match (source, target) {
        (Type::Literal(lit), _) => {
            let base_type = match lit {
                LiteralType::Number(n) => {
                    use crate::tokenize::NumberKind::*;
                    match n.0 {
                        I64(_) => Type::I64,
                        I32(_) => Type::I32,
                        I16(_) => Type::I16,
                        I8(_) => Type::I8,
                        F32(_) => Type::F32,
                        F64(_) => Type::F64,
                        U64(_) => Type::U64,
                        U32(_) => Type::U32,
                        U16(_) => Type::U16,
                        U8(_) => Type::U8,
                        ISize(_) => Type::ISize,
                        USize(_) => Type::USize,
                    }
                }
                LiteralType::Bool(_) => Type::Bool,
                LiteralType::String(_) => Type::String,
            };
            check_assignable_recursive(&base_type, target, is_explicit, seen)
        }

        (Type::Union(s_variants), Type::Union(t_variants)) => {
            s_variants.iter().all(|s| {
                t_variants
                    .iter()
                    .any(|t| check_assignable_recursive(s, t, is_explicit, seen))
            })
        }
        (Type::Union(s_variants), _) => s_variants
            .iter()
            .all(|s| check_assignable_recursive(s, target, is_explicit, seen)),
        (_, Type::Union(t_variants)) => t_variants
            .iter()
            .any(|t| check_assignable_recursive(source, t, is_explicit, seen)),
        (s, t) if is_integer(s) && is_integer(t) => {
            let s_rank = get_numeric_type_rank(s).unwrap();
            let t_rank = get_numeric_type_rank(t).unwrap();
            is_explicit || t_rank > s_rank
        }
        (s, t) if is_float(s) && is_float(t) => {
            let s_rank = get_numeric_type_rank(s).unwrap();
            let t_rank = get_numeric_type_rank(t).unwrap();
            is_explicit || t_rank > s_rank
        }
        (s, t) if is_integer(s) && is_float(t) => true,
        (s, t) if is_float(s) && is_integer(t) => is_explicit,

        // Struct -> Struct (Invariant)
        // Because structs are passed by reference and are mutable,
        // their fields must be strictly invariant to prevent memory corruption
        (Type::Struct(s_fields), Type::Struct(t_fields)) => {
            if s_fields.len() == t_fields.len() {
                s_fields.iter().zip(t_fields.iter()).all(|(sf, tf)| {
                    sf.identifier.name == tf.identifier.name
                        && check_invariant(&sf.ty, &tf.ty, seen)
                })
            } else {
                false
            }
        }

        // List -> List (Invariant)
        // Strictly Invariant to prevent the Array Covariance Problem
        (Type::List(s_elem), Type::List(t_elem)) => check_invariant(s_elem, t_elem, seen),

        // Fn -> Fn (Invariant)
        (Type::Fn(s_fn), Type::Fn(t_fn)) => {
            if s_fn.params.len() == t_fn.params.len() {
                let params_ok = s_fn
                    .params
                    .iter()
                    .zip(t_fn.params.iter())
                    .all(|(sp, tp)| check_invariant(&sp.ty, &tp.ty, seen));
                let return_ok =
                    check_invariant(&s_fn.return_type, &t_fn.return_type, seen);
                params_ok && return_ok
            } else {
                false
            }
        }

        _ => false,
    };

    seen.remove(&(source.clone(), target.clone()));
    result
}

pub fn type_mismatch_error(source: &Type, target: &Type) -> SemanticErrorKind {
    SemanticErrorKind::TypeMismatch {
        expected: target.clone(),
        received: source.clone(),
    }
}

pub fn arithmetic_supertype(
    left: &Type,
    left_span: Span,
    right: &Type,
    right_span: Span,
) -> Result<Type, SemanticError> {
    let span = Span {
        start: left_span.start,
        end: right_span.end,
        path: left_span.path.clone(),
    };

    let left_type = if is_float(left) || is_integer(left) {
        left
    } else {
        return Err(SemanticError {
            kind: SemanticErrorKind::ExpectedANumericOperand,
            span: left_span,
        });
    };

    let right_type = if is_float(right) || is_integer(right) {
        right
    } else {
        return Err(SemanticError {
            kind: SemanticErrorKind::ExpectedANumericOperand,
            span: right_span,
        });
    };

    if (is_float(left_type) && is_integer(right_type))
        || (is_integer(left_type) && is_float(right_type))
    {
        return Err(SemanticError {
            kind: SemanticErrorKind::MixedFloatAndInteger,
            span,
        });
    }

    if is_signed(left_type) != is_signed(right_type) {
        return Err(SemanticError {
            kind: SemanticErrorKind::MixedSignedAndUnsigned,
            span,
        });
    }

    if right_type == left_type {
        return Ok(left_type.clone());
    }

    let left_rank = get_numeric_type_rank(left_type);
    let right_rank = get_numeric_type_rank(right_type);

    if left_rank > right_rank {
        Ok(left_type.clone())
    } else {
        Ok(right_type.clone())
    }
}
