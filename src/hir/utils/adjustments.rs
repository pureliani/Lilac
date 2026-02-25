use crate::{
    ast::Span,
    hir::{
        errors::{SemanticError, SemanticErrorKind},
        types::checked_type::Type,
        utils::numeric::{get_numeric_type_rank, is_float, is_integer, is_signed},
    },
};

/// Checks if `source` can be assigned/cast to `target`
///
/// `is_explicit = false`: implicit conversions only (assignments, args, returns)
///   - Numeric widening (i32 → i64, f32 → f64, integer → float)
///   - Union coercion (i32 → i32 | string)
///   - Struct field coercion ({ value: i32 } → { value: i32 | string })
///
/// `is_explicit = true`: adds explicit conversions (typecast `as`)
///   - Numeric narrowing (i64 → i32, float → integer)
///   - Union unwrapping (i32 | string → i32)
pub fn check_assignable(source: &Type, target: &Type, is_explicit: bool) -> bool {
    check_assignable_recursive(source, target, is_explicit)
}

fn check_assignable_recursive(source: &Type, target: &Type, is_explicit: bool) -> bool {
    if source == target {
        return true;
    }

    if matches!(source, Type::Never) {
        return true;
    }

    if matches!(target, Type::Unknown) || matches!(source, Type::Unknown) {
        return true;
    }

    // Integer → Integer
    if is_integer(source) && is_integer(target) {
        let s_rank = get_numeric_type_rank(source).unwrap();
        let t_rank = get_numeric_type_rank(target).unwrap();
        if t_rank > s_rank || (t_rank < s_rank && is_explicit) {
            return true;
        }
    }

    // Float → Float
    if is_float(source) && is_float(target) {
        let s_rank = get_numeric_type_rank(source).unwrap();
        let t_rank = get_numeric_type_rank(target).unwrap();
        if t_rank > s_rank || (t_rank < s_rank && is_explicit) {
            return true;
        }
    }

    // Integer → Float
    if is_integer(source) && is_float(target) {
        return true;
    }

    // Float → Integer (explicit only)
    if is_float(source) && is_integer(target) && is_explicit {
        return true;
    }

    // Unions
    match (source.as_union_variants(), target.as_union_variants()) {
        (None, Some(_)) => todo!(),
        (Some(_), None) => todo!(),
        (Some(_), Some(_)) => todo!(),
        (None, None) => todo!(),
    }

    // Struct → Struct (field-level assignability)
    if let (Type::Struct(s_fields), Type::Struct(t_fields)) = (source, target) {
        if s_fields.len() == t_fields.len() {
            return s_fields.iter().zip(t_fields.iter()).all(|(sf, tf)| {
                sf.identifier.name == tf.identifier.name
                    && check_assignable_recursive(&sf.ty, &tf.ty, is_explicit)
            });
        }
    }

    if let (Type::List(s_elem), Type::List(t_elem)) = (source, target) {
        return check_assignable_recursive(s_elem, t_elem, is_explicit);
    }

    if let (Type::Fn(s_fn), Type::Fn(t_fn)) = (source, target) {
        if s_fn.params.len() != t_fn.params.len() {
            return false;
        }
        let params_ok = s_fn
            .params
            .iter()
            .zip(t_fn.params.iter())
            .all(|(sp, tp)| check_assignable_recursive(&sp.ty, &tp.ty, is_explicit));
        return params_ok
            && check_assignable_recursive(
                &s_fn.return_type,
                &t_fn.return_type,
                is_explicit,
            );
    }

    false
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
