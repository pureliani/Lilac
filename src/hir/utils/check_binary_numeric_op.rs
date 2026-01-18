use crate::{
    ast::Span,
    hir::{
        errors::{SemanticError, SemanticErrorKind},
        types::checked_type::Type,
        utils::numeric::{get_numeric_type_rank, is_float, is_integer, is_signed},
    },
};

pub fn check_binary_numeric_operation(
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
