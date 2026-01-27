use super::numeric::is_integer;
use crate::hir::{
    types::checked_type::{StructKind, Type},
    utils::adjustments::check_is_assignable,
};

pub fn check_is_equatable(left: &Type, right: &Type) -> bool {
    if let (Type::Pointer(l), Type::Pointer(r)) = (left, right) {
        return check_is_equatable(l, r);
    }

    if check_is_assignable(left, right) || check_is_assignable(right, left) {
        // Filter out types that shouldn't be compared even if they match
        return !matches!(
            left,
            Type::Void | Type::Fn(_) | Type::Unknown | Type::Buffer { .. }
        );
    }

    if is_integer(left) && is_integer(right) {
        return true;
    }

    if matches!(left, Type::Struct(StructKind::StringHeader))
        && matches!(right, Type::Struct(StructKind::StringHeader))
    {
        return true;
    }

    false
}
