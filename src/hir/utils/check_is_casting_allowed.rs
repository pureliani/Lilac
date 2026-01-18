use crate::hir::{
    types::checked_type::Type,
    utils::numeric::{get_numeric_type_rank, is_float, is_integer, is_signed},
};

pub fn check_is_casting_allowed(source_type: &Type, target_type: &Type) -> bool {
    match (&source_type, &target_type) {
        (
            Type::Pointer {
                constraint: source_constraint,
                ..
            },
            Type::Pointer {
                constraint: target_constraint,
                ..
            },
        ) => source_constraint == target_constraint,
        (source_t, target_t)
            if is_integer(source_t)
                && is_integer(target_t)
                && (is_signed(source_t) == is_signed(target_t)) =>
        {
            get_numeric_type_rank(source_t) <= get_numeric_type_rank(target_t)
        }
        (source_t, target_t) if is_float(source_t) && is_float(target_t) => {
            get_numeric_type_rank(source_t) <= get_numeric_type_rank(target_t)
        }
        (source_t, target_t) if is_integer(source_t) && is_float(target_t) => true,

        (Type::Pointer { .. }, target_t) if is_integer(target_t) => true,
        (source_t, Type::Pointer { .. }) if is_integer(source_t) => true,
        _ => false,
    }
}
