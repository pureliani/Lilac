use crate::hir::types::checked_type::Type;

pub fn get_numeric_type_rank(ty: &Type) -> Option<i32> {
    use Type::*;
    match &ty {
        I8 | U8 => Some(1),
        I16 | U16 => Some(2),
        I32 | U32 | ISize | USize => Some(3),
        I64 | U64 => Some(4),
        F32 => Some(5),
        F64 => Some(6),
        _ => None,
    }
}

pub fn is_float(ty: &Type) -> bool {
    use Type::*;
    matches!(ty, F32 | F64)
}

pub fn is_integer(ty: &Type) -> bool {
    use Type::*;
    matches!(
        ty,
        I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64 | ISize | USize
    )
}

pub fn is_signed(ty: &Type) -> bool {
    use Type::*;
    matches!(ty, I8 | I16 | I32 | I64 | ISize | F32 | F64)
}
