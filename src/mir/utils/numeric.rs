use crate::mir::types::checked_type::Type;

pub fn get_numeric_type_rank(ty: &Type) -> Option<i32> {
    use Type::*;
    match &ty {
        I8(_) | U8(_) => Some(1),
        I16(_) | U16(_) => Some(2),
        I32(_) | U32(_) | ISize(_) | USize(_) => Some(3),
        I64(_) | U64(_) => Some(4),
        F32(_) => Some(5),
        F64(_) => Some(6),
        _ => None,
    }
}

pub fn is_float(ty: &Type) -> bool {
    use Type::*;
    matches!(ty, F32(_) | F64(_))
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
