use crate::mir::types::checked_type::Type;

pub fn get_numeric_type_rank(ty: &Type) -> i32 {
    use Type::*;
    match &ty {
        I8 | U8 => 1,
        I16 | U16 => 2,
        I32 | U32 | ISize | USize => 3,
        I64 | U64 => 4,
        F32 => 5,
        F64 => 6,
        _ => 0,
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
