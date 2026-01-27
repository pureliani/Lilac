use std::collections::{HashMap, HashSet};

use crate::hir::errors::SemanticErrorKind;
use crate::hir::types::checked_type::{StructKind, Type};
use crate::hir::utils::numeric::{
    get_numeric_type_rank, is_float, is_integer, is_signed,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Adjustment {
    Identity,
    IntExtend { target: Type, is_signed: bool },
    IntTruncate { target: Type },
    FloatExtend { target: Type },
    FloatTruncate { target: Type },
    IntToFloat { target: Type, is_signed: bool },
    FloatToInt { target: Type, is_signed: bool },

    StoreVariantInUnion { discriminator: u16 },

    RemapUnionDiscriminator { remap: HashMap<u16, u16> },

    Incompatible,
}

pub fn analyze_adjustment(source: &Type, target: &Type) -> Adjustment {
    if check_structural_compatibility(source, target) {
        return Adjustment::Identity;
    }

    if is_integer(source) && is_integer(target) {
        let s_rank = get_numeric_type_rank(source);
        let t_rank = get_numeric_type_rank(target);

        if t_rank > s_rank {
            return Adjustment::IntExtend {
                target: target.clone(),
                is_signed: is_signed(source),
            };
        } else if t_rank < s_rank {
            return Adjustment::IntTruncate {
                target: target.clone(),
            };
        }

        return Adjustment::Incompatible;
    }

    if is_float(source) && is_float(target) {
        let s_rank = get_numeric_type_rank(source);
        let t_rank = get_numeric_type_rank(target);

        if t_rank > s_rank {
            return Adjustment::FloatExtend {
                target: target.clone(),
            };
        } else if t_rank < s_rank {
            return Adjustment::FloatTruncate {
                target: target.clone(),
            };
        }
        return Adjustment::Incompatible;
    }

    if is_integer(source) && is_float(target) {
        return Adjustment::IntToFloat {
            target: target.clone(),
            is_signed: is_signed(source),
        };
    }

    if is_float(source) && is_integer(target) {
        return Adjustment::FloatToInt {
            target: target.clone(),
            is_signed: is_signed(target),
        };
    }

    if let Type::Struct(StructKind::Union(t_variants)) = target {
        // Variant -> Union
        if !matches!(source, Type::Struct(StructKind::Union(_))) {
            if let Some(index) = t_variants
                .iter()
                .position(|v| check_structural_compatibility(source, v))
            {
                return Adjustment::StoreVariantInUnion {
                    discriminator: index as u16,
                };
            }
        }
        // Union -> Union (Widening/Reordering)
        else if let Type::Struct(StructKind::Union(s_variants)) = source {
            let mut remap = HashMap::new();
            let mut possible = true;

            for (s_idx, s_variant) in s_variants.iter().enumerate() {
                let match_idx = t_variants.iter().position(|t_variant| {
                    check_structural_compatibility(s_variant, t_variant)
                });

                if let Some(t_idx) = match_idx {
                    remap.insert(s_idx as u16, t_idx as u16);
                } else {
                    possible = false;
                    break;
                }
            }

            if possible {
                return Adjustment::RemapUnionDiscriminator { remap };
            }
        }
    }

    Adjustment::Incompatible
}

pub fn resolve_binary_op_type(lhs: &Type, rhs: &Type) -> Result<Type, SemanticErrorKind> {
    let lhs_numeric = is_integer(lhs) || is_float(lhs);
    let rhs_numeric = is_integer(rhs) || is_float(rhs);

    if !lhs_numeric || !rhs_numeric {
        return Err(SemanticErrorKind::ExpectedANumericOperand);
    }

    if lhs == rhs {
        return Ok(lhs.clone());
    }

    if is_float(lhs) != is_float(rhs) {
        return Err(SemanticErrorKind::MixedFloatAndInteger);
    }

    if is_signed(lhs) != is_signed(rhs) {
        return Err(SemanticErrorKind::MixedSignedAndUnsigned);
    }

    let l_rank = get_numeric_type_rank(lhs);
    let r_rank = get_numeric_type_rank(rhs);

    if l_rank >= r_rank {
        Ok(lhs.clone())
    } else {
        Ok(rhs.clone())
    }
}

fn check_structural_compatibility<'a>(source: &'a Type, target: &'a Type) -> bool {
    let mut visited = HashSet::new();
    check_recursive(source, target, &mut visited)
}

fn check_recursive<'a>(
    source: &'a Type,
    target: &'a Type,
    visited: &mut HashSet<(&'a Type, &'a Type)>,
) -> bool {
    if source == target {
        return true;
    }

    let pair = (source, target);
    if visited.contains(&pair) {
        return true;
    }
    visited.insert(pair);

    use Type::*;

    let result = match (source, target) {
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
        | (Void, Void) => true,

        (Never, _) => true,
        (_, Unknown) | (Unknown, _) => true,

        (Pointer(s_inner), Pointer(t_inner)) => {
            check_recursive(s_inner, t_inner, visited)
        }

        (Struct(s_kind), Struct(t_kind)) => match (s_kind, t_kind) {
            (StructKind::UserDefined(s_fields), StructKind::UserDefined(t_fields)) => {
                if s_fields.len() != t_fields.len() {
                    return false;
                }
                s_fields.iter().zip(t_fields.iter()).all(|(sp, tp)| {
                    sp.identifier.name == tp.identifier.name
                        && check_recursive(&sp.ty, &tp.ty, visited)
                })
            }
            (StructKind::ListHeader(s_inner), StructKind::ListHeader(t_inner)) => {
                check_recursive(s_inner, t_inner, visited)
            }
            (StructKind::StringHeader, StructKind::StringHeader) => true,

            (StructKind::Union(s_vars), StructKind::Union(t_vars)) => s_vars == t_vars,

            _ => false,
        },

        (Fn(s_fn), Fn(t_fn)) => {
            if s_fn.params.len() != t_fn.params.len() {
                return false;
            }

            let params_ok = s_fn
                .params
                .iter()
                .zip(t_fn.params.iter())
                .all(|(sp, tp)| check_recursive(&sp.ty, &tp.ty, visited));

            let return_ok =
                check_recursive(&s_fn.return_type, &t_fn.return_type, visited);

            params_ok && return_ok
        }

        _ => false,
    };

    visited.remove(&pair);
    result
}

pub fn check_is_assignable(source: &Type, target: &Type) -> bool {
    analyze_adjustment(source, target) != Adjustment::Incompatible
}
