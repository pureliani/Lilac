use crate::ast::Span;
use crate::compile::interner::StringId;
use crate::mir::builders::{Builder, InBlock, ValueId};
use crate::mir::errors::{SemanticError, SemanticErrorKind};
use crate::mir::types::checked_type::{StructKind, Type};
use crate::mir::utils::numeric::{
    get_numeric_type_rank, is_float, is_integer, is_signed,
};

pub enum AdjustmentError {
    Incompatible,
    TryExplicitCast,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Adjustment {
    Identity,

    SIToF,
    UIToF,
    FToSI,
    FToUI,
    FExt,
    FTrunc,
    Trunc,
    SExt,
    ZExt,
    BitCast,

    WrapInUnion(usize),
    UnwrapUnion,
    /// Maps old variant indices to new variant indices, Vec<(old_idx, new_idx)>
    ReTagUnion(Vec<(u64, u64)>),

    CoerceStruct {
        field_adjustments: Vec<(StringId, Adjustment)>,
    },
}

/// Computes the adjustment needed to convert `source_type` to `target_type`
pub fn compute_type_adjustment(
    source: &Type,
    target: &Type,
    is_explicit: bool,
) -> Result<Adjustment, AdjustmentError> {
    if source == target {
        return Ok(Adjustment::Identity);
    };

    if is_integer(source) && is_integer(target) {
        let s_rank = get_numeric_type_rank(source).unwrap();
        let t_rank = get_numeric_type_rank(target).unwrap();

        if t_rank > s_rank {
            return if is_signed(source) {
                Ok(Adjustment::SExt)
            } else {
                Ok(Adjustment::ZExt)
            };
        } else if t_rank < s_rank && is_explicit {
            return Ok(Adjustment::Trunc);
        } else {
            if is_explicit {
                return Ok(Adjustment::BitCast);
            }
        }
    }

    if is_float(source) && is_float(target) {
        let s_rank = get_numeric_type_rank(source).unwrap();
        let t_rank = get_numeric_type_rank(target).unwrap();

        if t_rank > s_rank {
            return Ok(Adjustment::FExt);
        } else if t_rank < s_rank && is_explicit {
            return Ok(Adjustment::FTrunc);
        }
    }

    if is_integer(source) && is_float(target) {
        let is_lossless = match (source, target.widen()) {
            (Type::I32(Some(v)), Type::F32(_)) => (*v as f32) as i32 == *v,
            (Type::U32(Some(v)), Type::F32(_)) => (*v as f32) as u32 == *v,
            (Type::I64(Some(v)), Type::F32(_)) => (*v as f32) as i64 == *v,
            (Type::U64(Some(v)), Type::F32(_)) => (*v as f32) as u64 == *v,
            (Type::I64(Some(v)), Type::F64(_)) => (*v as f64) as i64 == *v,
            (Type::U64(Some(v)), Type::F64(_)) => (*v as f64) as u64 == *v,
            (Type::ISize(Some(v)), Type::F32(_)) => (*v as f32) as isize == *v,
            (Type::USize(Some(v)), Type::F32(_)) => (*v as f32) as usize == *v,
            (Type::ISize(Some(v)), Type::F64(_)) => (*v as f64) as isize == *v,
            (Type::USize(Some(v)), Type::F64(_)) => (*v as f64) as usize == *v,

            (src, tgt) => match (src.widen(), tgt) {
                (
                    Type::I8(_) | Type::U8(_) | Type::I16(_) | Type::U16(_),
                    Type::F32(_) | Type::F64(_),
                ) => true,

                (Type::I32(_) | Type::U32(_), Type::F64(_)) => true,

                _ => false,
            },
        };

        if is_lossless || is_explicit {
            return if is_signed(source) {
                Ok(Adjustment::SIToF)
            } else {
                Ok(Adjustment::UIToF)
            };
        } else {
            return Err(AdjustmentError::TryExplicitCast);
        }
    }

    if is_float(source) && is_integer(target) && is_explicit {
        return if is_signed(target) {
            Ok(Adjustment::FToSI)
        } else {
            Ok(Adjustment::FToUI)
        };
    }

    if let (Some(source_variants), Some(target_variants)) =
        (source.get_union_variants(), target.get_union_variants())
    {
        let mut mapping = Vec::new();

        for (old_idx, sv) in source_variants.iter().enumerate() {
            if let Some(new_idx) = target_variants.iter().position(|tv| tv == sv) {
                mapping.push((old_idx as u64, new_idx as u64));
            } else {
                return Err(AdjustmentError::Incompatible);
            }
        }

        return Ok(Adjustment::ReTagUnion(mapping));
    }

    if let Some(target_variants) = target.get_union_variants() {
        if let Some(idx) = target_variants.iter().position(|v| v == source) {
            return Ok(Adjustment::WrapInUnion(idx));
        }
    }

    if let Some(source_variants) = source.get_union_variants() {
        if source_variants.len() == 1 && source_variants.contains(target) {
            return Ok(Adjustment::UnwrapUnion);
        }
    }

    if let (
        Type::Struct(StructKind::UserDefined(s_fields)),
        Type::Struct(StructKind::UserDefined(t_fields)),
    ) = (source, target)
    {
        if s_fields.len() == t_fields.len() {
            let mut field_adjustments = Vec::new();
            let mut possible = true;

            for (sf, tf) in s_fields.iter().zip(t_fields.iter()) {
                if sf.identifier.name != tf.identifier.name {
                    possible = false;
                    break;
                }

                match compute_type_adjustment(&sf.ty.kind, &tf.ty.kind, is_explicit) {
                    Ok(adj) => {
                        if adj != Adjustment::Identity {
                            field_adjustments.push((sf.identifier.name, adj));
                        }
                    }
                    Err(_) => {
                        possible = false;
                        break;
                    }
                }
            }

            if possible {
                if field_adjustments.is_empty() {
                    return Ok(Adjustment::Identity);
                }
                if is_explicit {
                    return Ok(Adjustment::CoerceStruct { field_adjustments });
                } else {
                    return Err(AdjustmentError::TryExplicitCast);
                }
            }
        }
    }

    Err(AdjustmentError::Incompatible)
}

impl<'a> Builder<'a, InBlock> {
    pub fn compute_adjustment(
        &self,
        source: ValueId,
        target: &Type,
        is_explicit: bool,
    ) -> Result<Adjustment, SemanticErrorKind> {
        let source_type = self.get_value_type(source).clone();

        compute_type_adjustment(&source_type, target, is_explicit).map_err(
            |err| match err {
                AdjustmentError::Incompatible => SemanticErrorKind::CannotCastType {
                    source_type,
                    target_type: target.clone(),
                },
                AdjustmentError::TryExplicitCast => SemanticErrorKind::TryExplicitCast,
            },
        )
    }

    /// Applies a previously computed adjustment, emitting the necessary
    /// instructions and returning the adjusted value.
    pub fn apply_adjustment(
        &mut self,
        source: ValueId,
        adjustment: Adjustment,
        target_type: &Type,
        span: Span,
    ) -> ValueId {
        match adjustment {
            Adjustment::Identity => source,

            Adjustment::SExt => self.emit_sext(source, target_type.clone()),
            Adjustment::ZExt => self.emit_zext(source, target_type.clone()),
            Adjustment::Trunc => self.emit_trunc(source, target_type.clone()),
            Adjustment::FExt => self.emit_fext(source, target_type.clone()),
            Adjustment::FTrunc => self.emit_ftrunc(source, target_type.clone()),
            Adjustment::SIToF => self.emit_sitof(source, target_type.clone()),
            Adjustment::UIToF => self.emit_uitof(source, target_type.clone()),
            Adjustment::FToSI => self.emit_ftosi(source, target_type.clone()),
            Adjustment::FToUI => self.emit_ftoui(source, target_type.clone()),
            Adjustment::BitCast => self.emit_bitcast(source, target_type.clone()),

            Adjustment::WrapInUnion(tag_idx) => {
                self.wrap_in_union(source, tag_idx, target_type)
            }
            Adjustment::UnwrapUnion => self.unwrap_from_union(source, target_type, span),
            Adjustment::ReTagUnion(mapping) => {
                self.retag_union(source, mapping, target_type, span)
            }
            Adjustment::CoerceStruct { field_adjustments } => {
                self.apply_struct_coercion(source, target_type, field_adjustments, span)
            }
        }
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
