use std::collections::{BTreeSet, HashSet};

use crate::ast::Span;
use crate::hir::builders::{Builder, InBlock, ValueId};
use crate::hir::errors::{SemanticError, SemanticErrorKind};
use crate::hir::types::checked_type::{StructKind, Type};
use crate::hir::utils::numeric::{
    get_numeric_type_rank, is_float, is_integer, is_signed,
};

pub enum Adjustment {
    // No conversion needed
    Identity,

    // Numeric conversions
    SExt {
        target: Type,
    },
    ZExt {
        target: Type,
    },
    Trunc {
        target: Type,
    },
    FExt {
        target: Type,
    },
    FTrunc {
        target: Type,
    },
    SIToF {
        target: Type,
    },
    UIToF {
        target: Type,
    },
    FToSI {
        target: Type,
    },
    FToUI {
        target: Type,
    },

    // Union conversions
    WrapInUnion {
        variants: BTreeSet<Type>,
    },
    WidenUnion {
        source_variants: BTreeSet<Type>,
        target_variants: BTreeSet<Type>,
    },
}

impl<'a> Builder<'a, InBlock> {
    pub fn compute_adjustment(
        &self,
        source: ValueId,
        target: &Type,
        is_explicit: bool,
    ) -> Result<Adjustment, SemanticErrorKind> {
        let source_type = self.get_value_type(&source).clone();

        if check_structural_compatibility(&source_type, target) {
            return Ok(Adjustment::Identity);
        }

        // Integer -> Integer
        if is_integer(&source_type) && is_integer(target) {
            let s_rank = get_numeric_type_rank(&source_type);
            let t_rank = get_numeric_type_rank(target);

            if t_rank > s_rank {
                return if is_signed(&source_type) {
                    Ok(Adjustment::SExt {
                        target: target.clone(),
                    })
                } else {
                    Ok(Adjustment::ZExt {
                        target: target.clone(),
                    })
                };
            } else if t_rank < s_rank && is_explicit {
                return Ok(Adjustment::Trunc {
                    target: target.clone(),
                });
            }
        }

        // Float -> Float
        if is_float(&source_type) && is_float(target) {
            let s_rank = get_numeric_type_rank(&source_type);
            let t_rank = get_numeric_type_rank(target);

            if t_rank > s_rank {
                return Ok(Adjustment::FExt {
                    target: target.clone(),
                });
            } else if t_rank < s_rank && is_explicit {
                return Ok(Adjustment::FTrunc {
                    target: target.clone(),
                });
            }
        }

        // Integer -> Float
        if is_integer(&source_type) && is_float(target) {
            return if is_signed(&source_type) {
                Ok(Adjustment::SIToF {
                    target: target.clone(),
                })
            } else {
                Ok(Adjustment::UIToF {
                    target: target.clone(),
                })
            };
        }

        // Float -> Integer
        if is_float(&source_type) && is_integer(target) && is_explicit {
            return if is_signed(target) {
                Ok(Adjustment::FToSI {
                    target: target.clone(),
                })
            } else {
                Ok(Adjustment::FToUI {
                    target: target.clone(),
                })
            };
        }

        // Variant -> Union
        if let Some(target_variants) = target.as_union_variants() {
            if target_variants
                .iter()
                .any(|v| check_structural_compatibility(&source_type, v))
            {
                return Ok(Adjustment::WrapInUnion {
                    variants: target_variants.clone(),
                });
            }
        }

        // Union -> Union (widening)
        if let (Some(source_variants), Some(target_variants)) =
            (source_type.as_union_variants(), target.as_union_variants())
        {
            let all_present = source_variants.iter().all(|sv| {
                target_variants
                    .iter()
                    .any(|tv| check_structural_compatibility(sv, tv))
            });

            if all_present {
                return Ok(Adjustment::WidenUnion {
                    source_variants: source_variants.clone(),
                    target_variants: target_variants.clone(),
                });
            }
        }

        Err(SemanticErrorKind::CannotCastType {
            source_type,
            target_type: target.clone(),
        })
    }

    pub fn apply_adjustment(
        &mut self,
        source: ValueId,
        adjustment: Adjustment,
        span: Span,
    ) -> ValueId {
        match adjustment {
            Adjustment::Identity => source,
            Adjustment::SExt { target } => self.emit_sext(source, target),
            Adjustment::ZExt { target } => self.emit_zext(source, target),
            Adjustment::Trunc { target } => self.emit_trunc(source, target),
            Adjustment::FExt { target } => self.emit_fext(source, target),
            Adjustment::FTrunc { target } => self.emit_ftrunc(source, target),
            Adjustment::SIToF { target } => self.emit_sitof(source, target),
            Adjustment::UIToF { target } => self.emit_uitof(source, target),
            Adjustment::FToSI { target } => self.emit_ftosi(source, target),
            Adjustment::FToUI { target } => self.emit_ftoui(source, target),
            Adjustment::WrapInUnion { variants } => {
                self.wrap_in_union(source, &variants, span)
            }
            Adjustment::WidenUnion {
                source_variants,
                target_variants,
            } => self.widen_union(source, &source_variants, &target_variants, span),
        }
    }

    pub fn adjust_value(
        &mut self,
        source: ValueId,
        source_span: Span,
        target: Type,
        is_explicit: bool,
    ) -> Result<ValueId, SemanticError> {
        let adjustment = self
            .compute_adjustment(source, &target, is_explicit)
            .map_err(|kind| SemanticError {
                kind,
                span: source_span.clone(),
            })?;

        Ok(self.apply_adjustment(source, adjustment, source_span))
    }
}

pub fn check_structural_compatibility<'a>(source: &'a Type, target: &'a Type) -> bool {
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
        (Type::UnionPayload(source_variants), Type::UnionPayload(target_variants)) => {
            source_variants.iter().all(|sv| {
                target_variants
                    .iter()
                    .any(|tv| check_structural_compatibility(sv, tv))
            })
        }
        _ => false,
    };

    visited.remove(&pair);
    result
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
