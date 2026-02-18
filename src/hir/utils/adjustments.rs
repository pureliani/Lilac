use std::collections::{BTreeSet, HashMap, HashSet};

use crate::ast::Span;
use crate::compile::interner::StringId;
use crate::hir::builders::{Builder, InBlock, ValueId};
use crate::hir::errors::{SemanticError, SemanticErrorKind};
use crate::hir::types::checked_type::Type;
use crate::hir::utils::numeric::{
    get_numeric_type_rank, is_float, is_integer, is_signed,
};
use crate::tokenize::NumberKind;

pub enum Adjustment {
    /// No conversion needed.
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

    /// Struct conversion
    CoerceStruct {
        target_inner_type: Type,
        field_adjustments: Vec<(StringId, Adjustment)>,
    },
}

/// Computes the adjustment needed to convert `source_type` to `target_type`.
pub fn compute_type_adjustment(
    source_type: &Type,
    target: &Type,
    is_explicit: bool,
) -> Result<Adjustment, SemanticErrorKind> {
    if check_structural_compatibility(source_type, target) {
        return Ok(Adjustment::Identity);
    }

    // Integer -> Integer
    if is_integer(source_type) && is_integer(target) {
        let s_rank = get_numeric_type_rank(source_type);
        let t_rank = get_numeric_type_rank(target);

        if t_rank > s_rank {
            return if is_signed(source_type) {
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
    if is_float(source_type) && is_float(target) {
        let s_rank = get_numeric_type_rank(source_type);
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
    if is_integer(source_type) && is_float(target) {
        return if is_signed(source_type) {
            Ok(Adjustment::SIToF {
                target: target.clone(),
            })
        } else {
            Ok(Adjustment::UIToF {
                target: target.clone(),
            })
        };
    }

    // Float -> Integer (explicit only)
    if is_float(source_type) && is_integer(target) && is_explicit {
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
            .any(|v| check_structural_compatibility(source_type, v))
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

    // Struct -> Struct (field-level coercion)
    if let (Type::Struct(s_fields), Type::Struct(t_fields)) = (source_type, target) {
        if s_fields.len() == t_fields.len() {
            let mut field_adjustments = Vec::new();
            let mut all_ok = true;

            for (sf, tf) in s_fields.iter().zip(t_fields.iter()) {
                if sf.identifier.name != tf.identifier.name {
                    all_ok = false;
                    break;
                }

                if check_structural_compatibility(&sf.ty, &tf.ty) {
                    continue;
                }

                match compute_type_adjustment(&sf.ty, &tf.ty, is_explicit) {
                    Ok(adj) => {
                        field_adjustments.push((sf.identifier.name, adj));
                    }
                    Err(_) => {
                        all_ok = false;
                        break;
                    }
                }
            }

            if all_ok {
                if field_adjustments.is_empty() {
                    return Ok(Adjustment::Identity);
                }
                return Ok(Adjustment::CoerceStruct {
                    target_inner_type: (**t_inner).clone(),
                    field_adjustments,
                });
            }
        }
    }

    Err(SemanticErrorKind::CannotCastType {
        source_type: source_type.clone(),
        target_type: target.clone(),
    })
}

impl<'a> Builder<'a, InBlock> {
    /// Computes the adjustment needed for a specific value.
    pub fn compute_adjustment(
        &self,
        source: ValueId,
        target: &Type,
        is_explicit: bool,
    ) -> Result<Adjustment, SemanticErrorKind> {
        let source_type = self.get_value_type(source).clone();
        compute_type_adjustment(&source_type, target, is_explicit)
    }

    /// Allocates a new struct with the target layout, copies all fields
    /// from the source, and applies per-field adjustments where needed.
    fn apply_struct_coercion(
        &mut self,
        source: ValueId,
        target_struct_type: Type,
        field_adjustments: Vec<(StringId, Adjustment)>,
        span: Span,
    ) -> ValueId {
        let source_type = self.get_value_type(source).clone();
        let source_inner = source_type
            .try_unwrap_pointer()
            .expect("INTERNAL COMPILER ERROR: CoerceStruct source is not a pointer");

        let source_fields = match &source_inner {
            Type::Struct(StructKind::UserDefined(fields)) => fields,
            _ => panic!(
                "INTERNAL COMPILER ERROR: CoerceStruct source is not a \
                 user-defined struct"
            ),
        };

        let one = self.emit_const_number(NumberKind::USize(1));
        let new_ptr = self.emit_heap_alloc(target_struct_type, one);

        let mut adj_map: HashMap<StringId, Adjustment> =
            field_adjustments.into_iter().collect();

        for field in source_fields.clone() {
            let name = field.identifier.name;
            let src_field_ptr = self.get_field_ptr(source, name);
            let src_val = self.emit_load(src_field_ptr);
            let dst_field_ptr = self.get_field_ptr(new_ptr, name);

            if let Some(adj) = adj_map.remove(&name) {
                let adjusted = self.apply_adjustment(src_val, adj, span.clone());
                self.emit_store(dst_field_ptr, adjusted, span.clone());
            } else {
                self.emit_store(dst_field_ptr, src_val, span.clone());
            }
        }

        new_ptr
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
        | (String, String)
        | (Void, Void) => true,

        (Never, _) => true,
        (_, Unknown) | (Unknown, _) => true,

        (Struct(s_fields), Struct(t_fields)) => {
            if s_fields.len() != t_fields.len() {
                return false;
            }

            s_fields.iter().zip(t_fields.iter()).all(|(sp, tp)| {
                sp.identifier.name == tp.identifier.name
                    && check_recursive(&sp.ty, &tp.ty, visited)
            })
        }

        (List(source_item), List(target_item)) => {
            check_recursive(source_item, target_item, visited)
        }

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

        (Type::Union(source_variants), Type::Union(target_variants)) => {
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
