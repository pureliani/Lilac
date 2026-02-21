use std::collections::{HashMap, HashSet};

use crate::ast::{IdentifierNode, Span};
use crate::compile::interner::StringId;
use crate::hir::builders::{Builder, InBlock, ValueId};
use crate::hir::errors::{SemanticError, SemanticErrorKind};
use crate::hir::instructions::{CastInstr, Instruction};
use crate::hir::types::checked_type::Type;
use crate::hir::utils::numeric::{
    get_numeric_type_rank, is_float, is_integer, is_signed,
};

pub enum Adjustment {
    Identity,
    NumericCast {
        target: Type,
    },
    UnionCoercion {
        target: Type,
    },
    UnwrapUnion {
        target: Type,
    },
    CoerceStruct {
        field_adjustments: Vec<(StringId, Adjustment)>,
    },
}

/// Computes the adjustment needed to convert `source_type` to `target_type`
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
        let s_rank = get_numeric_type_rank(source_type).unwrap();
        let t_rank = get_numeric_type_rank(target).unwrap();

        if t_rank > s_rank || (t_rank < s_rank && is_explicit) {
            return Ok(Adjustment::NumericCast {
                target: target.clone(),
            });
        }
    }

    // Float -> Float
    if is_float(source_type) && is_float(target) {
        let s_rank = get_numeric_type_rank(source_type).unwrap();
        let t_rank = get_numeric_type_rank(target).unwrap();

        if t_rank > s_rank || (t_rank < s_rank && is_explicit) {
            return Ok(Adjustment::NumericCast {
                target: target.clone(),
            });
        }
    }

    // Integer -> Float
    if is_integer(source_type) && is_float(target) {
        return Ok(Adjustment::NumericCast {
            target: target.clone(),
        });
    }

    // Float -> Integer (explicit only)
    if is_float(source_type) && is_integer(target) && is_explicit {
        return Ok(Adjustment::NumericCast {
            target: target.clone(),
        });
    }

    // Union Coercions (Wrapping, Widening, Narrowing)
    if let Some(target_variants) = target.as_union_variants() {
        let is_valid = if let Some(source_variants) = source_type.as_union_variants() {
            let is_widen = source_variants.iter().all(|sv| {
                target_variants
                    .iter()
                    .any(|tv| check_structural_compatibility(sv, tv))
            });

            let is_narrow = target_variants.iter().all(|tv| {
                source_variants
                    .iter()
                    .any(|sv| check_structural_compatibility(tv, sv))
            });

            is_widen || (is_narrow && is_explicit)
        } else {
            target_variants
                .iter()
                .any(|v| check_structural_compatibility(source_type, v))
        };

        if is_valid {
            return Ok(Adjustment::UnionCoercion {
                target: target.clone(),
            });
        }
    }

    // Union -> Single Variant (Unwrapping / Narrowing to 1)
    if is_explicit {
        if let Some(source_variants) = source_type.as_union_variants() {
            if source_variants
                .iter()
                .any(|sv| check_structural_compatibility(sv, target))
            {
                return Ok(Adjustment::UnwrapUnion {
                    target: target.clone(),
                });
            }
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
                return Ok(Adjustment::CoerceStruct { field_adjustments });
            }
        }
    }

    Err(SemanticErrorKind::CannotCastType {
        source_type: source_type.clone(),
        target_type: target.clone(),
    })
}

impl<'a> Builder<'a, InBlock> {
    pub fn compute_adjustment(
        &self,
        source: ValueId,
        target: &Type,
        is_explicit: bool,
    ) -> Result<Adjustment, SemanticErrorKind> {
        let source_type = self.get_value_type(source).clone();
        compute_type_adjustment(&source_type, target, is_explicit)
    }

    pub fn adjust_value(
        &mut self,
        source: ValueId,
        span: Span,
        target: Type,
        is_explicit: bool,
    ) -> Result<ValueId, SemanticError> {
        let adj = self
            .compute_adjustment(source, &target, is_explicit)
            .map_err(|kind| SemanticError {
                kind,
                span: span.clone(),
            })?;
        Ok(self.apply_adjustment(source, adj, span))
    }

    pub fn apply_adjustment(
        &mut self,
        source: ValueId,
        adjustment: Adjustment,
        span: Span,
    ) -> ValueId {
        match adjustment {
            Adjustment::Identity => source,
            Adjustment::NumericCast { target } => {
                let dest = self.new_value_id(target);
                self.push_instruction(Instruction::Cast(CastInstr { src: source, dest }));
                dest
            }
            Adjustment::UnionCoercion { target } => {
                self.coerce_to_union(source, &target, span)
            }
            Adjustment::UnwrapUnion { target } => {
                self.emit_unwrap_from_union(source, &target)
            }
            Adjustment::CoerceStruct { field_adjustments } => {
                self.apply_struct_coercion(source, field_adjustments, span)
            }
        }
    }

    fn apply_struct_coercion(
        &mut self,
        source: ValueId,
        field_adjustments: Vec<(StringId, Adjustment)>,
        span: Span,
    ) -> ValueId {
        let source_type = self.get_value_type(source).clone();

        let source_fields = match &source_type {
            Type::Struct(fields) => fields,
            _ => panic!("INTERNAL COMPILER ERROR: CoerceStruct source is not a struct"),
        };

        let mut adj_map: HashMap<StringId, Adjustment> =
            field_adjustments.into_iter().collect();

        let mut new_fields = Vec::with_capacity(source_fields.len());

        for field in source_fields.clone() {
            let name = field.identifier.name;
            let ident = IdentifierNode {
                name,
                span: span.clone(),
            };

            let src_val = self.emit_read_struct_field(source, ident.clone());

            let final_val = if let Some(adj) = adj_map.remove(&name) {
                self.apply_adjustment(src_val, adj, span.clone())
            } else {
                src_val
            };

            new_fields.push((ident, final_val));
        }

        self.emit_struct_init(new_fields)
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
            if source_variants.len() != target_variants.len() {
                return false;
            }

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
