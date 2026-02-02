use std::collections::{HashMap, HashSet};

use crate::ast::Span;
use crate::globals::COMMON_IDENTIFIERS;
use crate::hir::builders::{Builder, InBlock, ValueId};
use crate::hir::errors::{SemanticError, SemanticErrorKind};
use crate::hir::types::checked_type::{StructKind, Type};
use crate::hir::utils::layout::get_layout_of;
use crate::hir::utils::numeric::{
    get_numeric_type_rank, is_float, is_integer, is_signed,
};
use crate::tokenize::NumberKind;

type ValueAdjuster = dyn FnOnce(&mut Builder<'_, InBlock>) -> ValueId;
type MemoryWriteAdjuster = dyn FnOnce(&mut Builder<'_, InBlock>, ValueId);
pub enum Adjustment {
    Value(Box<ValueAdjuster>),
    Write(Box<MemoryWriteAdjuster>),
}

impl<'a> Builder<'a, InBlock> {
    pub fn adjust_assignment(
        &mut self,
        source: ValueId,
        source_span: Span,
        target: Type,
        is_explicit: bool,
    ) -> Result<Adjustment, SemanticError> {
        let source_type = self.get_value_type(&source).clone();
        let make_error = || SemanticError {
            kind: SemanticErrorKind::CannotCastType {
                source_type: source_type.clone(),
                target_type: target.clone(),
            },
            span: source_span.clone(),
        };

        if check_structural_compatibility(&source_type, &target) {
            return Ok(Adjustment::Value(Box::new(move |_| source)));
        }

        if is_integer(&source_type) && is_integer(&target) {
            let s_rank = get_numeric_type_rank(&source_type);
            let t_rank = get_numeric_type_rank(&target);

            if t_rank > s_rank {
                return Ok(Adjustment::Value(Box::new(move |b| {
                    let is_source_signed = is_signed(&source_type);
                    if is_source_signed {
                        b.emit_sext(source, target.clone())
                    } else {
                        b.emit_zext(source, target.clone())
                    }
                })));
            } else if (t_rank < s_rank) && is_explicit {
                return Ok(Adjustment::Value(Box::new(move |b| {
                    b.emit_trunc(source, target.clone())
                })));
            }

            return Err(make_error());
        }

        if is_float(&source_type) && is_float(&target) {
            let s_rank = get_numeric_type_rank(&source_type);
            let t_rank = get_numeric_type_rank(&target);

            if t_rank > s_rank {
                return Ok(Adjustment::Value(Box::new(move |b| {
                    b.emit_fext(source, target)
                })));
            } else if (t_rank < s_rank) && is_explicit {
                return Ok(Adjustment::Value(Box::new(move |b| {
                    b.emit_ftrunc(source, target)
                })));
            }

            return Err(make_error());
        }

        if is_integer(&source_type) && is_float(&target) {
            return Ok(Adjustment::Value(Box::new(move |b| {
                let is_source_signed = is_signed(&source_type);

                if is_source_signed {
                    b.emit_sitof(source, target)
                } else {
                    b.emit_uitof(source, target)
                }
            })));
        }

        if is_float(&source_type) && is_integer(&target) && is_explicit {
            return Ok(Adjustment::Value(Box::new(move |b| {
                let is_target_signed = is_signed(&target);

                if is_target_signed {
                    b.emit_ftosi(source, target)
                } else {
                    b.emit_ftoui(source, target)
                }
            })));
        }

        // Memory write adjustments (caller allocates)
        let target_inner = if let Type::Pointer(target_inner) = &target {
            if check_structural_compatibility(&source_type, target_inner) {
                return Ok(Adjustment::Write(Box::new(move |b, ptr| {
                    assert!(check_structural_compatibility(
                        b.get_value_type(&ptr),
                        &target
                    ));

                    b.emit_store(ptr, source, source_span);
                })));
            }

            target_inner
        } else {
            return Err(make_error());
        };

        if let Type::Struct(StructKind::Union(t_variants)) = &**target_inner {
            let s_variants = if let Type::Pointer(inner) = &source_type {
                if let Type::Struct(StructKind::Union(s_variants)) = &**inner {
                    Some(s_variants)
                } else {
                    None
                }
            } else {
                None
            };

            match s_variants {
                // Variant -> Union
                None => {
                    if let Some(discriminator) =
                        t_variants.iter().position(|union_variant| {
                            check_structural_compatibility(&source_type, union_variant)
                        })
                    {
                        return Ok(Adjustment::Write(Box::new(
                            move |b, target_union_ptr| {
                                assert!(check_structural_compatibility(
                                    b.get_value_type(&target_union_ptr),
                                    &target
                                ));

                                let union_id_ptr = b.get_field_ptr(
                                    target_union_ptr,
                                    COMMON_IDENTIFIERS.id,
                                );
                                let discriminator_value = b.emit_const_number(
                                    NumberKind::U16(discriminator as u16),
                                );
                                b.emit_store(
                                    union_id_ptr,
                                    discriminator_value,
                                    source_span.clone(),
                                );

                                let union_value_ptr = b.get_field_ptr(
                                    target_union_ptr,
                                    COMMON_IDENTIFIERS.value,
                                );
                                let union_value_ptr_bitcast = b.emit_bitcast(
                                    union_value_ptr,
                                    Type::Pointer(Box::new(source_type.clone())),
                                );
                                b.emit_store(
                                    union_value_ptr_bitcast,
                                    source,
                                    source_span,
                                );
                            },
                        )));
                    }
                }
                // Union -> Union (Widening/Reordering)
                Some(s_variants) => {
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

                    if possible && !remap.is_empty() {
                        return Ok(Adjustment::Write(Box::new(
                            move |b, target_union_ptr| {
                                assert!(check_structural_compatibility(
                                    b.get_value_type(&target_union_ptr),
                                    &target
                                ));

                                let source_id_ptr =
                                    b.get_field_ptr(source, COMMON_IDENTIFIERS.id);
                                let source_id_val = b.emit_load(source_id_ptr);

                                let mut sorted_remap: Vec<_> =
                                    remap.into_iter().collect();
                                sorted_remap.sort_by_key(|(src, _)| *src);

                                let mut calculated_id =
                                    b.emit_const_number(NumberKind::U16(0));

                                for (source_index, target_index) in sorted_remap {
                                    let source_value = b
                                        .emit_const_number(NumberKind::U16(source_index));
                                    let target_value = b
                                        .emit_const_number(NumberKind::U16(target_index));

                                    let is_match =
                                        b.emit_ieq(source_id_val, source_value);
                                    calculated_id = b.emit_select(
                                        is_match,
                                        target_value,
                                        calculated_id,
                                    );
                                }

                                let target_union_id_ptr = b.get_field_ptr(
                                    target_union_ptr,
                                    COMMON_IDENTIFIERS.id,
                                );
                                b.emit_store(
                                    target_union_id_ptr,
                                    calculated_id,
                                    source_span.clone(),
                                );

                                let source_union_value_ptr =
                                    b.get_field_ptr(source, COMMON_IDENTIFIERS.value);
                                let target_union_value_ptr = b.get_field_ptr(
                                    target_union_ptr,
                                    COMMON_IDENTIFIERS.value,
                                );

                                b.emit_memcopy(
                                    source_union_value_ptr,
                                    target_union_value_ptr,
                                );
                            },
                        )));
                    }
                }
            }
        }

        Err(make_error())
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
        (
            Buffer {
                size: buffer_size,
                alignment: buffer_alignment,
            },
            target,
        ) => {
            let target_layout = get_layout_of(target);

            target_layout.size == *buffer_size
                && target_layout.alignment == *buffer_alignment
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
