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

#[derive(Debug, Clone, PartialEq)]
pub enum ValueAdjustment {
    Compatible,
    IntExtend { target_type: Type, is_signed: bool },
    IntTruncate { target_type: Type },
    FloatExtend { target_type: Type },
    FloatTruncate { target_type: Type },
    IntToFloat { target_type: Type, is_signed: bool },
    FloatToInt { target_type: Type, is_signed: bool },
}

pub fn analyze_value_adjustment(
    source: &Type,
    source_span: Span,
    target: &Type,
) -> Result<ValueAdjustment, SemanticError> {
    let casting_err = Err(SemanticError {
        kind: SemanticErrorKind::CannotCastType {
            source_type: source.clone(),
            target_type: target.clone(),
        },
        span: source_span,
    });

    if check_structural_compatibility(source, target) {
        return Ok(ValueAdjustment::Compatible);
    }

    if is_integer(source) && is_integer(target) {
        let s_rank = get_numeric_type_rank(source);
        let t_rank = get_numeric_type_rank(target);

        if t_rank > s_rank {
            return Ok(ValueAdjustment::IntExtend {
                target_type: target.clone(),
                is_signed: is_signed(source),
            });
        } else if t_rank < s_rank {
            return Ok(ValueAdjustment::IntTruncate {
                target_type: target.clone(),
            });
        }

        return casting_err;
    }

    if is_float(source) && is_float(target) {
        let s_rank = get_numeric_type_rank(source);
        let t_rank = get_numeric_type_rank(target);

        if t_rank > s_rank {
            return Ok(ValueAdjustment::FloatExtend {
                target_type: target.clone(),
            });
        } else if t_rank < s_rank {
            return Ok(ValueAdjustment::FloatTruncate {
                target_type: target.clone(),
            });
        }
        return casting_err;
    }

    if is_integer(source) && is_float(target) {
        return Ok(ValueAdjustment::IntToFloat {
            target_type: target.clone(),
            is_signed: is_signed(source),
        });
    }

    if is_float(source) && is_integer(target) {
        return Ok(ValueAdjustment::FloatToInt {
            target_type: target.clone(),
            is_signed: is_signed(target),
        });
    }

    casting_err
}

#[derive(Debug, Clone, PartialEq)]
pub enum MemoryWriteAdjustment {
    DirectStore,
    AssignVariantToUnion {
        discriminator: u16,
    },
    AssignUnionToUnion {
        remap_discriminators: HashMap<u16, u16>,
    },
}

/// target _must_ be a pointer
pub fn analyze_memory_adjustment(
    source: &Type,
    source_span: Span,
    target: &Type,
) -> Result<MemoryWriteAdjustment, SemanticError> {
    let casting_err = Err(SemanticError {
        kind: SemanticErrorKind::CannotCastType {
            source_type: source.clone(),
            target_type: target.clone(),
        },
        span: source_span,
    });

    let target_inner = if let Type::Pointer(target_inner) = target {
        if check_structural_compatibility(source, target_inner) {
            return Ok(MemoryWriteAdjustment::DirectStore);
        }

        &**target_inner
    } else {
        panic!(
            "INTERNAL COMPILER ERROR: analyze_memory_adjustment expected the target to \
             be a pointer"
        );
    };

    if let Type::Struct(StructKind::Union(t_variants)) = target_inner {
        let s_variants = if let Type::Pointer(inner) = source {
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
                if let Some(index) = t_variants
                    .iter()
                    .position(|v| check_structural_compatibility(source, v))
                {
                    return Ok(MemoryWriteAdjustment::AssignVariantToUnion {
                        discriminator: index as u16,
                    });
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

                if possible {
                    return Ok(MemoryWriteAdjustment::AssignUnionToUnion {
                        remap_discriminators: remap,
                    });
                }
            }
        }
    }

    casting_err
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

impl<'a> Builder<'a, InBlock> {
    pub fn apply_value_adjustment(
        &mut self,
        source: ValueId,
        adjustment: ValueAdjustment,
    ) -> ValueId {
        match adjustment {
            ValueAdjustment::Compatible => source,
            ValueAdjustment::IntTruncate { target_type } => {
                self.emit_trunc(source, target_type)
            }
            ValueAdjustment::FloatExtend { target_type } => {
                self.emit_fext(source, target_type)
            }
            ValueAdjustment::FloatTruncate { target_type } => {
                self.emit_ftrunc(source, target_type)
            }
            ValueAdjustment::IntExtend {
                is_signed,
                target_type,
            } => {
                if is_signed {
                    self.emit_sext(source, target_type)
                } else {
                    self.emit_zext(source, target_type)
                }
            }
            ValueAdjustment::IntToFloat {
                is_signed,
                target_type,
            } => {
                if is_signed {
                    self.emit_sitof(source, target_type)
                } else {
                    self.emit_uitof(source, target_type)
                }
            }
            ValueAdjustment::FloatToInt {
                is_signed,
                target_type,
            } => {
                if is_signed {
                    self.emit_ftosi(source, target_type)
                } else {
                    self.emit_ftoui(source, target_type)
                }
            }
        }
    }

    pub fn adjust_value(
        &mut self,
        source: ValueId,
        source_span: Span,
        target_type: &Type,
    ) -> Result<ValueId, SemanticError> {
        let source_type = self.get_value_type(&source);
        let adjustment = analyze_value_adjustment(source_type, source_span, target_type)?;
        Ok(self.apply_value_adjustment(source, adjustment))
    }

    /// This function expects the memory slot to be allocated beforehand
    pub fn adjust_memory_and_write(
        &mut self,
        source: ValueId,
        source_span: Span,
        target_ptr: ValueId,
        adjustment: MemoryWriteAdjustment,
    ) -> Result<(), SemanticError> {
        let source_type = self.get_value_type(&source).clone();

        match adjustment {
            MemoryWriteAdjustment::DirectStore => {
                self.emit_store(target_ptr, source, source_span);
            }
            MemoryWriteAdjustment::AssignVariantToUnion { discriminator } => {
                let id_ptr = self.get_field_ptr(target_ptr, COMMON_IDENTIFIERS.id);
                let discriminator_value =
                    self.emit_const_number(NumberKind::U16(discriminator));
                self.emit_store(id_ptr, discriminator_value, source_span.clone());

                let value_ptr = self.get_field_ptr(target_ptr, COMMON_IDENTIFIERS.value);
                let value_ptr_bitcast = self.emit_bitcast(
                    value_ptr,
                    Type::Pointer(Box::new(source_type.clone())),
                );
                self.emit_store(value_ptr_bitcast, source, source_span);
            }
            MemoryWriteAdjustment::AssignUnionToUnion {
                remap_discriminators: remap,
            } => {
                let source_id_ptr = self.get_field_ptr(source, COMMON_IDENTIFIERS.id);
                let source_id_val = self.emit_load(source_id_ptr);

                let mut sorted_remap: Vec<_> = remap.into_iter().collect();
                sorted_remap.sort_by_key(|(src, _)| *src);

                let mut calculated_target_id = self.emit_const_number(NumberKind::U16(0));

                for (src_idx, tgt_idx) in sorted_remap {
                    let src_idx_val = self.emit_const_number(NumberKind::U16(src_idx));
                    let tgt_idx_val = self.emit_const_number(NumberKind::U16(tgt_idx));

                    let is_match = self.emit_ieq(source_id_val, src_idx_val);
                    calculated_target_id =
                        self.emit_select(is_match, tgt_idx_val, calculated_target_id);
                }

                let target_id_ptr = self.get_field_ptr(target_ptr, COMMON_IDENTIFIERS.id);
                self.emit_store(target_id_ptr, calculated_target_id, source_span.clone());

                let source_value_ptr =
                    self.get_field_ptr(source, COMMON_IDENTIFIERS.value);
                let target_value_ptr =
                    self.get_field_ptr(target_ptr, COMMON_IDENTIFIERS.value);

                self.emit_memcopy(source_value_ptr, target_value_ptr);
            }
        }

        Ok(())
    }

    pub fn adjust_initial_value(
        &mut self,
        value: ValueId,
        value_span: Span,
        constraint: &Type,
    ) -> Result<ValueId, SemanticError> {
        let value_type = self.get_value_type(&value).clone();

        let value_adjustment =
            analyze_value_adjustment(&value_type, value_span.clone(), constraint);

        let final_val_id = if let Ok(adj) = value_adjustment {
            self.apply_value_adjustment(value, adj)
        } else if let Type::Pointer(constraint_ptr_inner) = constraint {
            let usize_one = self.emit_const_number(NumberKind::USize(1));
            let heap_ptr = self.emit_heap_alloc(*constraint_ptr_inner.clone(), usize_one);
            let heap_ptr_type = self.get_value_type(&heap_ptr);

            let memory_adjustment = analyze_memory_adjustment(
                &value_type,
                value_span.clone(),
                heap_ptr_type,
            )?;

            self.adjust_memory_and_write(
                value,
                value_span.clone(),
                heap_ptr,
                memory_adjustment,
            )?;

            heap_ptr
        } else {
            return Err(SemanticError {
                span: value_span.clone(),
                kind: SemanticErrorKind::TypeMismatch {
                    expected: constraint.clone(),
                    received: value_type.clone(),
                },
            });
        };

        Ok(final_val_id)
    }
}
