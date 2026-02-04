use crate::{
    ast::{
        expr::{Expr, ExprKind},
        type_annotation::TypeAnnotation,
        Span,
    },
    compile::interner::StringId,
    globals::COMMON_IDENTIFIERS,
    hir::{
        builders::{Builder, InBlock, TypePredicate, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_type::{StructKind, Type},
        utils::{
            check_type::{check_type_annotation, TypeCheckerContext},
            union::{get_matching_variant_indices, get_non_matching_variant_indices},
        },
    },
    tokenize::NumberKind,
};

impl<'a> Builder<'a, InBlock> {
    fn emit_is_type_check(
        &mut self,
        union_ptr: ValueId,
        matching_indices: &[u16],
        total_variants: usize,
        span: Span,
    ) -> Result<ValueId, SemanticError> {
        if matching_indices.is_empty() {
            return Ok(self.emit_const_bool(false));
        }

        if matching_indices.len() == total_variants {
            return Ok(self.emit_const_bool(true));
        }

        let id_ptr = self.get_field_ptr(union_ptr, COMMON_IDENTIFIERS.id);
        let active_id = self.emit_load(id_ptr);

        let mut iter = matching_indices.iter();
        let &first_idx = iter.next().unwrap();

        let idx_val = self.emit_const_number(NumberKind::U16(first_idx));
        let mut result_id = self.emit_ieq(active_id, idx_val);

        for &match_idx in iter {
            result_id = self.emit_logical_or(result_id, span.clone(), |builder| {
                let idx_val = builder.emit_const_number(NumberKind::U16(match_idx));
                Ok(builder.emit_ieq(active_id, idx_val))
            })?;
        }

        Ok(result_id)
    }

    fn unwind_access_path(
        &self,
        expr: Expr,
    ) -> Result<(Expr, Vec<StringId>), SemanticError> {
        let mut path = Vec::new();
        let mut current = expr;

        loop {
            match current.kind {
                ExprKind::Access { left, field } => {
                    path.insert(0, field.name);
                    current = *left;
                }
                _ => return Ok((current, path)),
            }
        }
    }

    pub fn build_is_type_expr(
        &mut self,
        left: Expr,
        ty: TypeAnnotation,
    ) -> Result<ValueId, SemanticError> {
        let span = left.span.clone();
        let place = self.resolve_place(left)?;
        let place_path = place.path();
        let place_type = self.type_of_place(&place);

        let make_err = || SemanticError {
            span: span.clone(),
            kind: SemanticErrorKind::CannotNarrowNonUnion(place_type.clone()),
        };

        let variants = match &place_type {
            Type::Pointer(to) => {
                if let Type::Struct(StructKind::Union(variants)) = &**to {
                    variants
                } else {
                    return Err(make_err());
                }
            }
            _ => return Err(make_err()),
        };

        let mut type_ctx = TypeCheckerContext {
            scope: self.current_scope.clone(),
            declarations: &self.program.declarations,
            errors: self.errors,
        };
        let target_type = check_type_annotation(&mut type_ctx, &ty);

        if let Type::Pointer(to) = &target_type {
            if let Type::Struct(StructKind::Union(_)) = &**to {
                return Err(SemanticError {
                    kind: SemanticErrorKind::UnsupportedUnionNarrowing,
                    span: ty.span.clone(),
                });
            }
        }

        let match_indices =
            get_matching_variant_indices(variants, &place_path, &target_type);
        let non_match_indices =
            get_non_matching_variant_indices(variants, &place_path, &target_type);

        let union_ptr = self.read_place(&place, span.clone())?;
        let result_id =
            self.emit_is_type_check(union_ptr, &match_indices, variants.len(), span)?;

        let true_type =
            if !match_indices.is_empty() && match_indices.len() < variants.len() {
                Some(Type::make_union(filter_variants(variants, &match_indices)))
            } else {
                None
            };

        let false_type = if !non_match_indices.is_empty()
            && non_match_indices.len() < variants.len()
        {
            Some(Type::make_union(filter_variants(
                variants,
                &non_match_indices,
            )))
        } else {
            None
        };

        if true_type.is_some() || false_type.is_some() {
            self.type_predicates.insert(
                result_id,
                TypePredicate {
                    place,
                    on_true_type: true_type,
                    on_false_type: false_type,
                },
            );
        }

        Ok(result_id)
    }
}

fn filter_variants(
    variants: &std::collections::BTreeSet<Type>,
    indices: &[u16],
) -> Vec<Type> {
    let mut result = Vec::with_capacity(indices.len());
    let mut indices_iter = indices.iter().peekable();

    for (i, variant) in variants.iter().enumerate() {
        if let Some(&&next_idx) = indices_iter.peek() {
            if next_idx == i as u16 {
                result.push(variant.clone());
                indices_iter.next();
            }
        }
    }

    result
}
