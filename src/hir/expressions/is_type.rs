use crate::{
    ast::{expr::Expr, type_annotation::TypeAnnotation, Span},
    hir::{
        builders::{Builder, InBlock, TypePredicate, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_type::Type,
        utils::{
            check_type::{check_type_annotation, TypeCheckerContext},
            union::{get_matching_variant_indices, get_non_matching_variant_indices},
        },
    },
};

impl<'a> Builder<'a, InBlock> {
    /// Emits a runtime check that tests the union discriminant against
    /// one or more matching variant types. Returns a bool ValueId.
    fn emit_is_type_check(
        &mut self,
        union: ValueId,
        matching_variants: &[Type],
        total_variants: usize,
        span: Span,
    ) -> ValueId {
        if matching_variants.is_empty() {
            return self.emit_const_bool(false);
        }

        if matching_variants.len() == total_variants {
            return self.emit_const_bool(true);
        }

        let mut iter = matching_variants.iter();
        let first_variant = iter.next().unwrap();

        let mut result_id = self.emit_test_variant(union, first_variant);

        for variant in iter {
            result_id = self.emit_logical_or(result_id, span.clone(), |builder| {
                builder.emit_test_variant(union, variant)
            });
        }

        result_id
    }

    pub fn build_is_type_expr(&mut self, left: Expr, ty: TypeAnnotation) -> ValueId {
        let span = left.span.clone();
        let place = match self.resolve_place(left) {
            Ok(p) => p,
            Err(e) => return self.report_error_and_get_poison(e),
        };
        let place_path = place.path();

        let current_val = self.read_place(&place, span.clone());
        let current_ty = self.get_value_type(current_val).clone();

        let variants = match current_ty.as_union_variants() {
            Some(v) => v,
            None => {
                return self.report_error_and_get_poison(SemanticError {
                    span: span.clone(),
                    kind: SemanticErrorKind::CannotNarrowNonUnion(current_ty.clone()),
                });
            }
        };

        let mut type_ctx = TypeCheckerContext {
            scope: self.current_scope.clone(),
            declarations: &self.program.declarations,
            errors: self.errors,
        };
        let target_type = check_type_annotation(&mut type_ctx, &ty);

        if target_type.as_union_variants().is_some() {
            return self.report_error_and_get_poison(SemanticError {
                kind: SemanticErrorKind::UnsupportedUnionNarrowing,
                span: ty.span.clone(),
            });
        }

        let match_indices =
            get_matching_variant_indices(variants, &place_path, &target_type);
        let non_match_indices =
            get_non_matching_variant_indices(variants, &place_path, &target_type);

        let matching_variants = filter_variants(variants, &match_indices);

        let result_id = self.emit_is_type_check(
            current_val,
            &matching_variants,
            variants.len(),
            span.clone(),
        );

        let true_type =
            if !match_indices.is_empty() && match_indices.len() < variants.len() {
                Some(Type::make_union(matching_variants))
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

        result_id
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
