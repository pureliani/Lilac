use crate::{
    ast::{
        expr::{Expr, ExprKind},
        type_annotation::TypeAnnotation,
        Span,
    },
    hir::{
        builders::{Builder, InBlock, TypePredicate, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_type::Type,
        utils::{
            adjustments::check_structural_compatibility,
            check_type::{check_type_annotation, TypeCheckerContext},
        },
    },
};

impl<'a> Builder<'a, InBlock> {
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
            let variant_clone = variant.clone();
            result_id = self.emit_logical_or(result_id, span.clone(), |builder| {
                builder.emit_test_variant(union, &variant_clone)
            });
        }

        result_id
    }

    pub fn build_is_type_expr(&mut self, left: Expr, ty: TypeAnnotation) -> ValueId {
        let span = left.span.clone();

        let current_val = self.build_expr(left.clone());
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

        let mut matching_variants = Vec::new();
        let mut non_matching_variants = Vec::new();

        for variant in variants {
            if check_structural_compatibility(variant, &target_type) {
                matching_variants.push(variant.clone());
            } else {
                non_matching_variants.push(variant.clone());
            }
        }

        let result_id = self.emit_is_type_check(
            current_val,
            &matching_variants,
            variants.len(),
            span.clone(),
        );

        let true_type = if !matching_variants.is_empty()
            && matching_variants.len() < variants.len()
        {
            Some(Type::make_union(matching_variants))
        } else {
            None
        };

        let false_type = if !non_matching_variants.is_empty()
            && non_matching_variants.len() < variants.len()
        {
            Some(Type::make_union(non_matching_variants))
        } else {
            None
        };

        let is_narrowable =
            matches!(left.kind, ExprKind::Identifier(_) | ExprKind::Access { .. });

        if is_narrowable && (true_type.is_some() || false_type.is_some()) {
            self.type_predicates.insert(
                result_id,
                TypePredicate {
                    target: left,
                    on_true_type: true_type,
                    on_false_type: false_type,
                },
            );
        }

        result_id
    }
}
