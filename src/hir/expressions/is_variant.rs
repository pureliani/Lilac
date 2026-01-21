use crate::{
    ast::{expr::Expr, type_annotation::TagAnnotation},
    globals::TAG_INTERNER,
    hir::{
        builders::{Builder, InBlock, Phi, TypePredicate, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_type::Type,
        utils::try_unify_types::{intersect_types, subtract_types},
    },
    tokenize::NumberKind,
    unwrap_or_poison,
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_is_variant_expr(
        &mut self,
        left: Box<Expr>,
        variants: Vec<TagAnnotation>,
    ) -> ValueId {
        let left_span = left.span.clone();

        let source_val = self.build_expr(*left.clone());
        let source_ty = self.get_value_type(&source_val).clone();

        if !matches!(source_ty, Type::Union(_) | Type::Unknown) {
            return self.report_error_and_get_poison(SemanticError {
                kind: SemanticErrorKind::CannotNarrowNonUnion(source_ty),
                span: left_span,
            });
        }

        let actual_id_val = self.emit_get_tag_id(source_val);

        let true_path = self.as_fn().new_bb();
        let false_path = self.as_fn().new_bb();
        let merge_block = self.as_fn().new_bb();

        let mut target_tag_ids = Vec::new();

        let identity_id = if let Ok(place) = self.build_place(*left) {
            Some(place.root)
        } else {
            None
        };

        for (i, tag_ann) in variants.iter().enumerate() {
            if tag_ann.value_type.is_some() {
                self.errors.push(SemanticError {
                    kind: SemanticErrorKind::ValuedTagInIsExpression,
                    span: tag_ann.span.clone(),
                });
            }

            let tag_id = TAG_INTERNER.intern(&tag_ann.identifier.name);
            target_tag_ids.push(tag_id);

            let tag_id_const = self.emit_const_number(NumberKind::U16(tag_id.0));
            let is_match = unwrap_or_poison!(
                self,
                self.eq(
                    actual_id_val,
                    left_span.clone(),
                    tag_id_const,
                    tag_ann.span.clone()
                )
            );

            let is_last = i == variants.len() - 1;
            let next_check_block = if is_last {
                false_path
            } else {
                self.as_fn().new_bb()
            };

            self.cond_jmp(is_match, true_path, next_check_block);

            if !is_last {
                self.seal_block(next_check_block);
                self.use_basic_block(next_check_block);
            }
        }

        let true_ty = intersect_types(&source_ty, &target_tag_ids);
        let false_ty = subtract_types(&source_ty, &target_tag_ids);

        self.seal_block(true_path);
        self.use_basic_block(true_path);
        let true_refined_id = self.emit_refine_type(source_val, true_ty);

        if let Some(id) = identity_id {
            self.definitions
                .entry(true_path)
                .or_default()
                .insert(id, true_refined_id);
        }

        let const_true = self.emit_const_bool(true);
        let true_final_bb = self.context.block_id;
        self.jmp(merge_block);

        self.seal_block(false_path);
        self.use_basic_block(false_path);
        let false_refined_id = self.emit_refine_type(source_val, false_ty);

        if let Some(id) = identity_id {
            self.definitions
                .entry(false_path)
                .or_default()
                .insert(id, false_refined_id);
        }

        let const_false = self.emit_const_bool(false);
        let false_final_bb = self.context.block_id;
        self.jmp(merge_block);

        self.seal_block(merge_block);
        self.use_basic_block(merge_block);

        let result_bool_id = self.new_value_id(Type::Bool);
        let phi_operands = vec![
            Phi {
                from: true_final_bb,
                value: const_true,
            },
            Phi {
                from: false_final_bb,
                value: const_false,
            },
        ];
        self.bb_mut().phis.push((result_bool_id, phi_operands));

        if let Some(id) = identity_id {
            self.get_fn().predicates.insert(
                result_bool_id,
                TypePredicate {
                    source: id,
                    true_id: true_refined_id,
                    false_id: false_refined_id,
                },
            );
        }

        result_bool_id
    }
}
