use std::collections::HashMap;

use crate::{
    ast::{expr::Expr, type_annotation::TagAnnotation, IdentifierNode},
    globals::{COMMON_IDENTIFIERS, TAG_INTERNER},
    hir::{
        builders::{Builder, InBlock, TypePredicate, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_type::{StructKind, Type},
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

        let underlying_ty = match &source_ty {
            Type::Pointer { narrowed_to, .. } => narrowed_to.as_ref(),
            other => other,
        };

        if !matches!(
            underlying_ty,
            Type::Struct(StructKind::Union { .. }) | Type::Unknown
        ) {
            return self.report_error_and_get_poison(SemanticError {
                kind: SemanticErrorKind::CannotNarrowNonUnion(source_ty),
                span: left_span,
            });
        }

        let union_ptr = match self.build_place(*left) {
            Ok(place) => place.root,
            Err(_) => {
                let p = self.emit_stack_alloc(source_ty.clone(), 1);
                self.store(p, source_val, left_span.clone());
                p
            }
        };

        let id_field = IdentifierNode {
            name: COMMON_IDENTIFIERS.id,
            span: left_span.clone(),
        };
        let id_ptr = unwrap_or_poison!(self, self.get_field_ptr(union_ptr, &id_field));
        let actual_id_val = unwrap_or_poison!(self, self.load(id_ptr, left_span.clone()));

        let true_path = self.as_fn().new_bb();
        let false_path = self.as_fn().new_bb();
        let merge_block = self.as_fn().new_bb();

        let mut target_tag_ids = Vec::new();
        let result_bool_param = self.append_param_to_block(merge_block, Type::Bool);

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

            self.cond_jmp(
                is_match,
                true_path,
                HashMap::new(),
                next_check_block,
                HashMap::new(),
            );

            if !is_last {
                self.seal_block(next_check_block);
                self.use_basic_block(next_check_block);
            }
        }

        let true_ty = intersect_types(&source_ty, &target_tag_ids);
        let false_ty = subtract_types(&source_ty, &target_tag_ids);

        self.seal_block(true_path);
        self.use_basic_block(true_path);
        let true_id = self.emit_refine_type(source_val, true_ty);
        self.map_value(source_val, true_id);
        let const_true = self.emit_const_bool(true);
        self.jmp(
            merge_block,
            HashMap::from([(result_bool_param, const_true)]),
        );

        self.seal_block(false_path);
        self.use_basic_block(false_path);
        let false_id = self.emit_refine_type(source_val, false_ty);
        self.map_value(source_val, false_id);
        let const_false = self.emit_const_bool(false);
        self.jmp(
            merge_block,
            HashMap::from([(result_bool_param, const_false)]),
        );

        self.seal_block(merge_block);
        self.use_basic_block(merge_block);

        self.get_fn().predicates.insert(
            result_bool_param,
            TypePredicate {
                source: source_val,
                true_id,
                false_id,
            },
        );

        result_bool_param
    }
}
