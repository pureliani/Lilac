use crate::{
    ast::{expr::Expr, IdentifierNode, Span},
    globals::{COMMON_IDENTIFIERS, STRING_INTERNER, TAG_INTERNER},
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::{
            checked_declaration::TagType,
            checked_type::{StructKind, Type},
        },
        utils::check_is_assignable::check_is_assignable,
    },
    tokenize::NumberKind,
    unwrap_or_poison,
};
use std::collections::HashMap;

impl<'a> Builder<'a, InBlock> {
    pub fn build_index_expr(
        &mut self,
        left_expr: Box<Expr>,
        index_expr: Box<Expr>,
    ) -> ValueId {
        let left_span = left_expr.span.clone();
        let index_span = index_expr.span.clone();

        let list_val = self.build_expr(*left_expr);
        let list_type = self.get_value_type(&list_val).clone();

        let index_val = self.build_expr(*index_expr);
        let index_type = self.get_value_type(&index_val).clone();

        if !check_is_assignable(&index_type, &Type::USize) {
            return self.report_error_and_get_poison(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: Type::USize,
                    received: index_type,
                },
                span: index_span,
            });
        }

        let element_type = match list_type {
            Type::Pointer { narrowed_to, .. } => match &*narrowed_to {
                Type::Struct(StructKind::List(inner)) => *inner.clone(),
                _ => {
                    return self.report_error_and_get_poison(SemanticError {
                        kind: SemanticErrorKind::CannotIndex(*narrowed_to.clone()),
                        span: left_span,
                    })
                }
            },
            _ => {
                return self.report_error_and_get_poison(SemanticError {
                    kind: SemanticErrorKind::CannotIndex(list_type),
                    span: left_span,
                })
            }
        };

        let none_tag_id = TAG_INTERNER.intern(&STRING_INTERNER.intern("none"));
        let some_tag_id = TAG_INTERNER.intern(&STRING_INTERNER.intern("some"));

        let none_variant = TagType {
            id: none_tag_id,
            value_type: None,
            span: left_span.clone(),
        };
        let some_variant = TagType {
            id: some_tag_id,
            value_type: Some(Box::new(element_type.clone())),
            span: left_span.clone(),
        };

        let mut variants = vec![none_variant.clone(), some_variant.clone()];
        variants.sort_by(|a, b| a.id.0.cmp(&b.id.0));
        let result_union_type = Type::Struct(StructKind::Union { variants });

        let len_field = IdentifierNode {
            name: COMMON_IDENTIFIERS.len,
            span: left_span.clone(),
        };
        let len_ptr = unwrap_or_poison!(self, self.get_field_ptr(list_val, &len_field));
        let len_val = unwrap_or_poison!(self, self.load(len_ptr, left_span.clone()));

        let is_in_bounds = unwrap_or_poison!(
            self,
            self.lt(index_val, index_span.clone(), len_val, left_span.clone())
        );

        let success_bb = self.as_fn().new_bb();
        let fail_bb = self.as_fn().new_bb();
        let merge_bb = self.as_fn().new_bb();

        let result_param =
            self.append_param_to_block(merge_bb, result_union_type.clone());

        self.cond_jmp(
            is_in_bounds,
            success_bb,
            HashMap::new(),
            fail_bb,
            HashMap::new(),
        );

        self.seal_block(success_bb);
        self.use_basic_block(success_bb);

        let ptr_field = IdentifierNode {
            name: COMMON_IDENTIFIERS.ptr,
            span: left_span.clone(),
        };
        let internal_ptr_ptr =
            unwrap_or_poison!(self, self.get_field_ptr(list_val, &ptr_field));
        let buffer_ptr =
            unwrap_or_poison!(self, self.load(internal_ptr_ptr, left_span.clone()));

        let elem_ptr = unwrap_or_poison!(
            self,
            self.ptr_offset(buffer_ptr, index_val, index_span.clone())
        );
        let elem_val = unwrap_or_poison!(self, self.load(elem_ptr, index_span.clone()));

        let some_val = self.build_tag_from_parts(
            some_variant,
            Some(elem_val),
            &result_union_type,
            &left_span,
        );
        self.jmp(merge_bb, HashMap::from([(result_param, some_val)]));

        self.seal_block(fail_bb);
        self.use_basic_block(fail_bb);

        let none_val =
            self.build_tag_from_parts(none_variant, None, &result_union_type, &left_span);
        self.jmp(merge_bb, HashMap::from([(result_param, none_val)]));

        self.seal_block(merge_bb);
        self.use_basic_block(merge_bb);

        result_param
    }

    fn build_tag_from_parts(
        &mut self,
        tag_ty: TagType,
        val: Option<ValueId>,
        union_ty: &Type,
        span: &Span,
    ) -> ValueId {
        let struct_ty = Type::Struct(StructKind::Tag(tag_ty.clone()));
        let ptr = self.emit_stack_alloc(struct_ty, 1);

        let id_field = IdentifierNode {
            name: COMMON_IDENTIFIERS.id,
            span: span.clone(),
        };
        let id_ptr = unwrap_or_poison!(self, self.get_field_ptr(ptr, &id_field));
        let id_const = self.emit_const_number(NumberKind::U16(tag_ty.id.0));
        self.store(id_ptr, id_const, span.clone());

        if let Some(v) = val {
            let val_field = IdentifierNode {
                name: COMMON_IDENTIFIERS.value,
                span: span.clone(),
            };
            let val_ptr = unwrap_or_poison!(self, self.get_field_ptr(ptr, &val_field));
            self.store(val_ptr, v, span.clone());
        }

        let loaded = unwrap_or_poison!(self, self.load(ptr, span.clone()));

        self.emit_refine_type(loaded, union_ty.clone())
    }
}
