use std::collections::HashMap;

use crate::{
    ast::{expr::Expr, IdentifierNode},
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
                span: index_span.clone(),
            });
        }

        let element_type = match list_type {
            Type::Pointer { narrowed_to, .. } => match *narrowed_to {
                Type::Struct(StructKind::List(inner)) => *inner,
                _ => {
                    return self.report_error_and_get_poison(SemanticError {
                        kind: SemanticErrorKind::CannotIndex(*narrowed_to),
                        span: left_span.clone(),
                    });
                }
            },
            _ => {
                return self.report_error_and_get_poison(SemanticError {
                    kind: SemanticErrorKind::CannotIndex(list_type),
                    span: left_span.clone(),
                });
            }
        };

        let none_str_id = STRING_INTERNER.intern("none");
        let some_str_id = STRING_INTERNER.intern("some");
        let none_tag_id = TAG_INTERNER.intern(&none_str_id);
        let some_tag_id = TAG_INTERNER.intern(&some_str_id);

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

        let len_field_id = IdentifierNode {
            name: COMMON_IDENTIFIERS.len,
            span: left_span.clone(),
        };

        let len_ptr =
            unwrap_or_poison!(self, self.get_field_ptr(list_val, &len_field_id));
        let len_val = unwrap_or_poison!(self, self.load(len_ptr, left_span.clone()));

        let condition_val = unwrap_or_poison!(
            self,
            self.lt(index_val, index_span.clone(), len_val, left_span.clone(),)
        );

        let success_block = self.as_fn().new_bb();
        let fail_block = self.as_fn().new_bb();
        let merge_block = self.as_fn().new_bb();

        let result_param =
            self.append_param_to_block(merge_block, result_union_type.clone());

        self.cond_jmp(
            condition_val,
            success_block,
            HashMap::new(),
            fail_block,
            HashMap::new(),
        );

        self.seal_block(success_block);
        self.use_basic_block(success_block);

        let ptr_field_id = IdentifierNode {
            name: COMMON_IDENTIFIERS.ptr,
            span: left_span.clone(),
        };
        let internal_ptr_ptr =
            unwrap_or_poison!(self, self.get_field_ptr(list_val, &ptr_field_id));
        let buffer_ptr =
            unwrap_or_poison!(self, self.load(internal_ptr_ptr, left_span.clone()));

        let element_ptr = unwrap_or_poison!(
            self,
            self.get_element_ptr(buffer_ptr, index_val, index_span.clone())
        );
        let element_val =
            unwrap_or_poison!(self, self.load(element_ptr, index_span.clone()));

        let some_struct_type = Type::Struct(StructKind::Tag(some_variant));
        let some_ptr = self.emit_stack_alloc(some_struct_type.clone(), 1);

        let id_field = IdentifierNode {
            name: COMMON_IDENTIFIERS.id,
            span: left_span.clone(),
        };
        let some_id_ptr =
            unwrap_or_poison!(self, self.get_field_ptr(some_ptr, &id_field));
        let tag_id_val = self.emit_const_int(NumberKind::U16(some_tag_id.0));
        self.store(some_id_ptr, tag_id_val, left_span.clone());

        let val_field = IdentifierNode {
            name: COMMON_IDENTIFIERS.value,
            span: left_span.clone(),
        };
        let some_val_ptr =
            unwrap_or_poison!(self, self.get_field_ptr(some_ptr, &val_field));
        self.store(some_val_ptr, element_val, left_span.clone());

        let some_val = unwrap_or_poison!(self, self.load(some_ptr, left_span.clone()));
        let cast_some = unwrap_or_poison!(
            self,
            self.refine(some_val, result_union_type.clone(), left_span.clone())
        );

        self.jmp(merge_block, HashMap::from([(result_param, cast_some)]));

        self.seal_block(fail_block);
        self.use_basic_block(fail_block);

        let none_struct_type = Type::Struct(StructKind::Tag(none_variant));
        let none_ptr = self.emit_stack_alloc(none_struct_type.clone(), 1);

        let none_id_ptr =
            unwrap_or_poison!(self, self.get_field_ptr(none_ptr, &id_field));
        let none_tag_id_val = self.emit_const_int(NumberKind::U16(none_tag_id.0));
        self.store(none_id_ptr, none_tag_id_val, left_span.clone());

        let none_val = unwrap_or_poison!(self, self.load(none_ptr, left_span.clone()));
        let cast_none = unwrap_or_poison!(
            self,
            self.refine(none_val, result_union_type.clone(), left_span.clone())
        );

        self.jmp(merge_block, HashMap::from([(result_param, cast_none)]));

        self.seal_block(merge_block);
        self.use_basic_block(merge_block);

        result_param
    }
}
