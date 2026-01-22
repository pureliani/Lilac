use crate::{
    ast::{expr::Expr, IdentifierNode},
    globals::{COMMON_IDENTIFIERS, STRING_INTERNER, TAG_INTERNER},
    hir::{
        builders::{Builder, InBlock, Phi, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::{
            checked_declaration::TagType,
            checked_type::{StructKind, Type},
        },
        utils::check_is_assignable::check_is_assignable,
    },
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
                span: index_span,
            });
        }

        let element_type = match list_type {
            Type::Pointer { narrowed_to, .. } => match &*narrowed_to {
                Type::Struct(StructKind::ListHeader(inner)) => *inner.clone(),
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

        let none_tag_type = Type::Tag(TagType {
            id: none_tag_id,
            value_type: None,
            span: left_span.clone(),
        });
        let some_tag_type = Type::Tag(TagType {
            id: some_tag_id,
            value_type: Some(Box::new(element_type.clone())),
            span: left_span.clone(),
        });

        let mut variants = match (&none_tag_type, &some_tag_type) {
            (Type::Tag(t1), Type::Tag(t2)) => vec![t1.clone(), t2.clone()],
            _ => unreachable!(),
        };
        variants.sort_by(|a, b| a.id.0.cmp(&b.id.0));
        let result_union_type = Type::Union(variants);

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

        self.cond_jmp(is_in_bounds, success_bb, fail_bb);

        self.seal_block(success_bb);
        self.use_basic_block(success_bb);

        let buffer_ptr = self.get_list_buffer_ptr(list_val, left_span.clone());
        let elem_ptr = unwrap_or_poison!(
            self,
            self.ptr_offset(buffer_ptr, index_val, index_span.clone())
        );
        let elem_val = unwrap_or_poison!(self, self.load(elem_ptr, index_span.clone()));

        let some_val =
            self.emit_assemble_tag(some_tag_type, Some(elem_val), left_span.clone());
        let success_final_bb = self.context.block_id;
        self.jmp(merge_bb);

        self.seal_block(fail_bb);
        self.use_basic_block(fail_bb);

        let none_val = self.emit_assemble_tag(none_tag_type, None, left_span.clone());
        let fail_final_bb = self.context.block_id;
        self.jmp(merge_bb);

        self.seal_block(merge_bb);
        self.use_basic_block(merge_bb);

        let result_id = self.new_value_id(result_union_type);
        let phi_operands = vec![
            Phi {
                from: success_final_bb,
                value: some_val,
            },
            Phi {
                from: fail_final_bb,
                value: none_val,
            },
        ];

        self.bb_mut().phis.push((result_id, phi_operands));

        result_id
    }
}
