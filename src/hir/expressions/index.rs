use crate::{
    ast::{expr::Expr, IdentifierNode, Span},
    hir::{
        cfg::{BinaryOperationKind, Terminator, Value},
        errors::{SemanticError, SemanticErrorKind},
        types::{
            checked_declaration::TagType,
            checked_type::{StructKind, Type},
        },
        utils::check_is_assignable::check_is_assignable,
        FunctionBuilder, HIRContext,
    },
    tokenize::NumberKind,
};

impl FunctionBuilder {
    pub fn build_index_expr(
        &mut self,
        ctx: &mut HIRContext,
        left: Box<Expr>,
        index: Box<Expr>,
    ) -> Value {
        let left_span = left.span;
        let index_span = index.span;

        let list_val = self.build_expr(ctx, *left);
        let list_type = ctx.program_builder.get_value_type(&list_val);

        let index_val = self.build_expr(ctx, *index);
        let index_type = ctx.program_builder.get_value_type(&index_val);

        // TODO: maybe allow smaller unsigned integers?
        if !check_is_assignable(&index_type, &Type::USize) {
            return Value::Use(self.report_error_and_get_poison(
                ctx,
                SemanticError {
                    kind: SemanticErrorKind::TypeMismatch {
                        expected: Type::USize,
                        received: index_type,
                    },
                    span: index_span,
                },
            ));
        }

        let element_type = match list_type {
            Type::Struct(StructKind::List(inner)) => *inner,
            _ => {
                return Value::Use(self.report_error_and_get_poison(
                    ctx,
                    SemanticError {
                        kind: SemanticErrorKind::CannotIndex(list_type),
                        span: left_span,
                    },
                ));
            }
        };

        let none_str_id = ctx.program_builder.string_interner.intern("none");
        let some_str_id = ctx.program_builder.string_interner.intern("some");

        let none_tag_id = ctx.program_builder.tag_interner.intern(&none_str_id);
        let some_tag_id = ctx.program_builder.tag_interner.intern(&some_str_id);

        let none_variant = TagType {
            id: none_tag_id,
            value_type: None,
            span: left_span,
        };

        let some_variant = TagType {
            id: some_tag_id,
            value_type: Some(Box::new(element_type.clone())),
            span: left_span,
        };

        let mut variants = vec![none_variant.clone(), some_variant.clone()];
        variants.sort_by(|a, b| a.id.0.cmp(&b.id.0));

        let result_union_type = Type::Struct(StructKind::Union { variants });

        let list_ptr_id = match list_val {
            Value::Use(id) => id,
            _ => panic!("INTERNAL ERROR: List value should be a pointer"),
        };

        let len_field_id = IdentifierNode {
            name: COMMON_IDENTIFIERS.len,
            span: left_span,
        };

        let len_ptr = match self.emit_get_field_ptr(ctx, list_ptr_id, len_field_id) {
            Ok(id) => id,
            Err(e) => return Value::Use(self.report_error_and_get_poison(ctx, e)),
        };

        let len_val = Value::Use(self.emit_load(ctx, len_ptr));

        let condition_val = match self.emit_binary_op(
            ctx,
            BinaryOperationKind::LessThan,
            index_val.clone(),
            index_span,
            len_val,
            left_span,
        ) {
            Ok(id) => Value::Use(id),
            Err(e) => return Value::Use(self.report_error_and_get_poison(ctx, e)),
        };

        let success_block = self.new_basic_block();
        let fail_block = self.new_basic_block();
        let merge_block = self.new_basic_block();

        let result_param =
            self.append_block_param(ctx, merge_block, result_union_type.clone());

        self.set_basic_block_terminator(Terminator::CondJump {
            condition: condition_val,
            true_target: success_block,
            true_args: vec![],
            false_target: fail_block,
            false_args: vec![],
        });

        self.seal_block(ctx, success_block);
        self.use_basic_block(success_block);

        let ptr_field_id = IdentifierNode {
            name: COMMON_IDENTIFIERS.ptr,
            span: left_span,
        };
        let internal_ptr_ptr = self
            .emit_get_field_ptr(ctx, list_ptr_id, ptr_field_id)
            .unwrap();
        let buffer_ptr = self.emit_load(ctx, internal_ptr_ptr);

        let element_ptr = self
            .emit_get_element_ptr(ctx, buffer_ptr, index_val)
            .unwrap();
        let element_val = Value::Use(self.emit_load(ctx, element_ptr));

        let some_struct_type = Type::Struct(StructKind::Tag(some_variant));
        let some_ptr = self.emit_stack_alloc(ctx, some_struct_type.clone(), 1);

        let id_field = IdentifierNode {
            name: COMMON_IDENTIFIERS.id,
            span: left_span,
        };
        let some_id_ptr = self.emit_get_field_ptr(ctx, some_ptr, id_field).unwrap();
        self.emit_store(
            ctx,
            some_id_ptr,
            Value::NumberLiteral(NumberKind::U16(some_tag_id.0)),
            Span::default(),
        );

        let val_field = IdentifierNode {
            name: COMMON_IDENTIFIERS.value,
            span: left_span,
        };
        let some_val_ptr = self.emit_get_field_ptr(ctx, some_ptr, val_field).unwrap();
        self.emit_store(ctx, some_val_ptr, element_val, Span::default());

        let some_val = Value::Use(self.emit_load(ctx, some_ptr));
        let cast_some =
            self.emit_type_cast(ctx, some_val, left_span, result_union_type.clone());

        self.set_basic_block_terminator(Terminator::Jump {
            target: merge_block,
            args: vec![Value::Use(cast_some)],
        });

        self.seal_block(ctx, fail_block);
        self.use_basic_block(fail_block);

        let none_struct_type = Type::Struct(StructKind::Tag(none_variant));
        let none_ptr = self.emit_stack_alloc(ctx, none_struct_type.clone(), 1);

        let none_id_ptr = self.emit_get_field_ptr(ctx, none_ptr, id_field).unwrap();
        self.emit_store(
            ctx,
            none_id_ptr,
            Value::NumberLiteral(NumberKind::U16(none_tag_id.0)),
            Span::default(),
        );

        let none_val = Value::Use(self.emit_load(ctx, none_ptr));
        let cast_none =
            self.emit_type_cast(ctx, none_val, left_span, result_union_type.clone());

        self.set_basic_block_terminator(Terminator::Jump {
            target: merge_block,
            args: vec![Value::Use(cast_none)],
        });

        self.seal_block(ctx, merge_block);
        self.use_basic_block(merge_block);

        Value::Use(result_param)
    }
}
