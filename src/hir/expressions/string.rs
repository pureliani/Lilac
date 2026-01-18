use crate::{
    ast::{IdentifierNode, Span, StringNode},
    hir::{
        cfg::Value,
        types::checked_type::{StructKind, Type},
        FunctionBuilder, HIRContext,
    },
    tokenize::NumberKind,
};

impl FunctionBuilder {
    pub fn build_string_literal(
        &mut self,
        ctx: &mut HIRContext,
        node: StringNode,
    ) -> Value {
        let constant_id = ctx.program_builder.new_constant_id();
        ctx.program_builder
            .constant_data
            .insert(constant_id, node.value.as_bytes().to_vec());

        let string_header_type = Type::Struct(StructKind::String);

        let struct_ptr = self
            .emit_heap_alloc(
                ctx,
                string_header_type.clone(),
                Value::NumberLiteral(NumberKind::USize(1)),
            )
            .expect("INTERNAL COMPILER ERROR: Failed to allocate string header");

        let is_heap_id = IdentifierNode {
            name: COMMON_IDENTIFIERS.is_heap_allocated,
            span: node.span,
        };
        let is_heap_ptr = self
            .emit_get_field_ptr(ctx, struct_ptr, is_heap_id)
            .unwrap();

        self.emit_store(ctx, is_heap_ptr, Value::BoolLiteral(false), Span::default());

        let len_id = IdentifierNode {
            name: COMMON_IDENTIFIERS.len,
            span: node.span,
        };
        let len_ptr = self.emit_get_field_ptr(ctx, struct_ptr, len_id).unwrap();
        self.emit_store(
            ctx,
            len_ptr,
            Value::NumberLiteral(NumberKind::USize(node.len)),
            Span::default(),
        );

        let ptr_id = IdentifierNode {
            name: COMMON_IDENTIFIERS.ptr,
            span: node.span,
        };
        let data_ptr_field = self.emit_get_field_ptr(ctx, struct_ptr, ptr_id).unwrap();

        let constant_ptr_id = self.emit_load_constant(ctx, constant_id);
        self.emit_store(
            ctx,
            data_ptr_field,
            Value::Use(constant_ptr_id),
            Span::default(),
        );

        Value::Use(struct_ptr)
    }
}
