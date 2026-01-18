use crate::{
    ast::{expr::Expr, IdentifierNode, Span},
    hir::{
        cfg::Value,
        types::{
            checked_declaration::TagType,
            checked_type::{StructKind, Type},
        },
        FunctionBuilder, HIRContext,
    },
    tokenize::NumberKind,
};

impl FunctionBuilder {
    pub fn build_tag_expr(
        &mut self,
        ctx: &mut HIRContext,
        name: IdentifierNode,
        value: Option<Box<Expr>>,
        span: Span,
    ) -> Value {
        let identifier_value = COMMON_IDENTIFIERS.value;
        let identifier_id = COMMON_IDENTIFIERS.id;

        let tag_id = ctx.program_builder.tag_interner.intern(&name.name);

        let inner_value = value.map(|v| {
            let value_span = v.span;
            (Box::new(self.build_expr(ctx, *v)), value_span)
        });

        let inner_value_type = inner_value
            .as_ref()
            .map(|(value, _)| Box::new(ctx.program_builder.get_value_type(value)));

        let checked_type = Type::Struct(StructKind::Tag(TagType {
            span,
            id: tag_id,
            value_type: inner_value_type,
        }));

        let tag_ptr = self.emit_stack_alloc(ctx, checked_type.clone(), 1);

        let id_field_node = IdentifierNode {
            name: identifier_id,
            span,
        };

        let id_ptr = self
            .emit_get_field_ptr(ctx, tag_ptr, id_field_node)
            .expect("INTERNAL COMPILER ERROR: StructKind::Tag missing 'id' field");

        self.emit_store(
            ctx,
            id_ptr,
            Value::NumberLiteral(NumberKind::U16(tag_id.0)),
            Span::default(),
        );

        if let Some((v, v_span)) = inner_value {
            let val_field_node = IdentifierNode {
                name: identifier_value,
                span,
            };

            let value_ptr = self
                .emit_get_field_ptr(ctx, tag_ptr, val_field_node)
                .expect("INTERNAL COMPILER ERROR: StructKind::Tag missing 'value' field");

            self.emit_store(ctx, value_ptr, *v, v_span);
        }

        let final_val = self.emit_load(ctx, tag_ptr);
        Value::Use(final_val)
    }
}
