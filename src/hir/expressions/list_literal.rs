use crate::{
    ast::{expr::Expr, IdentifierNode, Span},
    hir::{
        cfg::Value,
        types::checked_type::{StructKind, Type},
        utils::try_unify_types::try_unify_types,
        FunctionBuilder, HIRContext,
    },
    tokenize::NumberKind,
};

impl FunctionBuilder {
    pub fn build_list_literal_expr(
        &mut self,
        ctx: &mut HIRContext,
        items: Vec<Expr>,
        expr_span: Span,
    ) -> Value {
        let identifier_capacity = COMMON_IDENTIFIERS.capacity;
        let identifier_ptr = COMMON_IDENTIFIERS.ptr;
        let identifier_len = COMMON_IDENTIFIERS.len;

        let mut item_values = Vec::with_capacity(items.len());
        let mut type_entries = Vec::with_capacity(items.len());

        for item in items {
            let span = item.span;
            let val = self.build_expr(ctx, item);
            let ty = ctx.program_builder.get_value_type(&val);

            item_values.push((val, span));
            type_entries.push((ty, span));
        }

        let element_type = match try_unify_types(&type_entries) {
            Ok(ty) => ty,
            Err(e) => {
                return Value::Use(self.report_error_and_get_poison(ctx, e));
            }
        };

        let capacity = item_values.len();
        let capacity_val = Value::NumberLiteral(NumberKind::USize(capacity));

        let buffer_ptr =
            match self.emit_heap_alloc(ctx, element_type.clone(), capacity_val.clone()) {
                Ok(id) => id,
                Err(e) => return Value::Use(self.report_error_and_get_poison(ctx, e)),
            };

        let list_type = Type::Struct(StructKind::List(Box::new(element_type)));
        let header_ptr = match self.emit_heap_alloc(
            ctx,
            list_type.clone(),
            Value::NumberLiteral(NumberKind::USize(1)),
        ) {
            Ok(id) => id,
            Err(e) => return Value::Use(self.report_error_and_get_poison(ctx, e)),
        };

        let mut set_field = |name, val| {
            let field_id = IdentifierNode {
                name,
                span: expr_span,
            };

            let ptr = self
                .emit_get_field_ptr(ctx, header_ptr, field_id)
                .expect("INTERNAL COMPILER ERROR: Failed to set field on List literal");

            self.emit_store(ctx, ptr, val, expr_span);
        };

        set_field(identifier_capacity, capacity_val.clone());
        set_field(identifier_len, capacity_val);
        set_field(identifier_ptr, Value::Use(buffer_ptr));

        for (i, (val, item_span)) in item_values.into_iter().enumerate() {
            let index_val = Value::NumberLiteral(NumberKind::USize(i));

            let elem_ptr = self
                .emit_get_element_ptr(ctx, buffer_ptr, index_val)
                .unwrap();

            self.emit_store(ctx, elem_ptr, val, item_span);
        }

        Value::Use(header_ptr)
    }
}
