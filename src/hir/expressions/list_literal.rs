use crate::{
    ast::{expr::Expr, Span},
    hir::{
        builders::{Builder, InBlock, ValueId},
        types::checked_type::Type,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_list_literal_expr(
        &mut self,
        items: Vec<Expr>,
        _expr_span: Span,
    ) -> ValueId {
        let mut item_values = Vec::with_capacity(items.len());
        let mut element_types = Vec::with_capacity(items.len());

        for item in items {
            let span = item.span.clone();
            let val_id = self.build_expr(item);
            let ty = self.get_value_type(val_id).clone();

            item_values.push((val_id, span));
            element_types.push(ty);
        }

        let element_type = Type::make_union(element_types);
        let is_union = element_type.as_union_variants().is_some();

        let mut final_items = Vec::with_capacity(item_values.len());
        for (val_id, span) in item_values {
            if is_union {
                final_items.push(self.coerce_to_union(val_id, &element_type, span));
            } else {
                final_items.push(val_id);
            }
        }

        self.emit_list_init(element_type, final_items)
    }
}
