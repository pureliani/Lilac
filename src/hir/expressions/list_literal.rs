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
            let val_id = self.build_expr(item);
            let ty = self.get_value_type(val_id).clone();

            item_values.push(val_id);
            element_types.push(ty);
        }

        let element_type = Type::make_union(element_types);
        self.emit_list_init(element_type, item_values)
    }
}
