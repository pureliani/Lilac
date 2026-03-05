use crate::{
    ast::{expr::Expr, Span},
    hir::{
        builders::{Builder, InBlock, ValueId},
        types::checked_type::{SpannedType, Type},
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_list_literal_expr(
        &mut self,
        expr_span: Span,
        items: Vec<Expr>,
        expected_type: Option<&SpannedType>,
    ) -> ValueId {
        let mut item_values = Vec::with_capacity(items.len());
        let mut element_types = Vec::with_capacity(items.len());

        let expected_element_type = if let Some(SpannedType {
            kind: Type::List(elem_type),
            span: _,
        }) = expected_type
        {
            Some(*elem_type.clone())
        } else {
            None
        };

        for item in items {
            let val_id = self.build_expr(item, expected_element_type.as_ref());
            let ty = self.get_value_type(val_id).clone();

            item_values.push(val_id);
            element_types.push(ty);
        }

        let element_type = SpannedType {
            kind: Type::make_union(element_types),
            span: expr_span.clone(),
        };
        let result = self.emit_list_init(element_type, item_values);
        self.check_expected(result, expr_span, expected_type)
    }
}
