use crate::{
    ast::{expr::Expr, IdentifierNode, Span},
    globals::COMMON_IDENTIFIERS,
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::SemanticError,
        types::checked_type::{StructKind, Type},
    },
    tokenize::NumberKind,
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_list_literal_expr(
        &mut self,
        items: Vec<Expr>,
        expr_span: Span,
    ) -> Result<ValueId, SemanticError> {
        let mut item_values = Vec::with_capacity(items.len());
        let mut type_entries = Vec::with_capacity(items.len());

        for item in items {
            let span = item.span.clone();
            let val_id = self.build_expr(item)?;
            let ty = self.get_value_type(&val_id).clone();

            item_values.push(val_id);
            type_entries.push((ty, span));
        }

        let element_types: Vec<Type> = type_entries.iter().map(|e| e.0.clone()).collect();
        let element_type = Type::make_union(element_types);

        let capacity = item_values.len();
        let capacity_val = self.emit_const_number(NumberKind::USize(capacity));

        let buffer_ptr = self.emit_heap_alloc(element_type.clone(), capacity_val)?;

        let list_header_type =
            Type::Struct(StructKind::ListHeader(Box::new(element_type.clone())));
        let const_one = self.emit_const_number(NumberKind::USize(1));

        let list_header_ptr = self.emit_heap_alloc(list_header_type, const_one)?;

        let set_header_field = |builder: &mut Builder<'_, InBlock>,
                                name,
                                val,
                                span: &Span|
         -> Result<(), SemanticError> {
            let field_node = IdentifierNode {
                name,
                span: span.clone(),
            };
            let field_ptr = builder.get_field_ptr(list_header_ptr, &field_node)?;
            builder.store(field_ptr, val, span.clone());
            Ok(())
        };

        set_header_field(self, COMMON_IDENTIFIERS.capacity, capacity_val, &expr_span)?;
        set_header_field(self, COMMON_IDENTIFIERS.len, capacity_val, &expr_span)?;
        set_header_field(self, COMMON_IDENTIFIERS.ptr, buffer_ptr, &expr_span)?;

        for (i, val_id) in item_values.into_iter().enumerate() {
            let index_val = self.emit_const_number(NumberKind::USize(i));
            let item_span = type_entries[i].1.clone();

            let elem_ptr = self.ptr_offset(buffer_ptr, index_val, item_span.clone())?;

            self.store(elem_ptr, val_id, item_span);
        }

        Ok(list_header_ptr)
    }
}
