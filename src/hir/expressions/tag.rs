use crate::{
    ast::{expr::Expr, IdentifierNode, Span},
    globals::{COMMON_IDENTIFIERS, TAG_INTERNER},
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::SemanticError,
        types::{
            checked_declaration::TagType,
            checked_type::{StructKind, Type},
        },
    },
    tokenize::NumberKind,
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_tag_expr(
        &mut self,
        name: IdentifierNode,
        value: Option<Box<Expr>>,
        span: Span,
    ) -> Result<ValueId, SemanticError> {
        let tag_id = TAG_INTERNER.intern(&name.name);

        let inner_info = if let Some(v) = value {
            let v_span = v.span.clone();
            let v_id = self.build_expr(*v)?;
            let v_ty = self.get_value_type(&v_id).clone();
            Some((v_id, v_ty, v_span))
        } else {
            None
        };

        let checked_type = Type::Struct(StructKind::Tag(TagType {
            id: tag_id,
            value_type: inner_info.as_ref().map(|(_, ty, _)| Box::new(ty.clone())),
            span: span.clone(),
        }));

        let tag_ptr = self.emit_stack_alloc(checked_type.clone(), 1);

        let id_field_node = IdentifierNode {
            name: COMMON_IDENTIFIERS.id,
            span: span.clone(),
        };

        let id_ptr = self.get_field_ptr(tag_ptr, &id_field_node)?;
        let tag_id_val = self.emit_const_number(NumberKind::U16(tag_id.0));
        self.store(id_ptr, tag_id_val, span.clone());

        if let Some((v_id, _, v_span)) = inner_info {
            let val_field_node = IdentifierNode {
                name: COMMON_IDENTIFIERS.value,
                span: span.clone(),
            };

            let value_ptr = self.get_field_ptr(tag_ptr, &val_field_node)?;
            self.store(value_ptr, v_id, v_span);
        }

        self.load(tag_ptr, span)
    }
}
