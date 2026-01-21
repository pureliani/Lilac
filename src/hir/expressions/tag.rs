use crate::{
    ast::{expr::Expr, IdentifierNode, Span},
    globals::TAG_INTERNER,
    hir::{
        builders::{Builder, InBlock, ValueId},
        types::{checked_declaration::TagType, checked_type::Type},
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_tag_expr(
        &mut self,
        name: IdentifierNode,
        value: Option<Box<Expr>>,
        span: Span,
    ) -> ValueId {
        let tag_id = TAG_INTERNER.intern(&name.name);
        let payload_id = value.map(|v| self.build_expr(*v));

        let tag_type = Type::Tag(TagType {
            id: tag_id,
            value_type: payload_id.map(|id| Box::new(self.get_value_type(&id).clone())),
            span: span.clone(),
        });

        self.emit_assemble_tag(tag_type, payload_id, span)
    }
}
