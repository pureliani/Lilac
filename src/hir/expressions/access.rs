use crate::{
    ast::{expr::Expr, IdentifierNode},
    hir::{
        builders::{Builder, InBlock, ValueId},
        types::checked_type::SpannedType,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_access_expr(
        &mut self,
        left: Expr,
        field: IdentifierNode,
        expected_type: Option<&SpannedType>,
    ) -> ValueId {
        let base_val = self.build_expr(left, None);
        let field_span = field.span.clone();
        let field_val = self.emit_read_struct_field(base_val, field);
        self.check_expected(field_val, field_span, expected_type)
    }
}
