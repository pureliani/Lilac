use crate::{
    ast::{expr::Expr, IdentifierNode},
    hir::builders::{Builder, InBlock, ValueId},
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_access_expr(&mut self, left: Expr, field: IdentifierNode) -> ValueId {
        let base_ptr = self.build_expr(left);

        let field_ptr = self.try_get_field_ptr(base_ptr, &field, false)?;
        self.emit_load(field_ptr)
    }
}
