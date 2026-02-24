use crate::{
    ast::{expr::Expr, IdentifierNode},
    hir::builders::{Builder, InBlock, ValueId},
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_access_expr(&mut self, left: Expr, field: IdentifierNode) -> ValueId {
        let base_val = self.build_expr(left);
        self.emit_read_struct_field(base_val, field)
    }
}
