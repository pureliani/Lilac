use crate::{
    ast::{expr::Expr, IdentifierNode},
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::SemanticError,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_access_expr(
        &mut self,
        left: Expr,
        field: IdentifierNode,
    ) -> Result<ValueId, SemanticError> {
        let base_ptr = self.build_expr(left)?;

        let field_ptr = self.try_get_field_ptr(base_ptr, &field, false)?;
        Ok(self.emit_load(field_ptr))
    }
}
