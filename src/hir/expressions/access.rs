use crate::{
    ast::{expr::Expr, IdentifierNode},
    hir::{
        builders::{Builder, InBlock, LValue, ValueId},
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

        let lval = LValue::Field {
            base_ptr,
            field: field.name,
        };

        self.read_lvalue(lval, field.span)
    }
}
