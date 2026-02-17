use crate::{
    ast::{expr::Expr, IdentifierNode},
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::SemanticError,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_static_access_expr(
        &mut self,
        _left: Expr,
        _field: IdentifierNode,
    ) -> ValueId {
        todo!("Implement static access expression builder")
    }
}
