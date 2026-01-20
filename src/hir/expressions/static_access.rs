use crate::{
    ast::{expr::Expr, IdentifierNode},
    hir::builders::{Builder, InBlock, ValueId},
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_static_access_expr(
        &mut self,
        left: Box<Expr>,
        field: IdentifierNode,
    ) -> ValueId {
        todo!("Implement static access expression builder")
    }
}
