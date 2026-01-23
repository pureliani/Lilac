use crate::{
    ast::expr::Expr,
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::SemanticError,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_access_expr(&mut self, expr: Expr) -> Result<ValueId, SemanticError> {
        let s = expr.span.clone();
        let field_ptr = self.build_lvalue(expr)?;
        self.load(field_ptr, s)
    }
}
