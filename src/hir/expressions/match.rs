use crate::{
    ast::expr::{Expr, MatchArm},
    hir::builders::{Builder, InBlock, ValueId},
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_match_expr(
        &mut self,
        conditions: Vec<Expr>,
        arms: Vec<MatchArm>,
    ) -> ValueId {
        todo!("Implement match expression builder")
    }
}
