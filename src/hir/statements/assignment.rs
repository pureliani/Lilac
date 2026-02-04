use crate::{
    ast::expr::Expr,
    hir::{
        builders::{Builder, InBlock},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_type::Type,
        utils::{adjustments::Adjustment, place::Place},
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_assignment_stmt(
        &mut self,
        target: Expr,
        value: Expr,
    ) -> Result<(), SemanticError> {
        let value_span = value.span.clone();
        let value_id = self.build_expr(value)?;

        let target_span = target.span.clone();
        let place = self.resolve_place(target)?;
        let place_path = place.path();
        let place_type = self.type_of_place(&place);

        todo!();
    }
}
