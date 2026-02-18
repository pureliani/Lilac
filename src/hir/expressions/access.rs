use crate::{
    ast::{expr::Expr, IdentifierNode},
    hir::{
        builders::{Builder, InBlock, ValueId},
        utils::place::Place,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_access_expr(&mut self, left: Expr, field: IdentifierNode) -> ValueId {
        let base_place = match self.resolve_place(left) {
            Ok(p) => p,
            Err(e) => return self.report_error_and_get_poison(e),
        };

        let field_place = Place::Field(Box::new(base_place), field.name);
        let canonical_field = field_place.canonicalize(self.aliases);

        self.read_place(&canonical_field, field.span)
    }
}
