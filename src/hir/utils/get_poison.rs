use crate::hir::{
    builders::{Builder, InBlock, ValueId},
    errors::SemanticError,
    types::checked_type::Type,
};

impl<'a> Builder<'a, InBlock> {
    pub fn report_error_and_get_poison(&mut self, error: SemanticError) -> ValueId {
        self.errors.push(error);
        self.new_value_id(Type::Unknown)
    }
}
