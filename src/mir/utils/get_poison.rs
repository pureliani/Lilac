use crate::mir::{
    builders::{Builder, InBlock, ValueId},
    errors::SemanticError,
};

impl<'a> Builder<'a, InBlock> {
    pub fn report_error_and_get_poison(&mut self, error: SemanticError) -> ValueId {
        self.errors.push(error);
        let type_unknown = self.types.unknown();
        self.new_value_id(type_unknown)
    }
}
