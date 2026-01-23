use crate::hir::{
    builders::{Builder, InBlock, ValueId},
    errors::SemanticError,
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_bool_literal(&mut self, value: bool) -> Result<ValueId, SemanticError> {
        Ok(self.emit_const_bool(value))
    }
}
