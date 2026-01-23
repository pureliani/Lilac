use crate::{
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::SemanticError,
    },
    tokenize::NumberKind,
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_number_literal(
        &mut self,
        value: NumberKind,
    ) -> Result<ValueId, SemanticError> {
        Ok(self.emit_const_number(value))
    }
}
