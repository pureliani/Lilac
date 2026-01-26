use crate::{
    compile::interner::TagId,
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::SemanticError,
        types::checked_type::Type,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_tag_expr(&mut self, tag_id: TagId) -> Result<ValueId, SemanticError> {
        Ok(self.new_value_id(Type::Tag(tag_id)))
    }
}
