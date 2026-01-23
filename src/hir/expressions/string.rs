use crate::{
    ast::StringNode,
    globals::next_constant_id,
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::SemanticError,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_string_literal(
        &mut self,
        node: StringNode,
    ) -> Result<ValueId, SemanticError> {
        let constant_id = next_constant_id();
        self.program
            .constant_data
            .insert(constant_id, node.value.as_bytes().to_vec());

        Ok(self.emit_const_string(constant_id))
    }
}
