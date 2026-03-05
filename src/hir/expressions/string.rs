use crate::{
    ast::StringNode,
    globals::next_constant_id,
    hir::{
        builders::{Builder, InBlock, ValueId},
        types::checked_type::SpannedType,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_string_literal(
        &mut self,
        node: StringNode,
        expected_type: Option<&SpannedType>,
    ) -> ValueId {
        let span = node.span.clone();
        let constant_id = next_constant_id();
        self.program
            .constant_data
            .insert(constant_id, node.value.as_bytes().to_vec());

        let result = self.emit_const_string(constant_id);
        self.check_expected(result, span, expected_type)
    }
}
