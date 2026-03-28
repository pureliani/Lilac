use crate::{
    ast::StringNode,
    globals::STRING_INTERNER,
    mir::{
        builders::{Builder, InBlock, ValueId},
        types::checked_type::{SpannedType, Type},
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_string_literal(
        &mut self,
        node: StringNode,
        expected_type: Option<&SpannedType>,
    ) -> ValueId {
        todo!()
    }
}
