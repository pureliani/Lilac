use crate::{
    compile::interner::TagId,
    hir::{
        builders::{Builder, InBlock, ValueId},
        instructions::{ConstInstr, Instruction},
        types::checked_type::Type,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_tag_expr(&mut self, tag_id: TagId) -> ValueId {
        let dest = self.new_value_id(Type::Tag(tag_id));

        self.push_instruction(Instruction::Const(ConstInstr::ConstTag {
            dest,
            val: tag_id,
        }));

        dest
    }
}
