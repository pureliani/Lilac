use crate::{
    hir::builders::{Builder, InBlock, ValueId},
    tokenize::NumberKind,
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_number_literal(&mut self, value: NumberKind) -> ValueId {
        self.emit_const_number(value)
    }
}
