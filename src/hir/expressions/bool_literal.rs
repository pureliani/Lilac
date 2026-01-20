use crate::hir::builders::{Builder, InBlock, ValueId};

impl<'a> Builder<'a, InBlock> {
    pub fn build_bool_literal(&mut self, value: bool) -> ValueId {
        self.emit_const_bool(value)
    }
}
