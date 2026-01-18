use std::{cell::RefCell, rc::Rc};

use crate::hir::{cfg::basic_block::BasicBlock, FunctionBuilder};

impl FunctionBuilder {
    pub fn new_basic_block(&mut self) -> &mut BasicBlock {
        BasicBlock::new(&Rc::new(RefCell::new(self)))
    }
}
