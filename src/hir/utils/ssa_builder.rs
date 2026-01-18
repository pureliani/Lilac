use crate::hir::{
    cfg::{
        basic_block::{BasicBlock, BasicBlockId},
        Terminator, ValueId,
    },
    FunctionBuilder,
};

impl FunctionBuilder {
    pub fn get_bb(&mut self, id: &BasicBlockId) -> &mut BasicBlock {
        self.cfg.blocks.get_mut(id).unwrap_or_else(|| {
            panic!(
                "INTERNAL COMPILER ERROR: Could not use basic block with id {} as it \
                 doesn't exist",
                id.0
            );
        })
    }

    pub fn append_arg_to_terminator(
        &mut self,
        from_block: &BasicBlockId,
        to_block: &BasicBlockId,
        arg: ValueId,
    ) {
        let block = self
            .cfg
            .blocks
            .get_mut(&from_block)
            .expect("INTERNAL COMPILER ERROR: Block not found");
        let terminator = block
            .terminator
            .as_mut()
            .expect("INTERNAL COMPILER ERROR: Terminator not found");

        match terminator {
            Terminator::Jump { target, args } => {
                if target == to_block {
                    args.push(arg);
                } else {
                    panic!("INTERNAL COMPILER ERROR: Invalid 'to_block' argument")
                }
            }
            Terminator::CondJump {
                true_target,
                true_args,
                false_target,
                false_args,
                ..
            } => {
                if true_target == to_block {
                    true_args.push(arg);
                }
                if false_target == to_block {
                    false_args.push(arg);
                }
                if true_target != to_block && false_target != to_block {
                    panic!(
                        "INTERNAL COMPILER ERROR: Invalid 'to_block' argument, didn't \
                         match neither 'true_target' nor 'false_target'"
                    )
                }
            }
            _ => {}
        }
    }

    /// Helper to find which ValueId is passed to a specific block parameter index
    fn get_terminator_arg_for_param(
        &self,
        from_block: BasicBlockId,
        to_block: BasicBlockId,
        param_index: usize,
    ) -> ValueId {
        let terminator = self.cfg.blocks[&from_block]
            .terminator
            .as_ref()
            .expect("Block must have terminator");

        match terminator {
            Terminator::Jump { target, args } => {
                assert_eq!(target, &to_block);
                args[param_index].clone()
            }
            Terminator::CondJump {
                true_target,
                true_args,
                false_target,
                false_args,
                ..
            } => {
                if true_target == &to_block {
                    true_args[param_index].clone()
                } else if false_target == &to_block {
                    false_args[param_index].clone()
                } else {
                    panic!("Inconsistent CFG: target block not found in CondJump")
                }
            }
            _ => panic!("Terminator type does not support block arguments"),
        }
    }
}
