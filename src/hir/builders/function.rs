use crate::hir::{
    builders::{BasicBlockId, Builder, Function, InBlock, InFunction, InModule},
    types::checked_declaration::CheckedDeclaration,
};

impl<'a> Builder<'a, InFunction> {
    pub fn at_block(self, block_id: BasicBlockId) -> Builder<'a, InBlock> {
        Builder {
            program: self.program,
            errors: self.errors,
            current_scope: self.current_scope,
            context: InBlock {
                path: self.context.path,
                func_id: self.context.func_id,
                block_id,
            },
        }
    }

    pub fn as_module(&'a mut self) -> Builder<'a, InModule> {
        Builder {
            program: self.program,
            errors: self.errors,
            current_scope: self.current_scope.clone(),
            context: InModule {
                path: self.context.path.clone(),
            },
        }
    }

    pub fn get_function(&mut self) -> &mut Function {
        let decl = self
            .program
            .declarations
            .get_mut(&self.context.func_id)
            .unwrap();

        match decl {
            CheckedDeclaration::Function(f) => f,
            _ => panic!("INTERNAL COMPILER ERROR: Expected function declaration"),
        }
    }
}
