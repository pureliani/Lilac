use crate::hir::builders::{Builder, InGlobal, InModule, Module};

impl<'a> Builder<'a, InModule> {
    pub fn as_program(&mut self) -> Builder<'_, InGlobal> {
        Builder {
            program: self.program,
            errors: self.errors,
            current_scope: self.current_scope.clone(),
            context: InGlobal,
            current_defs: self.current_defs,
            aliases: self.aliases,
            incomplete_phis: self.incomplete_phis,
            type_predicates: self.type_predicates,
        }
    }

    pub fn module(&mut self) -> &mut Module {
        self.program.modules.get_mut(&self.context.path).unwrap()
    }
}
