use crate::{
    ast::expr::BlockContents,
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::SemanticError,
        utils::scope::ScopeKind,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_codeblock_expr(
        &mut self,
        codeblock: BlockContents,
    ) -> Result<ValueId, SemanticError> {
        self.current_scope = self
            .current_scope
            .enter(ScopeKind::CodeBlock, codeblock.span.start);

        self.build_statements(codeblock.statements);
        let result_id = if let Some(final_expr) = codeblock.final_expr {
            self.build_expr(*final_expr)?
        } else {
            self.emit_const_void()
        };

        self.current_scope = self
            .current_scope
            .exit(codeblock.span.end)
            .expect("INTERNAL COMPILER ERROR: Scope stack mismatch in codeblock");

        Ok(result_id)
    }
}
