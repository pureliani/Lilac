use crate::{
    ast::{expr::Expr, Span},
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::SemanticError,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_fn_call_expr(
        &mut self,
        left: Box<Expr>,
        args: Vec<Expr>,
        span: Span,
    ) -> Result<ValueId, SemanticError> {
        let func_id = self.build_expr(*left)?;
        let mut arg_ids: Vec<(ValueId, Span)> = Vec::with_capacity(args.len());

        for arg_expr in args {
            let s = arg_expr.span.clone();
            arg_ids.push((self.build_expr(arg_expr)?, s))
        }

        self.call(func_id, arg_ids, span)
    }
}
