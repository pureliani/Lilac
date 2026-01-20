use crate::{
    ast::{expr::Expr, Span},
    hir::builders::{Builder, InBlock, ValueId},
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_fn_call_expr(
        &mut self,
        left: Box<Expr>,
        args: Vec<Expr>,
        span: Span,
    ) -> ValueId {
        let func_id = self.build_expr(*left);

        let arg_ids: Vec<(ValueId, Span)> = args
            .into_iter()
            .map(|arg_expr| {
                let s = arg_expr.span.clone();
                (self.build_expr(arg_expr), s)
            })
            .collect();

        match self.call(func_id, arg_ids, span) {
            Ok(Some(return_value_id)) => return_value_id,
            Ok(None) => self.emit_const_void(),
            Err(e) => self.report_error_and_get_poison(e),
        }
    }
}
