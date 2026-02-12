use crate::{
    ast::{expr::Expr, Span},
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_type::Type,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_fn_call_expr(
        &mut self,
        left: Expr,
        args: Vec<Expr>,
        span: Span,
    ) -> Result<ValueId, SemanticError> {
        let func_id = self.build_expr(left)?;
        let func_type = self.get_value_type(&func_id).clone();

        let (params, return_type) = match func_type {
            Type::Fn(f) => (f.params, *f.return_type),
            Type::Unknown => return Ok(self.new_value_id(Type::Unknown)),
            _ => {
                return Err(SemanticError {
                    kind: SemanticErrorKind::CannotCall(func_type),
                    span,
                });
            }
        };

        if args.len() != params.len() {
            return Err(SemanticError {
                kind: SemanticErrorKind::FnArgumentCountMismatch {
                    expected: params.len(),
                    received: args.len(),
                },
                span,
            });
        }

        let mut final_args = Vec::with_capacity(args.len());

        for (i, arg_expr) in args.into_iter().enumerate() {
            let arg_span = arg_expr.span.clone();
            let arg_value = self.build_expr(arg_expr)?;
            let param_type = params[i].ty.clone();

            let coerced_arg_value =
                self.adjust_value(arg_value, arg_span, param_type, false)?;

            final_args.push(coerced_arg_value);
        }

        Ok(self.emit_call(func_id, final_args, return_type))
    }
}
