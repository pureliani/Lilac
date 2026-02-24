use crate::{
    ast::{expr::Expr, Span},
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::{checked_declaration::CheckedParam, checked_type::Type},
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_fn_call_expr(
        &mut self,
        left: Expr,
        args: Vec<Expr>,
        span: Span,
    ) -> ValueId {
        let func_id = self.build_expr(left);
        let func_type = self.get_value_type(func_id).clone();

        let (params, return_type) = match func_type {
            Type::Fn(f) => (f.params, *f.return_type),
            Type::Unknown => return self.new_value_id(Type::Unknown),
            _ => {
                return self.report_error_and_get_poison(SemanticError {
                    kind: SemanticErrorKind::CannotCall(func_type),
                    span,
                });
            }
        };

        if args.len() != params.len() {
            return self.report_error_and_get_poison(SemanticError {
                kind: SemanticErrorKind::FnArgumentCountMismatch {
                    expected: params.len(),
                    received: args.len(),
                },
                span,
            });
        }

        let evaluated = self.evaluate_call_args(args);

        if let Err(e) = self.check_argument_aliasing(&evaluated) {
            return self.report_error_and_get_poison(e);
        }

        let final_args = match self.coerce_call_args(&evaluated, &params) {
            Ok(args) => args,
            Err(e) => return self.report_error_and_get_poison(e),
        };

        self.emit_call(func_id, final_args, return_type)
    }

    fn evaluate_call_args(&mut self, args: Vec<Expr>) -> Vec<(ValueId, Span)> {
        let mut evaluated = Vec::with_capacity(args.len());

        for arg_expr in args {
            let span = arg_expr.span.clone();
            let val_id = self.build_expr(arg_expr);

            evaluated.push((val_id, span));
        }

        evaluated
    }

    fn check_argument_aliasing(
        &self,
        args: &[(ValueId, Span)],
    ) -> Result<(), SemanticError> {
        let val_ids: Vec<ValueId> = args.iter().map(|(v, _)| *v).collect();

        if let Some(conflict) = self.ptg.check_aliasing(&val_ids) {
            return Err(SemanticError {
                kind: SemanticErrorKind::ArgumentAliasing {
                    passed_arg_span: args[conflict.arg_i].1.clone(),
                    passed_path: conflict.path_i,
                    aliased_arg_span: args[conflict.arg_j].1.clone(),
                    aliased_path: conflict.path_j,
                },
                span: args[conflict.arg_i].1.clone(),
            });
        }

        Ok(())
    }

    fn coerce_call_args(
        &mut self,
        args: &[(ValueId, Span)],
        params: &[CheckedParam],
    ) -> Result<Vec<ValueId>, SemanticError> {
        let mut final_args = Vec::with_capacity(args.len());

        for (i, (val_id, span)) in args.iter().enumerate() {
            let coerced =
                self.adjust_value(*val_id, span.clone(), params[i].ty.clone(), false)?;
            final_args.push(coerced);
        }

        Ok(final_args)
    }
}
