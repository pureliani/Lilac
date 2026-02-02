use crate::{
    ast::{expr::Expr, type_annotation::TypeAnnotation},
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::SemanticError,
        utils::{
            adjustments::Adjustment,
            check_type::{check_type_annotation, TypeCheckerContext},
        },
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_typecast_expr(
        &mut self,
        left: Expr,
        target: TypeAnnotation,
    ) -> Result<ValueId, SemanticError> {
        let source_span = left.span.clone();
        let source = self.build_expr(left)?;

        let mut type_ctx = TypeCheckerContext {
            scope: self.current_scope.clone(),
            declarations: &self.program.declarations,
            errors: self.errors,
        };
        let target_type = check_type_annotation(&mut type_ctx, &target);

        match self.adjust_assignment(source, source_span, target_type, true)? {
            Adjustment::Value(f) => Ok(f(self)),
            Adjustment::Write(f) => {
                todo!()
            }
        }
    }
}
