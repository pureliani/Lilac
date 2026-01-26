use crate::{
    ast::{expr::Expr, type_annotation::TypeAnnotation},
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::SemanticError,
        utils::check_type::{check_type_annotation, TypeCheckerContext},
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_typecast_expr(
        &mut self,
        left: Expr,
        target: TypeAnnotation,
    ) -> Result<ValueId, SemanticError> {
        let value_span = left.span.clone();
        let src_id = self.build_expr(left)?;

        let mut type_ctx = TypeCheckerContext {
            scope: self.current_scope.clone(),
            declarations: &self.program.declarations,
            errors: self.errors,
        };
        let target_type = check_type_annotation(&mut type_ctx, &target);

        self.cast(src_id, target_type, value_span)
    }
}
