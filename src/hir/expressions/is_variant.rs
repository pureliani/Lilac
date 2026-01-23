use crate::{
    ast::{expr::Expr, type_annotation::TagAnnotation},
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::SemanticError,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_is_variant_expr(
        &mut self,
        left: Box<Expr>,
        variants: Vec<TagAnnotation>,
    ) -> Result<ValueId, SemanticError> {
        todo!()
    }
}
