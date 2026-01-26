use crate::{
    ast::{expr::Expr, type_annotation::TypeAnnotation, IdentifierNode, Span},
    globals::COMMON_IDENTIFIERS,
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_type::{StructKind, Type},
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_is_type_expr(
        &mut self,
        left: Expr,
        ty: TypeAnnotation,
    ) -> Result<ValueId, SemanticError> {
        let span = left.span.clone();

        let val_id = self.build_expr(left)?;
        let val_ty = self.get_value_type(&val_id).clone();

        if !matches!(&val_ty, Type::Struct(StructKind::Union(_))) {
            return Err(SemanticError {
                kind: SemanticErrorKind::CannotNarrowNonUnion(val_ty),
                span,
            });
        }

        let id_field_node = IdentifierNode {
            name: COMMON_IDENTIFIERS.id,
            span: Span::default(),
        };

        let id_ptr = self.get_field_ptr(val_id, &id_field_node)?;
        let active_index = self.emit_load(id_ptr);

        todo!()
    }
}
