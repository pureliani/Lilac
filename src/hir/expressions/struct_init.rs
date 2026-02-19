use std::collections::HashSet;

use crate::{
    ast::{expr::Expr, IdentifierNode},
    compile::interner::StringId,
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_struct_init_expr(
        &mut self,
        fields: Vec<(IdentifierNode, Expr)>,
    ) -> ValueId {
        let mut field_values: Vec<(IdentifierNode, ValueId)> =
            Vec::with_capacity(fields.len());
        let mut seen_names: HashSet<StringId> = HashSet::new();

        for (field_name, field_expr) in fields {
            if !seen_names.insert(field_name.name) {
                self.errors.push(SemanticError {
                    kind: SemanticErrorKind::DuplicateStructFieldInitializer(
                        field_name.clone(),
                    ),
                    span: field_name.span.clone(),
                });
            }

            let val_id = self.build_expr(field_expr);
            field_values.push((field_name, val_id));
        }

        self.emit_struct_init(field_values)
    }
}
