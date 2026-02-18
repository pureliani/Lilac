use std::collections::{HashMap, HashSet};

use crate::{
    ast::{expr::Expr, IdentifierNode},
    compile::interner::StringId,
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_declaration::CheckedParam,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_struct_init_expr(
        &mut self,
        fields: Vec<(IdentifierNode, Expr)>,
    ) -> ValueId {
        let mut resolved_fields: Vec<CheckedParam> = Vec::with_capacity(fields.len());
        let mut field_values: HashMap<StringId, ValueId> =
            HashMap::with_capacity(fields.len());
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
            let val_type = self.get_value_type(val_id).clone();

            resolved_fields.push(CheckedParam {
                identifier: field_name.clone(),
                ty: val_type,
            });
            field_values.insert(field_name.name, val_id);
        }

        todo!("Emit Instruction::Struct(StructInstr::Construct(..))")
    }
}
