use std::collections::{HashMap, HashSet};

use crate::{
    ast::{expr::Expr, IdentifierNode},
    compile::interner::StringId,
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::{
            checked_declaration::CheckedParam,
            checked_type::{StructKind, Type},
        },
        utils::layout::pack_struct,
    },
    tokenize::NumberKind,
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_struct_init_expr(
        &mut self,
        fields: Vec<(IdentifierNode, Expr)>,
    ) -> Result<ValueId, SemanticError> {
        let mut resolved_fields: Vec<CheckedParam> = Vec::with_capacity(fields.len());
        let mut field_values: HashMap<StringId, ValueId> =
            HashMap::with_capacity(fields.len());
        let mut seen_names: HashSet<StringId> = HashSet::new();

        for (field_name, field_expr) in fields {
            if !seen_names.insert(field_name.name) {
                return Err(SemanticError {
                    kind: SemanticErrorKind::DuplicateStructFieldInitializer(
                        field_name.clone(),
                    ),
                    span: field_name.span,
                });
            }

            let val_id = self.build_expr(field_expr)?;
            let val_type = self.get_value_type(&val_id).clone();

            resolved_fields.push(CheckedParam {
                identifier: field_name.clone(),
                ty: val_type,
            });
            field_values.insert(field_name.name, val_id);
        }

        let packed_kind = pack_struct(StructKind::UserDefined(resolved_fields));
        let struct_type = Type::Struct(packed_kind.clone());

        let count_val = self.emit_const_number(NumberKind::USize(1));
        let struct_ptr = self.emit_heap_alloc(struct_type, count_val)?;

        if let StructKind::UserDefined(sorted_fields) = packed_kind {
            for field in sorted_fields {
                let field_ptr = self.get_field_ptr(struct_ptr, &field.identifier)?;

                let val_id = field_values.get(&field.identifier.name).expect(
                    "INTERNAL COMPILER ERROR: Field value missing during initialization",
                );

                self.store(field_ptr, *val_id, field.identifier.span.clone());
            }
        }

        Ok(struct_ptr)
    }
}
