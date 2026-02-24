use std::collections::HashMap;

use crate::{
    ast::IdentifierNode,
    compile::interner::StringId,
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        instructions::{Instruction, StructInstr},
        types::{checked_declaration::CheckedParam, checked_type::Type},
        utils::{
            adjustments::check_structural_compatibility, layout::pack_struct,
            points_to::PathSegment,
        },
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn emit_struct_init(
        &mut self,
        fields: Vec<(IdentifierNode, ValueId)>,
    ) -> ValueId {
        let type_fields: Vec<CheckedParam> = fields
            .iter()
            .map(|(ident, val_id)| CheckedParam {
                identifier: ident.clone(),
                ty: self.get_value_type(*val_id).clone(),
            })
            .collect();

        let packed_fields = pack_struct(type_fields);

        let mut instr_fields = Vec::with_capacity(fields.len());

        let val_lookup: HashMap<StringId, ValueId> = fields
            .into_iter()
            .map(|(ident, val)| (ident.name, val))
            .collect();

        for param in &packed_fields {
            let val_id = val_lookup
                .get(&param.identifier.name)
                .expect("Field missing after sort");
            instr_fields.push((param.identifier.name, *val_id));
        }

        let struct_type = Type::Struct(packed_fields);
        let dest = self.new_value_id(struct_type);

        let alloc_id = self.ptg.new_alloc();
        self.ptg.bind_value_to_alloc(dest, alloc_id);

        for (name, val_id) in &instr_fields {
            if let Some(val_allocs) = self.ptg.value_locations.get(val_id).cloned() {
                for v_alloc in val_allocs {
                    self.ptg
                        .add_heap_edge(alloc_id, PathSegment::Field(*name), v_alloc);
                }
            }
        }

        self.push_instruction(Instruction::Struct(StructInstr::Construct {
            dest,
            fields: instr_fields,
        }));

        dest
    }

    pub fn emit_read_struct_field(
        &mut self,
        base: ValueId,
        field_identifier: IdentifierNode,
    ) -> ValueId {
        let source_type = self.get_value_type(base).clone();

        match source_type {
            Type::Struct(_) => {
                if let Some((_, ty)) = source_type.get_field(&field_identifier.name) {
                    let dest = self.new_value_id(ty);
                    self.push_instruction(Instruction::Struct(StructInstr::ReadField {
                        dest,
                        base,
                        field: field_identifier.name,
                    }));

                    self.ptg.read_path(
                        dest,
                        base,
                        PathSegment::Field(field_identifier.name),
                    );

                    dest
                } else {
                    self.report_error_and_get_poison(SemanticError {
                        span: field_identifier.span.clone(),
                        kind: SemanticErrorKind::AccessToUndefinedField(field_identifier),
                    })
                }
            }
            Type::Unknown => self.new_value_id(Type::Unknown),
            _ => panic!(
                "INTERNAL COMPILER ERROR: Expected emit_read_struct_field to be called \
                 on a struct type, found {:?}",
                source_type
            ),
        }
    }

    pub fn emit_update_struct_field(
        &mut self,
        base: ValueId,
        field_identifier: IdentifierNode,
        value: ValueId,
    ) -> ValueId {
        let base_type = self.get_value_type(base).clone();

        match base_type {
            Type::Struct(_) => {
                if let Some((_, expected_type)) =
                    base_type.get_field(&field_identifier.name)
                {
                    let value_type = self.get_value_type(value);

                    if !check_structural_compatibility(value_type, &expected_type) {
                        return self.report_error_and_get_poison(SemanticError {
                            kind: SemanticErrorKind::TypeMismatch {
                                expected: expected_type,
                                received: value_type.clone(),
                            },
                            span: field_identifier.span.clone(),
                        });
                    }

                    // TODO: Should the union narrowing case be handled here? in that case base_type is not correct
                    let updated_base = self.new_value_id(base_type);
                    self.push_instruction(Instruction::Struct(
                        StructInstr::UpdateField {
                            dest: updated_base,
                            base,
                            field: field_identifier.name,
                            value,
                        },
                    ));

                    if let Some(allocs) = self.ptg.value_locations.get(&base).cloned() {
                        self.ptg.value_locations.insert(updated_base, allocs);
                    }
                    self.ptg.update_path(
                        base,
                        PathSegment::Field(field_identifier.name),
                        value,
                    );

                    updated_base
                } else {
                    self.report_error_and_get_poison(SemanticError {
                        span: field_identifier.span.clone(),
                        kind: SemanticErrorKind::AccessToUndefinedField(field_identifier),
                    })
                }
            }
            Type::Unknown => self.new_value_id(Type::Unknown),
            _ => panic!(
                "INTERNAL COMPILER ERROR: Expected emit_update_struct_field to be \
                 called on a struct type, found {:?}",
                base_type
            ),
        }
    }
}
