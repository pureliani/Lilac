use crate::{
    ast::IdentifierNode,
    compile::interner::StringId,
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        instructions::{Instruction, StructInstr},
        types::checked_type::Type,
        utils::adjustments::check_structural_compatibility,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn emit_struct_init(fields: Vec<(StringId, ValueId)>) -> ValueId {
        todo!()
    }

    pub fn emit_read_struct_field(
        &mut self,
        base: ValueId,
        field_identifier: IdentifierNode,
    ) -> ValueId {
        let source_type = self.get_value_type(base);

        if let Type::Struct(_) = source_type {
            let field = source_type.get_field(&field_identifier.name);

            if let Some((_, ty)) = field {
                let dest = self.new_value_id(ty);
                self.push_instruction(Instruction::Struct(StructInstr::ReadField {
                    dest,
                    base,
                    field: field_identifier.name,
                }));

                dest
            } else {
                return self.report_error_and_get_poison(SemanticError {
                    span: field_identifier.span.clone(),
                    kind: SemanticErrorKind::AccessToUndefinedField(field_identifier),
                });
            }
        } else {
            panic!("INTERNAL COMPILER ERROR: Expected emit_read_struct_field to be called on a struct type")
        }
    }

    pub fn emit_update_struct_field(
        &mut self,
        base: ValueId,
        field_identifier: IdentifierNode,
        value: ValueId,
    ) -> ValueId {
        todo!("add validations");
        let updated_base = self.new_value_id(Type::Unknown);
        self.push_instruction(Instruction::Struct(StructInstr::UpdateField {
            dest: updated_base,
            base,
            field: field_identifier.name,
            value,
        }));
        updated_base
    }
}
