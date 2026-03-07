use inkwell::values::BasicValue;

use crate::{
    codegen::CodeGenerator,
    hir::{instructions::StructInstr, types::checked_type::Type},
};

impl<'ctx> CodeGenerator<'ctx> {
    pub fn emit_struct(&mut self, instr: &StructInstr) {
        match instr {
            StructInstr::Construct { dest, fields } => {
                let struct_ty = self
                    .program
                    .value_types
                    .get(dest)
                    .expect("INTERNAL COMPILER ERROR: Struct type missing");

                let Type::Struct(def_fields) = struct_ty else {
                    panic!("INTERNAL COMPILER ERROR: Construct target is not a struct");
                };

                let layout = self.get_struct_layout(def_fields);

                let ptr = self
                    .builder
                    .build_malloc(layout.llvm_type, "struct_alloc")
                    .expect("Failed to emit malloc");

                for (field_name, val_id) in fields {
                    let (hir_index, field_def) = def_fields
                        .iter()
                        .enumerate()
                        .find(|(_, f)| f.identifier.name == *field_name)
                        .expect("INTERNAL COMPILER ERROR: Field not found in struct definition");

                    let val_ty = self
                        .program
                        .value_types
                        .get(val_id)
                        .expect("INTERNAL COMPILER ERROR: Field value type missing");

                    if val_ty != &field_def.ty.kind {
                        panic!(
                            "INTERNAL COMPILER ERROR: Struct field type mismatch.\n\
                             Field: {:?}\n\
                             Expected: {:?}\n\
                             Got: {:?}",
                            field_name, field_def.ty, val_ty
                        );
                    }

                    let Some(llvm_index) = layout.field_indices[hir_index] else {
                        continue; // ZST field, no physical storage
                    };

                    let val = self.get_val(*val_id).expect(
                        "INTERNAL COMPILER ERROR: ZST field passed non-ZST type check",
                    );

                    let field_ptr = self
                        .builder
                        .build_struct_gep(layout.llvm_type, ptr, llvm_index, "field_ptr")
                        .unwrap();

                    self.builder.build_store(field_ptr, val).unwrap();
                }

                self.fn_values.insert(*dest, ptr.as_basic_value_enum());
            }

            StructInstr::ReadField { dest, base, field } => {
                let base_ptr = self
                    .get_val(*base)
                    .expect("INTERNAL COMPILER ERROR: ReadField base is a ZST")
                    .into_pointer_value();

                let base_ty = self
                    .program
                    .value_types
                    .get(base)
                    .expect("INTERNAL COMPILER ERROR: ReadField base type missing");

                let Type::Struct(def_fields) = base_ty else {
                    panic!("INTERNAL COMPILER ERROR: ReadField base is not a struct");
                };

                let layout = self.get_struct_layout(def_fields);

                let (hir_index, field_def) = def_fields
                    .iter()
                    .enumerate()
                    .find(|(_, f)| f.identifier.name == *field)
                    .expect("INTERNAL COMPILER ERROR: Field not found");

                let Some(llvm_index) = layout.field_indices[hir_index] else {
                    // ZST field - the value is known from the type alone
                    let val = self.get_val(*dest)
                        .expect("INTERNAL COMPILER ERROR: ReadField of ZST field returned no value");
                    self.fn_values.insert(*dest, val);
                    return;
                };

                let field_ptr = self
                    .builder
                    .build_struct_gep(layout.llvm_type, base_ptr, llvm_index, "field_gep")
                    .unwrap();

                let res = self
                    .builder
                    .build_load(
                        self.lower_type(&field_def.ty.kind).unwrap(),
                        field_ptr,
                        "field_val",
                    )
                    .unwrap();

                self.fn_values.insert(*dest, res);
            }

            StructInstr::UpdateField {
                dest,
                base,
                field,
                value,
            } => {
                let base_ptr = self
                    .get_val(*base)
                    .expect("INTERNAL COMPILER ERROR: UpdateField base is a ZST")
                    .into_pointer_value();

                let base_ty = self
                    .program
                    .value_types
                    .get(base)
                    .expect("INTERNAL COMPILER ERROR: UpdateField base type missing");

                let new_val_ty =
                    self.program.value_types.get(value).expect(
                        "INTERNAL COMPILER ERROR: UpdateField value type missing",
                    );

                let Type::Struct(def_fields) = base_ty else {
                    panic!("INTERNAL COMPILER ERROR: UpdateField base is not a struct");
                };

                let layout = self.get_struct_layout(def_fields);

                let (hir_index, field_def) = def_fields
                    .iter()
                    .enumerate()
                    .find(|(_, f)| f.identifier.name == *field)
                    .expect("INTERNAL COMPILER ERROR: Field not found");

                if new_val_ty != &field_def.ty.kind {
                    panic!(
                        "INTERNAL COMPILER ERROR: UpdateField type mismatch.\n\
                         Field: {:?}\n\
                         Expected: {:?}\n\
                         Got: {:?}",
                        field, field_def.ty, new_val_ty
                    );
                }

                let Some(llvm_index) = layout.field_indices[hir_index] else {
                    // ZST field - updating it is a no-op, base pointer is unchanged
                    self.fn_values.insert(*dest, base_ptr.as_basic_value_enum());
                    return;
                };

                let new_val = self.get_val(*value).expect(
                    "INTERNAL COMPILER ERROR: ZST field passed non-ZST type check",
                );

                let field_ptr = self
                    .builder
                    .build_struct_gep(
                        layout.llvm_type,
                        base_ptr,
                        llvm_index,
                        "update_gep",
                    )
                    .unwrap();

                self.builder.build_store(field_ptr, new_val).unwrap();
                self.fn_values.insert(*dest, base_ptr.as_basic_value_enum());
            }
        }
    }
}
