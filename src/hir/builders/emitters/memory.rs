use crate::{
    ast::{IdentifierNode, Span},
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        instructions::{Instruction, MemoryInstr},
        types::checked_type::Type,
        utils::adjustments::check_is_assignable,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn emit_stack_alloc(&mut self, ty: Type, count: usize) -> ValueId {
        let dest = self.new_value_id(Type::Pointer(Box::new(ty)));
        self.push_instruction(Instruction::Memory(MemoryInstr::StackAlloc {
            dest,
            count,
        }));
        dest
    }

    pub fn emit_heap_alloc(&mut self, ty: Type, count: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Pointer(Box::new(ty)));
        self.push_instruction(Instruction::Memory(MemoryInstr::HeapAlloc {
            dest,
            count,
        }));
        dest
    }

    pub fn emit_load(&mut self, ptr: ValueId) -> ValueId {
        let ptr_ty = self.get_value_type(&ptr);
        let dest_ty = if let Type::Pointer(to) = ptr_ty {
            *to.clone()
        } else {
            panic!("INTERNAL COMPILER ERROR: Load expected pointer");
        };

        let dest = self.new_value_id(dest_ty);
        self.push_instruction(Instruction::Memory(MemoryInstr::Load { dest, ptr }));
        dest
    }

    pub fn emit_store(&mut self, ptr: ValueId, value: ValueId, span: Span) {
        let ptr_ty = self.get_value_type(&ptr).clone();
        let val_ty = self.get_value_type(&value).clone();

        if let Type::Pointer(to) = ptr_ty {
            if !check_is_assignable(&val_ty, &to) {
                self.as_program().errors.push(SemanticError {
                    kind: SemanticErrorKind::TypeMismatch {
                        expected: *to.clone(),
                        received: val_ty.clone(),
                    },
                    span,
                });
            }

            self.push_instruction(Instruction::Memory(MemoryInstr::Store { ptr, value }));
        } else {
            panic!("INTERNAL COMPILER ERROR: Store expected pointer");
        };
    }

    /// offset = ptr<T> + index * sizeof(T)
    pub fn ptr_offset(
        &mut self,
        base_ptr: ValueId,
        index: ValueId,
        span: Span,
    ) -> Result<ValueId, SemanticError> {
        let ptr_ty = self.get_value_type(&base_ptr).clone();
        let index_ty = self.get_value_type(&index);

        if !check_is_assignable(index_ty, &Type::USize) {
            return Err(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: Type::USize,
                    received: index_ty.clone(),
                },
                span,
            });
        }

        if let Type::Pointer(to) = ptr_ty {
            let dest_ty = Type::Pointer(to);
            let dest = self.new_value_id(dest_ty);

            self.push_instruction(Instruction::Memory(MemoryInstr::PtrOffset {
                dest,
                base_ptr,
                index,
            }));

            Ok(dest)
        } else {
            panic!(
                "INTERNAL COMPILER ERROR: Memory offset expected base to be a pointer"
            );
        }
    }

    pub fn get_field_ptr(
        &mut self,
        base_ptr: ValueId,
        field_name: &IdentifierNode,
    ) -> Result<ValueId, SemanticError> {
        let current_ty = self.get_value_type(&base_ptr);

        let struct_kind = match current_ty {
            Type::Pointer(to) => match &**to {
                Type::Struct(s) => s,
                _ => panic!("Expected pointer to struct, found {:?}", current_ty),
            },
            _ => {
                panic!("Expected pointer, found {:?}", current_ty);
            }
        };

        let field_index = match struct_kind.get_field(&field_name.name) {
            Some((idx, _)) => idx,
            None => {
                return Err(SemanticError {
                    span: field_name.span.clone(),
                    kind: SemanticErrorKind::AccessToUndefinedField(field_name.clone()),
                });
            }
        };

        let (_, field_type) = struct_kind.fields()[field_index].clone();

        let dest = self.new_value_id(Type::Pointer(Box::new(field_type)));

        self.push_instruction(Instruction::Memory(MemoryInstr::GetFieldPtr {
            dest,
            base_ptr,
            field_index,
        }));

        Ok(dest)
    }
}
