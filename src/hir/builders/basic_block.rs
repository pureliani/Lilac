use crate::hir::{
    builders::{BasicBlock, BasicBlockId, Builder, Function, InBlock, Module, ValueId},
    errors::SemanticError,
    globals::next_value_id,
    instructions::{Instruction, Terminator},
    types::{checked_declaration::CheckedDeclaration, checked_type::Type},
};

impl<'a> Builder<'a, InBlock> {
    pub fn bb(&mut self) -> &mut BasicBlock {
        self.get_bb(self.context.block_id)
    }

    pub fn get_bb(&mut self, block_id: BasicBlockId) -> &mut BasicBlock {
        let func_id = self.context.func_id;

        let decl = self
            .program
            .declarations
            .get_mut(&func_id)
            .expect("INTERNAL COMPILER ERROR: Function not found");

        let func = match decl {
            CheckedDeclaration::Function(f) => f,
            _ => panic!("INTERNAL COMPILER ERROR: Declaration is not a function"),
        };

        func.blocks
            .get_mut(&block_id)
            .expect("INTERNAL COMPILER ERROR: Block not found in function")
    }

    pub fn get_fn(&mut self) -> &mut Function {
        let func_id = self.context.func_id;

        match self.program.declarations.get_mut(&func_id).unwrap() {
            CheckedDeclaration::Function(f) => f,
            _ => panic!("INTERNAL COMPILER ERROR: Declaration is not a function"),
        }
    }

    pub fn get_module(&mut self) -> &mut Module {
        self.program
            .modules
            .get_mut(&self.context.path)
            .expect("INTERNAL COMPILER ERROR: Module not found")
    }

    fn push_instruction(&mut self, instruction: Instruction) {
        let bb = self.bb();
        if bb.terminator.is_some() {
            panic!(
                "INTERNAL COMPILER ERROR: Attempted to add instruction to a basic block \
                 (ID: {}) that has already been terminated",
                bb.id.0
            );
        }

        bb.instructions.push(instruction);
    }

    fn check_no_terminator(&mut self) {
        let bb = self.bb();

        if bb.terminator.is_some() {
            panic!(
                "INTERNAL COMPILER ERROR: Tried to re-set terminator for basic block (ID: {})",
                bb.id.0
            );
        }
    }

    fn get_value_type(&mut self, id: &ValueId) -> &Type {
        self.program.value_types.get(id)
        .unwrap_or_else(|| panic!("INTERNAL COMPILER ERROR: Expected ValueId({}) to have an associated type", id.0))
    }

    pub fn new_value_id(&mut self, ty: Type) -> ValueId {
        let value_id = next_value_id();
        let bb = self.bb();

        self.get_fn()
            .value_definitions
            .insert(value_id, self.context.block_id);
        self.program.value_types.insert(value_id, ty);

        value_id
    }

    pub fn emit_stack_alloc(&mut self, ty: Type, count: usize) -> ValueId {
        let destination = self.new_value_id(Type::Pointer {
            constraint: Box::new(ty.clone()),
            narrowed_to: Box::new(ty),
        });
        self.push_instruction(Instruction::StackAlloc { destination, count });

        destination
    }

    pub fn emit_heap_alloc(&mut self, ty: Type, count: ValueId) -> ValueId {
        // let count_type = ctx.program_builder.get_value_type(&count);
        // let expected_count_type = Type::USize;
        // if !check_is_assignable(&count_type, &expected_count_type) {
        //     return Err(SemanticError {
        //         span: Span::default(), // TODO: Fix span propagation
        //         kind: SemanticErrorKind::TypeMismatch {
        //             expected: expected_count_type,
        //             received: count_type,
        //         },
        //     });
        // }

        let destination = self.new_value_id(Type::Pointer {
            constraint: Box::new(ty.clone()),
            narrowed_to: Box::new(ty),
        });
        self.push_instruction(Instruction::HeapAlloc { destination, count });

        destination
    }

    pub fn emit_refine(&mut self, target: ValueId, new_type: Type) {
        unimplemented!()
    }

    pub fn jmp(&mut self, target: BasicBlockId, args: Vec<ValueId>) {
        self.check_no_terminator();
        let mut f = self.get_fn();

        self.get_bb(target)
            .predecessors
            .insert(self.context.block_id);

        self.bb().terminator = Some(Terminator::Jump { target, args });
    }

    pub fn cond_jmp(
        &mut self,
        condition: ValueId,
        true_target: BasicBlockId,
        true_args: Vec<ValueId>,
        false_target: BasicBlockId,
        false_args: Vec<ValueId>,
    ) {
        self.check_no_terminator();

        self.get_bb(true_target)
            .predecessors
            .insert(self.context.block_id);
        self.get_bb(false_target)
            .predecessors
            .insert(self.context.block_id);

        self.bb().terminator = Some(Terminator::CondJump {
            condition,
            true_target,
            true_args,
            false_target,
            false_args,
        });
    }

    pub fn ret(&mut self, value: Option<ValueId>) {
        self.check_no_terminator();
        self.bb().terminator = Some(Terminator::Return { value })
    }

    pub fn unreachable(&mut self) {
        self.check_no_terminator();
        self.bb().terminator = Some(Terminator::Unreachable)
    }

    pub fn emit_load(&mut self, ptr: ValueId) -> ValueId {
        let ptr_ty = self.get_value_type(&ptr);
        let dest_ty = match ptr_ty {
            Type::Pointer { narrowed_to, .. } => *narrowed_to,
            _ => panic!("INTERNAL ERROR: Load expected pointer"),
        };
        let destination = self.new_value_id(*dest_ty);
        self.push_instruction(Instruction::Load { destination, ptr });
        destination
    }

    pub fn emit_store(&mut self, ptr: ValueId, value: ValueId) {
        self.push_instruction(Instruction::Store { ptr, value });
    }

    pub fn emit_get_field_ptr_by_index(
        &mut self,
        base_ptr: ValueId,
        field_index: usize,
    ) -> ValueId {
        let current_ty = self.get_value_type(&base_ptr);

        let (constraint_struct, narrowed_struct) = match &current_ty {
            Type::Pointer {
                constraint,
                narrowed_to,
            } => match (&**constraint, &**narrowed_to) {
                (Type::Struct(c), Type::Struct(n)) => (c, n),
                _ => panic!("Expected pointer to struct, found {:?}", current_ty),
            },
            _ => panic!("Expected pointer, found {:?}", current_ty),
        };

        let prog = self.get_program();
        let prog_borrow = prog.borrow();

        let (_, field_constraint) =
            constraint_struct.fields(&prog_borrow)[field_index].clone();
        let (_, field_narrowed) =
            narrowed_struct.fields(&prog_borrow)[field_index].clone();

        let destination = self.new_value_id(Type::Pointer {
            constraint: Box::new(field_constraint),
            narrowed_to: Box::new(field_narrowed),
        });

        self.push_instruction(Instruction::GetFieldPtr {
            destination,
            base_ptr,
            field_index,
        });

        destination
    }

    pub fn read_place(&mut self, place: Place) -> ValueId {
        let mut current = self.use_value(place.root);

        for proj in place.projections {
            match proj {
                Projection::Deref => {
                    current = self.emit_load(current);
                }
                Projection::Field(idx) => {
                    current = self.emit_get_field_ptr_by_index(current, idx);
                }
                Projection::Index(idx_val) => {
                    current = self.emit_get_element_ptr(current, idx_val);
                }
            }
        }

        self.emit_load(current)
    }

    pub fn write_place(&mut self, place: Place, value: ValueId) {
        let mut current = self.use_value(place.root);

        // 1. Traverse to the target location
        for proj in &place.projections {
            match proj {
                Projection::Deref => {
                    current = self.emit_load(current);
                }
                Projection::Field(idx) => {
                    current = self.emit_get_field_ptr_by_index(current, *idx);
                }
                Projection::Index(idx_val) => {
                    current = self.emit_get_element_ptr(current, *idx_val);
                }
            }
        }

        // 2. Physical Store
        self.emit_store(current, value);

        // 3. Narrowing
        // We only narrow if the path doesn't contain Index (which is hard to track)
        let is_narrowable = !place
            .projections
            .iter()
            .any(|p| matches!(p, Projection::Index(_)));

        if is_narrowable {
            let source_ty = self.get_value_type(&value);
            let root_ty = self.get_value_type(&place.root);

            let narrowed_root_ty =
                narrow_type_at_path(&root_ty, &place.projections, &source_ty);

            let current_root = self.use_value(place.root);
            let narrowed_root_ptr = self.emit_type_cast(current_root, narrowed_root_ty);
            self.original_to_local_valueid
                .insert(place.root, narrowed_root_ptr);
        }
    }

    pub fn get_mapped_value(&self, original: ValueId) -> Option<ValueId> {
        self.original_to_local_valueid.get(&original).copied()
    }

    pub fn map_value(&mut self, original: ValueId, local: ValueId) {
        self.original_to_local_valueid.insert(original, local);
    }

    pub fn use_value(&mut self, original_value_id: ValueId) -> ValueId {
        let fn_rc = self.get_fn();
        let fn_borrow = fn_rc.borrow();

        if let Some(local_id) = self.get_mapped_value(original_value_id) {
            return local_id;
        }

        if let Some(def_block) = fn_borrow.value_definitions.get(&original_value_id) {
            if *def_block == self.id {
                return original_value_id;
            }
        }

        if !self.sealed {
            // We don't know all predecessors yet, so we MUST create a placeholder parameter.
            // We will fill in the terminator arguments later when we seal.
            let ty = self.get_value_type(&original_value_id);
            let param_id = self.append_param(ty);

            self.map_value(original_value_id, param_id);

            self.incomplete_params.push((param_id, original_value_id));

            return param_id;
        }

        let ty = self.get_value_type(&original_value_id);
        let param_id = self.append_param(ty);

        self.map_value(original_value_id, param_id);
        self.fill_predecessors(original_value_id, param_id);

        param_id
    }

    pub fn append_param(&mut self, ty: Type) -> ValueId {
        let id = self.new_value_id(ty);
        self.params.push(id);
        id
    }

    pub fn seal(&mut self) {
        if self.sealed {
            return;
        }
        self.sealed = true;

        let params = std::mem::take(&mut self.incomplete_params);
        for (param_id, original_value_id) in params {
            self.fill_predecessors(original_value_id, param_id);
        }
    }

    fn fill_predecessors(&mut self, original_value_id: ValueId, _param_id: ValueId) {
        for pred_id in &self.predecessors {
            let fn_rc = self.get_fn();
            let mut fn_borrow = fn_rc.borrow_mut();

            let pred_bb = fn_borrow.get_bb(pred_id);
            let val_in_pred = pred_bb.use_value(original_value_id);

            fn_borrow.append_arg_to_terminator(pred_id, &self.id, val_in_pred);
        }
    }

    pub fn emit_get_element_ptr(&mut self, base_ptr: ValueId, index: ValueId) -> ValueId {
        if let Type::Struct(StructKind::List(item_type)) = self.get_value_type(&base_ptr)
        {
            let destination = self.new_value_id(Type::Pointer {
                constraint: item_type.clone(),
                narrowed_to: item_type,
            });
            self.push_instruction(Instruction::GetElementPtr {
                destination,
                base_ptr,
                index,
            });
            destination
        } else {
            panic!("INTERNAL COMPILER ERROR: Cannot use emit_get_element_ptr on non-list type")
        }
    }

    pub fn emit_type_cast(&mut self, operand: ValueId, target_type: Type) -> ValueId {
        let destination = self.new_value_id(target_type.clone());
        self.push_instruction(Instruction::TypeCast {
            destination,
            operand,
            target_type,
        });
        destination
    }

    /// Records a semantic error and returns a new "poison" Value of type Unknown
    /// The caller is responsible for immediately returning the poison Value
    pub fn report_error_and_get_poison(&mut self, error: SemanticError) -> ValueId {
        self.get_module().borrow_mut().errors.push(error);
        self.new_value_id(Type::Unknown)
    }
}
