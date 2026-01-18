use crate::{
    globals::next_value_id,
    hir::{
        builders::{
            BasicBlock, BasicBlockId, Builder, Function, InBlock, InGlobal, Module,
            Place, Projection, ValueId,
        },
        errors::SemanticError,
        instructions::{Instruction, Terminator},
        types::{
            checked_declaration::CheckedDeclaration,
            checked_type::{StructKind, Type},
        },
        utils::try_unify_types::narrow_type_at_path,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn as_program(&mut self) -> Builder<'_, InGlobal> {
        Builder {
            context: InGlobal,
            program: self.program,
            errors: self.errors,
            current_scope: self.current_scope.clone(),
        }
    }

    pub fn bb_mut(&mut self) -> &mut BasicBlock {
        self.get_bb_mut(self.context.block_id)
    }

    pub fn get_bb_mut(&mut self, block_id: BasicBlockId) -> &mut BasicBlock {
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

    pub fn bb(&self) -> &BasicBlock {
        self.get_bb(self.context.block_id)
    }

    pub fn get_bb(&self, block_id: BasicBlockId) -> &BasicBlock {
        let func_id = self.context.func_id;

        let decl = self
            .program
            .declarations
            .get(&func_id)
            .expect("INTERNAL COMPILER ERROR: Function not found");

        let func = match decl {
            CheckedDeclaration::Function(f) => f,
            _ => panic!("INTERNAL COMPILER ERROR: Declaration is not a function"),
        };

        func.blocks
            .get(&block_id)
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
        let bb = self.bb_mut();
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
        let bb = self.bb_mut();

        if bb.terminator.is_some() {
            panic!(
                "INTERNAL COMPILER ERROR: Tried to re-set terminator for basic block (ID: {})",
                bb.id.0
            );
        }
    }

    fn get_value_type(&self, id: &ValueId) -> &Type {
        self.program.value_types.get(id)
        .unwrap_or_else(|| panic!("INTERNAL COMPILER ERROR: Expected ValueId({}) to have an associated type", id.0))
    }

    pub fn new_value_id(&mut self, ty: Type) -> ValueId {
        let value_id = next_value_id();
        let this_block_id = self.context.block_id;
        self.get_fn()
            .value_definitions
            .insert(value_id, this_block_id);
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
        let this_block_id = self.context.block_id;
        self.get_bb_mut(target).predecessors.insert(this_block_id);

        self.bb_mut().terminator = Some(Terminator::Jump { target, args });
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
        let this_block_id = self.context.block_id;

        self.get_bb_mut(true_target)
            .predecessors
            .insert(this_block_id);
        self.get_bb_mut(false_target)
            .predecessors
            .insert(this_block_id);

        self.bb_mut().terminator = Some(Terminator::CondJump {
            condition,
            true_target,
            true_args,
            false_target,
            false_args,
        });
    }

    pub fn ret(&mut self, value: Option<ValueId>) {
        self.check_no_terminator();
        self.bb_mut().terminator = Some(Terminator::Return { value })
    }

    pub fn unreachable(&mut self) {
        self.check_no_terminator();
        self.bb_mut().terminator = Some(Terminator::Unreachable)
    }

    pub fn emit_load(&mut self, ptr: ValueId) -> ValueId {
        let ptr_ty = self.get_value_type(&ptr);
        let dest_ty = match ptr_ty {
            Type::Pointer { narrowed_to, .. } => *narrowed_to.clone(),
            _ => panic!("INTERNAL ERROR: Load expected pointer"),
        };
        let destination = self.new_value_id(dest_ty);
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

        let (_, field_constraint) = constraint_struct.fields()[field_index].clone();
        let (_, field_narrowed) = narrowed_struct.fields()[field_index].clone();

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
            self.bb_mut()
                .original_to_local_valueid
                .insert(place.root, narrowed_root_ptr);
        }
    }

    pub fn get_mapped_value(&self, original: ValueId) -> Option<ValueId> {
        self.bb().original_to_local_valueid.get(&original).copied()
    }

    pub fn map_value(&mut self, original: ValueId, local: ValueId) {
        self.bb_mut()
            .original_to_local_valueid
            .insert(original, local);
    }

    pub fn use_value(&mut self, original_value_id: ValueId) -> ValueId {
        if let Some(local_id) = self.get_mapped_value(original_value_id) {
            return local_id;
        }

        let this_block_id = self.context.block_id;
        let f = self.get_fn();
        if let Some(def_block) = f.value_definitions.get(&original_value_id) {
            if *def_block == this_block_id {
                return original_value_id;
            }
        }

        if !self.bb().sealed {
            // We don't know all predecessors yet, so we MUST create a placeholder parameter.
            // We will fill in the terminator arguments later when we seal.
            let ty = self.get_value_type(&original_value_id);
            let param_id = self.append_param(ty.clone());

            self.map_value(original_value_id, param_id);

            self.bb_mut()
                .incomplete_params
                .push((param_id, original_value_id));

            return param_id;
        }

        let ty = self.get_value_type(&original_value_id);
        let param_id = self.append_param(ty.clone());

        self.map_value(original_value_id, param_id);
        self.fill_predecessors(original_value_id, param_id);

        param_id
    }

    pub fn append_param(&mut self, ty: Type) -> ValueId {
        let id = self.new_value_id(ty);
        self.bb_mut().params.push(id);
        id
    }

    pub fn seal(&mut self) {
        if self.bb().sealed {
            return;
        }
        self.bb_mut().sealed = true;

        let params = std::mem::take(&mut self.bb_mut().incomplete_params);
        for (param_id, original_value_id) in params {
            self.fill_predecessors(original_value_id, param_id);
        }
    }

    fn fill_predecessors(&mut self, original_value_id: ValueId, _param_id: ValueId) {
        todo!();

        for pred_id in &self.bb().predecessors {
            // let f = self.get_fn();

            //  How do we get predecessors basicblock builder?
            // let pred_bb = todo!("Get predecessor's basic block builder");
            // let val_in_pred = pred_bb.use_value(original_value_id);

            // self.append_arg_to_terminator(pred_id, &self.context.block_id, val_in_pred);
        }
    }

    pub fn emit_get_element_ptr(&mut self, base_ptr: ValueId, index: ValueId) -> ValueId {
        if let Type::Struct(StructKind::List(item_type)) = self.get_value_type(&base_ptr)
        {
            let destination = self.new_value_id(Type::Pointer {
                constraint: item_type.clone(),
                narrowed_to: item_type.clone(),
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

    pub fn append_arg_to_terminator(
        &mut self,
        from_block: &BasicBlockId,
        to_block: &BasicBlockId,
        arg: ValueId,
    ) {
        let from_bb = self.get_bb_mut(*from_block);
        let terminator = from_bb
            .terminator
            .as_mut()
            .expect("INTERNAL COMPILER ERROR: Terminator not found");

        match terminator {
            Terminator::Jump { target, args } => {
                if target == to_block {
                    args.push(arg);
                } else {
                    panic!("INTERNAL COMPILER ERROR: Invalid 'to_block' argument")
                }
            }
            Terminator::CondJump {
                true_target,
                true_args,
                false_target,
                false_args,
                ..
            } => {
                if true_target == to_block {
                    true_args.push(arg);
                }
                if false_target == to_block {
                    false_args.push(arg);
                }
                if true_target != to_block && false_target != to_block {
                    panic!(
                        "INTERNAL COMPILER ERROR: Invalid 'to_block' argument, didn't \
                         match neither 'true_target' nor 'false_target'"
                    )
                }
            }
            _ => {
                panic!(
                        "INTERNAL COMPILER ERROR: Invalid terminator kind for BasicBlockId({}), expected Terminator::Jump or Terminator::CondJump.", from_block.0
                    )
            }
        }
    }

    fn get_terminator_arg_for_param(
        &self,
        from_block: BasicBlockId,
        to_block: BasicBlockId,
        param_index: usize,
    ) -> ValueId {
        let terminator = self
            .get_bb(from_block)
            .terminator
            .as_ref()
            .expect("Block must have terminator");

        match terminator {
            Terminator::Jump { target, args } => {
                assert_eq!(target, &to_block);
                args[param_index].clone()
            }
            Terminator::CondJump {
                true_target,
                true_args,
                false_target,
                false_args,
                ..
            } => {
                if true_target == &to_block {
                    true_args[param_index].clone()
                } else if false_target == &to_block {
                    false_args[param_index].clone()
                } else {
                    panic!("Inconsistent CFG: target block not found in CondJump")
                }
            }
            _ => panic!("Terminator type does not support block arguments"),
        }
    }

    /// Records a semantic error and returns a new "poison" Value of type Unknown
    /// The caller is responsible for immediately returning the poison Value
    pub fn report_error_and_get_poison(&mut self, error: SemanticError) -> ValueId {
        self.as_program().errors.push(error);
        self.new_value_id(Type::Unknown)
    }
}
