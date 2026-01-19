use crate::{
    globals::next_value_id,
    hir::{
        builders::{
            BasicBlock, BasicBlockId, Builder, Function, InBlock, InGlobal, Module,
            Place, Projection, ValueId,
        },
        errors::SemanticError,
        instructions::Terminator,
        types::{checked_declaration::CheckedDeclaration, checked_type::Type},
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

    pub fn get_value_type(&self, id: &ValueId) -> &Type {
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

    pub fn read_place(&mut self, place: Place) -> ValueId {
        let mut current = self.use_value(place.root);

        for proj in &place.projections {
            match proj {
                Projection::Deref => {
                    current = self.emit_load(current);
                }
                Projection::Field(field_name) => {
                    current = self.get_field_ptr(current, field_name).expect(
                        "INTERNAL COMPILER ERROR: Invalid field in Place projection",
                    );
                }
                Projection::Index(idx_val, _) => {
                    current = self.emit_get_element_ptr(current, *idx_val);
                }
            }
        }

        self.emit_load(current)
    }

    pub fn write_place(&mut self, place: Place, value: ValueId) {
        let mut current = self.use_value(place.root);

        for proj in &place.projections {
            match proj {
                Projection::Deref => {
                    current = self.emit_load(current);
                }
                Projection::Field(field_name) => {
                    current = self.get_field_ptr(current, field_name).expect(
                        "INTERNAL COMPILER ERROR: Invalid field in Place projection",
                    );
                }
                Projection::Index(idx_val, _) => {
                    current = self.emit_get_element_ptr(current, *idx_val);
                }
            }
        }

        self.emit_store(current, value);

        let is_narrowable = !place
            .projections
            .iter()
            .any(|p| matches!(p, Projection::Index(_, _)));

        if is_narrowable {
            let source_ty = self.get_value_type(&value);
            let root_ty = self.get_value_type(&place.root);

            let narrowed_root_ty =
                narrow_type_at_path(&root_ty, &place.projections, &source_ty);

            let current_root = self.use_value(place.root);
            let narrowed_root_ptr = self.emit_refine_type(current_root, narrowed_root_ty);
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

    fn fill_predecessors(&mut self, original_value_id: ValueId, param_id: ValueId) {
        let predecessors: Vec<BasicBlockId> =
            self.bb().predecessors.iter().cloned().collect();
        let this_block_id = self.context.block_id;

        for pred_id in predecessors {
            let old_block_id = self.context.block_id;
            self.context.block_id = pred_id.clone();

            let val_in_pred = self.use_value(original_value_id);

            self.context.block_id = old_block_id;

            self.append_arg_to_terminator(
                &pred_id,
                &this_block_id,
                param_id,
                val_in_pred,
            );
        }
    }

    pub fn append_arg_to_terminator(
        &mut self,
        from_block: &BasicBlockId,
        to_block: &BasicBlockId,
        param_id: ValueId,
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
                    args.insert(param_id, arg);
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
                let mut matched = false;
                if true_target == to_block {
                    true_args.insert(param_id, arg);
                    matched = true;
                }
                if false_target == to_block {
                    false_args.insert(param_id, arg);
                    matched = true;
                }
                if !matched {
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
        param_id: ValueId,
    ) -> ValueId {
        let terminator = self
            .get_bb(from_block)
            .terminator
            .as_ref()
            .expect("Block must have terminator");

        match terminator {
            Terminator::Jump { target, args } => {
                assert_eq!(target, &to_block);
                *args
                    .get(&param_id)
                    .expect("INTERNAL COMPILER ERROR: Missing argument for parameter")
            }
            Terminator::CondJump {
                true_target,
                true_args,
                false_target,
                false_args,
                ..
            } => {
                if true_target == &to_block {
                    *true_args.get(&param_id).expect("INTERNAL COMPILER ERROR: Missing argument for parameter in true branch")
                } else if false_target == &to_block {
                    *false_args.get(&param_id).expect("INTERNAL COMPILER ERROR: Missing argument for parameter in false branch")
                } else {
                    panic!("INTERNAL COMPILER ERROR: Inconsistent CFG, target block not found in CondJump")
                }
            }
            _ => panic!("INTERNAL COMPILER ERROR: Terminator type does not support block arguments"),
        }
    }

    /// Records a semantic error and returns a new "poison" Value of type Unknown
    /// The caller is responsible for immediately returning the poison Value
    pub fn report_error_and_get_poison(&mut self, error: SemanticError) -> ValueId {
        self.as_program().errors.push(error);
        self.new_value_id(Type::Unknown)
    }
}
