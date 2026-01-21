use crate::{
    ast::{IdentifierNode, Span},
    globals::{next_value_id, COMMON_IDENTIFIERS},
    hir::{
        builders::{
            BasicBlock, BasicBlockId, Builder, Function, InBlock, InFunction, InGlobal,
            InModule, Phi, Place, Projection, ValueId,
        },
        errors::{SemanticError, SemanticErrorKind},
        types::{checked_declaration::CheckedDeclaration, checked_type::Type},
        utils::try_unify_types::narrow_type_at_path,
    },
    unwrap_or_poison,
};

impl<'a> Builder<'a, InBlock> {
    pub fn as_program(&mut self) -> Builder<'_, InGlobal> {
        Builder {
            context: InGlobal,
            program: self.program,
            errors: self.errors,
            current_scope: self.current_scope.clone(),
            definitions: self.definitions,
            incomplete_phis: self.incomplete_phis,
        }
    }

    pub fn as_module(&mut self) -> Builder<'_, InModule> {
        Builder {
            context: InModule {
                path: self.context.path.clone(),
            },
            program: self.program,
            errors: self.errors,
            current_scope: self.current_scope.clone(),
            definitions: self.definitions,
            incomplete_phis: self.incomplete_phis,
        }
    }

    pub fn as_fn(&mut self) -> Builder<'_, InFunction> {
        Builder {
            context: InFunction {
                path: self.context.path.clone(),
                func_id: self.context.func_id,
            },
            program: self.program,
            errors: self.errors,
            current_scope: self.current_scope.clone(),
            definitions: self.definitions,
            incomplete_phis: self.incomplete_phis,
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

        match decl {
            CheckedDeclaration::Function(f) => f
                .blocks
                .get_mut(&block_id)
                .expect("INTERNAL COMPILER ERROR: Block not found"),
            _ => panic!("INTERNAL COMPILER ERROR: Declaration is not a function"),
        }
    }

    pub fn get_bb(&self, block_id: BasicBlockId) -> &BasicBlock {
        let func_id = self.context.func_id;

        let decl = self
            .program
            .declarations
            .get(&func_id)
            .expect("INTERNAL COMPILER ERROR: Function not found");

        match decl {
            CheckedDeclaration::Function(f) => f
                .blocks
                .get(&block_id)
                .expect("INTERNAL COMPILER ERROR: Block not found"),
            _ => panic!("INTERNAL COMPILER ERROR: Declaration is not a function"),
        }
    }

    pub fn bb(&self) -> &BasicBlock {
        self.get_bb(self.context.block_id)
    }

    pub fn get_fn(&mut self) -> &mut Function {
        let func_id = self.context.func_id;

        match self.program.declarations.get_mut(&func_id).unwrap() {
            CheckedDeclaration::Function(f) => f,
            _ => panic!("INTERNAL COMPILER ERROR: Declaration is not a function"),
        }
    }

    pub fn get_value_type(&self, id: &ValueId) -> &Type {
        self.program.value_types.get(id).unwrap_or_else(|| {
            panic!("INTERNAL COMPILER ERROR: ValueId({}) has no type", id.0)
        })
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

    pub fn get_list_buffer_ptr(&mut self, list_ptr: ValueId, span: Span) -> ValueId {
        let ptr_field_node = IdentifierNode {
            name: COMMON_IDENTIFIERS.ptr,
            span: span.clone(),
        };
        let internal_ptr_ptr =
            unwrap_or_poison!(self, self.get_field_ptr(list_ptr, &ptr_field_node));
        unwrap_or_poison!(self, self.load(internal_ptr_ptr, span))
    }

    pub fn read_place(&mut self, place: Place, span: Span) -> ValueId {
        let mut current = self.use_var(place.root);

        for proj in place.projections {
            let current_ty = self.get_value_type(&current).clone();

            match proj {
                Projection::Field(field_node) => {
                    if field_node.name == COMMON_IDENTIFIERS.value {
                        if let Type::Pointer { narrowed_to, .. } = current_ty {
                            if matches!(*narrowed_to, Type::Tag(_) | Type::Union(_)) {
                                current = unwrap_or_poison!(
                                    self,
                                    self.load(current, span.clone())
                                );
                            }
                        }

                        current =
                            self.emit_get_tag_payload(current).unwrap_or_else(|| {
                                self.report_error_and_get_poison(SemanticError {
                                    kind: SemanticErrorKind::AccessToUndefinedField(
                                        field_node,
                                    ),
                                    span: span.clone(),
                                })
                            });
                    } else {
                        current = unwrap_or_poison!(
                            self,
                            self.get_field_ptr(current, &field_node)
                        );
                    }
                }
                Projection::Index(idx_val, s) => {
                    let buffer_ptr = self.get_list_buffer_ptr(current, s.clone());
                    current =
                        unwrap_or_poison!(self, self.ptr_offset(buffer_ptr, idx_val, s));
                }
                Projection::Deref => {
                    current = unwrap_or_poison!(self, self.load(current, span.clone()));
                }
            }
        }

        if matches!(self.get_value_type(&current), Type::Pointer { .. }) {
            unwrap_or_poison!(self, self.load(current, span))
        } else {
            current
        }
    }

    pub fn write_place(&mut self, place: Place, value: ValueId, span: Span) {
        if place.projections.is_empty() {
            self.definitions
                .entry(self.context.block_id)
                .or_default()
                .insert(place.root, value);
            return;
        }

        let mut current = self.use_var(place.root);
        let last_idx = place.projections.len() - 1;

        for i in 0..last_idx {
            let current_ty = self.get_value_type(&current).clone();
            match &place.projections[i] {
                Projection::Deref => {
                    current = unwrap_or_poison!(self, self.load(current, span.clone()));
                }
                Projection::Field(f) => {
                    if f.name == COMMON_IDENTIFIERS.value {
                        if let Type::Pointer { .. } = current_ty {
                            current =
                                unwrap_or_poison!(self, self.load(current, span.clone()));
                        }
                        current =
                            self.emit_get_tag_payload(current).unwrap_or_else(|| {
                                self.report_error_and_get_poison(SemanticError {
                                    kind: SemanticErrorKind::AccessToUndefinedField(
                                        f.clone(),
                                    ),
                                    span: span.clone(),
                                })
                            });
                    } else {
                        current = unwrap_or_poison!(self, self.get_field_ptr(current, f));
                        current =
                            unwrap_or_poison!(self, self.load(current, span.clone()));
                    }
                }
                Projection::Index(idx, s) => {
                    let buffer = self.get_list_buffer_ptr(current, s.clone());
                    let elem_ptr =
                        unwrap_or_poison!(self, self.ptr_offset(buffer, *idx, s.clone()));
                    current = unwrap_or_poison!(self, self.load(elem_ptr, span.clone()));
                }
            }
        }

        let final_proj = &place.projections[last_idx];

        let final_ptr = match final_proj {
            Projection::Deref => current,
            Projection::Field(f) => {
                if f.name == COMMON_IDENTIFIERS.value {
                    panic!("Semantic Error: Direct assignment to '.value' is not supported. Assign a new Tag to the parent field instead.");
                }
                unwrap_or_poison!(self, self.get_field_ptr(current, f))
            }
            Projection::Index(idx, s) => {
                let buffer = self.get_list_buffer_ptr(current, s.clone());
                unwrap_or_poison!(self, self.ptr_offset(buffer, *idx, s.clone()))
            }
        };

        self.store(final_ptr, value, span.clone());

        let is_narrowable = !place
            .projections
            .iter()
            .any(|p| matches!(p, Projection::Index(..)));

        if is_narrowable {
            let source_ty = self.get_value_type(&value);
            let root_ty = self.get_value_type(&place.root);

            let narrowed_root_ty =
                narrow_type_at_path(root_ty, &place.projections, source_ty);

            let current_root = self.use_var(place.root);
            let refined_val = self.emit_refine_type(current_root, narrowed_root_ty);

            self.definitions
                .entry(self.context.block_id)
                .or_default()
                .insert(place.root, refined_val);
        }
    }

    pub fn use_var(&mut self, identity_id: ValueId) -> ValueId {
        if let Some(defs) = self.definitions.get(&self.context.block_id) {
            if let Some(val) = defs.get(&identity_id) {
                return *val;
            }
        }
        self.read_var_recursive(self.context.block_id, identity_id)
    }

    fn read_var_recursive(
        &mut self,
        block_id: BasicBlockId,
        identity_id: ValueId,
    ) -> ValueId {
        let val: ValueId;
        if !self.get_bb(block_id).sealed {
            val = self.new_value_id(self.get_value_type(&identity_id).clone());
            self.incomplete_phis
                .entry(block_id)
                .or_default()
                .push((val, identity_id));
        } else {
            let preds: Vec<BasicBlockId> =
                self.get_bb(block_id).predecessors.iter().cloned().collect();
            if preds.len() == 1 {
                val = self.read_var_recursive(preds[0], identity_id);
            } else {
                val = self.new_value_id(self.get_value_type(&identity_id).clone());
                self.definitions
                    .entry(block_id)
                    .or_default()
                    .insert(identity_id, val);
                self.add_phi_operands(block_id, identity_id, val);
            }
        }
        self.definitions
            .entry(block_id)
            .or_default()
            .insert(identity_id, val);
        val
    }

    fn add_phi_operands(
        &mut self,
        block_id: BasicBlockId,
        identity_id: ValueId,
        phi_val: ValueId,
    ) {
        let preds: Vec<BasicBlockId> =
            self.get_bb(block_id).predecessors.iter().cloned().collect();
        let mut operands = Vec::new();
        for pred in preds {
            let val = self.read_var_recursive(pred, identity_id);
            operands.push(Phi {
                from: pred,
                value: val,
            });
        }
        self.get_bb_mut(block_id).phis.push((phi_val, operands));
    }

    pub fn use_basic_block(&mut self, block_id: BasicBlockId) {
        self.context.block_id = block_id;
    }

    pub fn seal(&mut self) {
        if self.bb().sealed {
            return;
        }

        let block_id = self.context.block_id;
        let incomplete = self.incomplete_phis.remove(&block_id).unwrap_or_default();
        for (phi_val, identity_id) in incomplete {
            self.add_phi_operands(block_id, identity_id, phi_val);
        }
        self.bb_mut().sealed = true;
    }

    pub fn seal_block(&mut self, block_id: BasicBlockId) {
        let old_block = self.context.block_id;
        self.context.block_id = block_id;
        self.seal();
        self.context.block_id = old_block;
    }

    pub fn report_error_and_get_poison(&mut self, error: SemanticError) -> ValueId {
        self.as_program().errors.push(error);
        self.new_value_id(Type::Unknown)
    }
}
