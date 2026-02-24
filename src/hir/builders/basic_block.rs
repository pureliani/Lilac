use std::collections::HashSet;

use crate::{
    ast::{DeclarationId, Span},
    globals::next_value_id,
    hir::{
        builders::{
            BasicBlock, BasicBlockId, Builder, Function, InBlock, InFunction, InGlobal,
            InModule, PhiSource, ValueId,
        },
        instructions::Terminator,
        types::{checked_declaration::CheckedDeclaration, checked_type::Type},
        utils::{
            adjustments::check_structural_compatibility, type_to_string::type_to_string,
        },
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn as_program(&mut self) -> Builder<'_, InGlobal> {
        Builder {
            context: InGlobal,
            program: self.program,
            errors: self.errors,
            current_scope: self.current_scope.clone(),
            current_defs: self.current_defs,
            incomplete_phis: self.incomplete_phis,
            type_predicates: self.type_predicates,
            ptg: self.ptg,
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
            current_defs: self.current_defs,
            incomplete_phis: self.incomplete_phis,
            type_predicates: self.type_predicates,
            ptg: self.ptg,
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
            current_defs: self.current_defs,
            incomplete_phis: self.incomplete_phis,
            type_predicates: self.type_predicates,
            ptg: self.ptg,
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

    pub fn get_value_type(&self, id: ValueId) -> &Type {
        self.program.value_types.get(&id).unwrap_or_else(|| {
            panic!("INTERNAL COMPILER ERROR: ValueId({}) has no type", id.0)
        })
    }

    pub fn write_variable(
        &mut self,
        variable: DeclarationId,
        block: BasicBlockId,
        value: ValueId,
    ) {
        self.current_defs
            .entry(block)
            .or_default()
            .insert(variable, value);
    }

    pub fn read_variable(
        &mut self,
        variable: DeclarationId,
        block: BasicBlockId,
        span: Span,
    ) -> ValueId {
        if let Some(block_defs) = self.current_defs.get(&block) {
            if let Some(val) = block_defs.get(&variable) {
                return *val;
            }
        }
        self.read_variable_recursive(variable, block, span)
    }

    fn read_variable_recursive(
        &mut self,
        variable: DeclarationId,
        block: BasicBlockId,
        span: Span,
    ) -> ValueId {
        let val_id;
        let sealed = self.get_bb(block).sealed;
        let predecessors: Vec<BasicBlockId> =
            self.get_bb(block).predecessors.iter().cloned().collect();

        if !sealed {
            val_id = self.new_value_id(Type::Unknown);
            self.incomplete_phis.entry(block).or_default().push((
                val_id,
                variable,
                span.clone(),
            ));
        } else if predecessors.len() == 1 {
            val_id = self.read_variable(variable, predecessors[0], span.clone());
        } else if predecessors.is_empty() {
            panic!("INTERNAL COMPILER ERROR: Uninitialized local variable read");
        } else {
            val_id = self.new_value_id(Type::Unknown);
            self.write_variable(variable, block, val_id);
            self.resolve_phi(block, val_id, variable, span.clone());
        }

        self.write_variable(variable, block, val_id);
        val_id
    }

    pub fn insert_phi(
        &mut self,
        basic_block_id: BasicBlockId,
        phi_id: ValueId,
        sources: HashSet<PhiSource>,
    ) {
        assert!(
            !sources.is_empty(),
            "Phi node must have at least one source"
        );

        let first_source = sources.iter().next().unwrap();
        let expected_type = self.get_value_type(first_source.value);

        for source in &sources {
            let current_type = self.get_value_type(source.value);

            if expected_type != current_type {
                panic!(
                    "INTERNAL COMPILER ERROR: Phi node type mismatch.\nPhi ID: \
                     {:?}\nBlock ID: {:?}\nExpected Type: {}\nFound Type: {} (from \
                     block {:?})",
                    phi_id,
                    basic_block_id,
                    type_to_string(expected_type),
                    type_to_string(current_type),
                    source.from
                );
            }
        }

        self.get_bb_mut(basic_block_id).phis.insert(phi_id, sources);
    }

    fn successor_count(&self, block_id: BasicBlockId) -> usize {
        let bb = self.get_bb(block_id);
        match &bb.terminator {
            Some(Terminator::CondJump { .. }) => 2,
            Some(Terminator::Jump { .. }) => 1,
            Some(Terminator::Return { .. }) => 0,
            None => 0,
        }
    }

    /// Replaces occurrences of `old_target` with `new_target` in the
    /// terminator of `block_id`.
    fn retarget_terminator(
        &mut self,
        block_id: BasicBlockId,
        old_target: BasicBlockId,
        new_target: BasicBlockId,
    ) {
        let bb = self.get_bb_mut(block_id);
        match &mut bb.terminator {
            Some(Terminator::Jump { target }) => {
                assert_eq!(
                    *target, old_target,
                    "INTERNAL COMPILER ERROR: retarget_terminator: jump target mismatch"
                );
                *target = new_target;
            }
            Some(Terminator::CondJump {
                true_target,
                false_target,
                ..
            }) => {
                if *true_target == old_target {
                    *true_target = new_target;
                }
                if *false_target == old_target {
                    *false_target = new_target;
                }
            }
            _ => panic!(
                "INTERNAL COMPILER ERROR: retarget_terminator: block has no branchable \
                 terminator"
            ),
        }
    }

    /// Splits a critical edge from `pred_block` to `target_block` by inserting
    /// a new block in between.
    ///
    /// A critical edge is one where the predecessor has multiple successors
    /// and the target has multiple predecessors. Splitting is necessary so that
    /// coercion instructions for one edge don't affect other edges.
    ///
    /// Before: pred_block -> target_block
    /// After:  pred_block -> split_block -> target_block
    fn split_critical_edge(
        &mut self,
        pred_block: BasicBlockId,
        target_block: BasicBlockId,
    ) -> BasicBlockId {
        let split_id = self.as_fn().new_bb();

        self.get_bb_mut(split_id).terminator = Some(Terminator::Jump {
            target: target_block,
        });
        self.get_bb_mut(split_id).sealed = true;
        self.get_bb_mut(split_id).predecessors.insert(pred_block);

        self.retarget_terminator(pred_block, target_block, split_id);

        self.get_bb_mut(target_block)
            .predecessors
            .remove(&pred_block);
        self.get_bb_mut(target_block).predecessors.insert(split_id);

        if let Some(defs) = self.current_defs.get(&pred_block).cloned() {
            self.current_defs.insert(split_id, defs);
        }

        split_id
    }

    /// Returns the block where coercion instructions should be emitted for
    /// the edge from `pred_block` to `target_block`. Splits the edge if it
    /// is critical.
    pub fn get_coercion_block(
        &mut self,
        pred_block: BasicBlockId,
        target_block: BasicBlockId,
    ) -> BasicBlockId {
        let pred_has_multiple_successors = self.successor_count(pred_block) > 1;
        let target_has_multiple_predecessors =
            self.get_bb(target_block).predecessors.len() > 1;

        if pred_has_multiple_successors && target_has_multiple_predecessors {
            self.split_critical_edge(pred_block, target_block)
        } else {
            pred_block
        }
    }

    /// Coerces a value to match a union type by wrapping, widening, or
    /// narrowing as needed. Must be called with `self.context.block_id`
    /// set to the block where the coercion instructions should be emitted.
    pub fn coerce_to_union(
        &mut self,
        val: ValueId,
        target_union: &Type,
        span: Span,
    ) -> ValueId {
        let val_type = self.get_value_type(val).clone();

        if val_type == *target_union {
            return val;
        }

        let target_variants = target_union
            .as_union_variants()
            .expect("INTERNAL COMPILER ERROR: coerce_to_union target is not a union");

        if let Some(source_variants) = val_type.as_union_variants() {
            let source_is_subset = source_variants.iter().all(|sv| {
                target_variants
                    .iter()
                    .any(|tv| check_structural_compatibility(sv, tv))
            });

            if source_is_subset {
                self.emit_widen_union(val, target_variants)
            } else {
                self.emit_narrow_union(val, target_variants)
            }
        } else {
            self.emit_wrap_in_union(val, target_variants)
        }
    }

    pub fn insert_on_edge<F, R>(
        &mut self,
        pred: BasicBlockId,
        succ: BasicBlockId,
        f: F,
    ) -> (BasicBlockId, R)
    where
        F: FnOnce(&mut Self) -> R,
    {
        let coercion_block = self.get_coercion_block(pred, succ);
        let old_block = self.context.block_id;

        self.use_basic_block(coercion_block);
        let term = self.bb_mut().terminator.take();

        let result = f(self);

        self.bb_mut().terminator = term;

        self.use_basic_block(old_block);

        (coercion_block, result)
    }

    pub fn resolve_phi(
        &mut self,
        block_id: BasicBlockId,
        phi_id: ValueId,
        variable_id: DeclarationId,
        span: Span,
    ) {
        let predecessors: Vec<BasicBlockId> =
            self.get_bb(block_id).predecessors.iter().cloned().collect();

        let mut phi_sources = Vec::new();
        let mut incoming_types = Vec::new();

        for pred in &predecessors {
            let val = self.read_variable(variable_id, *pred, span.clone());
            phi_sources.push((*pred, val));
            incoming_types.push(self.get_value_type(val).clone());
        }

        let unified_type = Type::make_union(incoming_types);

        let final_sources = if unified_type.as_union_variants().is_some() {
            let mut wrapped = HashSet::new();

            for (pred_block, val) in phi_sources {
                let (coercion_block, final_val) =
                    self.insert_on_edge(pred_block, block_id, |b| {
                        b.coerce_to_union(val, &unified_type, span.clone())
                    });

                wrapped.insert(PhiSource {
                    from: coercion_block,
                    value: final_val,
                });
            }

            wrapped
        } else {
            phi_sources
                .into_iter()
                .map(|(pred, val)| PhiSource {
                    from: pred,
                    value: val,
                })
                .collect()
        };

        if let Some(ty) = self.program.value_types.get_mut(&phi_id) {
            *ty = unified_type;
        }

        self.insert_phi(block_id, phi_id, final_sources.clone());

        let source_values: Vec<ValueId> =
            final_sources.into_iter().map(|s| s.value).collect();
        self.ptg.merge_values(phi_id, &source_values);
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

    pub fn use_basic_block(&mut self, block_id: BasicBlockId) {
        self.context.block_id = block_id;
    }

    pub fn seal(&mut self) {
        if self.bb().sealed {
            return;
        }

        let block_id = self.context.block_id;
        let incomplete = self.incomplete_phis.remove(&block_id).unwrap_or_default();

        for (phi_id, variable, span) in incomplete {
            self.resolve_phi(block_id, phi_id, variable, span);
        }

        self.bb_mut().sealed = true;
    }

    pub fn seal_block(&mut self, block_id: BasicBlockId) {
        let old_block = self.context.block_id;
        self.context.block_id = block_id;
        self.seal();
        self.context.block_id = old_block;
    }
}
