use std::collections::HashSet;

use crate::{
    ast::{IdentifierNode, Span},
    globals::next_value_id,
    hir::{
        builders::{
            BasicBlock, BasicBlockId, Builder, Function, InBlock, InFunction, InGlobal,
            InModule, LValue, PhiEntry, ValueId,
        },
        errors::{SemanticError, SemanticErrorKind},
        types::{checked_declaration::CheckedDeclaration, checked_type::Type},
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
            aliases: self.aliases,
            incomplete_phis: self.incomplete_phis,
            type_predicates: self.type_predicates,
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
            aliases: self.aliases,
            incomplete_phis: self.incomplete_phis,
            type_predicates: self.type_predicates,
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
            aliases: self.aliases,
            incomplete_phis: self.incomplete_phis,
            type_predicates: self.type_predicates,
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

    pub fn read_lvalue(
        &mut self,
        lval: LValue,
        span: Span,
    ) -> Result<ValueId, SemanticError> {
        let canonical_lval = match lval {
            LValue::Variable(decl_id) => {
                self.aliases.get(&decl_id).cloned().unwrap_or(lval)
            }
            _ => lval,
        };

        self.read_lvalue_from_block(self.context.block_id, canonical_lval, span)
    }

    fn read_lvalue_from_block(
        &mut self,
        block_id: BasicBlockId,
        lval: LValue,
        span: Span,
    ) -> Result<ValueId, SemanticError> {
        if let Some(block_defs) = self.current_defs.get(&block_id) {
            if let Some(val) = block_defs.get(&lval) {
                return Ok(*val);
            }
        }

        let val_id: ValueId;

        let predecessors: Vec<BasicBlockId> =
            self.get_bb(block_id).predecessors.iter().cloned().collect();

        if !self.get_bb(block_id).sealed {
            val_id = self.new_value_id(Type::Unknown);
            self.incomplete_phis.entry(block_id).or_default().push((
                val_id,
                lval.clone(),
                span,
            ));
        } else if predecessors.len() == 1 {
            val_id = self.read_lvalue_from_block(predecessors[0], lval.clone(), span)?;
        } else if predecessors.is_empty() {
            val_id = self.materialize_lvalue(lval.clone(), span)?;
        } else {
            val_id = self.new_value_id(Type::Unknown);

            self.current_defs
                .entry(block_id)
                .or_default()
                .insert(lval.clone(), val_id);

            self.resolve_phi(block_id, val_id, lval.clone(), span)?;
        }

        self.current_defs
            .entry(block_id)
            .or_default()
            .insert(lval, val_id);

        Ok(val_id)
    }

    fn materialize_lvalue(
        &mut self,
        lval: LValue,
        span: Span,
    ) -> Result<ValueId, SemanticError> {
        match lval {
            LValue::Variable(decl_id) => {
                let ident = match self.program.declarations.get(&decl_id).unwrap() {
                    CheckedDeclaration::Var(v) => v.identifier.clone(),
                    _ => unreachable!(),
                };
                Err(SemanticError {
                    kind: SemanticErrorKind::UndeclaredIdentifier(ident),
                    span,
                })
            }
            LValue::Field { base_ptr, field } => {
                let field_node = IdentifierNode {
                    name: field,
                    span: span.clone(),
                };

                let ptr = self.try_get_field_ptr(base_ptr, &field_node)?;

                Ok(self.emit_load(ptr))
            }
        }
    }

    pub fn remap_lvalue(&mut self, lval: LValue, val: ValueId) {
        self.current_defs
            .entry(self.context.block_id)
            .or_default()
            .insert(lval, val);
    }

    fn resolve_phi(
        &mut self,
        block_id: BasicBlockId,
        phi_id: ValueId,
        lval: LValue,
        span: Span,
    ) -> Result<ValueId, SemanticError> {
        let predecessors: Vec<BasicBlockId> =
            self.get_bb(block_id).predecessors.iter().cloned().collect();
        let mut phi_entries = HashSet::new();
        let mut incoming_types = Vec::new();

        for pred in predecessors {
            let val = self.read_lvalue_from_block(pred, lval.clone(), span.clone())?;
            phi_entries.insert(PhiEntry {
                from: pred,
                value: val,
            });
            incoming_types.push(self.get_value_type(&val).clone());
        }

        let unified_type = Type::make_union(incoming_types);
        if let Some(ty) = self.program.value_types.get_mut(&phi_id) {
            *ty = unified_type;
        }

        self.get_bb_mut(block_id).phis.insert(phi_id, phi_entries);
        Ok(phi_id)
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

    pub fn seal(&mut self) -> Result<(), SemanticError> {
        if self.bb().sealed {
            return Ok(());
        }

        let block_id = self.context.block_id;
        let incomplete = self.incomplete_phis.remove(&block_id).unwrap_or_default();

        for (phi_id, lval, span) in incomplete {
            self.resolve_phi(block_id, phi_id, lval, span)?;
        }

        self.bb_mut().sealed = true;

        Ok(())
    }

    pub fn seal_block(&mut self, block_id: BasicBlockId) -> Result<(), SemanticError> {
        let old_block = self.context.block_id;
        self.context.block_id = block_id;
        self.seal()?;
        self.context.block_id = old_block;
        Ok(())
    }
}
