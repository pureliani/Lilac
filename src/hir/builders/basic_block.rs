use std::collections::HashSet;

use crate::{
    ast::{
        expr::{Expr, ExprKind},
        IdentifierNode, Span,
    },
    globals::{next_value_id, COMMON_IDENTIFIERS},
    hir::{
        builders::{
            BasicBlock, BasicBlockId, Builder, Function, InBlock, InFunction, InGlobal,
            InModule, Phi, ValueId,
        },
        errors::{SemanticError, SemanticErrorKind},
        types::{
            checked_declaration::CheckedDeclaration,
            checked_type::{StructKind, Type},
        },
        utils::try_unify_types::try_unify_types,
    },
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

    pub fn get_list_buffer_ptr(
        &mut self,
        list_ptr: ValueId,
        span: Span,
    ) -> Result<ValueId, SemanticError> {
        let list_ptr_type = self.get_value_type(&list_ptr);

        let is_valid = if let Type::Pointer(inner) = &list_ptr_type {
            match &**inner {
                Type::Struct(StructKind::ListHeader(_)) => true,
                Type::Struct(StructKind::StringHeader) => true,
                _ => false,
            }
        } else {
            false
        };

        if !is_valid {
            return Err(SemanticError {
                kind: SemanticErrorKind::CannotIndex(list_ptr_type.clone()),
                span: span.clone(),
            });
        }

        let ptr_field_node = IdentifierNode {
            name: COMMON_IDENTIFIERS.ptr,
            span: span.clone(),
        };

        let buffer_ptr_ptr = self.get_field_ptr(list_ptr, &ptr_field_node)?;

        self.load(buffer_ptr_ptr, span)
    }

    /// Returns pointer to lvalue
    pub fn build_lvalue(&mut self, expr: Expr) -> Result<ValueId, SemanticError> {
        let mut get_base_ptr = |left: Box<Expr>| {
            let left_span = left.span.clone();
            let left_lvalue = self.build_lvalue(*left)?;
            let left_lvalue_type = self.get_value_type(&left_lvalue);

            match left_lvalue_type {
                // ptr<ptr<Struct>>
                Type::Pointer(inner) if matches!(&**inner, Type::Pointer(s) if matches!(&**s, Type::Struct(_))) => {
                    self.load(left_lvalue, left_span)
                }
                // ptr<Struct>
                Type::Pointer(inner) if matches!(&**inner, Type::Struct(_)) => {
                    Ok(left_lvalue)
                }
                _ => Err(SemanticError {
                    kind: SemanticErrorKind::InvalidLValue,
                    span: left_span,
                }),
            }
        };

        match expr.kind {
            ExprKind::Identifier(ident) => {
                let decl_id =
                    self.current_scope.lookup(ident.name).ok_or(SemanticError {
                        span: expr.span.clone(),
                        kind: SemanticErrorKind::UndeclaredIdentifier(ident.clone()),
                    })?;

                let decl = self
                    .program
                    .declarations
                    .get(&decl_id)
                    .expect("INTERNAL COMPILER ERROR: Declaration not found");

                match decl {
                    CheckedDeclaration::Var(var) => Ok(var.stack_ptr),
                    _ => Err(SemanticError {
                        kind: SemanticErrorKind::InvalidLValue,
                        span: expr.span,
                    }),
                }
            }
            ExprKind::Access { left, field } => {
                let base_ptr = get_base_ptr(left)?;
                let field_ptr = self.get_field_ptr(base_ptr, &field)?;

                Ok(field_ptr)
            }
            _ => Err(SemanticError {
                kind: SemanticErrorKind::InvalidLValue,
                span: expr.span,
            }),
        }
    }

    pub fn get_latest_alias(&mut self, initial_id: ValueId) -> ValueId {
        if let Some(defs) = self.definitions.get(&self.context.block_id) {
            if let Some(val) = defs.get(&initial_id) {
                return *val;
            }
        }
        self.get_latest_alias_recursive(self.context.block_id, initial_id)
    }

    fn get_latest_alias_recursive(
        &mut self,
        block_id: BasicBlockId,
        initial_id: ValueId,
    ) -> ValueId {
        if let Some(defs) = self.definitions.get(&block_id) {
            if let Some(val) = defs.get(&initial_id) {
                return *val;
            }
        }

        let result_valueid: ValueId = if !self.get_bb(block_id).sealed {
            let value_type = self.get_value_type(&initial_id).clone();
            let placeholder_value_id = self.new_value_id(value_type);
            self.incomplete_phis
                .entry(block_id)
                .or_default()
                .push((placeholder_value_id, initial_id));

            placeholder_value_id
        } else {
            let predecessors: Vec<BasicBlockId> =
                self.get_bb(block_id).predecessors.iter().cloned().collect();

            if predecessors.len() == 1 {
                let latest_alias_from_pred =
                    self.get_latest_alias_recursive(predecessors[0], initial_id);
                self.definitions
                    .entry(block_id)
                    .or_default()
                    .insert(initial_id, latest_alias_from_pred);

                latest_alias_from_pred
            } else {
                let original_value_type = self.get_value_type(&initial_id).clone();
                let phi_value_id = self.new_value_id(original_value_type);

                let mut phi_accumulator: HashSet<Phi> = HashSet::new();
                for pred in predecessors {
                    let latest_alias_from_pred =
                        self.get_latest_alias_recursive(pred, initial_id);

                    let phi = Phi {
                        from: pred,
                        value: latest_alias_from_pred,
                    };

                    phi_accumulator.insert(phi);
                }

                let accumulated_phi_types: Vec<Type> = phi_accumulator
                    .iter()
                    .map(|p| self.get_value_type(&p.value).clone())
                    .collect();

                let correct_phi_type = try_unify_types(&accumulated_phi_types);

                let current_phi_type = self
                    .as_program()
                    .program
                    .value_types
                    .get_mut(&phi_value_id)
                    .unwrap();

                *current_phi_type = correct_phi_type;

                self.bb_mut().phis.insert(phi_value_id, phi_accumulator);

                self.definitions
                    .entry(block_id)
                    .or_default()
                    .insert(initial_id, phi_value_id);

                phi_value_id
            }
        };

        result_valueid
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

        todo!();

        self.bb_mut().sealed = true;
    }

    pub fn seal_block(&mut self, block_id: BasicBlockId) {
        let old_block = self.context.block_id;
        self.context.block_id = block_id;
        self.seal();
        self.context.block_id = old_block;
    }
}
