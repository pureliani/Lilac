use std::collections::{HashMap, HashSet};

use crate::{
    ast::{IdentifierNode, Span},
    hir::{
        cfg::{
            BasicBlock, BinaryOperationKind, ConstantId, Instruction, UnaryOperationKind,
            Value, ValueId,
        },
        errors::{SemanticError, SemanticErrorKind},
        globals::next_value_id,
        types::{checked_declaration::CheckedDeclaration, checked_type::Type},
        utils::{
            check_is_assignable::check_is_assignable,
            check_is_equatable::check_is_equatable, numeric::is_signed,
        },
        FunctionBuilder, HIRContext, ModuleBuilder,
    },
};

impl FunctionBuilder {
    pub fn emit_store(
        &mut self,
        ctx: &mut HIRContext,
        ptr: ValueId,
        value: Value,
        value_span: Span,
    ) {
        let value_type = ctx.program_builder.get_value_type(&value);
        let ptr_type = ctx.program_builder.get_value_id_type(&ptr);

        if let Type::Pointer { constraint, .. } = ptr_type {
            if !check_is_assignable(&value_type, &constraint) {
                ctx.module_builder.errors.push(SemanticError {
                    span: value_span,
                    kind: SemanticErrorKind::TypeMismatch {
                        expected: *constraint,
                        received: value_type,
                    },
                });
                return;
            }
        } else {
            panic!("INTERNAL COMPILER ERROR: emit_store expected a pointer");
        }

        self.push_instruction(Instruction::Store { ptr, value });
    }

    pub fn emit_load(&mut self, ctx: &mut HIRContext, ptr: ValueId) -> ValueId {
        let ptr_type = ctx.program_builder.get_value_id_type(&ptr);

        let destination_type = match ptr_type {
            Type::Pointer {
                narrowed_to,
                constraint: _,
            } => *narrowed_to,
            _ => {
                panic!("INTERNAL COMPILER ERROR: emit_get_element_ptr expects a pointer")
            }
        };

        let destination = self.alloc_value(ctx, destination_type);
        self.push_instruction(Instruction::Load { destination, ptr });

        destination
    }

    pub fn emit_load_constant(
        &mut self,
        ctx: &mut HIRContext,
        constant_id: ConstantId,
    ) -> ValueId {
        let ptr_type = Type::Pointer {
            constraint: Box::new(Type::U8),
            narrowed_to: Box::new(Type::U8),
        };
        let destination = self.alloc_value(ctx, ptr_type);

        self.push_instruction(Instruction::LoadConstant {
            destination,
            constant_id,
        });

        destination
    }

    pub fn emit_get_field_ptr(
        &mut self,
        ctx: &mut HIRContext,
        base_ptr: ValueId,
        field: IdentifierNode,
    ) -> Result<ValueId, SemanticError> {
        let mut current_ptr = base_ptr;
        let mut current_ptr_ty = ctx.program_builder.get_value_id_type(&current_ptr);

        if let Type::Pointer {
            narrowed_to: outer_narrowed,
            ..
        } = &current_ptr_ty
        {
            if let Type::Pointer {
                narrowed_to: inner_narrowed,
                ..
            } = &**outer_narrowed
            {
                if matches!(**inner_narrowed, Type::Struct(_)) {
                    current_ptr = self.emit_load(ctx, current_ptr);
                    current_ptr_ty = ctx.program_builder.get_value_id_type(&current_ptr);
                }
            }
        }

        let (constraint_struct, narrowed_struct) = match current_ptr_ty {
            Type::Pointer {
                constraint,
                narrowed_to,
            } => match (*constraint, *narrowed_to.clone()) {
                (Type::Struct(c), Type::Struct(n)) => (c, n),
                _ => {
                    return Err(SemanticError {
                        kind: SemanticErrorKind::CannotAccess(
                            narrowed_to.as_ref().clone(),
                        ),
                        span: field.span,
                    })
                }
            },
            _ => panic!("Expected pointer"),
        };

        let (field_index, _) = narrowed_struct
            .get_field(ctx.program_builder, field.name)
            .ok_or_else(|| SemanticError {
                kind: SemanticErrorKind::AccessToUndefinedField(field),
                span: field.span,
            })?;

        let (_, field_constraint) =
            constraint_struct.fields(ctx.program_builder)[field_index].clone();

        let (_, field_narrowed) =
            narrowed_struct.fields(ctx.program_builder)[field_index].clone();

        let result_ptr_ty = Type::Pointer {
            constraint: Box::new(field_constraint),
            narrowed_to: Box::new(field_narrowed),
        };

        let destination = self.alloc_value(ctx, result_ptr_ty);
        self.push_instruction(Instruction::GetFieldPtr {
            destination,
            base_ptr,
            field_index,
        });

        Ok(destination)
    }

    pub fn emit_get_element_ptr(
        &mut self,
        ctx: &mut HIRContext,
        base_ptr: ValueId,
        index: Value,
    ) -> Result<ValueId, SemanticError> {
        let index_type = ctx.program_builder.get_value_type(&index);
        if !check_is_assignable(&index_type, &Type::USize) {
            return Err(SemanticError {
                span: Span::default(),
                kind: SemanticErrorKind::TypeMismatch {
                    expected: Type::USize,
                    received: index_type,
                },
            });
        }

        let base_ptr_type = ctx.program_builder.get_value_id_type(&base_ptr);

        if !matches!(base_ptr_type, Type::Pointer { .. }) {
            panic!(
                "INTERNAL COMPILER ERROR: emit_get_element_ptr expects a pointer, found {:?}",
                base_ptr_type
            );
        }

        let destination = self.alloc_value(ctx, base_ptr_type);
        self.push_instruction(Instruction::GetElementPtr {
            destination,
            base_ptr,
            index,
        });

        Ok(destination)
    }

    pub fn emit_unary_op(
        &mut self,
        ctx: &mut HIRContext,
        op_kind: UnaryOperationKind,
        value: Value,
    ) -> Result<ValueId, SemanticError> {
        let value_type = ctx.program_builder.get_value_type(&value);
        let span = Span::default(); // TODO: Fix span

        match op_kind {
            UnaryOperationKind::Neg => {
                if !is_signed(&value_type) {
                    let expected = HashSet::from([
                        Type::I8,
                        Type::I16,
                        Type::I32,
                        Type::I64,
                        Type::ISize,
                        Type::F32,
                        Type::F64,
                    ]);

                    return Err(SemanticError {
                        kind: SemanticErrorKind::TypeMismatchExpectedOneOf {
                            expected,
                            received: value_type.clone(),
                        },
                        span,
                    });
                }
            }
            UnaryOperationKind::Not => {
                let bool_type = Type::Bool;

                if !check_is_assignable(&value_type, &bool_type) {
                    return Err(SemanticError {
                        kind: SemanticErrorKind::TypeMismatch {
                            expected: bool_type.clone(),
                            received: value_type,
                        },
                        span,
                    });
                }
            }
        };

        let destination = self.alloc_value(ctx, value_type);
        self.push_instruction(Instruction::UnaryOp {
            op_kind,
            destination,
            operand: value,
        });

        Ok(destination)
    }

    pub fn emit_binary_op(
        &mut self,
        ctx: &mut HIRContext,
        op_kind: BinaryOperationKind,
        left: Value,
        left_span: Span,
        right: Value,
        right_span: Span,
    ) -> Result<ValueId, SemanticError> {
        let left_type = ctx.program_builder.get_value_type(&left);
        let right_type = ctx.program_builder.get_value_type(&right);
        let combined_span = Span {
            start: left_span.start,
            end: right_span.end,
        };

        let destination_type = match op_kind {
            BinaryOperationKind::Add
            | BinaryOperationKind::Subtract
            | BinaryOperationKind::Multiply
            | BinaryOperationKind::Divide
            | BinaryOperationKind::Modulo => self.check_binary_numeric_operation(
                &left_type,
                left_span,
                &right_type,
                right_span,
            )?,

            BinaryOperationKind::LessThan
            | BinaryOperationKind::LessThanOrEqual
            | BinaryOperationKind::GreaterThan
            | BinaryOperationKind::GreaterThanOrEqual => {
                self.check_binary_numeric_operation(
                    &left_type,
                    left_span,
                    &right_type,
                    right_span,
                )?;

                Type::Bool
            }

            BinaryOperationKind::Equal | BinaryOperationKind::NotEqual => {
                if !check_is_equatable(&left_type, &right_type) {
                    return Err(SemanticError {
                        span: combined_span,
                        kind: SemanticErrorKind::CannotCompareType {
                            of: left_type,
                            to: right_type,
                        },
                    });
                }

                Type::Bool
            }
        };

        let destination = self.alloc_value(ctx, destination_type);
        self.push_instruction(Instruction::BinaryOp {
            op_kind,
            destination,
            left,
            right,
        });

        Ok(destination)
    }

    pub fn emit_function_call(
        &mut self,
        ctx: &mut HIRContext,
        function_value: Value,
        args: Vec<Value>,
        call_span: Span,
    ) -> Result<Option<ValueId>, SemanticError> {
        let (fn_ptr_val, params, return_type) = match function_value {
            Value::Function(decl_id) => {
                let decl = ctx.program_builder.get_declaration(decl_id);
                match decl {
                    CheckedDeclaration::Function(f) => (
                        Value::Function(decl_id),
                        f.params.clone(),
                        f.return_type.clone(),
                    ),
                    _ => panic!(
                        "INTERNAL COMPILER ERROR: Value::Function(DeclarationId) \
                         contained non-function declaration id"
                    ),
                }
            }
            Value::Use(val_id) => {
                let ty = ctx.program_builder.get_value_id_type(&val_id);
                match ty {
                    Type::Fn(fn_type) => (
                        Value::Use(val_id),
                        fn_type.params.clone(),
                        *fn_type.return_type.clone(),
                    ),
                    _ => {
                        return Err(SemanticError {
                            kind: SemanticErrorKind::CannotCall(ty),
                            span: call_span,
                        })
                    }
                }
            }
            _ => {
                return Err(SemanticError {
                    kind: SemanticErrorKind::CannotCall(Type::Unknown),
                    span: call_span,
                })
            }
        };

        if args.len() != params.len() {
            return Err(SemanticError {
                kind: SemanticErrorKind::FnArgumentCountMismatch {
                    expected: params.len(),
                    received: args.len(),
                },
                span: call_span,
            });
        }

        for (arg, param) in args.iter().zip(params.iter()) {
            let arg_ty = ctx.program_builder.get_value_type(arg);
            if !check_is_assignable(&param.ty, &arg_ty) {
                return Err(SemanticError {
                    kind: SemanticErrorKind::TypeMismatch {
                        expected: param.ty.clone(),
                        received: arg_ty,
                    },
                    span: call_span,
                });
            }
        }

        let destination_id = if return_type != Type::Void {
            Some(self.alloc_value(ctx, return_type))
        } else {
            None
        };

        self.push_instruction(Instruction::FunctionCall {
            destination: destination_id,
            function_rvalue: fn_ptr_val,
            args,
        });

        Ok(destination_id)
    }

    pub fn emit_type_cast(
        &mut self,
        ctx: &mut HIRContext,
        value: Value,
        value_span: Span,
        target_type: Type,
    ) -> ValueId {
        let value_type = ctx.program_builder.get_value_type(&value);

        if !self.check_is_casting_allowed(&value_type, &target_type) {
            return self.report_error_and_get_poison(
                ctx,
                SemanticError {
                    span: value_span,
                    kind: SemanticErrorKind::CannotCastType {
                        source_type: value_type.clone(),
                        target_type: target_type.clone(),
                    },
                },
            );
        }

        let destination = self.alloc_value(ctx, target_type.clone());
        self.push_instruction(Instruction::TypeCast {
            destination,
            operand: value,
            target_type,
        });

        destination
    }

    pub fn emit_nop(&mut self, _module_builder: &mut ModuleBuilder) {
        self.push_instruction(Instruction::Nop);
    }
}
