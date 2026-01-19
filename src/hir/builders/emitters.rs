use crate::{
    ast::Span,
    hir::{
        builders::{BasicBlockId, Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        instructions::{Instruction, Terminator},
        types::checked_type::Type,
        utils::{
            check_binary_numeric_op::check_binary_numeric_operation,
            check_is_assignable::check_is_assignable,
            numeric::{is_float, is_signed},
        },
    },
    tokenize::NumberKind,
};

impl<'a> Builder<'a, InBlock> {
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

    // Constants
    pub fn emit_const_int(&mut self, val: NumberKind, ty: Type) -> ValueId {
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::ConstInt { dest, val });
        dest
    }

    pub fn emit_const_bool(&mut self, val: bool) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::ConstBool { dest, val });
        dest
    }

    pub fn emit_const_void(&mut self) -> ValueId {
        let dest = self.new_value_id(Type::Void);
        self.push_instruction(Instruction::ConstVoid { dest });
        dest
    }

    // Integer arithmetic

    pub fn emit_iadd(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(&lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::IAdd { dest, lhs, rhs });
        dest
    }

    pub fn emit_isub(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(&lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::ISub { dest, lhs, rhs });
        dest
    }

    pub fn emit_imul(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(&lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::IMul { dest, lhs, rhs });
        dest
    }

    pub fn emit_sdiv(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(&lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::SDiv { dest, lhs, rhs });
        dest
    }

    pub fn emit_udiv(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(&lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::UDiv { dest, lhs, rhs });
        dest
    }

    pub fn emit_srem(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(&lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::SRem { dest, lhs, rhs });
        dest
    }

    pub fn emit_urem(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(&lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::URem { dest, lhs, rhs });
        dest
    }

    // Float arithmetic

    pub fn emit_frem(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(&lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::FRem { dest, lhs, rhs });
        dest
    }

    pub fn emit_fadd(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(&lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::FAdd { dest, lhs, rhs });
        dest
    }

    pub fn emit_fsub(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(&lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::FSub { dest, lhs, rhs });
        dest
    }

    pub fn emit_fmul(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(&lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::FMul { dest, lhs, rhs });
        dest
    }

    pub fn emit_fdiv(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = self.get_value_type(&lhs).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::FDiv { dest, lhs, rhs });
        dest
    }

    // Comparisons

    pub fn emit_ieq(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::IEq { dest, lhs, rhs });
        dest
    }

    pub fn emit_ine(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::INe { dest, lhs, rhs });
        dest
    }

    pub fn emit_slt(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::SLt { dest, lhs, rhs });
        dest
    }

    pub fn emit_sle(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::SLe { dest, lhs, rhs });
        dest
    }

    pub fn emit_sgt(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::SGt { dest, lhs, rhs });
        dest
    }

    pub fn emit_sge(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::SGe { dest, lhs, rhs });
        dest
    }

    pub fn emit_ult(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::ULt { dest, lhs, rhs });
        dest
    }

    pub fn emit_ule(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::ULe { dest, lhs, rhs });
        dest
    }

    pub fn emit_ugt(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::UGt { dest, lhs, rhs });
        dest
    }

    pub fn emit_uge(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::UGe { dest, lhs, rhs });
        dest
    }

    pub fn emit_feq(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::FEq { dest, lhs, rhs });
        dest
    }

    pub fn emit_fne(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::FNe { dest, lhs, rhs });
        dest
    }

    pub fn emit_flt(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::FLt { dest, lhs, rhs });
        dest
    }

    pub fn emit_fle(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::FLe { dest, lhs, rhs });
        dest
    }

    pub fn emit_fgt(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::FGt { dest, lhs, rhs });
        dest
    }

    pub fn emit_fge(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::FGe { dest, lhs, rhs });
        dest
    }

    // Unary

    pub fn emit_ineg(&mut self, src: ValueId) -> ValueId {
        let ty = self.get_value_type(&src).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::INeg { dest, src });
        dest
    }

    pub fn emit_fneg(&mut self, src: ValueId) -> ValueId {
        let ty = self.get_value_type(&src).clone();
        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::FNeg { dest, src });
        dest
    }

    pub fn emit_bnot(&mut self, src: ValueId) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::BNot { dest, src });
        dest
    }

    // Casts

    pub fn emit_itof(&mut self, src: ValueId, target_ty: Type) -> ValueId {
        let dest = self.new_value_id(target_ty.clone());
        self.push_instruction(Instruction::IToF {
            dest,
            src,
            target_ty,
        });
        dest
    }

    pub fn emit_ftoi(&mut self, src: ValueId, target_ty: Type) -> ValueId {
        let dest = self.new_value_id(target_ty.clone());
        self.push_instruction(Instruction::FToI {
            dest,
            src,
            target_ty,
        });
        dest
    }

    pub fn emit_sext(&mut self, src: ValueId, target_ty: Type) -> ValueId {
        let dest = self.new_value_id(target_ty.clone());
        self.push_instruction(Instruction::SExt {
            dest,
            src,
            target_ty,
        });
        dest
    }

    pub fn emit_zext(&mut self, src: ValueId, target_ty: Type) -> ValueId {
        let dest = self.new_value_id(target_ty.clone());
        self.push_instruction(Instruction::ZExt {
            dest,
            src,
            target_ty,
        });
        dest
    }

    pub fn emit_trunc(&mut self, src: ValueId, target_ty: Type) -> ValueId {
        let dest = self.new_value_id(target_ty.clone());
        self.push_instruction(Instruction::Trunc {
            dest,
            src,
            target_ty,
        });
        dest
    }

    // Memory

    pub fn emit_stack_alloc(&mut self, ty: Type, count: usize) -> ValueId {
        let destination = self.new_value_id(Type::Pointer {
            constraint: Box::new(ty.clone()),
            narrowed_to: Box::new(ty),
        });
        self.push_instruction(Instruction::StackAlloc { destination, count });

        destination
    }

    pub fn emit_heap_alloc(
        &mut self,
        ty: Type,
        count: ValueId,
    ) -> Result<ValueId, SemanticError> {
        let destination = self.new_value_id(Type::Pointer {
            constraint: Box::new(ty.clone()),
            narrowed_to: Box::new(ty),
        });
        self.push_instruction(Instruction::HeapAlloc { destination, count });

        Ok(destination)
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

    pub fn emit_get_element_ptr(&mut self, base_ptr: ValueId, index: ValueId) -> ValueId {
        if let Type::Struct(crate::hir::types::checked_type::StructKind::List(
            item_type,
        )) = self.get_value_type(&base_ptr)
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

    pub fn emit_refine_type(&mut self, src: ValueId, new_type: Type) -> ValueId {
        let dest = self.new_value_id(new_type.clone());

        self.push_instruction(Instruction::RefineType {
            dest,
            src,
            new_type,
        });

        dest
    }

    pub fn emit_function_call(
        &mut self,
        func: ValueId,
        args: Vec<ValueId>,
        return_type: Type,
    ) -> Option<ValueId> {
        let destination = if return_type == Type::Void {
            None
        } else {
            Some(self.new_value_id(return_type))
        };

        self.push_instruction(Instruction::FunctionCall {
            destination,
            func,
            args,
        });

        destination
    }

    pub fn emit_jmp_terminator(&mut self, target: BasicBlockId, args: Vec<ValueId>) {
        self.check_no_terminator();
        let this_block_id = self.context.block_id;
        self.get_bb_mut(target).predecessors.insert(this_block_id);

        self.bb_mut().terminator = Some(Terminator::Jump { target, args });
    }

    pub fn emit_cond_jmp_terminator(
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

    pub fn emit_return_terminator(&mut self, value: Option<ValueId>) {
        self.check_no_terminator();
        self.bb_mut().terminator = Some(Terminator::Return { value })
    }

    pub fn emit_unreachable_terminator(&mut self) {
        self.check_no_terminator();
        self.bb_mut().terminator = Some(Terminator::Unreachable)
    }

    // Type-checked emitter wrappers

    pub fn add(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> Result<ValueId, SemanticError> {
        let lhs_ty = self.get_value_type(&lhs);
        let rhs_ty = self.get_value_type(&rhs);

        let _ = check_binary_numeric_operation(lhs_ty, lhs_span, rhs_ty, rhs_span)?;

        if is_float(lhs_ty) {
            Ok(self.emit_fadd(lhs, rhs))
        } else {
            Ok(self.emit_iadd(lhs, rhs))
        }
    }

    pub fn sub(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> Result<ValueId, SemanticError> {
        let lhs_ty = self.get_value_type(&lhs);
        let rhs_ty = self.get_value_type(&rhs);

        let _ = check_binary_numeric_operation(lhs_ty, lhs_span, rhs_ty, rhs_span)?;

        if is_float(lhs_ty) {
            Ok(self.emit_fsub(lhs, rhs))
        } else {
            Ok(self.emit_isub(lhs, rhs))
        }
    }

    pub fn mul(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> Result<ValueId, SemanticError> {
        let lhs_ty = self.get_value_type(&lhs);
        let rhs_ty = self.get_value_type(&rhs);

        let _ = check_binary_numeric_operation(lhs_ty, lhs_span, rhs_ty, rhs_span)?;

        if is_float(lhs_ty) {
            Ok(self.emit_fmul(lhs, rhs))
        } else {
            Ok(self.emit_imul(lhs, rhs))
        }
    }

    pub fn div(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> Result<ValueId, SemanticError> {
        let lhs_ty = self.get_value_type(&lhs);
        let rhs_ty = self.get_value_type(&rhs);

        let _ = check_binary_numeric_operation(lhs_ty, lhs_span, rhs_ty, rhs_span)?;

        if is_float(lhs_ty) {
            Ok(self.emit_fdiv(lhs, rhs))
        } else if is_signed(lhs_ty) {
            Ok(self.emit_sdiv(lhs, rhs))
        } else {
            Ok(self.emit_udiv(lhs, rhs))
        }
    }

    pub fn rem(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> Result<ValueId, SemanticError> {
        let lhs_ty = self.get_value_type(&lhs);
        let rhs_ty = self.get_value_type(&rhs);

        let _ = check_binary_numeric_operation(lhs_ty, lhs_span, rhs_ty, rhs_span)?;

        if is_float(lhs_ty) {
            Ok(self.emit_frem(lhs, rhs))
        } else if is_signed(lhs_ty) {
            Ok(self.emit_srem(lhs, rhs))
        } else {
            Ok(self.emit_urem(lhs, rhs))
        }
    }

    pub fn eq(
        &mut self,
        lhs: ValueId,
        _lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> Result<ValueId, SemanticError> {
        let lhs_ty = self.get_value_type(&lhs);
        let rhs_ty = self.get_value_type(&rhs);

        if !check_is_assignable(lhs_ty, rhs_ty) && !check_is_assignable(rhs_ty, lhs_ty) {
            return Err(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: lhs_ty.clone(),
                    received: rhs_ty.clone(),
                },
                span: rhs_span,
            });
        }

        if is_float(lhs_ty) {
            Ok(self.emit_feq(lhs, rhs))
        } else {
            Ok(self.emit_ieq(lhs, rhs))
        }
    }

    pub fn ne(
        &mut self,
        lhs: ValueId,
        _lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> Result<ValueId, SemanticError> {
        let lhs_ty = self.get_value_type(&lhs);
        let rhs_ty = self.get_value_type(&rhs);

        if !check_is_assignable(lhs_ty, rhs_ty) && !check_is_assignable(rhs_ty, lhs_ty) {
            return Err(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: lhs_ty.clone(),
                    received: rhs_ty.clone(),
                },
                span: rhs_span,
            });
        }

        if is_float(lhs_ty) {
            Ok(self.emit_fne(lhs, rhs))
        } else {
            Ok(self.emit_ine(lhs, rhs))
        }
    }

    pub fn lt(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> Result<ValueId, SemanticError> {
        let lhs_ty = self.get_value_type(&lhs);
        let rhs_ty = self.get_value_type(&rhs);

        let _ = check_binary_numeric_operation(lhs_ty, lhs_span, rhs_ty, rhs_span)?;

        if is_float(lhs_ty) {
            Ok(self.emit_flt(lhs, rhs))
        } else if is_signed(lhs_ty) {
            Ok(self.emit_slt(lhs, rhs))
        } else {
            Ok(self.emit_ult(lhs, rhs))
        }
    }

    pub fn le(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> Result<ValueId, SemanticError> {
        let lhs_ty = self.get_value_type(&lhs);
        let rhs_ty = self.get_value_type(&rhs);

        let _ = check_binary_numeric_operation(lhs_ty, lhs_span, rhs_ty, rhs_span)?;

        if is_float(lhs_ty) {
            Ok(self.emit_fle(lhs, rhs))
        } else if is_signed(lhs_ty) {
            Ok(self.emit_sle(lhs, rhs))
        } else {
            Ok(self.emit_ule(lhs, rhs))
        }
    }

    pub fn gt(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> Result<ValueId, SemanticError> {
        let lhs_ty = self.get_value_type(&lhs);
        let rhs_ty = self.get_value_type(&rhs);

        let _ = check_binary_numeric_operation(lhs_ty, lhs_span, rhs_ty, rhs_span)?;

        if is_float(lhs_ty) {
            Ok(self.emit_fgt(lhs, rhs))
        } else if is_signed(lhs_ty) {
            Ok(self.emit_sgt(lhs, rhs))
        } else {
            Ok(self.emit_ugt(lhs, rhs))
        }
    }

    pub fn ge(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> Result<ValueId, SemanticError> {
        let lhs_ty = self.get_value_type(&lhs);
        let rhs_ty = self.get_value_type(&rhs);

        let _ = check_binary_numeric_operation(lhs_ty, lhs_span, rhs_ty, rhs_span)?;

        if is_float(lhs_ty) {
            Ok(self.emit_fge(lhs, rhs))
        } else if is_signed(lhs_ty) {
            Ok(self.emit_sge(lhs, rhs))
        } else {
            Ok(self.emit_uge(lhs, rhs))
        }
    }

    pub fn neg(&mut self, src: ValueId, span: Span) -> Result<ValueId, SemanticError> {
        let ty = self.get_value_type(&src);

        if is_float(ty) {
            Ok(self.emit_fneg(src))
        } else if is_signed(ty) {
            Ok(self.emit_ineg(src))
        } else {
            Err(SemanticError {
                kind: SemanticErrorKind::ExpectedANumericOperand,
                span,
            })
        }
    }

    pub fn not(&mut self, src: ValueId, span: Span) -> Result<ValueId, SemanticError> {
        let ty = self.get_value_type(&src);

        if !check_is_assignable(ty, &Type::Bool) {
            return Err(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: Type::Bool,
                    received: ty.clone(),
                },
                span,
            });
        }

        Ok(self.emit_bnot(src))
    }

    pub fn call(
        &mut self,
        func: ValueId,
        args: Vec<ValueId>,
        span: Span,
    ) -> Result<Option<ValueId>, SemanticError> {
        let func_ty = self.get_value_type(&func).clone();

        match func_ty {
            Type::Fn(fn_type) => {
                if fn_type.params.len() != args.len() {
                    return Err(SemanticError {
                        kind: SemanticErrorKind::FnArgumentCountMismatch {
                            expected: fn_type.params.len(),
                            received: args.len(),
                        },
                        span,
                    });
                }

                for (param, arg_val) in fn_type.params.iter().zip(args.iter()) {
                    let arg_ty = self.get_value_type(arg_val);
                    if !check_is_assignable(arg_ty, &param.ty) {
                        // TODO: We need argument span here
                        return Err(SemanticError {
                            kind: SemanticErrorKind::TypeMismatch {
                                expected: param.ty.clone(),
                                received: arg_ty.clone(),
                            },
                            span,
                        });
                    }
                }

                Ok(self.emit_function_call(func, args, *fn_type.return_type))
            }
            _ => Err(SemanticError {
                kind: SemanticErrorKind::CannotCall(func_ty),
                span,
            }),
        }
    }
}
