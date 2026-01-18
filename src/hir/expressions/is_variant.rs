use crate::{
    ast::{expr::Expr, type_annotation::TagAnnotation, IdentifierNode, Span},
    hir::{
        cfg::{BinaryOperationKind, Terminator, Value},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_type::{StructKind, Type},
        utils::try_unify_types::{intersect_types, subtract_types},
        FunctionBuilder, HIRContext, TypePredicate,
    },
    tokenize::NumberKind,
};

impl FunctionBuilder {
    pub fn build_is_variant_expr(
        &mut self,
        ctx: &mut HIRContext,
        left: Box<Expr>,
        variants: Vec<TagAnnotation>,
    ) -> Value {
        let left_span = left.span;

        let union_val = self.build_expr(ctx, *left.clone());
        let source_id = match union_val {
            Value::Use(id) => id,
            _ => {
                let ty = ctx.program_builder.get_value_type(&union_val);
                self.emit_type_cast(ctx, union_val, Span::default(), ty)
            }
        };
        let source_ty = ctx.program_builder.get_value_id_type(&source_id);

        let underlying_ty = match &source_ty {
            Type::Pointer { narrowed_to, .. } => narrowed_to.as_ref(),
            other => other,
        };

        if !matches!(
            underlying_ty,
            Type::Struct(StructKind::Union { .. }) | Type::Unknown
        ) {
            return Value::Use(self.report_error_and_get_poison(
                ctx,
                SemanticError {
                    kind: SemanticErrorKind::CannotNarrowNonUnion(source_ty.clone()),
                    span: left_span,
                },
            ));
        }

        let union_ptr = match self.build_lvalue_expr(ctx, *left) {
            Ok((ptr, _)) => ptr,
            Err(_) => {
                let p = self.emit_stack_alloc(ctx, source_ty.clone(), 1);
                self.emit_store(ctx, p, Value::Use(source_id), left_span);
                p
            }
        };

        let id_field = IdentifierNode {
            name: COMMON_IDENTIFIERS.id,
            span: left_span,
        };
        let id_ptr = match self.emit_get_field_ptr(ctx, union_ptr, id_field) {
            Ok(ptr) => ptr,
            Err(e) => return Value::Use(self.report_error_and_get_poison(ctx, e)),
        };
        let actual_id = Value::Use(self.emit_load(ctx, id_ptr));

        let true_path = self.new_basic_block();
        let false_path = self.new_basic_block();
        let merge_block = self.new_basic_block();
        let mut target_tag_ids = Vec::new();

        for (i, tag_ann) in variants.iter().enumerate() {
            if tag_ann.value_type.is_some() {
                ctx.module_builder.errors.push(SemanticError {
                    kind: SemanticErrorKind::ValuedTagInIsExpression,
                    span: tag_ann.span,
                });
            }

            let tag_id = ctx
                .program_builder
                .tag_interner
                .intern(&tag_ann.identifier.name);
            target_tag_ids.push(tag_id);

            let is_match = match self.emit_binary_op(
                ctx,
                BinaryOperationKind::Equal,
                actual_id.clone(),
                left_span,
                Value::NumberLiteral(NumberKind::U16(tag_id.0)),
                tag_ann.span,
            ) {
                Ok(id) => id,
                Err(e) => return Value::Use(self.report_error_and_get_poison(ctx, e)),
            };

            let is_last = i == variants.len() - 1;
            let next_check_block = if is_last {
                false_path
            } else {
                self.new_basic_block()
            };

            self.set_basic_block_terminator(Terminator::CondJump {
                condition: Value::Use(is_match),
                true_target: true_path,
                true_args: vec![],
                false_target: next_check_block,
                false_args: vec![],
            });

            if !is_last {
                self.seal_block(ctx, next_check_block);
                self.use_basic_block(next_check_block);
            }
        }

        let true_ty = intersect_types(&source_ty, &target_tag_ids);
        let false_ty = subtract_types(&source_ty, &target_tag_ids);

        let true_id =
            self.emit_type_cast(ctx, Value::Use(source_id), Span::default(), true_ty);
        let false_id =
            self.emit_type_cast(ctx, Value::Use(source_id), Span::default(), false_ty);

        self.use_basic_block(true_path);
        self.seal_block(ctx, true_path);
        self.map_value(true_path, source_id, true_id);
        self.set_basic_block_terminator(Terminator::Jump {
            target: merge_block,
            args: vec![Value::BoolLiteral(true)],
        });

        self.use_basic_block(false_path);
        self.seal_block(ctx, false_path);
        self.map_value(false_path, source_id, false_id);
        self.set_basic_block_terminator(Terminator::Jump {
            target: merge_block,
            args: vec![Value::BoolLiteral(false)],
        });

        self.use_basic_block(merge_block);
        self.seal_block(ctx, merge_block);
        let result_bool_id = self.append_block_param(ctx, merge_block, Type::Bool);

        self.predicates.insert(
            result_bool_id,
            TypePredicate {
                source: source_id,
                true_id,
                false_id,
            },
        );

        Value::Use(result_bool_id)
    }
}
