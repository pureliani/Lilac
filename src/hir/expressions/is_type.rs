use crate::{
    ast::{
        expr::{Expr, ExprKind},
        type_annotation::TypeAnnotation,
        Span,
    },
    compile::interner::StringId,
    globals::COMMON_IDENTIFIERS,
    hir::{
        builders::{Builder, InBlock, LValue, TypePredicate, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::{
            checked_declaration::CheckedDeclaration,
            checked_type::{StructKind, Type},
        },
        utils::{
            check_type::{check_type_annotation, TypeCheckerContext},
            union::{get_matching_variant_indices, get_non_matching_variant_indices},
        },
    },
    tokenize::NumberKind,
};

impl<'a> Builder<'a, InBlock> {
    fn emit_is_type_check(
        &mut self,
        root_val_id: ValueId,
        matching_indices: &[u16],
        total_variants: usize,
        span: Span,
    ) -> Result<ValueId, SemanticError> {
        if matching_indices.is_empty() {
            return Ok(self.emit_const_bool(false));
        }

        if matching_indices.len() == total_variants {
            return Ok(self.emit_const_bool(true));
        }

        let id_ptr = self.get_field_ptr(root_val_id, COMMON_IDENTIFIERS.id);
        let active_id = self.emit_load(id_ptr);

        let mut iter = matching_indices.iter();
        let &first_idx = iter.next().unwrap();

        let idx_val = self.emit_const_number(NumberKind::U16(first_idx));
        let mut result_id = self.emit_ieq(active_id, idx_val);

        for &match_idx in iter {
            result_id = self.emit_logical_or(result_id, span.clone(), |builder| {
                let idx_val = builder.emit_const_number(NumberKind::U16(match_idx));
                Ok(builder.emit_ieq(active_id, idx_val))
            })?;
        }

        Ok(result_id)
    }

    fn unwind_to_lvalue(&mut self, expr: &Expr) -> Result<(LValue, Vec<StringId>), ()> {
        let mut path = Vec::new();
        let mut current = expr;

        loop {
            match &current.kind {
                ExprKind::Access { left, field } => {
                    path.insert(0, field.name);
                    current = left;
                }
                ExprKind::Identifier(ident) => {
                    if let Some(decl_id) = self.current_scope.lookup(ident.name) {
                        let decl = self.program.declarations.get(&decl_id).expect(
                            "INTERNAL COMPILER ERROR: Expected declaration id entry to \
                             exist",
                        );
                        if let CheckedDeclaration::Var(_) = decl {
                            let lvalue = self
                                .aliases
                                .get(&decl_id)
                                .cloned()
                                .unwrap_or(LValue::Variable(decl_id));

                            return Ok((lvalue, path));
                        }
                    }
                    return Err(());
                }
                _ => return Err(()),
            }
        }
    }

    fn unwind_access_path(
        &self,
        expr: Expr,
    ) -> Result<(Expr, Vec<StringId>), SemanticError> {
        let mut path = Vec::new();
        let mut current = expr;

        loop {
            match current.kind {
                ExprKind::Access { left, field } => {
                    path.insert(0, field.name);
                    current = *left;
                }
                _ => return Ok((current, path)),
            }
        }
    }

    fn build_is_type_fallback(
        &mut self,
        left: Expr,
        ty: TypeAnnotation,
        span: Span,
    ) -> Result<ValueId, SemanticError> {
        let (root_expr, path) = self.unwind_access_path(left)?;

        let root_val_id = self.build_expr(root_expr)?;
        let root_ty = self.get_value_type(&root_val_id).clone();

        let variants = match &root_ty {
            Type::Struct(StructKind::Union(variants)) => variants,
            _ => {
                return Err(SemanticError {
                    kind: SemanticErrorKind::CannotNarrowNonUnion(root_ty),
                    span,
                });
            }
        };

        let mut type_ctx = TypeCheckerContext {
            scope: self.current_scope.clone(),
            declarations: &self.program.declarations,
            errors: self.errors,
        };
        let target_type = check_type_annotation(&mut type_ctx, &ty);

        let matching_indices =
            get_matching_variant_indices(variants, &path, &target_type);

        self.emit_is_type_check(root_val_id, &matching_indices, variants.len(), span)
    }

    pub fn build_is_type_expr(
        &mut self,
        left: Expr,
        ty: TypeAnnotation,
    ) -> Result<ValueId, SemanticError> {
        let span = left.span.clone();

        let (lvalue, path) = match self.unwind_to_lvalue(&left) {
            Ok(res) => res,
            Err(_) => {
                return self.build_is_type_fallback(left, ty, span);
            }
        };

        let val_id = self.read_lvalue(lvalue, span.clone())?;
        let val_ty = self.get_value_type(&val_id).clone();

        let variants = match &val_ty {
            Type::Pointer(to) => {
                if let Type::Struct(StructKind::Union(variants)) = &**to {
                    variants
                } else {
                    return self.build_is_type_fallback(left, ty, span);
                }
            }
            _ => return self.build_is_type_fallback(left, ty, span),
        };

        let mut type_ctx = TypeCheckerContext {
            scope: self.current_scope.clone(),
            declarations: &self.program.declarations,
            errors: self.errors,
        };
        let target_type = check_type_annotation(&mut type_ctx, &ty);

        let match_indices = get_matching_variant_indices(variants, &path, &target_type);
        let non_match_indices =
            get_non_matching_variant_indices(variants, &path, &target_type);

        let result_id =
            self.emit_is_type_check(val_id, &match_indices, variants.len(), span)?;

        let true_type =
            if !match_indices.is_empty() && match_indices.len() < variants.len() {
                Some(Type::make_union(filter_variants(variants, &match_indices)))
            } else {
                None
            };

        let false_type = if !non_match_indices.is_empty()
            && non_match_indices.len() < variants.len()
        {
            Some(Type::make_union(filter_variants(
                variants,
                &non_match_indices,
            )))
        } else {
            None
        };

        if true_type.is_some() || false_type.is_some() {
            self.type_predicates.insert(
                result_id,
                TypePredicate {
                    lvalue,
                    on_true_type: true_type,
                    on_false_type: false_type,
                },
            );
        }

        Ok(result_id)
    }
}

fn filter_variants(
    variants: &std::collections::BTreeSet<Type>,
    indices: &[u16],
) -> Vec<Type> {
    let mut result = Vec::with_capacity(indices.len());
    let mut indices_iter = indices.iter().peekable();

    for (i, variant) in variants.iter().enumerate() {
        if let Some(&&next_idx) = indices_iter.peek() {
            if next_idx == i as u16 {
                result.push(variant.clone());
                indices_iter.next();
            }
        }
    }

    result
}
