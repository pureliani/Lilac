use crate::{
    ast::expr::{Expr, ExprKind},
    hir::{
        builders::{Builder, InBlock, Place, Projection},
        errors::{SemanticError, SemanticErrorKind},
        types::{
            checked_declaration::CheckedDeclaration,
            checked_type::{StructKind, Type},
        },
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_place(&mut self, expr: Expr) -> Result<Place, SemanticError> {
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
                    CheckedDeclaration::Var(var) => Ok(Place {
                        root: self.use_var(var.ptr),
                        projections: vec![],
                    }),
                    CheckedDeclaration::UninitializedVar { .. } => Err(SemanticError {
                        span: expr.span.clone(),
                        kind: SemanticErrorKind::UseOfUninitializedVariable(ident),
                    }),
                    _ => Err(SemanticError {
                        kind: SemanticErrorKind::InvalidLValue,
                        span: expr.span,
                    }),
                }
            }
            ExprKind::Access { left, field } => {
                let mut place = self.build_place(*left)?;

                let ty = self.get_place_type(&place)?;

                if let Type::Pointer { narrowed_to, .. } = &ty {
                    if matches!(**narrowed_to, Type::Pointer { .. }) {
                        place.projections.push(Projection::Deref);
                    }
                }

                place.projections.push(Projection::Field(field));
                Ok(place)
            }
            ExprKind::Index { left, index } => {
                let mut place = self.build_place(*left)?;
                let ty = self.get_place_type(&place)?;

                if let Type::Pointer { narrowed_to, .. } = &ty {
                    if matches!(**narrowed_to, Type::Pointer { .. }) {
                        place.projections.push(Projection::Deref);
                    }
                }

                let index_span = index.span.clone();
                let index_val = self.build_expr(*index);
                place
                    .projections
                    .push(Projection::Index(index_val, index_span));
                Ok(place)
            }
            _ => Err(SemanticError {
                kind: SemanticErrorKind::InvalidLValue,
                span: expr.span,
            }),
        }
    }

    pub fn build_assignment_stmt(&mut self, target: Expr, value: Expr) {
        let target_span = target.span.clone();
        let source_val = self.build_expr(value);

        let place = match self.build_place(target) {
            Ok(p) => p,
            Err(e) => {
                self.errors.push(e);
                return;
            }
        };

        self.write_place(place, source_val, target_span);
    }

    fn get_place_type(&mut self, place: &Place) -> Result<Type, SemanticError> {
        let current_ssa_id = self.use_var(place.root);
        let mut ty = self.get_value_type(&current_ssa_id).clone();

        for proj in &place.projections {
            match proj {
                Projection::Deref => {
                    if let Type::Pointer { narrowed_to, .. } = ty {
                        ty = *narrowed_to;
                    } else {
                        panic!("INTERNAL COMPILER ERROR: Cannot dereference non-pointer type {:?}", ty);
                    }
                }
                Projection::Field(field_node) => {
                    if let Type::Pointer { narrowed_to, .. } = &ty {
                        if let Type::Struct(kind) = &**narrowed_to {
                            if let Some((_, field_ty)) = kind.get_field(&field_node.name)
                            {
                                ty = Type::Pointer {
                                    constraint: Box::new(field_ty.clone()),
                                    narrowed_to: Box::new(field_ty),
                                };
                            } else {
                                return Err(SemanticError {
                                    kind: SemanticErrorKind::AccessToUndefinedField(
                                        field_node.clone(),
                                    ),
                                    span: field_node.span.clone(),
                                });
                            }
                        } else {
                            return Err(SemanticError {
                                kind: SemanticErrorKind::CannotAccess(
                                    *narrowed_to.clone(),
                                ),
                                span: field_node.span.clone(),
                            });
                        }
                    } else {
                        return Err(SemanticError {
                            kind: SemanticErrorKind::CannotAccess(ty.clone()),
                            span: field_node.span.clone(),
                        });
                    }
                }
                Projection::Index(_, span) => {
                    if let Type::Pointer { narrowed_to, .. } = &ty {
                        if let Type::Struct(StructKind::List(elem_ty)) = &**narrowed_to {
                            ty = Type::Pointer {
                                constraint: elem_ty.clone(),
                                narrowed_to: elem_ty.clone(),
                            };
                        } else {
                            return Err(SemanticError {
                                kind: SemanticErrorKind::CannotIndex(
                                    *narrowed_to.clone(),
                                ),
                                span: span.clone(),
                            });
                        }
                    } else {
                        return Err(SemanticError {
                            kind: SemanticErrorKind::CannotIndex(ty.clone()),
                            span: span.clone(),
                        });
                    }
                }
            }
        }

        Ok(ty)
    }
}
