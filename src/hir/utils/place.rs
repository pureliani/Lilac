use std::collections::HashMap;

use crate::{
    ast::{
        expr::{Expr, ExprKind},
        DeclarationId, IdentifierNode, Span,
    },
    compile::interner::StringId,
    hir::{
        builders::{BasicBlockId, Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::{
            checked_declaration::{CheckedDeclaration, FnType},
            checked_type::Type,
        },
    },
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Place {
    Local(DeclarationId),
    Field(Box<Place>, StringId),
}

impl Place {
    /// For `x.a.b`, returns the DeclarationId for `x`
    pub fn root(&self) -> DeclarationId {
        match self {
            Place::Local(id) => *id,
            Place::Field(base, _) => base.root(),
        }
    }

    /// For `x.a.b`, returns `[a, b]`
    pub fn path(&self) -> Vec<StringId> {
        let mut path = Vec::new();
        let mut current = self;
        while let Place::Field(base, field) = current {
            path.push(*field);
            current = base;
        }
        path.reverse();
        path
    }

    pub fn is_local(&self) -> bool {
        matches!(self, Place::Local(_))
    }

    pub fn canonicalize(&self, aliases: &HashMap<DeclarationId, Place>) -> Place {
        match self {
            Place::Local(decl_id) => {
                if let Some(aliased) = aliases.get(decl_id) {
                    aliased.canonicalize(aliases)
                } else {
                    self.clone()
                }
            }
            Place::Field(base, field) => {
                Place::Field(Box::new(base.canonicalize(aliases)), *field)
            }
        }
    }
}

impl<'a> Builder<'a, InBlock> {
    pub fn resolve_place(&mut self, expr: Expr) -> Result<Place, SemanticError> {
        let span = expr.span.clone();

        match expr.kind {
            ExprKind::Identifier(ident) => match self.current_scope.lookup(ident.name) {
                Some(decl_id) => Ok(Place::Local(decl_id)),
                None => Err(SemanticError {
                    kind: SemanticErrorKind::UndeclaredIdentifier(ident),
                    span,
                }),
            },
            ExprKind::Access { left, field } => {
                let base = self.resolve_place(*left)?;
                Ok(Place::Field(Box::new(base), field.name))
            }
            _ => Err(SemanticError {
                kind: SemanticErrorKind::InvalidLValue,
                span,
            }),
        }
    }

    pub fn read_place(
        &mut self,
        place: &Place,
        span: Span,
    ) -> Result<ValueId, SemanticError> {
        let canonical = place.canonicalize(self.aliases);
        self.read_place_from_block(self.context.block_id, &canonical, span)
    }

    pub fn read_place_from_block(
        &mut self,
        block_id: BasicBlockId,
        place: &Place,
        span: Span,
    ) -> Result<ValueId, SemanticError> {
        if let Some(block_defs) = self.current_defs.get(&block_id) {
            if let Some(val) = block_defs.get(place) {
                return Ok(*val);
            }
        }

        let predecessors: Vec<BasicBlockId> =
            self.get_bb(block_id).predecessors.iter().cloned().collect();

        let val_id = if !self.get_bb(block_id).sealed {
            let val_id = self.new_value_id(Type::Unknown);
            self.incomplete_phis.entry(block_id).or_default().push((
                val_id,
                place.clone(),
                span,
            ));
            val_id
        } else if predecessors.len() == 1 {
            self.read_place_from_block(predecessors[0], place, span)?
        } else if predecessors.is_empty() {
            panic!(
                "INTERNAL COMPILER ERROR: Tried to read place in a basic block which \
                 neither defined the place nor had the predecessors"
            );
        } else {
            let val_id = self.new_value_id(Type::Unknown);

            self.current_defs
                .entry(block_id)
                .or_default()
                .insert(place.clone(), val_id);

            self.resolve_phi(block_id, val_id, place, span)?;
            val_id
        };

        self.current_defs
            .entry(block_id)
            .or_default()
            .insert(place.clone(), val_id);

        Ok(val_id)
    }

    pub fn write_place(&mut self, place: &Place, value: ValueId, span: Span) {
        let canonical = place.canonicalize(self.aliases);

        self.cache_place_value(canonical.clone(), value);
        self.write_place_to_memory(&canonical, value, span);
    }

    fn write_place_to_memory(&mut self, place: &Place, value: ValueId, span: Span) {
        match place {
            Place::Local(decl_id) => {
                if let Some(aliased) = self.aliases.get(decl_id).cloned() {
                    self.write_place_to_memory(&aliased, value, span);
                }
            }
            Place::Field(base, field) => {
                let base_ptr = self
                    .read_place(base, span.clone())
                    .expect("INTERNAL COMPILER ERROR: Base must exist for field write");

                let field_node = IdentifierNode {
                    name: *field,
                    span: span.clone(),
                };
                let field_ptr = self
                    .try_get_field_ptr(base_ptr, &field_node)
                    .expect("INTERNAL COMPILER ERROR: Field must exist");

                self.emit_store(field_ptr, value, span);
            }
        }
    }

    fn cache_place_value(&mut self, place: Place, value: ValueId) {
        self.current_defs
            .entry(self.context.block_id)
            .or_default()
            .insert(place, value);
    }

    pub fn type_of_place(&self, place: &Place) -> Option<Type> {
        match place {
            Place::Local(decl_id) => {
                let decl = self.program.declarations.get(decl_id)?;
                match decl {
                    CheckedDeclaration::Var(v) => Some(v.constraint.clone()),
                    CheckedDeclaration::Function(f) => Some(Type::Fn(FnType {
                        params: f.params.clone(),
                        return_type: Box::new(f.return_type.clone()),
                    })),
                    _ => None,
                }
            }
            Place::Field(base, field) => {
                let base_ty = self.type_of_place(base)?;
                self.type_of_field(&base_ty, *field)
            }
        }
    }

    fn type_of_field(&self, base_ty: &Type, field: StringId) -> Option<Type> {
        use Type::*;

        let base_inner = match base_ty {
            Pointer(inner) => inner,
            _ => return None,
        };

        let struct_kind = match base_inner.as_ref() {
            Struct(s) => s,
            _ => return None,
        };

        struct_kind.get_field(&field).map(|(_, ty)| ty)
    }
}
