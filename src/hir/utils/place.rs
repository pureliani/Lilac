use std::collections::HashMap;

use crate::{
    ast::{
        expr::{Expr, ExprKind},
        DeclarationId, IdentifierNode, Span,
    },
    compile::interner::StringId,
    globals::STRING_INTERNER,
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
    Temporary(ValueId),
}

impl Place {
    pub fn root(&self) -> Option<DeclarationId> {
        match self {
            Place::Local(id) => Some(*id),
            Place::Field(base, _) => base.root(),
            Place::Temporary(_) => None,
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

    /// e.g. `a.b.c.starts_with(a.b)` is true. `a.b.starts_with(a.c)` is false
    pub fn starts_with(&self, prefix: &Place) -> bool {
        if self.root() != prefix.root() {
            return false;
        }

        if self.root().is_none() {
            return false;
        }

        let self_path = self.path();
        let prefix_path = prefix.path();

        if prefix_path.len() > self_path.len() {
            return false;
        }

        self_path[..prefix_path.len()] == prefix_path[..]
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
            Place::Temporary(_) => self.clone(),
        }
    }
}

impl<'a> Builder<'a, InBlock> {
    pub fn place_to_string(&self, place: &Place) -> String {
        match place {
            Place::Local(decl_id) => {
                let decl = self.program.declarations.get(decl_id).unwrap();
                let name_id = match decl {
                    CheckedDeclaration::Var(v) => v.identifier.name,
                    CheckedDeclaration::Function(f) => f.identifier.name,
                    _ => panic!("INTERNAL COMPILER ERROR: Place referred to neither a variable or a function")
                };
                STRING_INTERNER.resolve(name_id).to_string()
            }
            Place::Field(base, field) => {
                format!(
                    "{}.{}",
                    self.place_to_string(base),
                    STRING_INTERNER.resolve(*field)
                )
            }
            Place::Temporary(_) => "<temporary>".to_string(),
        }
    }

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

                let base_val = self.read_place(&base, field.span.clone());
                let base_type = self.get_value_type(base_val).clone();

                if matches!(&base_type, Type::Struct(_)) {
                    Ok(Place::Field(Box::new(base), field.name))
                } else {
                    Err(SemanticError {
                        span: field.span.clone(),
                        kind: SemanticErrorKind::AccessToUndefinedField(field.clone()),
                    })
                }
            }
            _ => {
                let val_id = self.build_expr(expr);
                Ok(Place::Temporary(val_id))
            }
        }
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

    pub fn read_place(&mut self, place: &Place, span: Span) -> ValueId {
        let canonical = place.canonicalize(self.aliases);
        self.read_place_internal(&canonical, span)
    }

    fn read_place_internal(&mut self, place: &Place, span: Span) -> ValueId {
        match place {
            Place::Temporary(val_id) => *val_id,

            Place::Local(decl_id) => {
                self.read_variable(*decl_id, self.context.block_id, span)
            }

            Place::Field(base, field) => {
                if let Some(val) = self.narrowed_fields.get(place) {
                    return *val;
                }

                let base_val = self.read_place_internal(base, span.clone());
                let ident = IdentifierNode {
                    name: *field,
                    span: span.clone(),
                };

                let val_id = self.emit_read_struct_field(base_val, ident);

                self.narrowed_fields.insert(place.clone(), val_id);
                val_id
            }
        }
    }

    pub fn write_place(&mut self, place: &Place, value: ValueId, span: Span) {
        let canonical = place.canonicalize(self.aliases);
        self.write_place_internal(&canonical, value, span);
    }

    fn write_place_internal(&mut self, place: &Place, value: ValueId, span: Span) {
        match place {
            Place::Temporary(_) => {
                panic!("INTERNAL COMPILER ERROR: Cannot write to a temporary place");
            }
            Place::Local(decl_id) => {
                self.write_variable(*decl_id, self.context.block_id, value);
                self.narrowed_fields.retain(|k, _| !k.starts_with(place));
            }
            Place::Field(base, field) => {
                let base_val = self.read_place_internal(base, span.clone());
                let ident = IdentifierNode {
                    name: *field,
                    span: span.clone(),
                };

                self.emit_update_struct_field(base_val, ident, value);

                self.narrowed_fields.retain(|k, _| !k.starts_with(place));
                self.narrowed_fields.insert(place.clone(), value);
            }
        }
    }

    pub fn remap_place(&mut self, place: &Place, value: ValueId) {
        let canonical = place.canonicalize(self.aliases);
        match canonical {
            Place::Temporary(_) => {
                panic!("INTERNAL COMPILER ERROR: Cannot remap temporary")
            }
            Place::Local(decl_id) => {
                self.write_variable(decl_id, self.context.block_id, value);
            }
            Place::Field(_, _) => {
                self.narrowed_fields.insert(canonical, value);
            }
        }
    }

    pub fn type_of_place(&self, place: &Place) -> Type {
        match place {
            Place::Local(decl_id) => {
                let decl = self.program.declarations.get(decl_id).unwrap();
                match decl {
                    CheckedDeclaration::Var(v) => v.constraint.clone(),
                    CheckedDeclaration::Function(f) => Type::Fn(FnType {
                        params: f.params.clone(),
                        return_type: Box::new(f.return_type.clone()),
                    }),
                    _ => panic!(),
                }
            }
            Place::Field(base, field) => {
                let base_ty = self.type_of_place(base);
                base_ty
                    .get_field(field)
                    .expect(
                        "INTERNAL COMPILER ERROR: Expected Place::Field to have a type",
                    )
                    .1
            }
            Place::Temporary(val_id) => self
                .program
                .value_types
                .get(val_id)
                .expect(
                    "INTERNAL COMPILER ERROR: Expected Place::Temporary to have a type",
                )
                .clone(),
        }
    }
}
