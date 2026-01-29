use crate::{
    ast::decl::VarDecl,
    hir::{
        builders::{Builder, InBlock, LValue},
        errors::{SemanticError, SemanticErrorKind},
        types::{
            checked_declaration::{CheckedDeclaration, CheckedVarDecl},
            checked_type::Type,
        },
        utils::{
            adjustments::{
                analyze_memory_adjustment, analyze_value_adjustment, check_is_assignable,
            },
            check_type::{check_type_annotation, TypeCheckerContext},
        },
    },
    tokenize::NumberKind,
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_var_decl(&mut self, var_decl: VarDecl) -> Result<(), SemanticError> {
        if self.current_scope.is_file_scope() {
            return Err(SemanticError {
                kind: SemanticErrorKind::CannotDeclareGlobalVariable,
                span: var_decl.identifier.span.clone(),
            });
        }

        let initial_value_span = var_decl.value.span.clone();

        let val_id = self.build_expr(var_decl.value)?;
        let val_type = self.get_value_type(&val_id).clone();

        let constraint = if let Some(annotation) = &var_decl.constraint {
            let mut type_ctx = TypeCheckerContext {
                scope: self.current_scope.clone(),
                declarations: &self.program.declarations,
                errors: self.errors,
            };

            let expected = check_type_annotation(&mut type_ctx, annotation);

            if !check_is_assignable(&val_type, &expected) {
                return Err(SemanticError {
                    span: initial_value_span.clone(),
                    kind: SemanticErrorKind::TypeMismatch {
                        expected: expected.clone(),
                        received: val_type.clone(),
                    },
                });
            }
            expected
        } else {
            val_type.clone()
        };

        let value_adjustment =
            analyze_value_adjustment(&val_type, initial_value_span.clone(), &constraint);

        let final_val_id = if let Ok(adj) = value_adjustment {
            self.apply_value_adjustment(val_id, adj)
        } else if let Type::Pointer(constraint_ptr_inner) = &constraint {
            let usize_one = self.emit_const_number(NumberKind::USize(1));
            let heap_ptr = self.emit_heap_alloc(*constraint_ptr_inner.clone(), usize_one);
            let heap_ptr_type = self.get_value_type(&heap_ptr);

            let memory_adjustment = analyze_memory_adjustment(
                &val_type,
                initial_value_span.clone(),
                heap_ptr_type,
            )?;

            self.adjust_memory_and_write(
                val_id,
                initial_value_span.clone(),
                heap_ptr,
                memory_adjustment,
            )?;

            heap_ptr
        } else {
            return Err(SemanticError {
                span: initial_value_span.clone(),
                kind: SemanticErrorKind::TypeMismatch {
                    expected: constraint.clone(),
                    received: val_type.clone(),
                },
            });
        };

        let lval = LValue::Variable(var_decl.id);
        self.remap_lvalue(lval, final_val_id);

        let checked_var_decl = CheckedVarDecl {
            id: var_decl.id,
            identifier: var_decl.identifier.clone(),
            documentation: var_decl.documentation,
            constraint,
        };

        self.program
            .declarations
            .insert(var_decl.id, CheckedDeclaration::Var(checked_var_decl));

        self.current_scope
            .map_name_to_decl(var_decl.identifier.name, var_decl.id);

        Ok(())
    }
}
