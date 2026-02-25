use crate::{
    ast::{expr::Expr, type_annotation::TypeAnnotation},
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        instructions::{CastInstr, Instruction},
        utils::{
            adjustments::check_assignable,
            check_type::{check_type_annotation, TypeCheckerContext},
        },
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_typecast_expr(&mut self, left: Expr, target: TypeAnnotation) -> ValueId {
        let source_span = left.span.clone();
        let source = self.build_expr(left);
        let source_type = self.get_value_type(source).clone();

        let mut type_ctx = TypeCheckerContext {
            scope: self.current_scope.clone(),
            declarations: &self.program.declarations,
            errors: self.errors,
        };
        let target_type = check_type_annotation(&mut type_ctx, &target);

        if source_type == target_type {
            return source;
        }

        if !check_assignable(&source_type, &target_type, true) {
            return self.report_error_and_get_poison(SemanticError {
                kind: SemanticErrorKind::CannotCastType {
                    source_type,
                    target_type,
                },
                span: source_span,
            });
        }

        let dest = self.new_value_id(target_type);
        self.push_instruction(Instruction::Cast(CastInstr { src: source, dest }));
        dest
    }
}
