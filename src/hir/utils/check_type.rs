use std::collections::HashMap;

use crate::{
    ast::{
        decl::Param,
        type_annotation::{TypeAnnotation, TypeAnnotationKind},
        DeclarationId, IdentifierNode,
    },
    hir::{
        errors::{SemanticError, SemanticErrorKind},
        types::{
            checked_declaration::{CheckedDeclaration, CheckedParam, FnType},
            checked_type::{StructKind, Type},
        },
        utils::{layout::pack_struct, scope::Scope},
    },
};

pub struct TypeCheckerContext<'a> {
    pub scope: Scope,
    pub declarations: &'a HashMap<DeclarationId, CheckedDeclaration>,
    pub errors: &'a mut Vec<SemanticError>,
}

pub fn check_params(ctx: &mut TypeCheckerContext, params: &[Param]) -> Vec<CheckedParam> {
    params
        .iter()
        .map(|p| CheckedParam {
            ty: check_type_annotation(ctx, &p.constraint),
            identifier: p.identifier.clone(),
        })
        .collect()
}

pub fn check_type_identifier_annotation(
    ctx: &mut TypeCheckerContext,
    id: IdentifierNode,
) -> Type {
    ctx.scope
        .lookup(id.name)
        .map(|entry| {
            match ctx.declarations.get(&entry).unwrap_or_else(|| {
                panic!(
                    "INTERNAL COMPILER ERROR: Expected declarations to contain \
                     DeclarationId({}) key",
                    entry.0
                )
            }) {
                CheckedDeclaration::TypeAlias(decl) => (*decl.value).clone(),
                CheckedDeclaration::Function(_) => {
                    ctx.errors.push(SemanticError {
                        kind: SemanticErrorKind::CannotUseFunctionDeclarationAsType,
                        span: id.span.clone(),
                    });

                    Type::Unknown
                }
                CheckedDeclaration::Var(_) => {
                    ctx.errors.push(SemanticError {
                        kind: SemanticErrorKind::CannotUseVariableDeclarationAsType,
                        span: id.span.clone(),
                    });

                    Type::Unknown
                }
            }
        })
        .unwrap_or_else(|| {
            ctx.errors.push(SemanticError {
                span: id.span.clone(),
                kind: SemanticErrorKind::UndeclaredType(id),
            });

            Type::Unknown
        })
}

pub fn check_type_annotation(
    ctx: &mut TypeCheckerContext,
    annotation: &TypeAnnotation,
) -> Type {
    match &annotation.kind {
        TypeAnnotationKind::Void => Type::Void,
        TypeAnnotationKind::Bool => Type::Bool,
        TypeAnnotationKind::U8 => Type::U8,
        TypeAnnotationKind::U16 => Type::U16,
        TypeAnnotationKind::U32 => Type::U32,
        TypeAnnotationKind::U64 => Type::U64,
        TypeAnnotationKind::I8 => Type::I8,
        TypeAnnotationKind::I16 => Type::I16,
        TypeAnnotationKind::I32 => Type::I32,
        TypeAnnotationKind::I64 => Type::I64,
        TypeAnnotationKind::F32 => Type::F32,
        TypeAnnotationKind::F64 => Type::F64,
        TypeAnnotationKind::Identifier(id) => {
            check_type_identifier_annotation(ctx, id.clone())
        }
        TypeAnnotationKind::FnType {
            params,
            return_type,
        } => {
            let checked_params = check_params(ctx, params);
            let checked_return_type = check_type_annotation(ctx, return_type);

            Type::Fn(FnType {
                params: checked_params,
                return_type: Box::new(checked_return_type),
            })
        }
        TypeAnnotationKind::Tag(t) => Type::Tag(*t),
        // heap-allocated, passed by pointer
        TypeAnnotationKind::Union(variants) => {
            let mut checked_variants = Vec::new();

            for v in variants {
                checked_variants.push(check_type_annotation(ctx, v));
            }

            Type::make_union(checked_variants)
        }
        TypeAnnotationKind::String => {
            let inner = Box::new(Type::Struct(StructKind::StringHeader));
            Type::Pointer(inner)
        }
        TypeAnnotationKind::List(item_type) => {
            let checked_item_type = check_type_annotation(ctx, item_type);
            let inner = Box::new(Type::Struct(StructKind::ListHeader(Box::new(
                checked_item_type,
            ))));

            Type::Pointer(inner)
        }
        TypeAnnotationKind::Struct(items) => {
            let checked_field_types = check_params(ctx, items);

            let packed = pack_struct(StructKind::UserDefined(checked_field_types));

            let inner = Box::new(Type::Struct(packed));

            Type::Pointer(inner)
        }
    }
}
