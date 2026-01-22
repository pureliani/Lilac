use std::collections::{HashMap, HashSet};

use crate::{
    ast::{
        decl::Param,
        type_annotation::{TagAnnotation, TypeAnnotation, TypeAnnotationKind},
        DeclarationId, IdentifierNode,
    },
    compile::interner::TagId,
    globals::TAG_INTERNER,
    hir::{
        errors::{SemanticError, SemanticErrorKind},
        types::{
            checked_declaration::{CheckedDeclaration, CheckedParam, FnType, TagType},
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

pub fn check_tag_annotation(
    ctx: &mut TypeCheckerContext,
    TagAnnotation {
        identifier,
        value_type,
        span,
    }: &TagAnnotation,
) -> TagType {
    let tag_id = TAG_INTERNER.intern(&identifier.name);
    let checked_value_type = value_type.as_ref().map(|v| check_type_annotation(ctx, v));

    TagType {
        id: tag_id,
        value_type: checked_value_type.clone().map(Box::new),
        span: span.clone(),
    }
}

pub fn check_type_annotation(
    ctx: &mut TypeCheckerContext,
    annotation: &TypeAnnotation,
) -> Type {
    let kind = match &annotation.kind {
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
        TypeAnnotationKind::Tag(t) => Type::Tag(check_tag_annotation(ctx, t)),
        TypeAnnotationKind::Union(tag_annotations) => {
            let mut checked_variants: Vec<TagType> =
                Vec::with_capacity(tag_annotations.len());
            let mut seen_tags: HashSet<TagId> = HashSet::new();

            for t in tag_annotations {
                let checked_tag = check_tag_annotation(ctx, t);

                if seen_tags.insert(checked_tag.id) {
                    checked_variants.push(checked_tag);
                } else {
                    ctx.errors.push(SemanticError {
                        span: t.span.clone(),
                        kind: SemanticErrorKind::DuplicateUnionVariant(
                            t.identifier.clone(),
                        ),
                    })
                }
            }

            checked_variants.sort_by(|a, b| a.id.0.cmp(&b.id.0));

            Type::Union(checked_variants)
        }
        // Passed by pointer
        TypeAnnotationKind::String => {
            let inner = Box::new(Type::Struct(StructKind::StringHeader));
            Type::Pointer {
                constraint: inner.clone(),
                narrowed_to: inner,
            }
        }
        TypeAnnotationKind::List(item_type) => {
            let checked_item_type = check_type_annotation(ctx, item_type);
            let inner =
                Box::new(Type::Struct(StructKind::ListHeader(Box::new(checked_item_type))));

            Type::Pointer {
                constraint: inner.clone(),
                narrowed_to: inner,
            }
        }
        TypeAnnotationKind::Struct(items) => {
            let checked_field_types = check_params(ctx, items);

            let packed = pack_struct(StructKind::UserDefined(checked_field_types));

            let inner = Box::new(Type::Struct(packed));

            Type::Pointer {
                constraint: inner.clone(),
                narrowed_to: inner,
            }
        }
    };

    kind
}
