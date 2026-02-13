use std::collections::{BTreeSet, HashSet};

use crate::{
    ast::{IdentifierNode, Span},
    globals::{STRING_INTERNER, TAG_INTERNER},
    hir::types::{
        checked_declaration::{CheckedParam, FnType},
        checked_type::{StructKind, Type},
    },
    tokenize::TokenKind,
};

pub fn token_kind_to_string(kind: &TokenKind) -> String {
    match kind {
        TokenKind::Identifier(id) => STRING_INTERNER.resolve(*id).to_string(),
        TokenKind::Punctuation(punctuation_kind) => punctuation_kind.to_string(),
        TokenKind::Keyword(keyword_kind) => keyword_kind.to_string(),
        TokenKind::String(value) => value.to_owned(),
        TokenKind::Number(number_kind) => number_kind.to_string(),
        TokenKind::Doc(value) => format!("---\n{}\n---", value),
    }
}

pub fn type_to_string(ty: &Type) -> String {
    let mut visited_set = HashSet::new();
    type_to_string_recursive(ty, &mut visited_set)
}

pub fn type_to_string_recursive(ty: &Type, visited_set: &mut HashSet<Type>) -> String {
    if !visited_set.insert(ty.clone()) {
        return "...".to_string();
    }

    let result = match ty {
        Type::Void => String::from("void"),
        Type::Bool => String::from("bool"),
        Type::U8 => String::from("u8"),
        Type::U16 => String::from("u16"),
        Type::U32 => String::from("u32"),
        Type::U64 => String::from("u64"),
        Type::USize => String::from("usize"),
        Type::ISize => String::from("isize"),
        Type::I8 => String::from("i8"),
        Type::I16 => String::from("i16"),
        Type::I32 => String::from("i32"),
        Type::I64 => String::from("i64"),
        Type::F32 => String::from("f32"),
        Type::F64 => String::from("f64"),
        Type::Unknown => String::from("unknown"),
        Type::Never => String::from("never"),
        Type::Struct(s) => {
            if std::env::var("DUMP_HIR").is_ok() {
                let fields = s
                    .fields()
                    .iter()
                    .map(|f| CheckedParam {
                        identifier: IdentifierNode {
                            name: f.0,
                            span: Span::default(),
                        },
                        ty: f.1.clone(),
                    })
                    .collect::<Vec<CheckedParam>>();

                struct_to_string_raw(&fields, visited_set)
            } else {
                struct_to_string(s, visited_set)
            }
        }
        Type::Fn(fn_type) => fn_signature_to_string(fn_type, visited_set),
        Type::Pointer(to) => {
            if std::env::var("DUMP_HIR").is_ok() {
                format!("ptr<{}>", type_to_string_recursive(to, visited_set))
            } else {
                type_to_string_recursive(to, visited_set)
            }
        }
        Type::UnionPayload(variants) => {
            union_variants_to_string(variants, visited_set, true)
        }
        Type::Tag(tag_id) => {
            let name_string_id = TAG_INTERNER.resolve(*tag_id);
            let name = STRING_INTERNER.resolve(name_string_id);
            format!("#{0}", name)
        }
    };

    visited_set.remove(ty);

    result
}

fn fn_signature_to_string(fn_type: &FnType, visited_set: &mut HashSet<Type>) -> String {
    let params_str = fn_type
        .params
        .iter()
        .map(|p| {
            format!(
                "{}: {}",
                STRING_INTERNER.resolve(p.identifier.name),
                type_to_string_recursive(&p.ty, visited_set)
            )
        })
        .collect::<Vec<String>>()
        .join(", ");

    let return_type_str = type_to_string_recursive(&fn_type.return_type, visited_set);

    format!("fn({}): {}", params_str, return_type_str)
}

fn struct_to_string(s: &StructKind, visited_set: &mut HashSet<Type>) -> String {
    match s {
        StructKind::Union(variants) => {
            if variants.is_empty() {
                return String::from("never");
            }

            union_variants_to_string(variants, visited_set, false)
        }
        StructKind::UserDefined(fields) => struct_to_string_raw(fields, visited_set),
        StructKind::ListHeader(item_type) => {
            let elem_type_str = type_to_string_recursive(item_type, visited_set);

            format!("{}[]", elem_type_str)
        }
        StructKind::StringHeader => String::from("string"),
    }
}

fn union_variants_to_string(
    variants: &BTreeSet<Type>,
    visited_set: &mut HashSet<Type>,
    raw: bool,
) -> String {
    let joined = variants
        .iter()
        .map(|tag| type_to_string_recursive(tag, visited_set))
        .collect::<Vec<String>>()
        .join(" | ");

    if raw {
        format!("oneOf<{}>", joined)
    } else {
        joined
    }
}

fn struct_to_string_raw(
    fields: &[CheckedParam],
    visited_set: &mut HashSet<Type>,
) -> String {
    let fields_str = fields
        .iter()
        .map(|f| {
            format!(
                "{}: {}",
                STRING_INTERNER.resolve(f.identifier.name),
                type_to_string_recursive(&f.ty, visited_set)
            )
        })
        .collect::<Vec<String>>()
        .join(", ");

    format!("{{ {} }}", fields_str)
}
