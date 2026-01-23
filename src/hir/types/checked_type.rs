use crate::{
    compile::interner::StringId,
    globals::COMMON_IDENTIFIERS,
    hir::{
        types::checked_declaration::{CheckedParam, FnType, TagType},
        utils::layout::get_layout_of,
    },
};
use std::{collections::BTreeSet, hash::Hash};

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum StructKind {
    UserDefined(Vec<CheckedParam>), // packed

    /// { id: u16, value: T }
    Tag(TagType),

    /// { id: u16, value: Buffer }
    Union(BTreeSet<TagType>),

    /// { capacity: usize, len: usize, ptr: ptr<T> }
    ListHeader(Box<Type>),

    /// { is_heap_allocated: bool, len: usize, ptr: ptr<u8> }
    StringHeader,
}

impl StructKind {
    pub fn fields(&self) -> Vec<(StringId, Type)> {
        match self {
            StructKind::UserDefined(params) => params
                .iter()
                .map(|p| (p.identifier.name, p.ty.clone()))
                .collect(),

            StructKind::ListHeader(elem_ty) => vec![
                (COMMON_IDENTIFIERS.capacity, Type::USize),
                (COMMON_IDENTIFIERS.len, Type::USize),
                (COMMON_IDENTIFIERS.ptr, Type::Pointer(elem_ty.clone())),
            ],

            StructKind::StringHeader => vec![
                (COMMON_IDENTIFIERS.is_heap_allocated, Type::Bool),
                (COMMON_IDENTIFIERS.len, Type::USize),
                (COMMON_IDENTIFIERS.ptr, Type::Pointer(Box::new(Type::U8))),
            ],
            StructKind::Tag(tag) => {
                let mut fields = vec![(COMMON_IDENTIFIERS.id, Type::U16)];
                if let Some(val) = &tag.value_type {
                    fields.push((COMMON_IDENTIFIERS.value, *val.clone()));
                }
                fields
            }
            StructKind::Union(variants) => {
                let mut max_size = 0;
                let mut max_align = 1;

                for tag_type in variants {
                    if let Some(val_ty) = &tag_type.value_type {
                        let layout = get_layout_of(val_ty);
                        max_size = std::cmp::max(max_size, layout.size);
                        max_align = std::cmp::max(max_align, layout.alignment);
                    }
                }

                vec![
                    (COMMON_IDENTIFIERS.id, Type::U16),
                    (
                        COMMON_IDENTIFIERS.value,
                        Type::Buffer {
                            size: max_size,
                            alignment: max_align,
                        },
                    ),
                ]
            }
        }
    }

    /// Maps a field name -> (Index, Type).
    pub fn get_field(&self, name: &StringId) -> Option<(usize, Type)> {
        self.fields()
            .into_iter()
            .enumerate()
            .find(|(_, (field_name, _))| field_name == name)
            .map(|(index, (_, ty))| (index, ty))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Type {
    Void,
    Bool,
    U8,
    U16,
    U32,
    U64,
    USize,
    ISize,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,

    Pointer(Box<Type>),

    /// Represents any block of memory with named fields
    Struct(StructKind),

    /// Represents a function pointer signature
    Fn(FnType),

    Buffer {
        size: usize,
        alignment: usize,
    },

    Unknown,

    Never,
}
