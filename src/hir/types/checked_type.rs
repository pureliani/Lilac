use crate::{
    compile::interner::StringId,
    globals::COMMON_IDENTIFIERS,
    hir::types::checked_declaration::{CheckedParam, FnType, TagType},
};
use std::hash::Hash;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StructKind {
    UserDefined(Vec<CheckedParam>), // packed

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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

    Tag(TagType),

    Union(Vec<TagType>),

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
