use crate::{
    compile::interner::{StringId, TagId},
    globals::COMMON_IDENTIFIERS,
    hir::{
        types::checked_declaration::{CheckedParam, FnType},
        utils::layout::get_layout_of,
    },
};
use std::{collections::BTreeSet, hash::Hash};

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum StructKind {
    UserDefined(Vec<CheckedParam>), // packed

    /// { id: u16, value: Buffer }
    Union(BTreeSet<Type>),

    /// { capacity: usize, len: usize, ptr: ptr<T> }
    ListHeader(Box<Type>),

    /// { is_heap_allocated: bool, len: usize, ptr: ptr<u8> }
    StringHeader,
}

impl Type {
    pub fn make_union(types: impl IntoIterator<Item = Type>) -> Type {
        let mut flat_set = BTreeSet::new();

        for ty in types {
            if matches!(ty, Type::Never) {
                continue;
            }

            if let Type::Pointer(inner) = &ty {
                if let Type::Struct(StructKind::Union(variants)) = &**inner {
                    flat_set.extend(variants.clone());
                    continue;
                }
            }

            flat_set.insert(ty);
        }

        match flat_set.len() {
            0 => Type::Never,
            1 => flat_set.into_iter().next().unwrap(),
            _ => Type::Pointer(Box::new(Type::Struct(StructKind::Union(flat_set)))),
        }
    }

    pub fn union(self, other: Type) -> Type {
        Type::make_union(vec![self, other])
    }

    pub fn intersect(self, other: Type) -> Type {
        let s1 = self.into_set();
        let s2 = other.into_set();
        let result = s1.intersection(&s2).cloned();

        Type::make_union(result)
    }

    pub fn subtract(self, other: Type) -> Type {
        let mut s1 = self.into_set();
        let s2 = other.into_set();

        for t in s2 {
            s1.remove(&t);
        }

        Type::make_union(s1)
    }

    fn into_set(self) -> BTreeSet<Type> {
        if matches!(self, Type::Never) {
            return BTreeSet::new();
        }

        if let Type::Pointer(inner) = &self {
            if let Type::Struct(StructKind::Union(variants)) = &**inner {
                return variants.clone();
            }
        }

        let mut set = BTreeSet::new();
        set.insert(self);
        set
    }
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
            StructKind::Union(variants) => {
                if variants.len() < 2 {
                    panic!(
                        "INTERNAL COMPILER ERROR: Unflattened or empty Union detected. \
                         Always use Type::make_union()"
                    );
                }

                let mut max_size = 0;
                let mut max_align = 1;

                for tag_type in variants {
                    let layout = get_layout_of(tag_type);
                    max_size = std::cmp::max(max_size, layout.size);
                    max_align = std::cmp::max(max_align, layout.alignment);
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

    Tag(TagId),

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

impl Type {
    pub fn try_unwrap_pointer(&self) -> Option<&Type> {
        if let Type::Pointer(inner) = self {
            Some(inner)
        } else {
            None
        }
    }
}
