use crate::{
    compile::interner::{StringId, TagId},
    hir::types::checked_declaration::{CheckedParam, FnType},
};
use std::{collections::BTreeSet, hash::Hash};

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
    Struct(Vec<CheckedParam>),
    Union(BTreeSet<Type>),
    List(Box<Type>),
    String,
    Fn(FnType),
    Unknown,
    Never,
}

impl Type {
    pub fn make_union(types: impl IntoIterator<Item = Type>) -> Type {
        let mut flat_set = BTreeSet::new();

        for ty in types {
            if matches!(ty, Type::Never) {
                continue;
            }

            if let Type::Union(variants) = ty {
                flat_set.extend(variants);
                continue;
            }

            flat_set.insert(ty);
        }

        match flat_set.len() {
            0 => Type::Never,
            1 => flat_set.into_iter().next().unwrap(),
            _ => Type::Union(flat_set),
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

        if let Type::Union(variants) = self {
            return variants;
        }

        BTreeSet::from([self])
    }

    pub fn as_union_variants(&self) -> Option<&BTreeSet<Type>> {
        match self {
            Type::Union(variants) => Some(variants),
            _ => None,
        }
    }

    /// Maps a struct field name -> (Index, Type)
    pub fn get_field(&self, name: &StringId) -> Option<(usize, Type)> {
        match self {
            Type::Struct(fields) => fields
                .iter()
                .enumerate()
                .find(|(_, param)| &param.identifier.name == name)
                .map(|(index, param)| (index, param.ty.clone())),
            _ => None,
        }
    }
}
