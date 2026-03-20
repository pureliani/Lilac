use crate::{
    ast::Span,
    compile::interner::{StringId, TypeId},
    globals::{COMMON_IDENTIFIERS, TYPE_INTERNER},
    mir::types::{
        checked_declaration::{CheckedParam, FnType},
        ordered_float::{OrderedF32, OrderedF64},
    },
    tokenize::NumberKind,
};
use std::{cmp::Ordering, collections::BTreeSet, hash::Hash};

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum StructKind {
    // packed
    UserDefined(Vec<CheckedParam>),

    /// { id: u32, value: TaglessUnion }
    TaggedUnion(BTreeSet<TypeId>),

    /// { len: usize, cap: usize, ptr: ptr<T> }
    ListHeader(TypeId),

    /// { len: usize, cap: usize, ptr: ptr<u8> }
    StringHeader(Option<StringId>),
}

impl StructKind {
    pub fn fields(&self) -> Vec<(StringId, TypeId)> {
        match self {
            StructKind::UserDefined(params) => params
                .iter()
                .map(|p| (p.identifier.name, p.ty.id))
                .collect(),

            StructKind::ListHeader(elem_ty_id) => vec![
                (
                    COMMON_IDENTIFIERS.len,
                    TYPE_INTERNER.intern(&Type::USize(None)),
                ),
                (
                    COMMON_IDENTIFIERS.cap,
                    TYPE_INTERNER.intern(&Type::USize(None)),
                ),
                (
                    COMMON_IDENTIFIERS.ptr,
                    TYPE_INTERNER.intern(&Type::Pointer(*elem_ty_id)),
                ),
            ],

            StructKind::StringHeader(_) => vec![
                (
                    COMMON_IDENTIFIERS.len,
                    TYPE_INTERNER.intern(&Type::USize(None)),
                ),
                (
                    COMMON_IDENTIFIERS.cap,
                    TYPE_INTERNER.intern(&Type::USize(None)),
                ),
                (
                    COMMON_IDENTIFIERS.ptr,
                    TYPE_INTERNER
                        .intern(&Type::Pointer(TYPE_INTERNER.intern(&Type::U8(None)))),
                ),
            ],
            StructKind::TaggedUnion(variants) => {
                vec![
                    (
                        COMMON_IDENTIFIERS.id,
                        TYPE_INTERNER.intern(&Type::U32(None)),
                    ),
                    (
                        COMMON_IDENTIFIERS.val,
                        TYPE_INTERNER.intern(&Type::TaglessUnion(variants.clone())),
                    ),
                ]
            }
        }
    }

    /// Maps a field name -> (Index, TypeId)
    pub fn get_field(&self, name: &StringId) -> Option<(usize, TypeId)> {
        self.fields()
            .into_iter()
            .enumerate()
            .find(|(_, (field_name, _))| field_name == name)
            .map(|(index, (_, ty_id))| (index, ty_id))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Type {
    Void,
    Never,
    Unknown,
    Null,
    Bool(Option<bool>),
    U8(Option<u8>),
    U16(Option<u16>),
    U32(Option<u32>),
    U64(Option<u64>),
    USize(Option<usize>),
    ISize(Option<isize>),
    I8(Option<i8>),
    I16(Option<i16>),
    I32(Option<i32>),
    I64(Option<i64>),
    F32(Option<OrderedF32>),
    F64(Option<OrderedF64>),
    Pointer(TypeId),
    Struct(StructKind),
    TaglessUnion(BTreeSet<TypeId>),
    Fn(FnType),
}

impl Type {
    pub fn id(&self) -> TypeId {
        TYPE_INTERNER.intern(self)
    }

    pub fn unwrap_ptr(&self) -> TypeId {
        if let Type::Pointer(to) = self {
            return *to;
        }

        panic!("INTERNAL COMPILER ERROR: Called unwrap_ptr on non-pointer type")
    }

    pub fn from_number_kind(val: &NumberKind) -> Type {
        match *val {
            NumberKind::I64(v) => Type::I64(Some(v)),
            NumberKind::I32(v) => Type::I32(Some(v)),
            NumberKind::I16(v) => Type::I16(Some(v)),
            NumberKind::I8(v) => Type::I8(Some(v)),
            NumberKind::F32(v) => Type::F32(Some(OrderedF32(v))),
            NumberKind::F64(v) => Type::F64(Some(OrderedF64(v))),
            NumberKind::U64(v) => Type::U64(Some(v)),
            NumberKind::U32(v) => Type::U32(Some(v)),
            NumberKind::U16(v) => Type::U16(Some(v)),
            NumberKind::U8(v) => Type::U8(Some(v)),
            NumberKind::ISize(v) => Type::ISize(Some(v)),
            NumberKind::USize(v) => Type::USize(Some(v)),
        }
    }

    pub fn make_union(types: impl IntoIterator<Item = TypeId>) -> TypeId {
        let mut flat = BTreeSet::new();

        for ty_id in types {
            let ty = TYPE_INTERNER.resolve(ty_id);

            if matches!(ty, Type::Never) {
                continue;
            }

            if let Type::Struct(StructKind::TaggedUnion(variants)) = ty {
                flat.extend(variants);
            } else {
                flat.insert(ty_id);
            }
        }

        if flat.is_empty() {
            return TYPE_INTERNER.intern(&Type::Never);
        }

        if flat.len() == 1 {
            return *flat.iter().next().unwrap();
        }

        TYPE_INTERNER.intern(&Type::Struct(StructKind::TaggedUnion(flat)))
    }

    pub fn union(a: TypeId, b: TypeId) -> TypeId {
        Type::make_union(vec![a, b])
    }

    pub fn intersect(a: TypeId, b: TypeId) -> TypeId {
        let s1 = TYPE_INTERNER.resolve(a).into_set();
        let s2 = TYPE_INTERNER.resolve(b).into_set();

        let result_types = s1.intersection(&s2).copied().collect::<Vec<_>>();

        Type::make_union(result_types)
    }

    pub fn subtract(a: TypeId, b: TypeId) -> TypeId {
        let mut s1 = TYPE_INTERNER.resolve(a).into_set();
        let s2 = TYPE_INTERNER.resolve(b).into_set();

        for t in s2 {
            s1.remove(&t);
        }

        Type::make_union(s1)
    }

    fn into_set(self) -> BTreeSet<TypeId> {
        if matches!(self, Type::Never) {
            return BTreeSet::new();
        }
        if let Type::Struct(StructKind::TaggedUnion(variants)) = self {
            return variants;
        }

        BTreeSet::from([TYPE_INTERNER.intern(&self)])
    }

    pub fn get_union_variants(&self) -> Option<BTreeSet<TypeId>> {
        if let Type::Struct(StructKind::TaggedUnion(variants)) = self {
            Some(variants.clone())
        } else {
            None
        }
    }

    /// Helper to strip the literal value, returning the generic type.
    /// e.g., I32(Some(5)) -> I32(None)
    pub fn widen(&self) -> Self {
        match self {
            Type::Bool(_) => Type::Bool(None),
            Type::U8(_) => Type::U8(None),
            Type::U16(_) => Type::U16(None),
            Type::U32(_) => Type::U32(None),
            Type::U64(_) => Type::U64(None),
            Type::USize(_) => Type::USize(None),
            Type::ISize(_) => Type::ISize(None),
            Type::I8(_) => Type::I8(None),
            Type::I16(_) => Type::I16(None),
            Type::I32(_) => Type::I32(None),
            Type::I64(_) => Type::I64(None),
            Type::F32(_) => Type::F32(None),
            Type::F64(_) => Type::F64(None),

            Type::Struct(StructKind::StringHeader(_)) => {
                Type::Struct(StructKind::StringHeader(None))
            }

            _ => self.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct SpannedType {
    pub id: TypeId,
    pub span: Span,
}

impl Hash for SpannedType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl Eq for SpannedType {}
impl PartialEq for SpannedType {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Ord for SpannedType {
    fn cmp(&self, other: &Self) -> Ordering {
        self.id.cmp(&other.id)
    }
}

impl PartialOrd for SpannedType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
