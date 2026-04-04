use crate::{
    ast::{DeclarationId, Span},
    compile::interner::{StringId, TypeId, TypeInterner},
    globals::COMMON_IDENTIFIERS,
    mir::types::{
        checked_declaration::{CheckedParam, FnType},
        ordered_float::{OrderedF32, OrderedF64},
    },
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
    StringHeader,
}

impl StructKind {
    pub fn fields(&self, t: &TypeInterner) -> Vec<(StringId, TypeId)> {
        match self {
            StructKind::UserDefined(params) => params
                .iter()
                .map(|p| (p.identifier.name, p.ty.id))
                .collect(),

            StructKind::ListHeader(elem_ty_id) => vec![
                (COMMON_IDENTIFIERS.len, t.usize(None)),
                (COMMON_IDENTIFIERS.cap, t.usize(None)),
                (COMMON_IDENTIFIERS.ptr, t.ptr(*elem_ty_id)),
            ],

            StructKind::StringHeader => vec![
                (COMMON_IDENTIFIERS.len, t.usize(None)),
                (COMMON_IDENTIFIERS.cap, t.usize(None)),
                (COMMON_IDENTIFIERS.ptr, t.ptr(t.u8(None))),
            ],
            StructKind::TaggedUnion(variants) => vec![
                (COMMON_IDENTIFIERS.id, t.u32(None)),
                (
                    COMMON_IDENTIFIERS.val,
                    t.intern(&Type::TaglessUnion(variants.clone())),
                ),
            ],
        }
    }

    pub fn get_field(
        &self,
        t: &TypeInterner,
        name: &StringId,
    ) -> Option<(usize, TypeId)> {
        self.fields(t)
            .into_iter()
            .enumerate()
            .find(|(_, (field_name, _))| field_name == name)
            .map(|(index, (_, ty_id))| (index, ty_id))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum LiteralType {
    Void,
    Never,
    Unknown,
    Null,
    Bool(bool),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    USize(usize),
    ISize(isize),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(OrderedF32),
    F64(OrderedF64),
    String(StringId),
    Fn(DeclarationId),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Type {
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
    Pointer(TypeId),
    Struct(StructKind),
    TaglessUnion(BTreeSet<TypeId>),
    IndirectFn(FnType),
    Literal(LiteralType),
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
