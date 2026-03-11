use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
};

use crate::tokenize::NumberKind;

/// A wrapper around NumberKind that implements Eq, Hash, and Ord
/// by treating floats as raw bits.
#[derive(Debug, Clone, Copy)]
pub struct OrderedNumberKind(pub NumberKind);

impl OrderedNumberKind {
    fn variant_index(&self) -> u8 {
        match self.0 {
            NumberKind::I64(_) => 0,
            NumberKind::I32(_) => 1,
            NumberKind::I16(_) => 2,
            NumberKind::I8(_) => 3,
            NumberKind::F32(_) => 4,
            NumberKind::F64(_) => 5,
            NumberKind::U64(_) => 6,
            NumberKind::U32(_) => 7,
            NumberKind::U16(_) => 8,
            NumberKind::U8(_) => 9,
            NumberKind::ISize(_) => 10,
            NumberKind::USize(_) => 11,
        }
    }
}

impl PartialEq for OrderedNumberKind {
    fn eq(&self, other: &Self) -> bool {
        match (self.0, other.0) {
            (NumberKind::I64(a), NumberKind::I64(b)) => a == b,
            (NumberKind::I32(a), NumberKind::I32(b)) => a == b,
            (NumberKind::I16(a), NumberKind::I16(b)) => a == b,
            (NumberKind::I8(a), NumberKind::I8(b)) => a == b,
            (NumberKind::U64(a), NumberKind::U64(b)) => a == b,
            (NumberKind::U32(a), NumberKind::U32(b)) => a == b,
            (NumberKind::U16(a), NumberKind::U16(b)) => a == b,
            (NumberKind::U8(a), NumberKind::U8(b)) => a == b,
            (NumberKind::ISize(a), NumberKind::ISize(b)) => a == b,
            (NumberKind::USize(a), NumberKind::USize(b)) => a == b,
            (NumberKind::F32(a), NumberKind::F32(b)) => a.to_bits() == b.to_bits(),
            (NumberKind::F64(a), NumberKind::F64(b)) => a.to_bits() == b.to_bits(),
            _ => false,
        }
    }
}

impl Eq for OrderedNumberKind {}

impl Hash for OrderedNumberKind {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(&self.0).hash(state);
        match self.0 {
            NumberKind::I64(v) => v.hash(state),
            NumberKind::I32(v) => v.hash(state),
            NumberKind::I16(v) => v.hash(state),
            NumberKind::I8(v) => v.hash(state),
            NumberKind::U64(v) => v.hash(state),
            NumberKind::U32(v) => v.hash(state),
            NumberKind::U16(v) => v.hash(state),
            NumberKind::U8(v) => v.hash(state),
            NumberKind::ISize(v) => v.hash(state),
            NumberKind::USize(v) => v.hash(state),
            NumberKind::F32(v) => v.to_bits().hash(state),
            NumberKind::F64(v) => v.to_bits().hash(state),
        }
    }
}

impl PartialOrd for OrderedNumberKind {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for OrderedNumberKind {
    fn cmp(&self, other: &Self) -> Ordering {
        let self_idx = self.variant_index();
        let other_idx = other.variant_index();

        match self_idx.cmp(&other_idx) {
            Ordering::Equal => match (self.0, other.0) {
                (NumberKind::I64(a), NumberKind::I64(b)) => a.cmp(&b),
                (NumberKind::I32(a), NumberKind::I32(b)) => a.cmp(&b),
                (NumberKind::I16(a), NumberKind::I16(b)) => a.cmp(&b),
                (NumberKind::I8(a), NumberKind::I8(b)) => a.cmp(&b),
                (NumberKind::U64(a), NumberKind::U64(b)) => a.cmp(&b),
                (NumberKind::U32(a), NumberKind::U32(b)) => a.cmp(&b),
                (NumberKind::U16(a), NumberKind::U16(b)) => a.cmp(&b),
                (NumberKind::U8(a), NumberKind::U8(b)) => a.cmp(&b),
                (NumberKind::ISize(a), NumberKind::ISize(b)) => a.cmp(&b),
                (NumberKind::USize(a), NumberKind::USize(b)) => a.cmp(&b),
                (NumberKind::F32(a), NumberKind::F32(b)) => a.to_bits().cmp(&b.to_bits()),
                (NumberKind::F64(a), NumberKind::F64(b)) => a.to_bits().cmp(&b.to_bits()),
                _ => unreachable!(
                    "OrderedNumberKind::cmp: variant indices matched but types differed"
                ),
            },
            other_order => other_order,
        }
    }
}
