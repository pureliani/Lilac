use lazy_static::lazy_static;

use crate::compile::interner::{SharedStringInterner, SharedTagInterner, StringId};
use crate::hir::builders::{BasicBlockId, ValueId};
use std::sync::atomic::{AtomicUsize, Ordering};

pub struct CommonIdentifiers {
    pub ptr: StringId,
    pub capacity: StringId,
    pub is_heap_allocated: StringId,
    pub len: StringId,
    pub id: StringId,
    pub value: StringId,
}

lazy_static! {
    pub static ref VALUE_COUNTER: AtomicUsize = AtomicUsize::new(0);
    pub static ref BLOCK_COUNTER: AtomicUsize = AtomicUsize::new(0);
    pub static ref CONSTANT_COUNTER: AtomicUsize = AtomicUsize::new(0);
    pub static ref DECLARATION_COUNTER: AtomicUsize = AtomicUsize::new(0);
    pub static ref TAG_INTERNER: SharedTagInterner = SharedTagInterner::default();
    pub static ref STRING_INTERNER: SharedStringInterner =
        SharedStringInterner::default();
    pub static ref COMMON_IDENTIFIERS: CommonIdentifiers = CommonIdentifiers {
        id: STRING_INTERNER.intern("id"),
        value: STRING_INTERNER.intern("value"),
        capacity: STRING_INTERNER.intern("capacity"),
        is_heap_allocated: STRING_INTERNER.intern("is_heap_allocated"),
        len: STRING_INTERNER.intern("len"),
        ptr: STRING_INTERNER.intern("ptr"),
    };
}

pub fn next_value_id() -> ValueId {
    ValueId(VALUE_COUNTER.fetch_add(1, Ordering::SeqCst))
}

pub fn next_block_id() -> BasicBlockId {
    BasicBlockId(BLOCK_COUNTER.fetch_add(1, Ordering::SeqCst))
}

pub fn next_constant_id() -> BasicBlockId {
    BasicBlockId(CONSTANT_COUNTER.fetch_add(1, Ordering::SeqCst))
}

pub fn next_declaration_id() -> BasicBlockId {
    BasicBlockId(DECLARATION_COUNTER.fetch_add(1, Ordering::SeqCst))
}

pub fn reset_counters() {
    VALUE_COUNTER.store(0, Ordering::SeqCst);
    BLOCK_COUNTER.store(0, Ordering::SeqCst);
    CONSTANT_COUNTER.store(0, Ordering::SeqCst);
    DECLARATION_COUNTER.store(0, Ordering::SeqCst);
    TAG_INTERNER.clear();
    STRING_INTERNER.clear();
}
