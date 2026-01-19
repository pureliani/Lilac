use crate::ast::DeclarationId;
use crate::compile::interner::{SharedStringInterner, SharedTagInterner, StringId};
use crate::hir::builders::{BasicBlockId, ConstantId, ValueId};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::LazyLock;

pub struct CommonIdentifiers {
    pub ptr: StringId,
    pub capacity: StringId,
    pub is_heap_allocated: StringId,
    pub len: StringId,
    pub id: StringId,
    pub value: StringId,
}

pub static VALUE_COUNTER: LazyLock<AtomicUsize> = LazyLock::new(|| AtomicUsize::new(0));
pub static BLOCK_COUNTER: LazyLock<AtomicUsize> = LazyLock::new(|| AtomicUsize::new(0));
pub static CONSTANT_COUNTER: LazyLock<AtomicUsize> =
    LazyLock::new(|| AtomicUsize::new(0));
pub static DECLARATION_COUNTER: LazyLock<AtomicUsize> =
    LazyLock::new(|| AtomicUsize::new(0));
pub static TAG_INTERNER: LazyLock<SharedTagInterner> =
    LazyLock::new(|| SharedTagInterner::default());
pub static STRING_INTERNER: LazyLock<SharedStringInterner> =
    LazyLock::new(|| SharedStringInterner::default());
pub static COMMON_IDENTIFIERS: LazyLock<CommonIdentifiers> =
    LazyLock::new(|| CommonIdentifiers {
        id: STRING_INTERNER.intern("id"),
        value: STRING_INTERNER.intern("value"),
        capacity: STRING_INTERNER.intern("capacity"),
        is_heap_allocated: STRING_INTERNER.intern("is_heap_allocated"),
        len: STRING_INTERNER.intern("len"),
        ptr: STRING_INTERNER.intern("ptr"),
    });

pub fn next_value_id() -> ValueId {
    ValueId(VALUE_COUNTER.fetch_add(1, Ordering::SeqCst))
}

pub fn next_block_id() -> BasicBlockId {
    BasicBlockId(BLOCK_COUNTER.fetch_add(1, Ordering::SeqCst))
}

pub fn next_constant_id() -> ConstantId {
    ConstantId(CONSTANT_COUNTER.fetch_add(1, Ordering::SeqCst))
}

pub fn next_declaration_id() -> DeclarationId {
    DeclarationId(DECLARATION_COUNTER.fetch_add(1, Ordering::SeqCst))
}

pub fn reset_globals() {
    VALUE_COUNTER.store(0, Ordering::SeqCst);
    BLOCK_COUNTER.store(0, Ordering::SeqCst);
    CONSTANT_COUNTER.store(0, Ordering::SeqCst);
    DECLARATION_COUNTER.store(0, Ordering::SeqCst);
    TAG_INTERNER.clear();
    STRING_INTERNER.clear();
}
