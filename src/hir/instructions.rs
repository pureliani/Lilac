use std::collections::BTreeSet;

use crate::{
    ast::DeclarationId,
    compile::interner::{StringId, TagId},
    hir::{
        builders::{BasicBlockId, ConstantId, ValueId},
        types::checked_type::Type,
    },
    tokenize::NumberKind,
};

#[derive(Clone, Debug)]
pub enum ConstInstr {
    ConstTag {
        dest: ValueId,
        val: TagId,
    },
    ConstNumber {
        dest: ValueId,
        val: NumberKind,
    },
    ConstBool {
        dest: ValueId,
        val: bool,
    },
    ConstString {
        dest: ValueId,
        constant_id: ConstantId,
    },
    ConstFn {
        dest: ValueId,
        decl_id: DeclarationId,
    },
    ConstVoid {
        dest: ValueId,
    },
}

#[derive(Clone, Debug)]
pub enum UnaryInstr {
    Neg { dest: ValueId, src: ValueId },
    Not { dest: ValueId, src: ValueId },
}

#[derive(Clone, Debug)]
pub enum BinaryInstr {
    Add {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    Sub {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    Mul {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    Div {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    Rem {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
}

#[derive(Clone, Debug)]
pub enum CompInstr {
    Eq {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    Neq {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    Lt {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    Lte {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    Gt {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    Gte {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
}

#[derive(Clone, Debug)]
pub enum StructInstr {
    Construct {
        dest: ValueId,
        fields: Vec<(StringId, ValueId)>,
    },
    ReadField {
        dest: ValueId,
        base: ValueId,
        field: StringId,
    },
    UpdateField {
        dest: ValueId,
        base: ValueId,
        field: StringId,
        value: ValueId,
    },
}

#[derive(Clone, Debug)]
pub enum UnionInstr {
    WrapInUnion {
        dest: ValueId,
        src: ValueId,
        target_variants: BTreeSet<Type>,
    },
    UnwrapUnion {
        dest: ValueId,
        src: ValueId,
        variant_type: Type,
    },
    TestVariant {
        dest: ValueId,
        src: ValueId,
        variant_type: Type,
    },
    WidenUnion {
        dest: ValueId,
        src: ValueId,
        target_variants: BTreeSet<Type>,
    },
    NarrowUnion {
        dest: ValueId,
        src: ValueId,
        target_variants: BTreeSet<Type>,
    },
}

#[derive(Clone, Debug)]
pub enum ListInstr {
    Init {
        dest: ValueId,
        element_type: Type,
        items: Vec<ValueId>,
    },
    Get {
        dest: ValueId,
        list: ValueId,
        index: ValueId,
    },
    Set {
        dest: ValueId,
        list: ValueId,
        index: ValueId,
        value: ValueId,
    },
    Len {
        dest: ValueId,
        list: ValueId,
    },
}

#[derive(Clone, Debug)]
pub enum Instruction {
    Binary(BinaryInstr),
    Unary(UnaryInstr),
    Const(ConstInstr),
    Comp(CompInstr),
    Struct(StructInstr),
    Union(UnionInstr),
    List(ListInstr),
    Call {
        dest: ValueId,
        func: ValueId,
        args: Vec<ValueId>,
    },
    Select {
        dest: ValueId,
        cond: ValueId,
        true_val: ValueId,
        false_val: ValueId,
    },
}

#[derive(Clone, Debug)]
pub enum Terminator {
    Jump {
        target: BasicBlockId,
    },
    CondJump {
        condition: ValueId,
        true_target: BasicBlockId,
        false_target: BasicBlockId,
    },
    Return {
        value: ValueId,
    },
}
