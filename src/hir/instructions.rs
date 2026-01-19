use std::collections::HashMap;

use crate::{
    ast::DeclarationId,
    hir::{
        builders::{BasicBlockId, ConstantId, ValueId},
        types::checked_type::Type,
    },
    tokenize::NumberKind,
};

#[derive(Clone, Debug)]
pub enum Instruction {
    // Constants
    ConstInt {
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
    IAdd {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    ISub {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    IMul {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    SDiv {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    UDiv {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    SRem {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    URem {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    FRem {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    FAdd {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    FSub {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    FMul {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    FDiv {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    FExt {
        dest: ValueId,
        src: ValueId,
        target_ty: Type,
    },
    FTrunc {
        dest: ValueId,
        src: ValueId,
        target_ty: Type,
    },
    IEq {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    INe {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    SLt {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    SLe {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    SGt {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    SGe {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    ULt {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    ULe {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    UGt {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    UGe {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    FEq {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    FNe {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    FLt {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    FLe {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    FGt {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    FGe {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    INeg {
        dest: ValueId,
        src: ValueId,
    },
    FNeg {
        dest: ValueId,
        src: ValueId,
    },
    BNot {
        dest: ValueId,
        src: ValueId,
    },
    IToF {
        dest: ValueId,
        src: ValueId,
        target_ty: Type,
    },
    FToI {
        dest: ValueId,
        src: ValueId,
        target_ty: Type,
    },
    SExt {
        dest: ValueId,
        src: ValueId,
        target_ty: Type,
    },
    ZExt {
        dest: ValueId,
        src: ValueId,
        target_ty: Type,
    },
    Trunc {
        dest: ValueId,
        src: ValueId,
        target_ty: Type,
    },
    // Memory
    StackAlloc {
        destination: ValueId,
        count: usize,
    },
    HeapAlloc {
        destination: ValueId,
        count: ValueId,
    },
    HeapFree {
        ptr: ValueId,
    },
    Store {
        ptr: ValueId,
        value: ValueId,
    },
    Load {
        destination: ValueId,
        ptr: ValueId,
    },
    // Access
    GetFieldPtr {
        destination: ValueId,
        base_ptr: ValueId,
        field_index: usize,
    },
    GetElementPtr {
        destination: ValueId,
        base_ptr: ValueId,
        index: ValueId,
    },
    // Calls
    FunctionCall {
        destination: Option<ValueId>,
        func: ValueId,
        args: Vec<ValueId>,
    },
    // Other
    RefineType {
        dest: ValueId,
        src: ValueId,
        new_type: Type,
    },
    Nop,
}

#[derive(Clone, Debug)]
pub enum Terminator {
    Jump {
        target: BasicBlockId,
        // param id -> argument id
        args: HashMap<ValueId, ValueId>,
    },
    CondJump {
        condition: ValueId,
        true_target: BasicBlockId,
        true_args: HashMap<ValueId, ValueId>,
        false_target: BasicBlockId,
        false_args: HashMap<ValueId, ValueId>,
    },
    Return {
        value: Option<ValueId>,
    },
    Unreachable,
}
