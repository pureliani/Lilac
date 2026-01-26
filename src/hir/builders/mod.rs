use std::collections::{HashMap, HashSet};

use crate::{
    ast::{DeclarationId, IdentifierNode, ModulePath, Span},
    compile::interner::StringId,
    hir::{
        errors::SemanticError,
        instructions::{Instruction, Terminator},
        types::{
            checked_declaration::{CheckedDeclaration, CheckedParam},
            checked_type::Type,
        },
        utils::scope::Scope,
    },
};

pub mod basic_block;
pub mod emitters;
pub mod function;
pub mod module;
pub mod program;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BasicBlockId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ValueId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ConstantId(pub usize);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum LValue {
    Variable(DeclarationId),
    Field { base_ptr: ValueId, field: StringId },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct LoopJumpTargets {
    pub on_break: BasicBlockId,
    pub on_continue: BasicBlockId,
}

pub struct Program {
    pub modules: HashMap<ModulePath, Module>,
    pub value_types: HashMap<ValueId, Type>,
    pub declarations: HashMap<DeclarationId, CheckedDeclaration>,
    pub constant_data: HashMap<ConstantId, Vec<u8>>,
}

pub struct Module {
    pub path: ModulePath,
    pub root_scope: Scope,
}

#[derive(Debug, Clone)]
pub struct Function {
    // Signature
    pub id: DeclarationId,
    pub identifier: IdentifierNode,
    pub params: Vec<CheckedParam>,
    pub return_type: Type,
    pub is_exported: bool,

    // CFG
    pub entry_block: BasicBlockId,
    pub blocks: HashMap<BasicBlockId, BasicBlock>,

    pub value_definitions: HashMap<ValueId, BasicBlockId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PhiEntry {
    pub from: BasicBlockId,
    pub value: ValueId,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BasicBlockId,
    pub instructions: Vec<Instruction>,
    pub terminator: Option<Terminator>,
    pub predecessors: HashSet<BasicBlockId>,
    pub phis: HashMap<ValueId, HashSet<PhiEntry>>,
    pub sealed: bool,
}

pub trait BuilderContext {}
pub struct Builder<'a, C: BuilderContext> {
    pub context: C,
    pub program: &'a mut Program,

    pub errors: &'a mut Vec<SemanticError>,
    pub current_scope: Scope,

    pub current_defs: &'a mut HashMap<BasicBlockId, HashMap<LValue, ValueId>>,
    pub aliases: &'a mut HashMap<DeclarationId, LValue>,
    pub incomplete_phis: &'a mut HashMap<BasicBlockId, Vec<(ValueId, LValue, Span)>>,
}

pub struct InGlobal;
impl BuilderContext for InGlobal {}

pub struct InModule {
    pub path: ModulePath,
}
impl BuilderContext for InModule {}

pub struct InFunction {
    pub path: ModulePath,
    pub func_id: DeclarationId,
}
impl BuilderContext for InFunction {}

pub struct InBlock {
    pub path: ModulePath,
    pub func_id: DeclarationId,
    pub block_id: BasicBlockId,
}
impl BuilderContext for InBlock {}
