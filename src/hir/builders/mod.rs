use std::collections::{HashMap, HashSet};

use crate::{
    ast::{DeclarationId, IdentifierNode, Span},
    hir::{
        errors::SemanticError,
        instructions::{Instruction, Terminator},
        types::{
            checked_declaration::{CheckedDeclaration, CheckedParam},
            checked_type::Type,
        },
        utils::scope::Scope,
    },
    ModulePath,
};

#[macro_export]
macro_rules! unwrap_or_poison {
    ($builder:expr, $result:expr) => {
        match $result {
            Ok(val) => val,
            Err(e) => $builder.report_error_and_get_poison(e),
        }
    };
}

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

#[derive(Debug, Clone)]
pub struct TypePredicate {
    /// The original ValueId that was checked, could be a pointer or a value
    pub source: ValueId,
    /// The new ValueId to use in the true path
    pub true_id: ValueId,
    /// The new ValueId to use in the false path
    pub false_id: ValueId,
}

pub struct Place {
    /// The local variable (stack slot) this starts from
    pub root: ValueId,
    /// The sequence of projections (fields, index, deref)
    pub projections: Vec<Projection>,
}

pub enum Projection {
    Field(IdentifierNode),
    Index(ValueId, Span),
    Deref,
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

    // Metadata
    pub value_definitions: HashMap<ValueId, BasicBlockId>,
    /// Maps a boolean ValueId to the narrowing facts it carries
    pub predicates: HashMap<ValueId, TypePredicate>,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BasicBlockId,
    pub instructions: Vec<Instruction>,
    pub terminator: Option<Terminator>,
    pub predecessors: HashSet<BasicBlockId>,
    pub params: Vec<ValueId>,

    // list of (placeholder valueid, original valueid)
    pub incomplete_params: Vec<(ValueId, ValueId)>,

    // Map original valueid -> block-local valueid
    pub original_to_local_valueid: HashMap<ValueId, ValueId>,

    pub sealed: bool,
}

pub trait BuilderContext {}

pub struct Builder<'a, C: BuilderContext> {
    pub context: C,
    pub program: &'a mut Program,

    pub errors: &'a mut Vec<SemanticError>,
    pub current_scope: Scope,
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
