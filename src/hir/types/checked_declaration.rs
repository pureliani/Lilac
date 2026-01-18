use std::hash::{Hash, Hasher};

use crate::{
    ast::{DeclarationId, IdentifierNode, Span},
    compile::interner::TagId,
    hir::{
        builders::Function,
        cfg::{ControlFlowGraph, ValueId},
        types::checked_type::Type,
    },
    parse::DocAnnotation,
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CheckedParam {
    pub identifier: IdentifierNode,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct TagType {
    pub id: TagId,
    pub value_type: Option<Box<Type>>,
    pub span: Span,
}

impl Eq for TagType {}
impl PartialEq for TagType {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.value_type == other.value_type
    }
}
impl Hash for TagType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.value_type.hash(state);
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FnType {
    pub params: Vec<CheckedParam>,
    pub return_type: Box<Type>,
}

#[derive(Clone, Debug)]
pub struct CheckedTypeAliasDecl {
    pub id: DeclarationId,
    pub identifier: IdentifierNode,
    pub documentation: Option<DocAnnotation>,
    pub value: Box<Type>,
    pub is_exported: bool,
    pub span: Span,
}

impl Eq for CheckedTypeAliasDecl {}
impl PartialEq for CheckedTypeAliasDecl {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl Hash for CheckedTypeAliasDecl {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.identifier.hash(state);
        self.value.hash(state);
    }
}

#[derive(Clone, Debug)]
pub struct CheckedVarDecl {
    pub id: DeclarationId,
    /// ptr is a ValueId which holds a pointer to the actual data
    pub ptr: ValueId,
    pub identifier: IdentifierNode,
    pub documentation: Option<DocAnnotation>,
    pub constraint: Type,
}

#[derive(Clone, Debug)]
pub enum CheckedDeclaration {
    TypeAlias(CheckedTypeAliasDecl),
    Function(Function),
    Var(CheckedVarDecl),
    // This is for detecting the Temporal Dead Zone
    UninitializedVar {
        id: DeclarationId,
        identifier: IdentifierNode,
    },
}
