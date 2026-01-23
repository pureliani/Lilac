use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
};

use crate::{
    ast::{DeclarationId, IdentifierNode, Span},
    compile::interner::TagId,
    hir::{
        builders::{Function, ValueId},
        types::checked_type::Type,
    },
    parse::DocAnnotation,
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CheckedParam {
    pub identifier: IdentifierNode,
    pub ty: Type,
}

impl Ord for CheckedParam {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.ty.cmp(&other.ty) {
            Ordering::Equal => self.identifier.cmp(&other.identifier),
            other_order => other_order,
        }
    }
}

impl PartialOrd for CheckedParam {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
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

impl Ord for TagType {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.id.0.cmp(&other.id.0) {
            Ordering::Equal => self.value_type.cmp(&other.value_type),
            ord => ord,
        }
    }
}

impl PartialOrd for TagType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
    // Pointer to stack slot where the value is stored
    pub stack_ptr: ValueId,
    pub identifier: IdentifierNode,
    pub documentation: Option<DocAnnotation>,
    pub constraint: Type,
}

#[derive(Clone, Debug)]
pub enum CheckedDeclaration {
    TypeAlias(CheckedTypeAliasDecl),
    Function(Function),
    Var(CheckedVarDecl),
}
