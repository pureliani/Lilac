use std::hash::{Hash, Hasher};

use crate::{compile::interner::StringId, ModulePath};

pub mod decl;
pub mod expr;
pub mod stmt;
pub mod type_annotation;
pub mod visitor;

#[derive(Debug, Clone)]
pub struct IdentifierNode {
    pub name: StringId,
    pub span: Span,
}

impl Eq for IdentifierNode {}
impl PartialEq for IdentifierNode {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl Hash for IdentifierNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

#[derive(Debug, Clone)]
pub struct StringNode {
    pub value: String,
    pub len: usize,
    pub span: Span,
}

impl Eq for StringNode {}
impl PartialEq for StringNode {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
impl Hash for StringNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Copy, Default)]
pub struct Position {
    pub line: usize,
    pub col: usize,
    pub byte_offset: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Span {
    pub start: Position,
    pub end: Position,
    pub path: ModulePath,
}

impl Span {
    pub fn contains(&self, byte_offset: usize) -> bool {
        byte_offset >= self.start.byte_offset && byte_offset <= self.end.byte_offset
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct DeclarationId(pub usize);
