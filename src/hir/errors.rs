use std::collections::HashSet;

use crate::{
    ast::{IdentifierNode, ModulePath, Span},
    compile::interner::StringId,
    hir::types::checked_type::Type,
};

#[derive(Debug, Clone)]
pub enum SemanticErrorKind {
    ExpectedTagWithoutValue {
        received: Type,
    },
    ExpectedTagWithValue {
        expected: Type,
    },
    ArgumentAliasing {
        passed: String,
        aliased_with: String,
    },
    UnsupportedUnionNarrowing,
    CannotNarrowNonUnion(Type),
    ValuedTagInIsExpression,
    UnreachableCode,
    DuplicateIdentifier(IdentifierNode),
    DuplicateUnionVariant(IdentifierNode),
    CannotIndex(Type),
    FromStatementMustBeDeclaredAtTopLevel,
    ModuleNotFound(ModulePath),
    CannotDeclareGlobalVariable,
    DuplicateStructFieldInitializer(IdentifierNode),
    UnknownStructFieldInitializer(IdentifierNode),
    MissingStructFieldInitializers(HashSet<StringId>),
    CannotCall(Type),
    ExpectedANumericOperand,
    ExpectedASignedNumericOperand,
    IncompatibleBranchTypes {
        first: Type,
        second: Type,
    },
    MixedSignedAndUnsigned,
    MixedFloatAndInteger,
    CannotCompareType {
        of: Type,
        to: Type,
    },
    UndeclaredIdentifier(IdentifierNode),
    UndeclaredType(IdentifierNode),
    ReturnKeywordOutsideFunction,
    BreakKeywordOutsideLoop,
    ContinueKeywordOutsideLoop,
    InvalidLValue,
    CannotGetLen(Type),
    TypeMismatch {
        expected: Type,
        received: Type,
    },
    TypeMismatchExpectedOneOf {
        expected: HashSet<Type>,
        received: Type,
    },
    ReturnNotLastStatement,
    ReturnTypeMismatch {
        expected: Type,
        received: Type,
    },
    CannotAccess(Type),
    CannotStaticAccess(Type),
    AccessToUndefinedField(IdentifierNode),
    AccessToUndefinedStaticField(IdentifierNode),
    FnArgumentCountMismatch {
        expected: usize,
        received: usize,
    },
    CannotUseVariableDeclarationAsType,
    CannotUseFunctionDeclarationAsType,
    CannotUseTypeDeclarationAsValue,
    TypeAliasMustBeDeclaredAtTopLevel,
    IfExpressionMissingElse,
    CannotCastType {
        source_type: Type,
        target_type: Type,
    },
    SymbolNotExported {
        module_path: ModulePath,
        symbol: IdentifierNode,
    },
    ClosuresNotSupportedYet,
}

#[derive(Debug, Clone)]
pub struct SemanticError {
    pub kind: SemanticErrorKind,
    pub span: Span,
}

impl SemanticErrorKind {
    pub fn code(&self) -> usize {
        match self {
            SemanticErrorKind::ExpectedANumericOperand => 1,
            SemanticErrorKind::MixedSignedAndUnsigned => 2,
            SemanticErrorKind::MixedFloatAndInteger => 3,
            SemanticErrorKind::CannotCompareType { .. } => 4,
            SemanticErrorKind::UndeclaredIdentifier { .. } => 5,
            SemanticErrorKind::ReturnKeywordOutsideFunction => 6,
            SemanticErrorKind::BreakKeywordOutsideLoop => 7,
            SemanticErrorKind::ContinueKeywordOutsideLoop => 8,
            SemanticErrorKind::InvalidLValue => 9,
            SemanticErrorKind::TypeMismatch { .. } => 10,
            SemanticErrorKind::ReturnNotLastStatement => 11,
            SemanticErrorKind::ReturnTypeMismatch { .. } => 12,
            SemanticErrorKind::UndeclaredType { .. } => 13,
            SemanticErrorKind::CannotAccess { .. } => 14,
            SemanticErrorKind::CannotCall { .. } => 15,
            SemanticErrorKind::CannotUseVariableDeclarationAsType => 16,
            SemanticErrorKind::AccessToUndefinedField { .. } => 18,
            SemanticErrorKind::FnArgumentCountMismatch { .. } => 19,
            SemanticErrorKind::TypeAliasMustBeDeclaredAtTopLevel => 20,
            SemanticErrorKind::DuplicateStructFieldInitializer { .. } => 21,
            SemanticErrorKind::UnknownStructFieldInitializer { .. } => 22,
            SemanticErrorKind::MissingStructFieldInitializers { .. } => 23,
            SemanticErrorKind::DuplicateIdentifier { .. } => 24,
            SemanticErrorKind::IncompatibleBranchTypes { .. } => 25,
            SemanticErrorKind::IfExpressionMissingElse => 26,
            SemanticErrorKind::TypeMismatchExpectedOneOf { .. } => 27,
            SemanticErrorKind::CannotCastType { .. } => 28,
            SemanticErrorKind::CannotIndex { .. } => 29,
            SemanticErrorKind::CannotStaticAccess { .. } => 30,
            SemanticErrorKind::AccessToUndefinedStaticField { .. } => 31,
            SemanticErrorKind::CannotUseTypeDeclarationAsValue => 32,
            SemanticErrorKind::CannotDeclareGlobalVariable => 33,
            SemanticErrorKind::UnreachableCode => 34,
            SemanticErrorKind::FromStatementMustBeDeclaredAtTopLevel => 35,
            SemanticErrorKind::ModuleNotFound { .. } => 36,
            SemanticErrorKind::CannotUseFunctionDeclarationAsType => 37,
            SemanticErrorKind::DuplicateUnionVariant(_) => 39,
            SemanticErrorKind::SymbolNotExported { .. } => 40,
            SemanticErrorKind::ClosuresNotSupportedYet => 41,
            SemanticErrorKind::ValuedTagInIsExpression => 42,
            SemanticErrorKind::CannotNarrowNonUnion(_) => 43,
            SemanticErrorKind::ExpectedTagWithoutValue { .. } => 44,
            SemanticErrorKind::ExpectedTagWithValue { .. } => 45,
            SemanticErrorKind::UnsupportedUnionNarrowing => 46,
            SemanticErrorKind::ExpectedASignedNumericOperand => 47,
            SemanticErrorKind::CannotGetLen { .. } => 48,
            SemanticErrorKind::ArgumentAliasing { .. } => 49,
        }
    }
}
