use std::collections::HashSet;

use crate::{
    ast::{IdentifierNode, ModulePath, Span},
    compile::interner::StringId,
    hir::{types::checked_type::Type, utils::points_to::PathSegment},
};

#[derive(Debug, Clone)]
pub enum SemanticErrorKind {
    ArgumentAliasing {
        passed_arg_span: Span,
        passed_path: Vec<PathSegment>,
        aliased_arg_span: Span,
        aliased_path: Vec<PathSegment>,
    },
    UnsupportedUnionNarrowing,
    CannotNarrowNonUnion(Type),
    ValuedTagInIsExpression,
    UnreachableCode,
    DuplicateIdentifier(IdentifierNode),
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
    TryExplicitCast,
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

#[derive(Debug, Clone)]
pub enum SemanticErrorSeverity {
    Error,
    Warn,
    Info,
}

impl SemanticErrorKind {
    pub fn severity(&self) -> SemanticErrorSeverity {
        match self {
            SemanticErrorKind::ExpectedANumericOperand => SemanticErrorSeverity::Error,
            SemanticErrorKind::MixedSignedAndUnsigned => SemanticErrorSeverity::Error,
            SemanticErrorKind::MixedFloatAndInteger => SemanticErrorSeverity::Error,
            SemanticErrorKind::CannotCompareType { .. } => SemanticErrorSeverity::Error,
            SemanticErrorKind::UndeclaredIdentifier { .. } => {
                SemanticErrorSeverity::Error
            }
            SemanticErrorKind::ReturnKeywordOutsideFunction => {
                SemanticErrorSeverity::Error
            }
            SemanticErrorKind::BreakKeywordOutsideLoop => SemanticErrorSeverity::Error,
            SemanticErrorKind::ContinueKeywordOutsideLoop => SemanticErrorSeverity::Error,
            SemanticErrorKind::InvalidLValue => SemanticErrorSeverity::Error,
            SemanticErrorKind::TypeMismatch { .. } => SemanticErrorSeverity::Error,
            SemanticErrorKind::ReturnTypeMismatch { .. } => SemanticErrorSeverity::Error,
            SemanticErrorKind::UndeclaredType { .. } => SemanticErrorSeverity::Error,
            SemanticErrorKind::CannotAccess { .. } => SemanticErrorSeverity::Error,
            SemanticErrorKind::CannotCall { .. } => SemanticErrorSeverity::Error,
            SemanticErrorKind::CannotUseVariableDeclarationAsType => {
                SemanticErrorSeverity::Error
            }
            SemanticErrorKind::AccessToUndefinedField { .. } => {
                SemanticErrorSeverity::Error
            }
            SemanticErrorKind::FnArgumentCountMismatch { .. } => {
                SemanticErrorSeverity::Error
            }
            SemanticErrorKind::TypeAliasMustBeDeclaredAtTopLevel => {
                SemanticErrorSeverity::Error
            }
            SemanticErrorKind::DuplicateStructFieldInitializer { .. } => {
                SemanticErrorSeverity::Error
            }
            SemanticErrorKind::UnknownStructFieldInitializer { .. } => {
                SemanticErrorSeverity::Error
            }
            SemanticErrorKind::MissingStructFieldInitializers { .. } => {
                SemanticErrorSeverity::Error
            }
            SemanticErrorKind::DuplicateIdentifier { .. } => SemanticErrorSeverity::Error,
            SemanticErrorKind::IfExpressionMissingElse => SemanticErrorSeverity::Error,
            SemanticErrorKind::CannotCastType { .. } => SemanticErrorSeverity::Error,
            SemanticErrorKind::CannotIndex { .. } => SemanticErrorSeverity::Error,
            SemanticErrorKind::CannotStaticAccess { .. } => SemanticErrorSeverity::Error,
            SemanticErrorKind::AccessToUndefinedStaticField { .. } => {
                SemanticErrorSeverity::Error
            }
            SemanticErrorKind::CannotUseTypeDeclarationAsValue => {
                SemanticErrorSeverity::Error
            }
            SemanticErrorKind::CannotDeclareGlobalVariable => {
                SemanticErrorSeverity::Error
            }
            SemanticErrorKind::UnreachableCode => SemanticErrorSeverity::Error,
            SemanticErrorKind::FromStatementMustBeDeclaredAtTopLevel => {
                SemanticErrorSeverity::Error
            }
            SemanticErrorKind::ModuleNotFound { .. } => SemanticErrorSeverity::Error,
            SemanticErrorKind::CannotUseFunctionDeclarationAsType => {
                SemanticErrorSeverity::Error
            }
            SemanticErrorKind::SymbolNotExported { .. } => SemanticErrorSeverity::Error,
            SemanticErrorKind::ClosuresNotSupportedYet => SemanticErrorSeverity::Error,
            SemanticErrorKind::ValuedTagInIsExpression => SemanticErrorSeverity::Error,
            SemanticErrorKind::CannotNarrowNonUnion(_) => SemanticErrorSeverity::Error,
            SemanticErrorKind::UnsupportedUnionNarrowing => SemanticErrorSeverity::Error,
            SemanticErrorKind::ExpectedASignedNumericOperand => {
                SemanticErrorSeverity::Error
            }
            SemanticErrorKind::CannotGetLen { .. } => SemanticErrorSeverity::Error,
            SemanticErrorKind::ArgumentAliasing { .. } => SemanticErrorSeverity::Error,
            SemanticErrorKind::TryExplicitCast => SemanticErrorSeverity::Error,
        }
    }

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
            SemanticErrorKind::ReturnTypeMismatch { .. } => 11,
            SemanticErrorKind::UndeclaredType { .. } => 12,
            SemanticErrorKind::CannotAccess { .. } => 13,
            SemanticErrorKind::CannotCall { .. } => 14,
            SemanticErrorKind::CannotUseVariableDeclarationAsType => 15,
            SemanticErrorKind::AccessToUndefinedField { .. } => 16,
            SemanticErrorKind::FnArgumentCountMismatch { .. } => 17,
            SemanticErrorKind::TypeAliasMustBeDeclaredAtTopLevel => 18,
            SemanticErrorKind::DuplicateStructFieldInitializer { .. } => 19,
            SemanticErrorKind::UnknownStructFieldInitializer { .. } => 20,
            SemanticErrorKind::MissingStructFieldInitializers { .. } => 21,
            SemanticErrorKind::DuplicateIdentifier { .. } => 22,
            SemanticErrorKind::IfExpressionMissingElse => 23,
            SemanticErrorKind::CannotCastType { .. } => 24,
            SemanticErrorKind::CannotIndex { .. } => 25,
            SemanticErrorKind::CannotStaticAccess { .. } => 26,
            SemanticErrorKind::AccessToUndefinedStaticField { .. } => 27,
            SemanticErrorKind::CannotUseTypeDeclarationAsValue => 28,
            SemanticErrorKind::CannotDeclareGlobalVariable => 29,
            SemanticErrorKind::UnreachableCode => 30,
            SemanticErrorKind::FromStatementMustBeDeclaredAtTopLevel => 31,
            SemanticErrorKind::ModuleNotFound { .. } => 32,
            SemanticErrorKind::CannotUseFunctionDeclarationAsType => 33,
            SemanticErrorKind::SymbolNotExported { .. } => 34,
            SemanticErrorKind::ClosuresNotSupportedYet => 35,
            SemanticErrorKind::ValuedTagInIsExpression => 36,
            SemanticErrorKind::CannotNarrowNonUnion(_) => 37,
            SemanticErrorKind::UnsupportedUnionNarrowing => 38,
            SemanticErrorKind::ExpectedASignedNumericOperand => 39,
            SemanticErrorKind::CannotGetLen { .. } => 40,
            SemanticErrorKind::ArgumentAliasing { .. } => 41,
            SemanticErrorKind::TryExplicitCast => 42,
        }
    }
}
