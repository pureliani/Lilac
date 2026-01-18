use ariadne::{Color, Label, Report};

use crate::{
    compile::{Compiler, CompilerErrorKind},
    hir::{
        errors::SemanticErrorKind,
        utils::type_to_string::{token_kind_to_string, type_to_string},
    },
    parse::ParsingErrorKind,
    tokenize::TokenizationErrorKind,
};

impl Compiler {
    pub fn report_errors(&self) {
        let mut cache = self.files.lock().unwrap();

        for error in &self.errors {
            match error {
                CompilerErrorKind::Tokenization { path, errors } => {
                    errors.iter().for_each(|e| {
                        let span = e.span.start.byte_offset..e.span.end.byte_offset;
                        let report = Report::build(
                            ariadne::ReportKind::Error,
                            (path.clone(), span.clone()),
                        )
                        .with_code(format!("T{}", e.kind.code()));
                        let label =
                            Label::new((path.clone(), span)).with_color(Color::Red);

                        let final_report =
                            match &e.kind {
                                TokenizationErrorKind::UnterminatedString => {
                                    report.with_message("Unterminated string").with_label(
                                        label.with_message(
                                            "This string is not terminated",
                                        ),
                                    )
                                }
                                TokenizationErrorKind::UnknownToken(ref char_str) => {
                                    let readable_char = match char_str.as_str() {
                                        "\n" => "\"\\n\"".to_string(),
                                        "\r" => "\"\\r\"".to_string(),
                                        "\t" => "\"\\t\"".to_string(),
                                        " " => "\"<whitespace>\"".to_string(),
                                        _ => format!("'{}'", char_str),
                                    };

                                    report.with_message("Unknown token").with_label(
                                        label.with_message(format!(
                                            "This character {} is not recognized by the \
                                             tokenizer",
                                            readable_char
                                        )),
                                    )
                                }
                                TokenizationErrorKind::UnknownEscapeSequence => report
                                    .with_message("Unknown escape sequence")
                                    .with_label(label.with_message(
                                        "The escape sequence here is invalid",
                                    )),
                                TokenizationErrorKind::InvalidFloatingNumber => report
                                    .with_message("Invalid floating-point number")
                                    .with_label(label.with_message(
                                        "This is not a valid floating-point number",
                                    )),
                                TokenizationErrorKind::InvalidIntegerNumber => report
                                    .with_message("Invalid integer number")
                                    .with_label(label.with_message(
                                        "This is not a valid integer number",
                                    )),
                                TokenizationErrorKind::UnterminatedDoc => report
                                    .with_message("Unterminated documentation")
                                    .with_label(label.with_message(
                                        "This documentation block is not terminated",
                                    )),
                            };

                        let _ = final_report.finish().print(&mut *cache);
                    });
                }
                CompilerErrorKind::Parsing { path, errors } => {
                    errors.iter().for_each(|e| {
                        let span = e.span.start.byte_offset..e.span.end.byte_offset;
                        let report = Report::build(
                            ariadne::ReportKind::Error,
                            (path.clone(), span.clone()),
                        )
                        .with_code(format!("P{}", e.kind.code()));
                        let label =
                            Label::new((path.clone(), span)).with_color(Color::Red);

                        let final_report = match &e.kind {
                            ParsingErrorKind::DocMustBeFollowedByDeclaration => report
                                .with_message(
                                    "Documentation must be followed by a declaration of ",
                                )
                                .with_label(label.with_message(
                                    "This documentation must be followed by a \
                                     declaration of a type alias or a variable",
                                )),
                            ParsingErrorKind::ExpectedAnExpressionButFound(token) => {
                                report.with_message("Expected an expression").with_label(
                                    label.with_message(format!(
                                        "Expected an expression but instead found token \
                                         \"{}\"",
                                        token_kind_to_string(
                                            &token.kind,
                                            &self.interners
                                        )
                                    )),
                                )
                            }
                            ParsingErrorKind::ExpectedATypeButFound(token) => report
                                .with_message("Expected a type")
                                .with_label(label.with_message(format!(
                                    "Expected a type but instead found token \"{}\"",
                                    token_kind_to_string(&token.kind, &self.interners)
                                ))),
                            ParsingErrorKind::InvalidSuffixOperator(token) => report
                                .with_message("Invalid suffix operator")
                                .with_label(label.with_message(format!(
                                    "Invalid token as expression suffix operator \"{}\"",
                                    token_kind_to_string(&token.kind, &self.interners)
                                ))),
                            ParsingErrorKind::UnexpectedEndOfInput => {
                                report.with_message("Unexpected end of input").with_label(
                                    label.with_message("Unexpected end of input"),
                                )
                            }
                            ParsingErrorKind::ExpectedAnIdentifier => report
                                .with_message("Expected an identifier")
                                .with_label(label.with_message("Expected an identifier")),
                            ParsingErrorKind::ExpectedAPunctuationMark(
                                punctuation_kind,
                            ) => report
                                .with_message("Expected a punctuation mark")
                                .with_label(label.with_message(format!(
                                    "Expected the \"{}\" punctuation mark",
                                    punctuation_kind.to_string()
                                ))),
                            ParsingErrorKind::ExpectedAKeyword(keyword_kind) => report
                                .with_message("Expected a keyword")
                                .with_label(label.with_message(format!(
                                    "Expected the \"{}\" keyword",
                                    keyword_kind.to_string()
                                ))),
                            ParsingErrorKind::ExpectedAStringValue => report
                                .with_message("Expected a string literal")
                                .with_label(
                                    label.with_message("Expected a string literal"),
                                ),
                            ParsingErrorKind::ExpectedANumericValue => report
                                .with_message("Expected a numeric literal")
                                .with_label(
                                    label.with_message("Expected a numeric literal"),
                                ),
                            ParsingErrorKind::UnknownStaticMethod(identifier_node) => {
                                let name = self
                                    .interners
                                    .string_interner
                                    .resolve(identifier_node.name);
                                report.with_message("Unknown static method").with_label(
                                    label.with_message(format!(
                                        "Static method with name \"{}\" doesn't exist",
                                        name
                                    )),
                                )
                            }
                            ParsingErrorKind::UnexpectedStatementAfterFinalExpression => {
                                report
                                    .with_message(
                                        "Unexpected statement after final expression",
                                    )
                                    .with_label(label.with_message(
                                        "Final expression of a codeblock must not be \
                                         followed by another statement",
                                    ))
                            }
                            ParsingErrorKind::ExpectedStatementOrExpression { found } => {
                                report
                                    .with_message("Expected a statement or an expression")
                                    .with_label(label.with_message(format!(
                                        "Expected a statement or an expression but \
                                         instead found token \"{}\"",
                                        token_kind_to_string(
                                            &found.kind,
                                            &self.interners
                                        )
                                    )))
                            }
                            ParsingErrorKind::UnexpectedTokenAfterFinalExpression {
                                found,
                            } => report
                                .with_message("Unexpected token after final expression")
                                .with_label(label.with_message(format!(
                                    "Unexpected token \"{}\" after final expression",
                                    token_kind_to_string(&found.kind, &self.interners)
                                ))),
                            ParsingErrorKind::ExpectedATagTypeButFound(_) => report
                                .with_message("Expected a tag type")
                                .with_label(label.with_message(
                                    "Union variants must be tag types (starting with \
                                     '#')",
                                )),
                            ParsingErrorKind::ExpectedToBeFollowedByOneOfTheTokens(
                                tokens,
                            ) => {
                                let expected_list: Vec<String> = tokens
                                    .iter()
                                    .map(|t| {
                                        token_kind_to_string(&t.kind, &self.interners)
                                    })
                                    .collect();
                                let joined = expected_list.join(", ");
                                report.with_message("Unexpected token").with_label(
                                    label.with_message(format!(
                                        "Expected to be followed by one of: {}",
                                        joined
                                    )),
                                )
                            }
                        };

                        let _ = final_report.finish().print(&mut *cache);
                    });
                }
                CompilerErrorKind::Semantic { path, errors } => {
                    errors.iter().for_each(|e| {
                        let span = e.span.start.byte_offset..e.span.end.byte_offset;
                        let report = Report::build(
                            ariadne::ReportKind::Error,
                            (path.clone(), span.clone()),
                        )
                        .with_code(format!("S{}", e.kind.code()));
                        let label =
                            Label::new((path.clone(), span)).with_color(Color::Red);

                        let final_report = match &e.kind {
                            SemanticErrorKind::CannotNarrowNonUnion(ref ty) => {
                                let type_str = type_to_string(ty, &self.interners);
                                report.with_message("Redundant type check").with_label(
                                    label.with_message(format!(
                                        "This value is already known to be `{}`, the `::is()` operator can only be used on union types",
                                        type_str
                                    )),
                                )
                            }
                            SemanticErrorKind::ExpectedANumericOperand => report
                                .with_message("Expected a numeric operand")
                                .with_label(label.with_message(
                                    "Expected this value to have a numeric type",
                                )),
                            SemanticErrorKind::MixedSignedAndUnsigned => report
                                .with_message("Mixed signed and unsigned operands")
                                .with_label(label.with_message(
                                    "Mixing signed and unsigned operands in an \
                                     arithmetic operation is not allowed",
                                )),
                            SemanticErrorKind::MixedFloatAndInteger => report
                                .with_message("Mixed float and integer operands")
                                .with_label(label.with_message(
                                    "Mixing integer and floating-point numbers in an \
                                     arithmetic operation is not allowed",
                                )),
                            SemanticErrorKind::CannotCompareType { of, to } => report
                                .with_message("Cannot compare types")
                                .with_label(label.with_message(format!(
                                    "Cannot compare type \"{}\" to type \"{}\"",
                                    type_to_string(of, &self.interners),
                                    type_to_string(to, &self.interners)
                                ))),
                            SemanticErrorKind::UndeclaredIdentifier(id) => {
                                let name =
                                    self.interners.string_interner.resolve(id.name);

                                report.with_message("Undeclared identifier").with_label(
                                    label.with_message(format!(
                                        "Undeclared identifier \"{}\"",
                                        name
                                    )),
                                )
                            }
                            SemanticErrorKind::UndeclaredType(id) => {
                                let name =
                                    self.interners.string_interner.resolve(id.name);

                                report.with_message("Undeclared type").with_label(
                                    label.with_message(format!(
                                        "Undeclared type \"{}\"",
                                        name
                                    )),
                                )
                            }
                            SemanticErrorKind::ReturnKeywordOutsideFunction => report
                                .with_message(
                                    "Keyword \"return\" used outside of a function scope",
                                )
                                .with_label(label.with_message(
                                    "Cannot use the \"return\" keyword outside of a \
                                     function scope",
                                )),
                            SemanticErrorKind::BreakKeywordOutsideLoop => report
                                .with_message(
                                    "Keyword \"break\" used outside of a loop scope",
                                )
                                .with_label(label.with_message(
                                    "Cannot use the \"break\" keyword outside of a loop \
                                     scope",
                                )),
                            SemanticErrorKind::ContinueKeywordOutsideLoop => report
                                .with_message(
                                    "Keyword \"continue\" used outside of a loop scope",
                                )
                                .with_label(label.with_message(
                                    "Cannot use the \"continue\" keyword outside of a \
                                     loop scope",
                                )),
                            SemanticErrorKind::InvalidLValue => report
                                .with_message("Invalid assignment target")
                                .with_label(
                                    label.with_message("Invalid assignment target"),
                                ),
                            SemanticErrorKind::TypeMismatch { expected, received } => {
                                let expected_type_str =
                                    type_to_string(expected, &self.interners);
                                let received_type_str =
                                    type_to_string(received, &self.interners);

                                report.with_message("Type mismatch").with_label(
                                    label.with_message(format!(
                                        "Type mismatch, expected \"{}\", instead found \
                                         \"{}\"",
                                        expected_type_str, received_type_str
                                    )),
                                )
                            }
                            SemanticErrorKind::ReturnNotLastStatement => report
                                .with_message(
                                    "Expected the return statement to be the last \
                                     statement in the function",
                                )
                                .with_label(label.with_message(
                                    "Expected the return statement to be the last \
                                     statement in the function",
                                )),
                            SemanticErrorKind::ReturnTypeMismatch {
                                expected,
                                received,
                            } => report.with_message("Return type mismatch").with_label(
                                label.with_message(format!(
                                    "Expected the return value to be assignable to \
                                     \"{}\", found \"{}\"",
                                    type_to_string(expected, &self.interners),
                                    type_to_string(received, &self.interners)
                                )),
                            ),
                            SemanticErrorKind::CannotAccess(target) => report
                                .with_message("Cannot access field")
                                .with_label(label.with_message(format!(
                                    "Cannot use the access operator on the type \"{}\"",
                                    type_to_string(target, &self.interners)
                                ))),
                            SemanticErrorKind::CannotCall(target) => report
                                .with_message("Cannot use the function call operator")
                                .with_label(label.with_message(format!(
                                    "Cannot use the function-call operator on type \
                                     \"{}\"",
                                    type_to_string(target, &self.interners)
                                ))),
                            SemanticErrorKind::FnArgumentCountMismatch {
                                expected,
                                received,
                                ..
                            } => {
                                let s = if *expected > 1 { "s" } else { "" };
                                report
                                    .with_message("Function argument count mismatch")
                                    .with_label(label.with_message(format!(
                                        "This function expects {} argument{}, but \
                                         instead received {}",
                                        expected, s, received
                                    )))
                            }
                            SemanticErrorKind::CannotUseVariableDeclarationAsType => {
                                report
                                    .with_message(
                                        "Cannot use variable declaration as a type",
                                    )
                                    .with_label(label.with_message(
                                        "Cannot use variable declaration as a type",
                                    ))
                            }
                            SemanticErrorKind::AccessToUndefinedField(field) => {
                                let name =
                                    self.interners.string_interner.resolve(field.name);
                                report
                                    .with_message("Access to an undefined field")
                                    .with_label(label.with_message(format!(
                                        "Field \"{}\" is not defined",
                                        name
                                    )))
                            }
                            SemanticErrorKind::TypeAliasMustBeDeclaredAtTopLevel => {
                                report
                                    .with_message(
                                        "Type aliases must be declared in the file scope",
                                    )
                                    .with_label(label.with_message(
                                        "Type aliases must be declared in the file scope",
                                    ))
                            }
                            SemanticErrorKind::DuplicateStructFieldInitializer(id) => {
                                let name =
                                    self.interners.string_interner.resolve(id.name);
                                report
                                    .with_message(
                                        "Duplicate initializer for a struct field",
                                    )
                                    .with_label(label.with_message(format!(
                                        "Struct field \"{}\" cannot be initialized \
                                         multiple times",
                                        name
                                    )))
                            }
                            SemanticErrorKind::UnknownStructFieldInitializer(id) => {
                                let name =
                                    self.interners.string_interner.resolve(id.name);
                                report
                                    .with_message(
                                        "Unknown field in the struct initializer",
                                    )
                                    .with_label(label.with_message(format!(
                                        "Unknown struct field \"{}\"",
                                        name
                                    )))
                            }
                            SemanticErrorKind::MissingStructFieldInitializers(
                                missing_fields,
                            ) => {
                                let field_names: Vec<String> = missing_fields
                                    .iter()
                                    .map(|f| self.interners.string_interner.resolve(*f))
                                    .collect();
                                let joined = field_names
                                    .iter()
                                    .map(|n| format!("\"{}\"", n))
                                    .collect::<Vec<String>>()
                                    .join(", ");
                                report
                                    .with_message("Missing field initializers")
                                    .with_label(label.with_message(format!(
                                        "Missing initializers for the following struct \
                                         fields {}",
                                        joined
                                    )))
                            }
                            SemanticErrorKind::DuplicateIdentifier(id) => {
                                let identifier_name =
                                    self.interners.string_interner.resolve(id.name);
                                report.with_message("Duplicate identifier").with_label(
                                    label.with_message(format!(
                                        "Duplicate identifier declaration \"{}\"",
                                        identifier_name
                                    )),
                                )
                            }
                            SemanticErrorKind::CannotIndex(ty) => report
                                .with_message("Cannot index type")
                                .with_label(label.with_message(format!(
                                    "Type \"{}\" cannot be indexed",
                                    type_to_string(ty, &self.interners)
                                ))),
                            SemanticErrorKind::IncompatibleBranchTypes {
                                first,
                                second,
                            } => report
                                .with_message("Incompatible branch types")
                                .with_label(label.with_message(format!(
                                    "This branch returns \"{}\", but the previous \
                                     branch returned \"{}\"",
                                    type_to_string(second, &self.interners),
                                    type_to_string(first, &self.interners)
                                ))),
                            SemanticErrorKind::TypeMismatchExpectedOneOf {
                                expected,
                                received,
                            } => {
                                let mut expected_strings: Vec<String> = expected
                                    .iter()
                                    .map(|t| {
                                        format!(
                                            "\"{}\"",
                                            type_to_string(t, &self.interners)
                                        )
                                    })
                                    .collect();
                                // Sort for deterministic error messages
                                expected_strings.sort();
                                let expected_str = expected_strings.join(", ");

                                report.with_message("Type mismatch").with_label(
                                    label.with_message(format!(
                                        "Expected one of {}, but found \"{}\"",
                                        expected_str,
                                        type_to_string(received, &self.interners)
                                    )),
                                )
                            }
                            SemanticErrorKind::CannotStaticAccess(ty) => report
                                .with_message("Cannot perform static access")
                                .with_label(label.with_message(format!(
                                    "Type \"{}\" does not support static access via ::",
                                    type_to_string(ty, &self.interners)
                                ))),
                            SemanticErrorKind::AccessToUndefinedStaticField(id) => {
                                let name =
                                    self.interners.string_interner.resolve(id.name);
                                report.with_message("Undefined static field").with_label(
                                    label.with_message(format!(
                                        "Static field \"{}\" does not exist",
                                        name
                                    )),
                                )
                            }
                            SemanticErrorKind::IfExpressionMissingElse => report
                                .with_message("`if` expression missing `else` block")
                                .with_label(label.with_message(
                                    "`if` expressions used as values must have an \
                                     `else` block",
                                )),
                            SemanticErrorKind::CannotCastType {
                                source_type,
                                target_type,
                            } => report.with_message("Invalid type cast").with_label(
                                label.with_message(format!(
                                    "Cannot cast type \"{}\" to \"{}\"",
                                    type_to_string(source_type, &self.interners),
                                    type_to_string(target_type, &self.interners)
                                )),
                            ),
                            SemanticErrorKind::CannotUseTypeDeclarationAsValue => report
                                .with_message("Expected value, found type")
                                .with_label(label.with_message(
                                    "Cannot use a type declaration as a value",
                                )),
                            SemanticErrorKind::UnreachableCode => {
                                report.with_message("Unreachable code").with_label(
                                    label
                                        .with_message("This code will never be executed"),
                                )
                            }
                            SemanticErrorKind::FromStatementMustBeDeclaredAtTopLevel => {
                                report.with_message("Invalid import location").with_label(
                                    label.with_message(
                                        "`from` statements must be declared at the top \
                                         level of the file",
                                    ),
                                )
                            }
                            SemanticErrorKind::ModuleNotFound(path_buf) => report
                                .with_message("Module not found")
                                .with_label(label.with_message(format!(
                                    "Could not find module at path \"{}\"",
                                    path_buf.0.display()
                                ))),
                            SemanticErrorKind::CannotDeclareGlobalVariable => report
                                .with_message("Global variables not allowed")
                                .with_label(label.with_message(
                                    "Variables cannot be declared at the file scope \
                                     (top-level)",
                                )),
                            SemanticErrorKind::CannotUseFunctionDeclarationAsType => {
                                report
                                    .with_message("Expected type, found function")
                                    .with_label(label.with_message(
                                        "Cannot use a function declaration as a type",
                                    ))
                            }
                            SemanticErrorKind::UseOfUninitializedVariable(id) => {
                                let name =
                                    self.interners.string_interner.resolve(id.name);
                                report
                                    .with_message("Use of uninitialized variable")
                                    .with_label(label.with_message(format!(
                                        "Variable \"{}\" is used before being \
                                         initialized",
                                        name
                                    )))
                            }
                            SemanticErrorKind::DuplicateUnionVariant(id) => {
                                let name =
                                    self.interners.string_interner.resolve(id.name);
                                report.with_message("Duplicate union variant").with_label(
                                    label.with_message(format!(
                                        "Variant \"{}\" is defined multiple times in \
                                         this union",
                                        name
                                    )),
                                )
                            }
                            SemanticErrorKind::SymbolNotExported {
                                module_path,
                                symbol,
                            } => {
                                let name =
                                    self.interners.string_interner.resolve(symbol.name);
                                report.with_message("Symbol not exported").with_label(
                                    label.with_message(format!(
                                        "Symbol \"{}\" is not exported from module \
                                         \"{}\"",
                                        name,
                                        module_path.0.display()
                                    )),
                                )
                            }
                            SemanticErrorKind::ClosuresNotSupportedYet => report
                                .with_message("Closures not supported")
                                .with_label(label.with_message(
                                    "Capturing variables from outer scopes (closures) \
                                     is not supported yet",
                                )),
                            SemanticErrorKind::ValuedTagInIsExpression => report
                                .with_message(
                                    "Valued tag not allowed in `::is()` expression",
                                )
                                .with_label(label.with_message(
                                    "The `::is()` operator only checks the variant \
                                     identifier. Remove the value type (e.g., use \
                                     `#Tag` instead of `#Tag(Type)`)",
                                )),
                        };

                        let _ = final_report.finish().print(&mut *cache);
                    });
                }
                CompilerErrorKind::CouldNotReadFile { path, error } => {
                    println!(
                        "Could not read file at path \"{}\", error {}",
                        path.display(),
                        error
                    )
                }
                CompilerErrorKind::ModuleNotFound {
                    importing_module,
                    target_path,
                    error,
                } => {
                    println!(
                        "Module not found \"{}\", imported from \"{}\", error: {}",
                        target_path.display(),
                        importing_module.display(),
                        error
                    )
                }
            };
        }
    }
}
