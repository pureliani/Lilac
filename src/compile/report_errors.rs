use ariadne::{Color, Label, Report};

use crate::{
    ast::{ModulePath, Span},
    compile::{Compiler, CompilerErrorKind},
    globals::STRING_INTERNER,
    hir::{
        errors::SemanticErrorKind,
        utils::type_to_string::{token_kind_to_string, type_to_string},
    },
    parse::ParsingErrorKind,
    tokenize::TokenizationErrorKind,
};

/// Generates a sort key: (Rank, Path, Offset)
/// Rank 0 = Spanned errors (come first)
/// Rank 1 = Global/IO errors (come last)
fn get_err_sort_key(err: &CompilerErrorKind) -> (u8, std::path::PathBuf, usize) {
    match err {
        CompilerErrorKind::Tokenization(e) => {
            (0, e.span.path.0.to_path_buf(), e.span.start.byte_offset)
        }
        CompilerErrorKind::Parsing(e) => {
            (0, e.span.path.0.to_path_buf(), e.span.start.byte_offset)
        }
        CompilerErrorKind::Semantic(e) => {
            (0, e.span.path.0.to_path_buf(), e.span.start.byte_offset)
        }
        CompilerErrorKind::CouldNotReadFile { path, .. } => (1, path.0.to_path_buf(), 0),
        CompilerErrorKind::ModuleNotFound { target_path, .. } => {
            (1, target_path.0.to_path_buf(), 0)
        }
    }
}

impl Compiler {
    pub fn report_errors(&mut self) {
        let mut cache = self.files.lock().unwrap();

        self.errors.sort_by(|a, b| {
            let key_a = get_err_sort_key(a);
            let key_b = get_err_sort_key(b);
            key_a.cmp(&key_b)
        });

        for error in &self.errors {
            match error {
                CompilerErrorKind::Tokenization(e) => {
                    let (path, range) = self.extract_span(&e.span);
                    let report = Report::build(
                        ariadne::ReportKind::Error,
                        (path.clone(), range.clone()),
                    )
                    .with_code(format!("T{}", e.kind.code()));
                    let label = Label::new((path, range)).with_color(Color::Red);

                    let final_report = match &e.kind {
                        TokenizationErrorKind::UnterminatedString => {
                            report.with_message("Unterminated string").with_label(
                                label.with_message("This string is not terminated"),
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
                                    "This character {} is not recognized",
                                    readable_char
                                )),
                            )
                        }
                        TokenizationErrorKind::UnknownEscapeSequence => {
                            report.with_message("Unknown escape sequence").with_label(
                                label.with_message("The escape sequence here is invalid"),
                            )
                        }
                        TokenizationErrorKind::InvalidFloatingNumber => report
                            .with_message("Invalid floating-point number")
                            .with_label(label.with_message(
                                "This is not a valid floating-point number",
                            )),
                        TokenizationErrorKind::InvalidIntegerNumber => {
                            report.with_message("Invalid integer number").with_label(
                                label.with_message("This is not a valid integer number"),
                            )
                        }
                        TokenizationErrorKind::UnterminatedDoc => report
                            .with_message("Unterminated documentation")
                            .with_label(label.with_message(
                                "This documentation block is not terminated",
                            )),
                    };
                    let _ = final_report.finish().print(&mut *cache);
                }

                CompilerErrorKind::Parsing(e) => {
                    let (path, range) = self.extract_span(&e.span);
                    let report = Report::build(
                        ariadne::ReportKind::Error,
                        (path.clone(), range.clone()),
                    )
                    .with_code(format!("P{}", e.kind.code()));
                    let label = Label::new((path, range)).with_color(Color::Red);

                    let final_report =
                        match &e.kind {
                            ParsingErrorKind::DocMustBeFollowedByDeclaration => report
                                .with_message(
                                    "Documentation must be followed by a declaration",
                                )
                                .with_label(label.with_message(
                                    "Expected a type alias or variable here",
                                )),
                            ParsingErrorKind::ExpectedAnExpressionButFound(token) => {
                                report.with_message("Expected an expression").with_label(
                                    label.with_message(format!(
                                        "Found token \"{}\"",
                                        token_kind_to_string(&token.kind)
                                    )),
                                )
                            }
                            ParsingErrorKind::ExpectedATypeButFound(token) => report
                                .with_message("Expected a type")
                                .with_label(label.with_message(format!(
                                    "Found token \"{}\"",
                                    token_kind_to_string(&token.kind)
                                ))),
                            ParsingErrorKind::InvalidSuffixOperator(token) => report
                                .with_message("Invalid suffix operator")
                                .with_label(label.with_message(format!(
                                    "Token \"{}\" cannot be used as a suffix",
                                    token_kind_to_string(&token.kind)
                                ))),
                            ParsingErrorKind::UnexpectedEndOfInput => report
                                .with_message("Unexpected end of input")
                                .with_label(label.with_message("Input ended abruptly")),
                            ParsingErrorKind::ExpectedAnIdentifier => report
                                .with_message("Expected an identifier")
                                .with_label(label.with_message("Expected an identifier")),
                            ParsingErrorKind::ExpectedAPunctuationMark(p) => report
                                .with_message("Expected punctuation")
                                .with_label(label.with_message(format!(
                                    "Expected \"{}\"",
                                    p.to_string()
                                ))),
                            ParsingErrorKind::ExpectedAKeyword(k) => report
                                .with_message("Expected keyword")
                                .with_label(label.with_message(format!(
                                    "Expected \"{}\"",
                                    k.to_string()
                                ))),
                            ParsingErrorKind::ExpectedAStringValue => report
                                .with_message("Expected string literal")
                                .with_label(label.with_message("Expected a string")),
                            ParsingErrorKind::ExpectedANumericValue => report
                                .with_message("Expected numeric literal")
                                .with_label(label.with_message("Expected a number")),
                            ParsingErrorKind::UnknownStaticMethod(id) => {
                                let name = STRING_INTERNER.resolve(id.name);
                                report.with_message("Unknown static method").with_label(
                                    label.with_message(format!(
                                        "Method \"{}\" doesn't exist",
                                        name
                                    )),
                                )
                            }
                            ParsingErrorKind::UnexpectedStatementAfterFinalExpression => {
                                report.with_message("Unexpected statement").with_label(
                                    label.with_message(
                                        "Statements cannot follow the final expression",
                                    ),
                                )
                            }
                            ParsingErrorKind::ExpectedStatementOrExpression { found } => {
                                report
                                    .with_message("Expected statement or expression")
                                    .with_label(label.with_message(format!(
                                        "Found \"{}\"",
                                        token_kind_to_string(&found.kind)
                                    )))
                            }
                            ParsingErrorKind::UnexpectedTokenAfterFinalExpression {
                                found,
                            } => report.with_message("Unexpected token").with_label(
                                label.with_message(format!(
                                    "Token \"{}\" follows final expression",
                                    token_kind_to_string(&found.kind)
                                )),
                            ),
                            ParsingErrorKind::ExpectedATagTypeButFound(_) => {
                                report.with_message("Expected a tag type").with_label(
                                    label.with_message(
                                        "Union variants must start with '#'",
                                    ),
                                )
                            }
                            ParsingErrorKind::ExpectedToBeFollowedByOneOfTheTokens(
                                tokens,
                            ) => {
                                let expected: Vec<_> = tokens
                                    .iter()
                                    .map(|t| token_kind_to_string(&t.kind))
                                    .collect();
                                report.with_message("Unexpected token").with_label(
                                    label.with_message(format!(
                                        "Expected one of: {}",
                                        expected.join(", ")
                                    )),
                                )
                            }
                        };
                    let _ = final_report.finish().print(&mut *cache);
                }

                CompilerErrorKind::Semantic(e) => {
                    let (path, range) = self.extract_span(&e.span);
                    let report = Report::build(
                        ariadne::ReportKind::Error,
                        (path.clone(), range.clone()),
                    )
                    .with_code(format!("S{}", e.kind.code()));
                    let label = Label::new((path, range)).with_color(Color::Red);

                    let final_report = match &e.kind {
                        SemanticErrorKind::CannotNarrowNonUnion(ref ty) => {
                            let type_str = type_to_string(ty);
                            report.with_message("Redundant type check").with_label(
                                label.with_message(format!(
                                    "Value is already `{}`, `::is()` only works on \
                                     unions",
                                    type_str
                                )),
                            )
                        }
                        SemanticErrorKind::ExpectedANumericOperand => {
                            report.with_message("Expected numeric operand").with_label(
                                label.with_message("This must be a numeric type"),
                            )
                        }
                        SemanticErrorKind::MixedSignedAndUnsigned => {
                            report.with_message("Mixed signedness").with_label(
                                label.with_message(
                                    "Cannot mix signed and unsigned operands",
                                ),
                            )
                        }
                        SemanticErrorKind::MixedFloatAndInteger => {
                            report.with_message("Mixed types").with_label(
                                label.with_message(
                                    "Cannot mix float and integer operands",
                                ),
                            )
                        }
                        SemanticErrorKind::CannotCompareType { of, to } => report
                            .with_message("Incompatible comparison")
                            .with_label(label.with_message(format!(
                                "Cannot compare \"{}\" to \"{}\"",
                                type_to_string(of),
                                type_to_string(to)
                            ))),
                        SemanticErrorKind::UndeclaredIdentifier(id) => {
                            let name = STRING_INTERNER.resolve(id.name);
                            report.with_message("Undeclared identifier").with_label(
                                label
                                    .with_message(format!("\"{}\" is not defined", name)),
                            )
                        }
                        SemanticErrorKind::UndeclaredType(id) => {
                            let name = STRING_INTERNER.resolve(id.name);
                            report.with_message("Undeclared type").with_label(
                                label.with_message(format!(
                                    "Type \"{}\" is not defined",
                                    name
                                )),
                            )
                        }
                        SemanticErrorKind::TypeMismatch { expected, received } => report
                            .with_message("Type mismatch")
                            .with_label(label.with_message(format!(
                                "Expected \"{}\", found \"{}\"",
                                type_to_string(expected),
                                type_to_string(received)
                            ))),
                        SemanticErrorKind::ReturnTypeMismatch { expected, received } => {
                            report.with_message("Return type mismatch").with_label(
                                label.with_message(format!(
                                    "Function expects \"{}\", but returned \"{}\"",
                                    type_to_string(expected),
                                    type_to_string(received)
                                )),
                            )
                        }
                        SemanticErrorKind::ModuleNotFound(path_buf) => report
                            .with_message("Module not found")
                            .with_label(label.with_message(format!(
                                "Could not find module at \"{}\"",
                                path_buf.0.display()
                            ))),
                        SemanticErrorKind::SymbolNotExported {
                            module_path,
                            symbol,
                        } => {
                            let name = STRING_INTERNER.resolve(symbol.name);
                            report.with_message("Symbol not exported").with_label(
                                label.with_message(format!(
                                    "\"{}\" is not exported from \"{}\"",
                                    name,
                                    module_path.0.display()
                                )),
                            )
                        }
                        SemanticErrorKind::ValuedTagInIsExpression => report
                            .with_message("Valued tag not allowed in `::is()` expression")
                            .with_label(label.with_message(
                                "The `::is()` operator only checks the variant \
                                 identifier. Remove the value type (e.g., use `#Tag` \
                                 instead of `#Tag(Type)`)",
                            )),
                        SemanticErrorKind::UnreachableCode => {
                            report.with_message("Unreachable code").with_label(
                                label.with_message("This code will never be executed"),
                            )
                        }
                        SemanticErrorKind::DuplicateIdentifier(id) => {
                            let identifier_name = STRING_INTERNER.resolve(id.name);
                            report.with_message("Duplicate identifier").with_label(
                                label.with_message(format!(
                                    "Duplicate identifier declaration \"{}\"",
                                    identifier_name
                                )),
                            )
                        }
                        SemanticErrorKind::DuplicateUnionVariant(id) => {
                            let name = STRING_INTERNER.resolve(id.name);
                            report.with_message("Duplicate union variant").with_label(
                                label.with_message(format!(
                                    "Variant \"{}\" is defined multiple times in this \
                                     union",
                                    name
                                )),
                            )
                        }
                        SemanticErrorKind::CannotIndex(ty) => report
                            .with_message("Cannot index type")
                            .with_label(label.with_message(format!(
                                "Type \"{}\" cannot be indexed",
                                type_to_string(ty)
                            ))),
                        SemanticErrorKind::FromStatementMustBeDeclaredAtTopLevel => {
                            report.with_message("Invalid import location").with_label(
                                label.with_message(
                                    "`from` statements must be declared at the top \
                                     level of the file",
                                ),
                            )
                        }
                        SemanticErrorKind::CannotDeclareGlobalVariable => report
                            .with_message("Global variables not allowed")
                            .with_label(label.with_message(
                                "Variables cannot be declared at the file scope \
                                 (top-level)",
                            )),
                        SemanticErrorKind::DuplicateStructFieldInitializer(id) => {
                            let name = STRING_INTERNER.resolve(id.name);

                            report
                                .with_message("Duplicate initializer for a struct field")
                                .with_label(label.with_message(format!(
                                    "Struct field \"{}\" cannot be initialized multiple \
                                     times",
                                    name
                                )))
                        }
                        SemanticErrorKind::UnknownStructFieldInitializer(id) => {
                            let name = STRING_INTERNER.resolve(id.name);

                            report
                                .with_message("Unknown field in the struct initializer")
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
                                .map(|f| STRING_INTERNER.resolve(*f))
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
                        SemanticErrorKind::CannotCall(target) => report
                            .with_message("Cannot use the function call operator")
                            .with_label(label.with_message(format!(
                                "Cannot use the function-call operator on type \"{}\"",
                                type_to_string(target)
                            ))),
                        SemanticErrorKind::IncompatibleBranchTypes { first, second } => {
                            report.with_message("Incompatible branch types").with_label(
                                label.with_message(format!(
                                    "This branch returns \"{}\", but the previous \
                                     branch returned \"{}\"",
                                    type_to_string(second),
                                    type_to_string(first)
                                )),
                            )
                        }
                        SemanticErrorKind::UseOfUninitializedVariable(id) => {
                            let name = STRING_INTERNER.resolve(id.name);
                            report
                                .with_message("Use of uninitialized variable")
                                .with_label(label.with_message(format!(
                                    "Variable \"{}\" is used before being initialized",
                                    name
                                )))
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
                                "Cannot use the \"continue\" keyword outside of a loop \
                                 scope",
                            )),
                        SemanticErrorKind::InvalidLValue => report
                            .with_message("Invalid assignment target")
                            .with_label(label.with_message("Invalid assignment target")),
                        SemanticErrorKind::TypeMismatchExpectedOneOf {
                            expected,
                            received,
                        } => {
                            let mut expected_strings: Vec<String> = expected
                                .iter()
                                .map(|t| format!("\"{}\"", type_to_string(t)))
                                .collect();

                            expected_strings.sort();
                            let expected_str = expected_strings.join(", ");

                            report.with_message("Type mismatch").with_label(
                                label.with_message(format!(
                                    "Expected one of {}, but found \"{}\"",
                                    expected_str,
                                    type_to_string(received)
                                )),
                            )
                        }
                        SemanticErrorKind::ReturnNotLastStatement => report
                            .with_message(
                                "Expected the return statement to be the last statement \
                                 in the function",
                            )
                            .with_label(label.with_message(
                                "Expected the return statement to be the last statement \
                                 in the function",
                            )),
                        SemanticErrorKind::CannotAccess(target) => report
                            .with_message("Cannot access field")
                            .with_label(label.with_message(format!(
                                "Cannot use the access operator on the type \"{}\"",
                                type_to_string(target)
                            ))),
                        SemanticErrorKind::CannotStaticAccess(_) => todo!(),
                        SemanticErrorKind::AccessToUndefinedField(field) => {
                            let name = STRING_INTERNER.resolve(field.name);
                            report
                                .with_message("Access to an undefined field")
                                .with_label(label.with_message(format!(
                                    "Field \"{}\" is not defined",
                                    name
                                )))
                        }
                        SemanticErrorKind::AccessToUndefinedStaticField(id) => {
                            let name = STRING_INTERNER.resolve(id.name);
                            report.with_message("Undefined static field").with_label(
                                label.with_message(format!(
                                    "Static field \"{}\" does not exist",
                                    name
                                )),
                            )
                        }
                        SemanticErrorKind::FnArgumentCountMismatch {
                            expected,
                            received,
                        } => {
                            let s = if *expected > 1 { "s" } else { "" };
                            report
                                .with_message("Function argument count mismatch")
                                .with_label(label.with_message(format!(
                                    "This function expects {} argument{}, but instead \
                                     received {}",
                                    expected, s, received
                                )))
                        }
                        SemanticErrorKind::CannotUseVariableDeclarationAsType => report
                            .with_message("Cannot use variable declaration as a type")
                            .with_label(label.with_message(
                                "Cannot use variable declaration as a type",
                            )),
                        SemanticErrorKind::CannotUseFunctionDeclarationAsType => report
                            .with_message("Expected type, found function")
                            .with_label(label.with_message(
                                "Cannot use a function declaration as a type",
                            )),
                        SemanticErrorKind::CannotUseTypeDeclarationAsValue => report
                            .with_message("Expected value, found type")
                            .with_label(label.with_message(
                                "Cannot use a type declaration as a value",
                            )),
                        SemanticErrorKind::TypeAliasMustBeDeclaredAtTopLevel => report
                            .with_message(
                                "Type aliases must be declared in the file scope",
                            )
                            .with_label(label.with_message(
                                "Type aliases must be declared in the file scope",
                            )),
                        SemanticErrorKind::IfExpressionMissingElse => report
                            .with_message("`if` expression missing `else` block")
                            .with_label(label.with_message(
                                "`if` expressions used as values must have an `else` \
                                 block",
                            )),
                        SemanticErrorKind::CannotCastType {
                            source_type,
                            target_type,
                        } => report.with_message("Invalid type cast").with_label(
                            label.with_message(format!(
                                "Cannot cast type \"{}\" to \"{}\"",
                                type_to_string(source_type),
                                type_to_string(target_type)
                            )),
                        ),
                        SemanticErrorKind::ClosuresNotSupportedYet => report
                            .with_message("Closures not supported")
                            .with_label(label.with_message(
                                "Capturing variables from outer scopes (closures) is \
                                 not supported yet",
                            )),
                        SemanticErrorKind::ExpectedTagWithoutValue { received } => {
                            let received_str = type_to_string(received);
                            report
                                .with_message("Unexpected value for tag")
                                .with_label(label.with_message(format!(
                                    "This tag is defined without a value, but found a value of type \"{}\"",
                                    received_str
                                )))
                                .with_help("Remove the parentheses and the value following the tag identifier")
                        }
                        SemanticErrorKind::ExpectedTagWithValue { expected } => {
                            let expected_str = type_to_string(expected);
                            report
                                .with_message("Missing value for tag")
                                .with_label(label.with_message(format!(
                                    "This tag requires a value of type \"{}\"",
                                    expected_str
                                )))
                                .with_help(format!(
                                    "Provide a value of type \"{}\" in parentheses, e.g., #Tag(value)",
                                    expected_str
                                ))
                        }
                    };
                    let _ = final_report.finish().print(&mut *cache);
                }

                CompilerErrorKind::CouldNotReadFile { path, error } => {
                    println!(
                        "Error: Could not read file \"{}\": {}",
                        path.0.display(),
                        error
                    );
                }
                CompilerErrorKind::ModuleNotFound {
                    importing_module,
                    target_path,
                    error,
                } => {
                    println!(
                        "Error: Module \"{}\" (imported by \"{}\") not found: {}",
                        target_path.0.display(),
                        importing_module.0.display(),
                        error
                    );
                }
            }
        }
    }

    fn extract_span(&self, span: &Span) -> (ModulePath, std::ops::Range<usize>) {
        (
            span.path.clone(),
            span.start.byte_offset..span.end.byte_offset,
        )
    }
}
