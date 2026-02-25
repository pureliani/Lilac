Project Path: src

Source Tree:

```txt
src
├── ast
│   ├── decl.rs
│   ├── expr.rs
│   ├── mod.rs
│   ├── stmt.rs
│   └── type_annotation.rs
├── codegen
│   └── mod.rs
├── compile
│   ├── file_cache.rs
│   ├── interner.rs
│   ├── mod.rs
│   └── report_errors.rs
├── globals.rs
├── hir
│   ├── builders
│   │   ├── basic_block.rs
│   │   ├── emitters
│   │   │   ├── binary.rs
│   │   │   ├── comp.rs
│   │   │   ├── const.rs
│   │   │   ├── control_flow.rs
│   │   │   ├── list.rs
│   │   │   ├── mod.rs
│   │   │   ├── struct.rs
│   │   │   ├── unary.rs
│   │   │   └── union.rs
│   │   ├── function.rs
│   │   ├── mod.rs
│   │   ├── module.rs
│   │   └── program.rs
│   ├── errors.rs
│   ├── expressions
│   │   ├── access.rs
│   │   ├── and.rs
│   │   ├── binary_op.rs
│   │   ├── codeblock.rs
│   │   ├── fn.rs
│   │   ├── fn_call.rs
│   │   ├── identifier.rs
│   │   ├── if.rs
│   │   ├── is_type.rs
│   │   ├── list_literal.rs
│   │   ├── mod.rs
│   │   ├── or.rs
│   │   ├── static_access.rs
│   │   ├── string.rs
│   │   ├── struct_init.rs
│   │   ├── typecast.rs
│   │   └── unary_op.rs
│   ├── instructions.rs
│   ├── mod.rs
│   ├── statements
│   │   ├── assignment.rs
│   │   ├── from.rs
│   │   ├── mod.rs
│   │   ├── return.rs
│   │   ├── type_alias_decl.rs
│   │   ├── var_decl.rs
│   │   └── while.rs
│   ├── types
│   │   ├── checked_declaration.rs
│   │   ├── checked_type.rs
│   │   ├── mod.rs
│   │   └── ordered_number_kind.rs
│   └── utils
│       ├── adjustments.rs
│       ├── check_type.rs
│       ├── dump.rs
│       ├── get_poison.rs
│       ├── layout.rs
│       ├── mod.rs
│       ├── numeric.rs
│       ├── points_to.rs
│       ├── scope.rs
│       ├── type_to_string.rs
│       └── union.rs
├── lib.rs
├── main.rs
├── parse
│   ├── expressions
│   │   ├── mod.rs
│   │   ├── parse_codeblock_expr.rs
│   │   ├── parse_fn_call_expr.rs
│   │   ├── parse_fn_expr.rs
│   │   ├── parse_if_expr.rs
│   │   ├── parse_list_literal_expr.rs
│   │   ├── parse_parenthesized_expr.rs
│   │   └── parse_struct_init_expr.rs
│   ├── mod.rs
│   ├── statements
│   │   ├── mod.rs
│   │   ├── parse_assignment_stmt.rs
│   │   ├── parse_break_stmt.rs
│   │   ├── parse_continue_stmt.rs
│   │   ├── parse_expr_stmt.rs
│   │   ├── parse_from_stmt.rs
│   │   ├── parse_return_stmt.rs
│   │   ├── parse_type_alias_decl.rs
│   │   ├── parse_var_decl.rs
│   │   └── parse_while_stmt.rs
│   └── type_annotations
│       ├── mod.rs
│       ├── parse_fn_type_annotation.rs
│       ├── parse_parenthesized_type_annotation.rs
│       └── parse_struct_type_annotation.rs
└── tokenize
    ├── mod.rs
    ├── tokenize_documentation.rs
    ├── tokenize_identifier.rs
    ├── tokenize_number.rs
    ├── tokenize_punctuation.rs
    └── tokenize_string.rs

```

`src/ast/decl.rs`:

```rs
use crate::{
    ast::{expr::BlockContents, DeclarationId, IdentifierNode},
    parse::DocAnnotation,
};

use super::{expr::Expr, type_annotation::TypeAnnotation};

#[derive(Clone, Debug, PartialEq)]
pub struct Param {
    pub identifier: IdentifierNode,
    pub constraint: TypeAnnotation,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnDecl {
    pub id: DeclarationId,
    pub documentation: Option<DocAnnotation>,
    pub identifier: IdentifierNode,
    pub params: Vec<Param>,
    pub return_type: TypeAnnotation,
    pub body: BlockContents,
    pub is_exported: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeAliasDecl {
    pub id: DeclarationId,
    pub documentation: Option<DocAnnotation>,
    pub identifier: IdentifierNode,
    pub value: TypeAnnotation,
    pub is_exported: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Declaration {
    TypeAlias(TypeAliasDecl),
    Fn(FnDecl),
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDecl {
    pub id: DeclarationId,
    pub documentation: Option<DocAnnotation>,
    pub identifier: IdentifierNode,
    pub constraint: Option<TypeAnnotation>,
    pub value: Expr,
}

```

`src/ast/expr.rs`:

```rs
use crate::{
    ast::{decl::FnDecl, IdentifierNode, Span, StringNode},
    tokenize::NumberKind,
};

use super::{stmt::Stmt, type_annotation::TypeAnnotation};

#[derive(Clone, Debug, PartialEq)]
pub struct BlockContents {
    pub statements: Vec<Stmt>,
    pub final_expr: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Not {
        right: Box<Expr>,
    },
    Neg {
        right: Box<Expr>,
    },
    Add {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Subtract {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Multiply {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Divide {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Modulo {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    LessThan {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    LessThanOrEqual {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    GreaterThan {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    GreaterThanOrEqual {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Equal {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    NotEqual {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    And {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Or {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Struct(Vec<(IdentifierNode, Expr)>),
    Access {
        left: Box<Expr>,
        field: IdentifierNode,
    },
    StaticAccess {
        left: Box<Expr>,
        field: IdentifierNode,
    },
    TypeCast {
        left: Box<Expr>,
        target: TypeAnnotation,
    },
    IsType {
        left: Box<Expr>,
        ty: TypeAnnotation,
    },
    FnCall {
        left: Box<Expr>,
        args: Vec<Expr>,
    },
    BoolLiteral(bool),
    Number(NumberKind),
    String(StringNode),
    Identifier(IdentifierNode),
    Fn(Box<FnDecl>),
    If {
        branches: Vec<(Box<Expr>, BlockContents)>,
        else_branch: Option<BlockContents>,
    },
    List(Vec<Expr>),
    CodeBlock(BlockContents),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

```

`src/ast/mod.rs`:

```rs
use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    path::PathBuf,
    sync::Arc,
};

use crate::compile::interner::StringId;

pub mod decl;
pub mod expr;
pub mod stmt;
pub mod type_annotation;

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

impl Ord for IdentifierNode {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.0.cmp(&other.name.0)
    }
}

impl PartialOrd for IdentifierNode {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
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

#[derive(Default, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ModulePath(pub Arc<PathBuf>);

impl From<ModulePath> for PathBuf {
    fn from(value: ModulePath) -> Self {
        value.0.to_path_buf()
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

```

`src/ast/stmt.rs`:

```rs
use crate::ast::{IdentifierNode, Span, StringNode};

use super::{
    decl::{TypeAliasDecl, VarDecl},
    expr::{BlockContents, Expr},
};

#[derive(Clone, Debug, PartialEq)]
pub enum StmtKind {
    Expression(Expr),
    TypeAliasDecl(TypeAliasDecl),
    VarDecl(VarDecl),
    Break,
    Continue,
    Return {
        value: Expr,
    },
    Assignment {
        target: Expr,
        value: Expr,
    },
    From {
        path: StringNode,
        identifiers: Vec<(IdentifierNode, Option<IdentifierNode>)>, // optional alias
    },
    While {
        condition: Box<Expr>,
        body: BlockContents,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

```

`src/ast/type_annotation.rs`:

```rs
use crate::{
    ast::{IdentifierNode, Span},
    hir::types::checked_type::LiteralType,
};

use super::decl::Param;

#[derive(Clone, Debug, PartialEq)]
pub enum TypeAnnotationKind {
    Void,
    Bool,
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    String,
    Null,
    Identifier(IdentifierNode),
    Struct(Vec<Param>),
    Literal(LiteralType),
    Union(Vec<TypeAnnotation>),
    List(Box<TypeAnnotation>),
    FnType {
        params: Vec<Param>,
        return_type: Box<TypeAnnotation>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeAnnotation {
    pub kind: TypeAnnotationKind,
    pub span: Span,
}

```

`src/compile/file_cache.rs`:

```rs
use codespan_reporting::files::{Files, SimpleFiles};
use std::collections::HashMap;
use std::ops::Range;

use crate::ast::ModulePath;

#[derive(Default)]
pub struct FileCache {
    files: SimpleFiles<String, String>,
    path_to_id: HashMap<ModulePath, usize>,
}

impl FileCache {
    pub fn insert(&mut self, path: ModulePath, source: String) -> Option<usize> {
        let name = path.0.display().to_string();
        let id = self.files.add(name, source);
        self.path_to_id.insert(path, id)
    }

    pub fn get_id(&self, path: &ModulePath) -> Option<usize> {
        self.path_to_id.get(path).copied()
    }
}

impl<'a> Files<'a> for FileCache {
    type FileId = usize;
    type Name = String;
    type Source = &'a str;

    fn name(&'a self, id: usize) -> Result<Self::Name, codespan_reporting::files::Error> {
        self.files.name(id)
    }

    fn source(
        &'a self,
        id: usize,
    ) -> Result<Self::Source, codespan_reporting::files::Error> {
        self.files.source(id)
    }

    fn line_index(
        &'a self,
        id: usize,
        byte_index: usize,
    ) -> Result<usize, codespan_reporting::files::Error> {
        self.files.line_index(id, byte_index)
    }

    fn line_range(
        &'a self,
        id: usize,
        line_index: usize,
    ) -> Result<Range<usize>, codespan_reporting::files::Error> {
        self.files.line_range(id, line_index)
    }
}

```

`src/compile/interner.rs`:

```rs
use std::{collections::HashMap, sync::RwLock};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct StringId(pub usize);

#[derive(Default)]
struct StringInterner {
    forward: HashMap<String, usize>,
    backward: Vec<String>,
}

impl StringInterner {
    fn intern(&mut self, key: &str) -> StringId {
        if let Some(&index) = self.forward.get(key) {
            return StringId(index);
        }

        let index = self.backward.len();
        self.backward.push(key.to_owned());
        self.forward.insert(key.to_owned(), index);

        StringId(index)
    }

    fn resolve(&self, key: StringId) -> &str {
        self.backward.get(key.0).unwrap_or_else(|| {
            panic!(
                "INTERNAL COMPILER ERROR: interner expected key {} to exist",
                key.0
            )
        })
    }

    fn clear(&mut self) {
        self.forward.clear();
        self.backward.clear();
    }
}

#[derive(Default)]
pub struct SharedStringInterner {
    interner: RwLock<StringInterner>,
}

impl SharedStringInterner {
    pub fn intern(&self, key: &str) -> StringId {
        let reader = self.interner.read().unwrap();
        if let Some(&index) = reader.forward.get(key) {
            return StringId(index);
        }
        drop(reader);

        let mut writer = self.interner.write().unwrap();

        if let Some(&index) = writer.forward.get(key) {
            return StringId(index);
        }

        writer.intern(key)
    }

    pub fn resolve(&self, key: StringId) -> String {
        let reader = self.interner.read().unwrap();
        reader.resolve(key).to_owned()
    }

    pub fn clear(&self) {
        let mut writer = self.interner.write().unwrap();
        writer.clear();
    }
}

```

`src/compile/mod.rs`:

```rs
use std::{
    collections::{HashMap, HashSet},
    fs,
    path::PathBuf,
    sync::{Arc, Mutex},
};

pub mod file_cache;
pub mod interner;
pub mod report_errors;

use crate::{
    ast::{
        decl::Declaration,
        expr::{Expr, ExprKind},
        stmt::{Stmt, StmtKind},
        ModulePath, Span,
    },
    compile::file_cache::FileCache,
    hir::{
        builders::{Builder, InGlobal, Program},
        errors::SemanticError,
        utils::{
            dump::dump_program,
            points_to::PointsToGraph,
            scope::{Scope, ScopeKind},
        },
    },
    parse::{Parser, ParsingError},
    tokenize::{TokenizationError, Tokenizer},
};

#[derive(Debug)]
pub enum CompilerErrorKind {
    CouldNotReadFile {
        path: ModulePath,
        error: std::io::Error,
    },
    ModuleNotFound {
        importing_module: ModulePath,
        target_path: ModulePath,
        error: std::io::Error,
    },
    Tokenization(TokenizationError),
    Parsing(ParsingError),
    Semantic(SemanticError),
}

pub struct Compiler {
    files: Arc<Mutex<FileCache>>,
    errors: Vec<CompilerErrorKind>,
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            files: Arc::new(Mutex::new(FileCache::default())),
            errors: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct ParallelParseResult {
    pub path: ModulePath,
    pub statements: Vec<Stmt>,
    pub tokenization_errors: Vec<TokenizationError>,
    pub parsing_errors: Vec<ParsingError>,
    pub declarations: Vec<Declaration>,
}

impl Compiler {
    pub fn compile(&mut self, main_path: PathBuf) {
        let parsed_modules = self.parallel_parse_modules(main_path);
        let mut modules_to_compile = Vec::new();

        for m in parsed_modules {
            match m {
                Err(e) => self.errors.push(e),
                Ok(mut module) => {
                    let has_tokenization_errors = !module.tokenization_errors.is_empty();
                    let has_parsing_errors = !module.parsing_errors.is_empty();

                    self.errors.extend(
                        std::mem::take(&mut module.tokenization_errors)
                            .into_iter()
                            .map(CompilerErrorKind::Tokenization),
                    );

                    self.errors.extend(
                        std::mem::take(&mut module.parsing_errors)
                            .into_iter()
                            .map(CompilerErrorKind::Parsing),
                    );

                    if !has_tokenization_errors && !has_parsing_errors {
                        modules_to_compile.push(module);
                    }
                }
            };
        }

        if !self.errors.is_empty() {
            self.report_errors();
            return;
        }

        let mut builder_errors = vec![];
        let mut current_defs = HashMap::new();
        let mut incomplete_phis = HashMap::new();
        let mut type_predicates = HashMap::new();

        let mut program = Program {
            constant_data: HashMap::new(),
            declarations: HashMap::new(),
            modules: HashMap::new(),
            value_types: HashMap::new(),
        };

        let global_scope = Scope::new_root(ScopeKind::Global, Span::default());
        let mut global_ptg = PointsToGraph::new();

        let mut program_builder = Builder {
            context: InGlobal,
            current_scope: global_scope,
            errors: &mut builder_errors,
            program: &mut program,
            current_defs: &mut current_defs,
            incomplete_phis: &mut incomplete_phis,
            type_predicates: &mut type_predicates,
            ptg: &mut global_ptg,
        };

        program_builder.build(modules_to_compile);

        if std::env::var("DUMP_HIR").is_ok() {
            dump_program(&program);
        }

        self.errors
            .extend(builder_errors.into_iter().map(CompilerErrorKind::Semantic));

        if !self.errors.is_empty() {
            self.report_errors();
            return;
        }

        println!(
            "Compilation successful: HIR generated for {} modules.",
            program.modules.len()
        );
    }

    pub fn parallel_parse_modules(
        &self,
        main_path: PathBuf,
    ) -> Vec<Result<ParallelParseResult, CompilerErrorKind>> {
        let canonical_main = ModulePath(Arc::new(
            main_path
                .canonicalize()
                .expect("Could not find the main module"),
        ));

        let visited = Arc::new(Mutex::new(HashSet::new()));
        let all_results = Arc::new(Mutex::new(Vec::new()));

        rayon::scope(|s| {
            fn parse_recursive(
                s: &rayon::Scope,
                path: ModulePath,
                files: Arc<Mutex<FileCache>>,
                visited: Arc<Mutex<HashSet<ModulePath>>>,
                all_results: Arc<
                    Mutex<Vec<Result<ParallelParseResult, CompilerErrorKind>>>,
                >,
            ) {
                {
                    let mut visited_guard = visited.lock().unwrap();
                    if !visited_guard.insert(path.clone()) {
                        return;
                    }
                }

                let source_code = match fs::read_to_string(path.0.as_path()) {
                    Ok(sc) => sc,
                    Err(e) => {
                        all_results.lock().unwrap().push(Err(
                            CompilerErrorKind::CouldNotReadFile {
                                path: path.clone(),
                                error: e,
                            },
                        ));
                        return;
                    }
                };

                let (tokens, tokenization_errors) =
                    Tokenizer::tokenize(&source_code, path.clone());
                let (statements, parsing_errors) = Parser::parse(tokens, path.clone());

                let (dependencies, dependency_errors, declarations) =
                    find_dependencies(path.clone(), &statements);

                for dep_path in dependencies {
                    let files = Arc::clone(&files);
                    let visited = Arc::clone(&visited);
                    let all_results = Arc::clone(&all_results);

                    s.spawn(move |s| {
                        parse_recursive(s, dep_path, files, visited, all_results);
                    });
                }

                files.lock().unwrap().insert(path.clone(), source_code);

                let mut results_guard = all_results.lock().unwrap();
                results_guard.extend(dependency_errors.into_iter().map(Err));
                results_guard.push(Ok(ParallelParseResult {
                    path,
                    statements,
                    declarations,
                    tokenization_errors,
                    parsing_errors,
                }));
            }

            parse_recursive(
                s,
                canonical_main,
                self.files.clone(),
                visited,
                all_results.clone(),
            );
        });

        Arc::try_unwrap(all_results)
            .expect("Arc unwrap failed")
            .into_inner()
            .expect("Mutex into_inner failed")
    }
}

fn find_dependencies(
    current_module_path: ModulePath,
    statements: &[Stmt],
) -> (
    HashSet<ModulePath>,
    Vec<CompilerErrorKind>,
    Vec<Declaration>,
) {
    let mut dependencies = HashSet::new();
    let mut errors = vec![];
    let mut declarations: Vec<Declaration> = vec![];

    for stmt in statements {
        match &stmt.kind {
            StmtKind::From { path, .. } => {
                let relative_path_str = &path.value;
                let mut target_path = current_module_path.0.to_path_buf();
                target_path.pop();
                target_path.push(relative_path_str);

                match fs::canonicalize(target_path.clone()) {
                    Ok(canonical_path) => {
                        dependencies.insert(ModulePath(Arc::new(canonical_path)));
                    }
                    Err(e) => {
                        errors.push(CompilerErrorKind::ModuleNotFound {
                            importing_module: current_module_path.clone(),
                            target_path: ModulePath(Arc::new(target_path)),
                            error: e,
                        });
                    }
                }
            }
            StmtKind::Expression(Expr {
                kind: ExprKind::Fn(decl),
                ..
            }) => {
                declarations.push(Declaration::Fn(*decl.clone()));
            }
            StmtKind::TypeAliasDecl(decl) => {
                declarations.push(Declaration::TypeAlias(decl.clone()));
            }
            _ => {}
        }
    }

    (dependencies, errors, declarations)
}

```

`src/compile/report_errors.rs`:

```rs
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::Files;
use codespan_reporting::term::termcolor::{ColorChoice, NoColor, StandardStream};
use codespan_reporting::term::{self, Config};

use crate::compile::file_cache::FileCache;
use crate::hir::utils::points_to::PathSegment;
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
        let cache = self.files.lock().unwrap();
        let config = Config {
            start_context_lines: 8,
            end_context_lines: 8,
            tab_width: 8,
            after_label_lines: 8,
            before_label_lines: 8,
            display_style: term::DisplayStyle::Rich,
            ..Default::default()
        };

        let mut buffer: Vec<u8> = Vec::new();
        let mut buf_writer = NoColor::new(&mut buffer);
        let mut stderr = StandardStream::stderr(ColorChoice::Auto);

        self.errors.sort_by_key(get_err_sort_key);

        for error in &self.errors {
            let diagnostic = match error {
                CompilerErrorKind::Tokenization(e) => {
                    let (path, range) = self.extract_span(&e.span);
                    let Ok(file_id) = self.resolve_file_id(&cache, &path) else {
                        println!("Error: File not found: {:?}", path);
                        continue;
                    };

                    let diag = Diagnostic::error()
                        .with_code(format!("T{}", e.kind.code()))
                        .with_labels(vec![Label::primary(file_id, range.clone())]);

                    match &e.kind {
                        TokenizationErrorKind::UnterminatedString => diag
                            .with_message("Unterminated string")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("This string is not terminated")]),

                        TokenizationErrorKind::UnknownToken(char_str) => {
                            let readable = match char_str.as_str() {
                                "\n" => "`\\n`",
                                "\r" => "`\\r`",
                                "\t" => "`\\t`",
                                " " => "`<whitespace>`",
                                c => &format!("'{}'", c),
                            };
                            diag.with_message("Unknown token").with_labels(vec![
                                Label::primary(file_id, range).with_message(format!(
                                    "This character {} is not recognized",
                                    readable
                                )),
                            ])
                        }

                        TokenizationErrorKind::UnknownEscapeSequence => diag
                            .with_message("Unknown escape sequence")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("The escape sequence here is invalid")]),

                        TokenizationErrorKind::InvalidFloatingNumber => diag
                            .with_message("Invalid floating-point number")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(
                                    "This is not a valid floating-point number",
                                )]),

                        TokenizationErrorKind::InvalidIntegerNumber => diag
                            .with_message("Invalid integer number")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("This is not a valid integer number")]),

                        TokenizationErrorKind::UnterminatedDoc => diag
                            .with_message("Unterminated documentation")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(
                                    "This documentation block is not terminated",
                                )]),
                    }
                }

                CompilerErrorKind::Parsing(e) => {
                    let (path, range) = self.extract_span(&e.span);
                    let Ok(file_id) = self.resolve_file_id(&cache, &path) else {
                        println!("Error: File not found: {:?}", path);
                        continue;
                    };

                    let diag = Diagnostic::error()
                        .with_code(format!("P{}", e.kind.code()))
                        .with_labels(vec![Label::primary(file_id, range.clone())]);

                    match &e.kind {
                        ParsingErrorKind::DocMustBeFollowedByDeclaration => diag
                            .with_message(
                                "Documentation must be followed by a declaration",
                            )
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("Expected a type alias or variable here")]),

                        ParsingErrorKind::ExpectedAnExpressionButFound(token) => diag
                            .with_message("Expected an expression")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(format!(
                                    "Found token `{}`",
                                    token_kind_to_string(&token.kind)
                                ))]),

                        ParsingErrorKind::ExpectedATypeButFound(token) => diag
                            .with_message("Expected a type")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(format!(
                                    "Found token `{}`",
                                    token_kind_to_string(&token.kind)
                                ))]),

                        ParsingErrorKind::InvalidSuffixOperator(token) => diag
                            .with_message("Invalid suffix operator")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(format!(
                                    "Token `{}` cannot be used as a suffix",
                                    token_kind_to_string(&token.kind)
                                ))]),

                        ParsingErrorKind::UnexpectedEndOfInput => diag
                            .with_message("Unexpected end of input")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("Input ended abruptly")]),

                        ParsingErrorKind::ExpectedAnIdentifier => diag
                            .with_message("Expected an identifier")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("Expected an identifier")]),

                        ParsingErrorKind::ExpectedAPunctuationMark(p) => diag
                            .with_message("Expected punctuation")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(format!("Expected `{}`", p.to_string()))]),

                        ParsingErrorKind::ExpectedAKeyword(k) => diag
                            .with_message("Expected keyword")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(format!("Expected `{}`", k.to_string()))]),

                        ParsingErrorKind::ExpectedAStringValue => diag
                            .with_message("Expected string literal")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("Expected a string")]),

                        ParsingErrorKind::ExpectedANumericValue => diag
                            .with_message("Expected numeric literal")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("Expected a number")]),

                        ParsingErrorKind::UnknownStaticMethod(id) => {
                            let name = STRING_INTERNER.resolve(id.name);
                            diag.with_message("Unknown static method").with_labels(vec![
                                Label::primary(file_id, range).with_message(format!(
                                    "Method `{}` doesn't exist",
                                    name
                                )),
                            ])
                        }

                        ParsingErrorKind::UnexpectedStatementAfterFinalExpression => diag
                            .with_message("Unexpected statement")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(
                                    "Statements cannot follow the final expression",
                                )]),

                        ParsingErrorKind::ExpectedStatementOrExpression { found } => diag
                            .with_message("Expected statement or expression")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(format!(
                                    "Found `{}`",
                                    token_kind_to_string(&found.kind)
                                ))]),

                        ParsingErrorKind::UnexpectedTokenAfterFinalExpression {
                            found,
                        } => diag.with_message("Unexpected token").with_labels(vec![
                            Label::primary(file_id, range).with_message(format!(
                                "Token `{}` follows final expression",
                                token_kind_to_string(&found.kind)
                            )),
                        ]),

                        ParsingErrorKind::ExpectedATagTypeButFound(_) => diag
                            .with_message("Expected a tag type")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("Union variants must start with '#'")]),

                        ParsingErrorKind::ExpectedToBeFollowedByOneOfTheTokens(
                            tokens,
                        ) => {
                            let expected: Vec<_> = tokens
                                .iter()
                                .map(|t| token_kind_to_string(&t.kind))
                                .collect();
                            diag.with_message("Unexpected token").with_labels(vec![
                                Label::primary(file_id, range).with_message(format!(
                                    "Expected one of: {}",
                                    expected.join(", ")
                                )),
                            ])
                        }
                    }
                }

                CompilerErrorKind::Semantic(e) => {
                    let (path, range) = self.extract_span(&e.span);
                    let Ok(file_id) = self.resolve_file_id(&cache, &path) else {
                        println!("Error: File not found: {:?}", path);
                        continue;
                    };

                    let diag = Diagnostic::error()
                        .with_code(format!("S{}", e.kind.code()))
                        .with_labels(vec![Label::primary(file_id, range.clone())]);

                    match &e.kind {
                        SemanticErrorKind::ArgumentAliasing {
                            passed_arg_span,
                            passed_path,
                            aliased_arg_span,
                            aliased_path,
                        } => {
                            let get_snippet = |span: &Span| -> String {
                                if let Ok(f_id) = self.resolve_file_id(&cache, &span.path)
                                {
                                    if let Ok(source) = cache.source(f_id) {
                                        let start = span.start.byte_offset;
                                        let end = span.end.byte_offset;
                                        if start <= source.len() && end <= source.len() {
                                            return source[start..end].to_string();
                                        }
                                    }
                                }
                                "<unknown>".to_string()
                            };

                            let format_full_path =
                                |span: &Span, path: &[PathSegment]| -> String {
                                    let mut s = get_snippet(span);
                                    for seg in path {
                                        match seg {
                                            PathSegment::Field(name) => {
                                                s.push('.');
                                                s.push_str(
                                                    &STRING_INTERNER.resolve(*name),
                                                );
                                            }
                                            PathSegment::Index => {
                                                s.push_str("[index]");
                                            }
                                        }
                                    }
                                    s
                                };

                            let passed_str =
                                format_full_path(passed_arg_span, passed_path);
                            let aliased_str =
                                format_full_path(aliased_arg_span, aliased_path);

                            diag.with_message("Argument aliasing detected").with_labels(
                                vec![Label::primary(file_id, range).with_message(
                                    format!(
                                        "Cannot pass argument `{}` which is an alias of \
                                         another argument `{}`",
                                        passed_str, aliased_str
                                    ),
                                )],
                            )
                        }
                        SemanticErrorKind::CannotGetLen(_ty) => {
                            diag.with_message("Cannot get length")
                        }
                        SemanticErrorKind::CannotNarrowNonUnion(ty) => {
                            let type_str = type_to_string(ty);
                            diag.with_message("Redundant type check").with_labels(vec![
                                Label::primary(file_id, range).with_message(format!(
                                    "Value is already `{}`, `::is()` only works on \
                                     unions",
                                    type_str
                                )),
                            ])
                        }
                        SemanticErrorKind::ExpectedANumericOperand => diag
                            .with_message("Expected numeric operand")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("This must be a numeric type")]),
                        SemanticErrorKind::ExpectedASignedNumericOperand => diag
                            .with_message("Expected signed numeric operand")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("This must be a signed numeric type")]),
                        SemanticErrorKind::MixedSignedAndUnsigned => diag
                            .with_message("Mixed signedness")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(
                                    "Cannot mix signed and unsigned operands",
                                )]),
                        SemanticErrorKind::MixedFloatAndInteger => diag
                            .with_message("Mixed types")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("Cannot mix float and integer operands")]),
                        SemanticErrorKind::CannotCompareType { of, to } => diag
                            .with_message("Incompatible comparison")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(format!(
                                    "Cannot compare `{}` to `{}`",
                                    type_to_string(of),
                                    type_to_string(to)
                                ))]),
                        SemanticErrorKind::UndeclaredIdentifier(id) => {
                            let name = STRING_INTERNER.resolve(id.name);
                            diag.with_message("Undeclared identifier")
                                .with_labels(vec![Label::primary(file_id, range)
                                    .with_message(format!("`{}` is not defined", name))])
                        }
                        SemanticErrorKind::UndeclaredType(id) => {
                            let name = STRING_INTERNER.resolve(id.name);
                            diag.with_message("Undeclared type").with_labels(vec![
                                Label::primary(file_id, range).with_message(format!(
                                    "Type `{}` is not defined",
                                    name
                                )),
                            ])
                        }
                        SemanticErrorKind::TypeMismatch { expected, received } => diag
                            .with_message("Type mismatch")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(format!(
                                    "Expected `{}`, found `{}`",
                                    type_to_string(expected),
                                    type_to_string(received)
                                ))]),
                        SemanticErrorKind::ReturnTypeMismatch { expected, received } => {
                            diag.with_message("Function return type mismatch")
                                .with_labels(vec![Label::primary(file_id, range)
                                    .with_message(format!(
                                        "Expected the returned value to have a type \
                                         that is assignable to `{}`, but found `{}`",
                                        type_to_string(expected),
                                        type_to_string(received)
                                    ))])
                        }
                        SemanticErrorKind::ModuleNotFound(path_buf) => diag
                            .with_message("Module not found")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(format!(
                                    "Could not find module at `{}`",
                                    path_buf.0.display()
                                ))]),
                        SemanticErrorKind::SymbolNotExported {
                            module_path,
                            symbol,
                        } => {
                            let name = STRING_INTERNER.resolve(symbol.name);
                            diag.with_message("Symbol not exported").with_labels(vec![
                                Label::primary(file_id, range).with_message(format!(
                                    "`{}` is not exported from `{}`",
                                    name,
                                    module_path.0.display()
                                )),
                            ])
                        }
                        SemanticErrorKind::ValuedTagInIsExpression => diag
                            .with_message("Valued tag not allowed in `::is()` expression")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(
                                    "The `::is()` operator only checks the variant \
                                     identifier. Remove the value type (e.g., use \
                                     `#Tag` instead of `#Tag(Type)`)",
                                )]),
                        SemanticErrorKind::UnreachableCode => diag
                            .with_message("Unreachable code")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("This code will never be executed")]),
                        SemanticErrorKind::DuplicateIdentifier(id) => {
                            let name = STRING_INTERNER.resolve(id.name);
                            diag.with_message("Duplicate identifier").with_labels(vec![
                                Label::primary(file_id, range).with_message(format!(
                                    "Duplicate identifier declaration `{}`",
                                    name
                                )),
                            ])
                        }
                        SemanticErrorKind::DuplicateUnionVariant(id) => {
                            let name = STRING_INTERNER.resolve(id.name);
                            diag.with_message("Duplicate union variant").with_labels(
                                vec![Label::primary(file_id, range).with_message(
                                    format!(
                                        "Variant `{}` is defined multiple times in this \
                                         union",
                                        name
                                    ),
                                )],
                            )
                        }
                        SemanticErrorKind::CannotIndex(ty) => diag
                            .with_message("Cannot index type")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(format!(
                                    "Type `{}` cannot be indexed",
                                    type_to_string(ty)
                                ))]),
                        SemanticErrorKind::FromStatementMustBeDeclaredAtTopLevel => diag
                            .with_message("Invalid import location")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(
                                    "`from` statements must be declared at the top \
                                     level of the file",
                                )]),
                        SemanticErrorKind::CannotDeclareGlobalVariable => diag
                            .with_message("Global variables not allowed")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(
                                    "Variables cannot be declared at the file scope \
                                     (top-level)",
                                )]),
                        SemanticErrorKind::DuplicateStructFieldInitializer(id) => {
                            let name = STRING_INTERNER.resolve(id.name);
                            diag.with_message("Duplicate initializer for a struct field")
                                .with_labels(vec![Label::primary(file_id, range)
                                    .with_message(format!(
                                        "Struct field `{}` cannot be initialized \
                                         multiple times",
                                        name
                                    ))])
                        }
                        SemanticErrorKind::UnknownStructFieldInitializer(id) => {
                            let name = STRING_INTERNER.resolve(id.name);
                            diag.with_message("Unknown field in the struct initializer")
                                .with_labels(vec![Label::primary(file_id, range)
                                    .with_message(format!(
                                        "Unknown struct field `{}`",
                                        name
                                    ))])
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
                                .map(|n| format!("`{}`", n))
                                .collect::<Vec<_>>()
                                .join(", ");
                            diag.with_message("Missing field initializers").with_labels(
                                vec![Label::primary(file_id, range).with_message(
                                    format!(
                                        "Missing initializers for the following struct \
                                         fields {}",
                                        joined
                                    ),
                                )],
                            )
                        }
                        SemanticErrorKind::CannotCall(target) => diag
                            .with_message("Cannot use the function call operator")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(format!(
                                    "Cannot use the function-call operator on type `{}`",
                                    type_to_string(target)
                                ))]),
                        SemanticErrorKind::IncompatibleBranchTypes { first, second } => {
                            diag.with_message("Incompatible branch types").with_labels(
                                vec![Label::primary(file_id, range).with_message(
                                    format!(
                                        "This branch returns `{}`, but the previous \
                                         branch returned `{}`",
                                        type_to_string(second),
                                        type_to_string(first)
                                    ),
                                )],
                            )
                        }
                        SemanticErrorKind::ReturnKeywordOutsideFunction => diag
                            .with_message(
                                "Keyword `return` used outside of a function scope",
                            )
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(
                                    "Cannot use the `return` keyword outside of a \
                                     function scope",
                                )]),
                        SemanticErrorKind::BreakKeywordOutsideLoop => diag
                            .with_message("Keyword `break` used outside of a loop scope")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(
                                    "Cannot use the `break` keyword outside of a loop \
                                     scope",
                                )]),
                        SemanticErrorKind::ContinueKeywordOutsideLoop => diag
                            .with_message(
                                "Keyword `continue` used outside of a loop scope",
                            )
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(
                                    "Cannot use the `continue` keyword outside of a \
                                     loop scope",
                                )]),
                        SemanticErrorKind::InvalidLValue => diag
                            .with_message("Invalid assignment target")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("Invalid assignment target")]),
                        SemanticErrorKind::TypeMismatchExpectedOneOf {
                            expected,
                            received,
                        } => {
                            let mut expected_strings: Vec<String> = expected
                                .iter()
                                .map(|t| format!("`{}`", type_to_string(t)))
                                .collect();
                            expected_strings.sort();
                            let expected_str = expected_strings.join(", ");
                            diag.with_message("Type mismatch").with_labels(vec![
                                Label::primary(file_id, range).with_message(format!(
                                    "Expected one of {}, but found `{}`",
                                    expected_str,
                                    type_to_string(received)
                                )),
                            ])
                        }
                        SemanticErrorKind::ReturnNotLastStatement => diag
                            .with_message(
                                "Expected the return statement to be the last statement \
                                 in the function",
                            )
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(
                                    "Expected the return statement to be the last \
                                     statement in the function",
                                )]),
                        SemanticErrorKind::CannotAccess(target) => diag
                            .with_message("Cannot access field")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(format!(
                                    "Cannot use the access operator on the type `{}`",
                                    type_to_string(target)
                                ))]),
                        SemanticErrorKind::CannotStaticAccess(_) => diag
                            .with_message("Cannot access static field")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("Invalid static access")]),
                        SemanticErrorKind::AccessToUndefinedField(field) => {
                            let name = STRING_INTERNER.resolve(field.name);
                            diag.with_message("Access to an undefined field")
                                .with_labels(vec![Label::primary(file_id, range)
                                    .with_message(format!(
                                        "Field `{}` is not defined",
                                        name
                                    ))])
                        }
                        SemanticErrorKind::AccessToUndefinedStaticField(id) => {
                            let name = STRING_INTERNER.resolve(id.name);
                            diag.with_message("Undefined static field")
                                .with_labels(vec![Label::primary(file_id, range)
                                    .with_message(format!(
                                        "Static field `{}` does not exist",
                                        name
                                    ))])
                        }
                        SemanticErrorKind::FnArgumentCountMismatch {
                            expected,
                            received,
                        } => {
                            let s = if *expected > 1 { "s" } else { "" };
                            diag.with_message("Function argument count mismatch")
                                .with_labels(vec![Label::primary(file_id, range)
                                    .with_message(format!(
                                        "This function expects {} argument{}, but \
                                         instead received {}",
                                        expected, s, received
                                    ))])
                        }
                        SemanticErrorKind::CannotUseVariableDeclarationAsType => diag
                            .with_message("Cannot use variable declaration as a type")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(
                                    "Cannot use variable declaration as a type",
                                )]),
                        SemanticErrorKind::CannotUseFunctionDeclarationAsType => diag
                            .with_message("Expected type, found function")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(
                                    "Cannot use a function declaration as a type",
                                )]),
                        SemanticErrorKind::CannotUseTypeDeclarationAsValue => diag
                            .with_message("Expected value, found type")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(
                                    "Cannot use a type declaration as a value",
                                )]),
                        SemanticErrorKind::TypeAliasMustBeDeclaredAtTopLevel => diag
                            .with_message(
                                "Type aliases must be declared in the file scope",
                            )
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(
                                    "Type aliases must be declared in the file scope",
                                )]),
                        SemanticErrorKind::IfExpressionMissingElse => diag
                            .with_message("`if` expression missing `else` block")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(
                                    "`if` expressions used as values must have an \
                                     `else` block",
                                )]),
                        SemanticErrorKind::CannotCastType {
                            source_type,
                            target_type,
                        } => diag.with_message("Invalid type cast").with_labels(vec![
                            Label::primary(file_id, range).with_message(format!(
                                "Cannot cast type `{}` to `{}`",
                                type_to_string(source_type),
                                type_to_string(target_type)
                            )),
                        ]),
                        SemanticErrorKind::ClosuresNotSupportedYet => diag
                            .with_message("Closures not supported")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(
                                    "Capturing variables from outer scopes (closures) \
                                     is not supported yet",
                                )]),
                        SemanticErrorKind::ExpectedTagWithoutValue { received } => {
                            let received_str = type_to_string(received);
                            diag.with_message("Unexpected value for tag")
                                .with_labels(vec![Label::primary(file_id, range)
                                    .with_message(format!(
                                        "This tag is defined without a value, but found \
                                         a value of type `{}`",
                                        received_str
                                    ))])
                                .with_notes(vec!["Remove the parentheses and the value \
                                                  following the tag identifier"
                                    .to_string()])
                        }
                        SemanticErrorKind::ExpectedTagWithValue { expected } => {
                            let expected_str = type_to_string(expected);
                            diag.with_message("Missing value for tag")
                                .with_labels(vec![Label::primary(file_id, range)
                                    .with_message(format!(
                                        "This tag requires a value of type `{}`",
                                        expected_str
                                    ))])
                                .with_notes(vec![format!(
                                    "Provide a value of type `{}` in parentheses, e.g., \
                                     #Tag(value)",
                                    expected_str
                                )])
                        }
                        SemanticErrorKind::UnsupportedUnionNarrowing => diag
                            .with_message("Union-to-union narrowing not supported")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(
                                    "Narrowing to a subset union type is not yet \
                                     supported; try narrowing to a specific variant \
                                     instead",
                                )]),
                    }
                }

                CompilerErrorKind::CouldNotReadFile { path, error } => {
                    println!(
                        "Error: Could not read file `{}`: {}",
                        path.0.display(),
                        error
                    );
                    continue;
                }

                CompilerErrorKind::ModuleNotFound {
                    importing_module,
                    target_path,
                    error,
                } => {
                    println!(
                        "Error: Module `{}` (imported by `{}`) not found: {}",
                        target_path.0.display(),
                        importing_module.0.display(),
                        error
                    );
                    continue;
                }
            };

            if let Err(e) =
                term::emit_to_write_style(&mut stderr, &config, &*cache, &diagnostic)
            {
                eprintln!("Failed to emit diagnostic to stderr: {}", e);
            }

            if let Err(e) =
                term::emit_to_write_style(&mut buf_writer, &config, &*cache, &diagnostic)
            {
                eprintln!("Failed to emit diagnostic to buffer: {}", e);
            }
        }

        if !buffer.is_empty() {
            if let Err(e) = std::fs::write("diagnostics.log", &buffer) {
                eprintln!("Failed to write diagnostics to file: {}", e);
            }
        }
    }

    fn resolve_file_id(&self, cache: &FileCache, path: &ModulePath) -> Result<usize, ()> {
        cache.get_id(path).ok_or(())
    }

    fn extract_span(&self, span: &Span) -> (ModulePath, std::ops::Range<usize>) {
        (
            span.path.clone(),
            span.start.byte_offset..span.end.byte_offset,
        )
    }
}

```

`src/globals.rs`:

```rs
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::LazyLock;

use crate::ast::DeclarationId;
use crate::compile::interner::{SharedStringInterner, StringId};
use crate::hir::builders::{BasicBlockId, ConstantId, ValueId};

pub struct CommonIdentifiers {
    pub ptr: StringId,
    pub capacity: StringId,
    pub is_heap_allocated: StringId,
    pub len: StringId,
    pub id: StringId,
    pub value: StringId,
}

pub static VALUE_COUNTER: LazyLock<AtomicUsize> = LazyLock::new(|| AtomicUsize::new(0));
pub static BLOCK_COUNTER: LazyLock<AtomicUsize> = LazyLock::new(|| AtomicUsize::new(0));
pub static CONSTANT_COUNTER: LazyLock<AtomicUsize> =
    LazyLock::new(|| AtomicUsize::new(0));
pub static DECLARATION_COUNTER: LazyLock<AtomicUsize> =
    LazyLock::new(|| AtomicUsize::new(0));
pub static STRING_INTERNER: LazyLock<SharedStringInterner> =
    LazyLock::new(SharedStringInterner::default);
pub static COMMON_IDENTIFIERS: LazyLock<CommonIdentifiers> =
    LazyLock::new(|| CommonIdentifiers {
        id: STRING_INTERNER.intern("id"),
        value: STRING_INTERNER.intern("value"),
        capacity: STRING_INTERNER.intern("capacity"),
        is_heap_allocated: STRING_INTERNER.intern("is_heap_allocated"),
        len: STRING_INTERNER.intern("len"),
        ptr: STRING_INTERNER.intern("ptr"),
    });

pub fn next_value_id() -> ValueId {
    ValueId(VALUE_COUNTER.fetch_add(1, Ordering::SeqCst))
}

pub fn next_block_id() -> BasicBlockId {
    BasicBlockId(BLOCK_COUNTER.fetch_add(1, Ordering::SeqCst))
}

pub fn next_constant_id() -> ConstantId {
    ConstantId(CONSTANT_COUNTER.fetch_add(1, Ordering::SeqCst))
}

pub fn next_declaration_id() -> DeclarationId {
    DeclarationId(DECLARATION_COUNTER.fetch_add(1, Ordering::SeqCst))
}

pub fn reset_globals() {
    VALUE_COUNTER.store(0, Ordering::SeqCst);
    BLOCK_COUNTER.store(0, Ordering::SeqCst);
    CONSTANT_COUNTER.store(0, Ordering::SeqCst);
    DECLARATION_COUNTER.store(0, Ordering::SeqCst);
    STRING_INTERNER.clear();
}

```

`src/hir/builders/basic_block.rs`:

```rs
use std::collections::HashSet;

use crate::{
    ast::{DeclarationId, Span},
    globals::next_value_id,
    hir::{
        builders::{
            BasicBlock, BasicBlockId, Builder, Function, InBlock, InFunction, InGlobal,
            InModule, PhiSource, ValueId,
        },
        types::{checked_declaration::CheckedDeclaration, checked_type::Type},
        utils::type_to_string::type_to_string,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn as_program(&mut self) -> Builder<'_, InGlobal> {
        Builder {
            context: InGlobal,
            program: self.program,
            errors: self.errors,
            current_scope: self.current_scope.clone(),
            current_defs: self.current_defs,
            incomplete_phis: self.incomplete_phis,
            type_predicates: self.type_predicates,
            ptg: self.ptg,
        }
    }

    pub fn as_module(&mut self) -> Builder<'_, InModule> {
        Builder {
            context: InModule {
                path: self.context.path.clone(),
            },
            program: self.program,
            errors: self.errors,
            current_scope: self.current_scope.clone(),
            current_defs: self.current_defs,
            incomplete_phis: self.incomplete_phis,
            type_predicates: self.type_predicates,
            ptg: self.ptg,
        }
    }

    pub fn as_fn(&mut self) -> Builder<'_, InFunction> {
        Builder {
            context: InFunction {
                path: self.context.path.clone(),
                func_id: self.context.func_id,
            },
            program: self.program,
            errors: self.errors,
            current_scope: self.current_scope.clone(),
            current_defs: self.current_defs,
            incomplete_phis: self.incomplete_phis,
            type_predicates: self.type_predicates,
            ptg: self.ptg,
        }
    }

    pub fn bb_mut(&mut self) -> &mut BasicBlock {
        self.get_bb_mut(self.context.block_id)
    }

    pub fn get_bb_mut(&mut self, block_id: BasicBlockId) -> &mut BasicBlock {
        let func_id = self.context.func_id;

        let decl = self
            .program
            .declarations
            .get_mut(&func_id)
            .expect("INTERNAL COMPILER ERROR: Function not found");

        match decl {
            CheckedDeclaration::Function(f) => f
                .blocks
                .get_mut(&block_id)
                .expect("INTERNAL COMPILER ERROR: Block not found"),
            _ => panic!("INTERNAL COMPILER ERROR: Declaration is not a function"),
        }
    }

    pub fn get_bb(&self, block_id: BasicBlockId) -> &BasicBlock {
        let func_id = self.context.func_id;

        let decl = self
            .program
            .declarations
            .get(&func_id)
            .expect("INTERNAL COMPILER ERROR: Function not found");

        match decl {
            CheckedDeclaration::Function(f) => f
                .blocks
                .get(&block_id)
                .expect("INTERNAL COMPILER ERROR: Block not found"),
            _ => panic!("INTERNAL COMPILER ERROR: Declaration is not a function"),
        }
    }

    pub fn bb(&self) -> &BasicBlock {
        self.get_bb(self.context.block_id)
    }

    pub fn get_fn(&mut self) -> &mut Function {
        let func_id = self.context.func_id;

        match self.program.declarations.get_mut(&func_id).unwrap() {
            CheckedDeclaration::Function(f) => f,
            _ => panic!("INTERNAL COMPILER ERROR: Declaration is not a function"),
        }
    }

    pub fn get_value_type(&self, id: ValueId) -> &Type {
        self.program.value_types.get(&id).unwrap_or_else(|| {
            panic!("INTERNAL COMPILER ERROR: ValueId({}) has no type", id.0)
        })
    }

    pub fn write_variable(
        &mut self,
        variable: DeclarationId,
        block: BasicBlockId,
        value: ValueId,
    ) {
        self.current_defs
            .entry(block)
            .or_default()
            .insert(variable, value);
    }

    pub fn read_variable(
        &mut self,
        variable: DeclarationId,
        block: BasicBlockId,
        span: Span,
    ) -> ValueId {
        if let Some(block_defs) = self.current_defs.get(&block) {
            if let Some(val) = block_defs.get(&variable) {
                return *val;
            }
        }
        self.read_variable_recursive(variable, block, span)
    }

    fn read_variable_recursive(
        &mut self,
        variable: DeclarationId,
        block: BasicBlockId,
        span: Span,
    ) -> ValueId {
        let val_id;
        let sealed = self.get_bb(block).sealed;
        let predecessors: Vec<BasicBlockId> =
            self.get_bb(block).predecessors.iter().cloned().collect();

        if !sealed {
            val_id = self.new_value_id(Type::Unknown);
            self.incomplete_phis.entry(block).or_default().push((
                val_id,
                variable,
                span.clone(),
            ));
        } else if predecessors.len() == 1 {
            val_id = self.read_variable(variable, predecessors[0], span.clone());
        } else if predecessors.is_empty() {
            panic!("INTERNAL COMPILER ERROR: Uninitialized local variable read");
        } else {
            val_id = self.new_value_id(Type::Unknown);
            self.write_variable(variable, block, val_id);
            self.resolve_phi(block, val_id, variable, span.clone());
        }

        self.write_variable(variable, block, val_id);
        val_id
    }

    pub fn insert_phi(
        &mut self,
        basic_block_id: BasicBlockId,
        phi_id: ValueId,
        sources: HashSet<PhiSource>,
    ) {
        assert!(
            !sources.is_empty(),
            "Phi node must have at least one source"
        );

        let first_source = sources.iter().next().unwrap();
        let expected_type = self.get_value_type(first_source.value);

        for source in &sources {
            let current_type = self.get_value_type(source.value);

            if expected_type != current_type {
                panic!(
                    "INTERNAL COMPILER ERROR: Phi node type mismatch.\nPhi ID: \
                     {:?}\nBlock ID: {:?}\nExpected Type: {}\nFound Type: {} (from \
                     block {:?})",
                    phi_id,
                    basic_block_id,
                    type_to_string(expected_type),
                    type_to_string(current_type),
                    source.from
                );
            }
        }

        self.get_bb_mut(basic_block_id).phis.insert(phi_id, sources);
    }

    pub fn resolve_phi(
        &mut self,
        block_id: BasicBlockId,
        phi_id: ValueId,
        variable_id: DeclarationId,
        span: Span,
    ) {
        let predecessors: Vec<BasicBlockId> =
            self.get_bb(block_id).predecessors.iter().cloned().collect();

        let mut phi_sources = Vec::new();
        let mut incoming_types = Vec::new();

        for pred in &predecessors {
            let val = self.read_variable(variable_id, *pred, span.clone());
            phi_sources.push((*pred, val));
            incoming_types.push(self.get_value_type(val).clone());
        }

        let unified_type = Type::make_union(incoming_types);

        let final_sources: HashSet<PhiSource> = phi_sources
            .into_iter()
            .map(|(pred, val)| PhiSource {
                from: pred,
                value: val,
            })
            .collect();

        if let Some(ty) = self.program.value_types.get_mut(&phi_id) {
            *ty = unified_type;
        }

        self.insert_phi(block_id, phi_id, final_sources.clone());

        let source_values: Vec<ValueId> =
            final_sources.into_iter().map(|s| s.value).collect();
        self.ptg.merge_values(phi_id, &source_values);
    }

    pub fn new_value_id(&mut self, ty: Type) -> ValueId {
        let value_id = next_value_id();
        let this_block_id = self.context.block_id;

        self.get_fn()
            .value_definitions
            .insert(value_id, this_block_id);

        self.program.value_types.insert(value_id, ty);

        value_id
    }

    pub fn use_basic_block(&mut self, block_id: BasicBlockId) {
        self.context.block_id = block_id;
    }

    pub fn seal(&mut self) {
        if self.bb().sealed {
            return;
        }

        let block_id = self.context.block_id;
        let incomplete = self.incomplete_phis.remove(&block_id).unwrap_or_default();

        for (phi_id, variable, span) in incomplete {
            self.resolve_phi(block_id, phi_id, variable, span);
        }

        self.bb_mut().sealed = true;
    }

    pub fn seal_block(&mut self, block_id: BasicBlockId) {
        let old_block = self.context.block_id;
        self.context.block_id = block_id;
        self.seal();
        self.context.block_id = old_block;
    }
}

```

`src/hir/builders/emitters/binary.rs`:

```rs
use crate::{
    ast::Span,
    hir::{
        builders::{Builder, InBlock, ValueId},
        instructions::{BinaryInstr, Instruction},
        utils::adjustments::arithmetic_supertype,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn emit_arithmetic_op<OP: FnOnce(ValueId, ValueId, ValueId) -> BinaryInstr>(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
        op: OP,
    ) -> ValueId {
        let lhs_type = self.get_value_type(lhs);
        let rhs_type = self.get_value_type(rhs);

        let result_type = match arithmetic_supertype(
            lhs_type,
            lhs_span.clone(),
            rhs_type,
            rhs_span.clone(),
        ) {
            Ok(t) => t,
            Err(e) => return self.report_error_and_get_poison(e),
        };

        let dest = self.new_value_id(result_type);
        let binary_instr = op(dest, lhs, rhs);

        self.push_instruction(Instruction::Binary(binary_instr));
        dest
    }

    pub fn emit_add(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> ValueId {
        self.emit_arithmetic_op(lhs, lhs_span, rhs, rhs_span, |dest, lhs, rhs| {
            BinaryInstr::Add { dest, lhs, rhs }
        })
    }

    pub fn emit_sub(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> ValueId {
        self.emit_arithmetic_op(lhs, lhs_span, rhs, rhs_span, |dest, lhs, rhs| {
            BinaryInstr::Sub { dest, lhs, rhs }
        })
    }

    pub fn emit_mul(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> ValueId {
        self.emit_arithmetic_op(lhs, lhs_span, rhs, rhs_span, |dest, lhs, rhs| {
            BinaryInstr::Mul { dest, lhs, rhs }
        })
    }

    pub fn emit_div(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> ValueId {
        self.emit_arithmetic_op(lhs, lhs_span, rhs, rhs_span, |dest, lhs, rhs| {
            BinaryInstr::Div { dest, lhs, rhs }
        })
    }

    pub fn emit_rem(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> ValueId {
        self.emit_arithmetic_op(lhs, lhs_span, rhs, rhs_span, |dest, lhs, rhs| {
            BinaryInstr::Rem { dest, lhs, rhs }
        })
    }
}

```

`src/hir/builders/emitters/comp.rs`:

```rs
use crate::{
    ast::Span,
    hir::{
        builders::{Builder, InBlock, ValueId},
        instructions::{CompInstr, Instruction, SelectInstr},
        types::checked_type::Type,
        utils::adjustments::check_assignable,
    },
};

impl<'a> Builder<'a, InBlock> {
    fn emit_comp_op<F>(
        &mut self,
        lhs: ValueId,
        _lhs_span: Span,
        rhs: ValueId,
        _rhs_span: Span,
        make_instr: F,
    ) -> ValueId
    where
        F: FnOnce(ValueId, ValueId, ValueId) -> CompInstr,
    {
        // TODO: validate that operation is allowed using lhs_span and rhs_span
        // e.g. check if types are comparable

        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Comp(make_instr(dest, lhs, rhs)));
        dest
    }

    pub fn emit_eq(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> ValueId {
        self.emit_comp_op(lhs, lhs_span, rhs, rhs_span, |dest, lhs, rhs| {
            CompInstr::Eq { dest, lhs, rhs }
        })
    }

    pub fn emit_neq(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> ValueId {
        self.emit_comp_op(lhs, lhs_span, rhs, rhs_span, |dest, lhs, rhs| {
            CompInstr::Neq { dest, lhs, rhs }
        })
    }

    pub fn emit_lt(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> ValueId {
        self.emit_comp_op(lhs, lhs_span, rhs, rhs_span, |dest, lhs, rhs| {
            CompInstr::Lt { dest, lhs, rhs }
        })
    }

    pub fn emit_lte(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> ValueId {
        self.emit_comp_op(lhs, lhs_span, rhs, rhs_span, |dest, lhs, rhs| {
            CompInstr::Lte { dest, lhs, rhs }
        })
    }

    pub fn emit_gt(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> ValueId {
        self.emit_comp_op(lhs, lhs_span, rhs, rhs_span, |dest, lhs, rhs| {
            CompInstr::Gt { dest, lhs, rhs }
        })
    }

    pub fn emit_gte(
        &mut self,
        lhs: ValueId,
        lhs_span: Span,
        rhs: ValueId,
        rhs_span: Span,
    ) -> ValueId {
        self.emit_comp_op(lhs, lhs_span, rhs, rhs_span, |dest, lhs, rhs| {
            CompInstr::Gte { dest, lhs, rhs }
        })
    }

    pub fn emit_select(
        &mut self,
        condition: ValueId,
        true_value: ValueId,
        false_value: ValueId,
    ) -> ValueId {
        let condition_type = self.get_value_type(condition);

        if !check_assignable(condition_type, &Type::Bool, false) {
            panic!(
                "INTERNAL COMPILER ERROR: Select instruction expected the condition to \
                 be a boolean value"
            );
        }

        let true_value_type = self.get_value_type(true_value);
        let false_value_type = self.get_value_type(false_value);

        if !check_assignable(true_value_type, false_value_type, false) {
            panic!(
                "INTERNAL COMPILER ERROR: Select instruction expected both operands to \
                 have the same type"
            );
        }

        let dest = self.new_value_id(true_value_type.clone());
        self.push_instruction(Instruction::Select(SelectInstr {
            dest,
            cond: condition,
            true_val: true_value,
            false_val: false_value,
        }));

        dest
    }
}

```

`src/hir/builders/emitters/const.rs`:

```rs
use crate::{
    ast::DeclarationId,
    hir::{
        builders::{Builder, ConstantId, InBlock, ValueId},
        instructions::{ConstInstr, Instruction},
        types::{
            checked_declaration::{CheckedDeclaration, FnType},
            checked_type::Type,
        },
    },
    tokenize::NumberKind,
};

impl<'a> Builder<'a, InBlock> {
    pub fn emit_const_number(&mut self, val: NumberKind) -> ValueId {
        let ty = match val {
            NumberKind::I64(_) => Type::I64,
            NumberKind::I32(_) => Type::I32,
            NumberKind::I16(_) => Type::I16,
            NumberKind::I8(_) => Type::I8,
            NumberKind::F32(_) => Type::F32,
            NumberKind::F64(_) => Type::F64,
            NumberKind::U64(_) => Type::U64,
            NumberKind::U32(_) => Type::U32,
            NumberKind::U16(_) => Type::U16,
            NumberKind::U8(_) => Type::U8,
            NumberKind::ISize(_) => Type::ISize,
            NumberKind::USize(_) => Type::USize,
        };

        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Const(ConstInstr::ConstNumber { dest, val }));
        dest
    }

    pub fn emit_const_bool(&mut self, val: bool) -> ValueId {
        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Const(ConstInstr::ConstBool { dest, val }));
        dest
    }

    pub fn emit_const_string(&mut self, constant_id: ConstantId) -> ValueId {
        let dest = self.new_value_id(Type::String);
        self.push_instruction(Instruction::Const(ConstInstr::ConstString {
            dest,
            constant_id,
        }));
        dest
    }

    pub fn emit_const_void(&mut self) -> ValueId {
        let dest = self.new_value_id(Type::Void);
        self.push_instruction(Instruction::Const(ConstInstr::ConstVoid { dest }));
        dest
    }

    pub fn emit_const_fn(&mut self, decl_id: DeclarationId) -> ValueId {
        let decl = self
            .program
            .declarations
            .get(&decl_id)
            .expect("INTERNAL COMPILER ERROR: Function declaration not found");

        let (params, return_type) = match decl {
            CheckedDeclaration::Function(f) => {
                (f.params.clone(), Box::new(f.return_type.clone()))
            }
            _ => panic!("INTERNAL COMPILER ERROR: Declaration is not a function"),
        };

        let ty = Type::Fn(FnType {
            params,
            return_type,
        });

        let dest = self.new_value_id(ty);
        self.push_instruction(Instruction::Const(ConstInstr::ConstFn { dest, decl_id }));
        dest
    }
}

```

`src/hir/builders/emitters/control_flow.rs`:

```rs
use std::collections::HashSet;

use crate::{
    ast::Span,
    hir::{
        builders::{BasicBlockId, Builder, InBlock, PhiSource, TypePredicate, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        instructions::{CallInstr, Instruction, Terminator},
        types::checked_type::Type,
        utils::adjustments::check_assignable,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn push_instruction(&mut self, instruction: Instruction) {
        self.check_no_terminator();
        let bb = self.bb_mut();
        bb.instructions.push(instruction);
    }

    pub fn check_no_terminator(&mut self) {
        let bb = self.bb_mut();

        if bb.terminator.is_some() {
            panic!(
                "INTERNAL COMPILER ERROR: Tried re-set terminator or tried to add an \
                 instruction to a basic block (ID: {}) that has already been terminated",
                bb.id.0
            );
        }
    }

    pub fn emit_call(
        &mut self,
        func: ValueId,
        args: Vec<ValueId>,
        return_type: Type,
    ) -> ValueId {
        let dest = self.new_value_id(return_type);
        self.push_instruction(Instruction::Call(CallInstr { dest, func, args }));
        dest
    }

    pub fn emit_jmp(&mut self, target: BasicBlockId) {
        self.check_no_terminator();
        let this_block_id = self.context.block_id;
        self.get_bb_mut(target).predecessors.insert(this_block_id);

        self.bb_mut().terminator = Some(Terminator::Jump { target });
    }

    pub fn emit_cond_jmp(
        &mut self,
        condition: ValueId,
        true_target: BasicBlockId,
        false_target: BasicBlockId,
    ) {
        self.check_no_terminator();
        let this_block_id = self.context.block_id;

        self.get_bb_mut(true_target)
            .predecessors
            .insert(this_block_id);
        self.get_bb_mut(false_target)
            .predecessors
            .insert(this_block_id);

        self.bb_mut().terminator = Some(Terminator::CondJump {
            condition,
            true_target,
            false_target,
        });
    }

    pub fn emit_return(&mut self, value: ValueId) {
        self.check_no_terminator();
        self.bb_mut().terminator = Some(Terminator::Return { value })
    }

    pub fn emit_logical_or<F>(
        &mut self,
        left: ValueId,
        left_span: Span,
        produce_right: F,
    ) -> ValueId
    where
        F: FnOnce(&mut Self) -> ValueId,
    {
        let left_type = self.get_value_type(left);
        if !check_assignable(left_type, &Type::Bool, false) {
            self.errors.push(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: Type::Bool,
                    received: left_type.clone(),
                },
                span: left_span.clone(),
            });
        }

        let left_preds = self.type_predicates.get(&left).cloned().unwrap_or_default();

        let left_block = self.context.block_id;
        let right_entry_block = self.as_fn().new_bb();
        let merge_block = self.as_fn().new_bb();

        let const_true = self.emit_const_bool(true);

        self.emit_cond_jmp(left, merge_block, right_entry_block);

        self.seal_block(right_entry_block);
        self.use_basic_block(right_entry_block);

        self.apply_predicate_list(&left_preds, false, &left_span);

        let right = produce_right(self);
        let right_block = self.context.block_id;

        let right_type = self.get_value_type(right);
        if !check_assignable(right_type, &Type::Bool, false) {
            self.errors.push(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: Type::Bool,
                    received: right_type.clone(),
                },
                span: left_span.clone(),
            });
        }

        let right_preds = self
            .type_predicates
            .get(&right)
            .cloned()
            .unwrap_or_default();

        self.emit_jmp(merge_block);

        self.seal_block(merge_block);
        self.use_basic_block(merge_block);

        let phi_id = self.new_value_id(Type::Bool);
        let phi_sources = HashSet::from([
            PhiSource {
                from: left_block,
                value: const_true,
            },
            PhiSource {
                from: right_block,
                value: right,
            },
        ]);

        self.insert_phi(self.context.block_id, phi_id, phi_sources);

        let combined = Self::combine_predicates(&left_preds, &right_preds, false);
        if !combined.is_empty() {
            self.type_predicates.insert(phi_id, combined);
        }

        phi_id
    }

    pub fn emit_logical_and<F>(
        &mut self,
        left: ValueId,
        left_span: Span,
        produce_right: F,
    ) -> ValueId
    where
        F: FnOnce(&mut Self) -> ValueId,
    {
        let left_type = self.get_value_type(left);
        if !check_assignable(left_type, &Type::Bool, false) {
            self.errors.push(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: Type::Bool,
                    received: left_type.clone(),
                },
                span: left_span.clone(),
            });
        }

        let left_preds = self.type_predicates.get(&left).cloned().unwrap_or_default();

        let left_block = self.context.block_id;
        let right_entry_block = self.as_fn().new_bb();
        let merge_block = self.as_fn().new_bb();

        let const_false = self.emit_const_bool(false);

        self.emit_cond_jmp(left, right_entry_block, merge_block);

        self.seal_block(right_entry_block);
        self.use_basic_block(right_entry_block);

        self.apply_predicate_list(&left_preds, true, &left_span);

        let right = produce_right(self);
        let right_block = self.context.block_id;

        let right_type = self.get_value_type(right);
        if !check_assignable(right_type, &Type::Bool, false) {
            self.errors.push(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: Type::Bool,
                    received: right_type.clone(),
                },
                span: left_span.clone(),
            });
        }

        let right_preds = self
            .type_predicates
            .get(&right)
            .cloned()
            .unwrap_or_default();

        self.emit_jmp(merge_block);

        self.seal_block(merge_block);
        self.use_basic_block(merge_block);

        let phi_id = self.new_value_id(Type::Bool);
        let phi_sources = HashSet::from([
            PhiSource {
                from: left_block,
                value: const_false,
            },
            PhiSource {
                from: right_block,
                value: right,
            },
        ]);

        self.insert_phi(self.context.block_id, phi_id, phi_sources);

        let combined = Self::combine_predicates(&left_preds, &right_preds, true);
        if !combined.is_empty() {
            self.type_predicates.insert(phi_id, combined);
        }

        phi_id
    }

    fn combine_predicates(
        left_preds: &[TypePredicate],
        right_preds: &[TypePredicate],
        keep_true_side: bool,
    ) -> Vec<TypePredicate> {
        let mut combined = Vec::new();

        for pred in left_preds.iter().chain(right_preds.iter()) {
            let (on_true, on_false) = if keep_true_side {
                (pred.on_true_type.clone(), None)
            } else {
                (None, pred.on_false_type.clone())
            };

            if on_true.is_some() || on_false.is_some() {
                combined.push(TypePredicate {
                    decl_id: pred.decl_id,
                    on_true_type: on_true,
                    on_false_type: on_false,
                });
            }
        }

        combined
    }
}

```

`src/hir/builders/emitters/list.rs`:

```rs
use crate::{
    ast::Span,
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        instructions::{Instruction, ListInstr},
        types::checked_type::Type,
        utils::{
            adjustments::check_assignable, numeric::is_integer, points_to::PathSegment,
        },
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn emit_list_init(&mut self, element_type: Type, items: Vec<ValueId>) -> ValueId {
        let dest = self.new_value_id(Type::List(Box::new(element_type.clone())));

        let alloc_id = self.ptg.new_alloc();
        self.ptg.bind_value_to_alloc(dest, alloc_id);

        for item_val in &items {
            if let Some(val_allocs) = self.ptg.value_locations.get(item_val).cloned() {
                for v_alloc in val_allocs {
                    self.ptg
                        .add_heap_edge(alloc_id, PathSegment::Index, v_alloc);
                }
            }
        }

        self.push_instruction(Instruction::List(ListInstr::Init {
            dest,
            element_type,
            items,
        }));

        dest
    }

    pub fn emit_list_get(
        &mut self,
        list: ValueId,
        index: ValueId,
        span: Span,
    ) -> ValueId {
        let list_type = self.get_value_type(list).clone();
        let index_type = self.get_value_type(index).clone();

        if !is_integer(&index_type) && !matches!(index_type, Type::Unknown) {
            self.errors.push(SemanticError {
                kind: SemanticErrorKind::ExpectedANumericOperand,
                span: span.clone(),
            });
        }

        match list_type {
            Type::List(inner) => {
                let result_type = Type::make_union([*inner, Type::Null]);

                let dest = self.new_value_id(result_type);
                self.push_instruction(Instruction::List(ListInstr::Get {
                    dest,
                    list,
                    index,
                }));

                self.ptg.read_path(dest, list, PathSegment::Index);

                dest
            }
            Type::Unknown => self.new_value_id(Type::Unknown),
            _ => self.report_error_and_get_poison(SemanticError {
                kind: SemanticErrorKind::CannotIndex(list_type),
                span,
            }),
        }
    }

    pub fn emit_list_set(
        &mut self,
        list: ValueId,
        index: ValueId,
        value: ValueId,
        span: Span,
    ) -> ValueId {
        let list_type = self.get_value_type(list).clone();
        let index_type = self.get_value_type(index).clone();
        let value_type = self.get_value_type(value).clone();

        if !is_integer(&index_type) && !matches!(index_type, Type::Unknown) {
            self.errors.push(SemanticError {
                kind: SemanticErrorKind::ExpectedANumericOperand,
                span: span.clone(),
            });
        }

        match list_type {
            Type::List(inner) => {
                if !check_assignable(&value_type, &inner, false) {
                    return self.report_error_and_get_poison(SemanticError {
                        kind: SemanticErrorKind::TypeMismatch {
                            expected: *inner,
                            received: value_type,
                        },
                        span,
                    });
                }

                let dest = self.new_value_id(Type::List(inner));
                self.push_instruction(Instruction::List(ListInstr::Set {
                    dest,
                    list,
                    index,
                    value,
                }));

                if let Some(allocs) = self.ptg.value_locations.get(&list).cloned() {
                    self.ptg.value_locations.insert(dest, allocs);
                }
                self.ptg.update_path(list, PathSegment::Index, value);

                dest
            }
            Type::Unknown => self.new_value_id(Type::Unknown),
            _ => self.report_error_and_get_poison(SemanticError {
                kind: SemanticErrorKind::CannotIndex(list_type),
                span,
            }),
        }
    }

    pub fn emit_list_len(&mut self, list: ValueId, span: Span) -> ValueId {
        let list_type = self.get_value_type(list).clone();

        match list_type {
            Type::List(_) => {
                let dest = self.new_value_id(Type::USize);
                self.push_instruction(Instruction::List(ListInstr::Len { dest, list }));
                dest
            }
            Type::Unknown => self.new_value_id(Type::Unknown),
            _ => self.report_error_and_get_poison(SemanticError {
                kind: SemanticErrorKind::CannotGetLen(list_type),
                span,
            }),
        }
    }
}

```

`src/hir/builders/emitters/mod.rs`:

```rs
pub mod binary;
pub mod comp;
pub mod r#const;
pub mod control_flow;
pub mod list;
pub mod r#struct;
pub mod unary;
pub mod union;

```

`src/hir/builders/emitters/struct.rs`:

```rs
use std::collections::HashMap;

use crate::{
    ast::IdentifierNode,
    compile::interner::StringId,
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        instructions::{Instruction, StructInstr},
        types::{checked_declaration::CheckedParam, checked_type::Type},
        utils::{layout::pack_struct, points_to::PathSegment},
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn emit_struct_init(
        &mut self,
        fields: Vec<(IdentifierNode, ValueId)>,
    ) -> ValueId {
        let type_fields: Vec<CheckedParam> = fields
            .iter()
            .map(|(ident, val_id)| CheckedParam {
                identifier: ident.clone(),
                ty: self.get_value_type(*val_id).clone(),
            })
            .collect();

        let packed_fields = pack_struct(type_fields);

        let mut instr_fields = Vec::with_capacity(fields.len());

        let val_lookup: HashMap<StringId, ValueId> = fields
            .into_iter()
            .map(|(ident, val)| (ident.name, val))
            .collect();

        for param in &packed_fields {
            let val_id = val_lookup
                .get(&param.identifier.name)
                .expect("Field missing after sort");
            instr_fields.push((param.identifier.name, *val_id));
        }

        let struct_type = Type::Struct(packed_fields);
        let dest = self.new_value_id(struct_type);

        let alloc_id = self.ptg.new_alloc();
        self.ptg.bind_value_to_alloc(dest, alloc_id);

        for (name, val_id) in &instr_fields {
            if let Some(val_allocs) = self.ptg.value_locations.get(val_id).cloned() {
                for v_alloc in val_allocs {
                    self.ptg
                        .add_heap_edge(alloc_id, PathSegment::Field(*name), v_alloc);
                }
            }
        }

        self.push_instruction(Instruction::Struct(StructInstr::Construct {
            dest,
            fields: instr_fields,
        }));

        dest
    }

    pub fn emit_read_struct_field(
        &mut self,
        base: ValueId,
        field_identifier: IdentifierNode,
    ) -> ValueId {
        let source_type = self.get_value_type(base).clone();

        match source_type {
            Type::Struct(_) => {
                if let Some((_, ty)) = source_type.get_field(&field_identifier.name) {
                    let dest = self.new_value_id(ty);
                    self.push_instruction(Instruction::Struct(StructInstr::ReadField {
                        dest,
                        base,
                        field: field_identifier.name,
                    }));

                    self.ptg.read_path(
                        dest,
                        base,
                        PathSegment::Field(field_identifier.name),
                    );

                    dest
                } else {
                    self.report_error_and_get_poison(SemanticError {
                        span: field_identifier.span.clone(),
                        kind: SemanticErrorKind::AccessToUndefinedField(field_identifier),
                    })
                }
            }
            Type::Unknown => self.new_value_id(Type::Unknown),
            _ => panic!(
                "INTERNAL COMPILER ERROR: Expected emit_read_struct_field to be called \
                 on a struct type, found {:?}",
                source_type
            ),
        }
    }

    /// Updates a struct field and returns a new struct value with the field's
    /// precise type. Type compatibility is NOT checked here, that's the
    /// caller's responsibility
    pub fn emit_update_struct_field(
        &mut self,
        base: ValueId,
        field_identifier: IdentifierNode,
        value: ValueId,
    ) -> ValueId {
        let base_type = self.get_value_type(base).clone();

        match base_type {
            Type::Struct(_) => {
                if base_type.get_field(&field_identifier.name).is_none() {
                    return self.report_error_and_get_poison(SemanticError {
                        span: field_identifier.span.clone(),
                        kind: SemanticErrorKind::AccessToUndefinedField(field_identifier),
                    });
                }

                let value_type = self.get_value_type(value).clone();
                let updated_type = Self::replace_field_type(
                    &base_type,
                    field_identifier.name,
                    value_type,
                );

                let updated_base = self.new_value_id(updated_type);
                self.push_instruction(Instruction::Struct(StructInstr::UpdateField {
                    dest: updated_base,
                    base,
                    field: field_identifier.name,
                    value,
                }));

                if let Some(allocs) = self.ptg.value_locations.get(&base).cloned() {
                    self.ptg.value_locations.insert(updated_base, allocs);
                }
                self.ptg.update_path(
                    base,
                    PathSegment::Field(field_identifier.name),
                    value,
                );

                updated_base
            }
            Type::Unknown => self.new_value_id(Type::Unknown),
            _ => panic!(
                "INTERNAL COMPILER ERROR: Expected emit_update_struct_field to be \
                 called on a struct type, found {:?}",
                base_type
            ),
        }
    }
}

```

`src/hir/builders/emitters/unary.rs`:

```rs
use crate::{
    ast::Span,
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        instructions::{Instruction, UnaryInstr},
        types::checked_type::Type,
        utils::{adjustments::check_assignable, numeric::is_signed},
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn emit_neg(&mut self, src: ValueId, span: Span) -> ValueId {
        let ty = self.get_value_type(src);

        if !is_signed(ty) {
            return self.report_error_and_get_poison(SemanticError {
                kind: SemanticErrorKind::ExpectedASignedNumericOperand,
                span,
            });
        }

        let dest = self.new_value_id(ty.clone());
        self.push_instruction(Instruction::Unary(UnaryInstr::Neg { dest, src }));
        dest
    }

    pub fn emit_not(&mut self, src: ValueId, span: Span) -> ValueId {
        let ty = self.get_value_type(src);

        if !check_assignable(ty, &Type::Bool, false) {
            return self.report_error_and_get_poison(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: Type::Bool,
                    received: ty.clone(),
                },
                span,
            });
        }

        let dest = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Unary(UnaryInstr::Not { dest, src }));
        dest
    }
}

```

`src/hir/builders/emitters/union.rs`:

```rs
use crate::hir::{
    builders::{Builder, InBlock, ValueId},
    instructions::{Instruction, UnionInstr},
    types::checked_type::Type,
};

impl<'a> Builder<'a, InBlock> {
    /// Tests whether a union value holds a specific variant. Returns a
    /// bool ValueId.
    pub fn emit_test_variant(
        &mut self,
        union_value: ValueId,
        variant_type: &Type,
    ) -> ValueId {
        let union_type = self.get_value_type(union_value);
        let variants = union_type
            .as_union_variants()
            .expect("INTERNAL COMPILER ERROR: test_variant called with non-union");

        assert!(
            variants.iter().any(|v| v == variant_type),
            "INTERNAL COMPILER ERROR: variant not found in union"
        );

        let bool_value = self.new_value_id(Type::Bool);
        self.push_instruction(Instruction::Union(UnionInstr::TestVariant {
            dest: bool_value,
            src: union_value,
            variant_type: variant_type.clone(),
        }));
        bool_value
    }
}

```

`src/hir/builders/function.rs`:

```rs
use std::collections::{HashMap, HashSet};

use crate::{
    globals::next_block_id,
    hir::{
        builders::{BasicBlock, BasicBlockId, Builder, InFunction},
        types::checked_declaration::CheckedDeclaration,
    },
};

impl<'a> Builder<'a, InFunction> {
    pub fn new_bb(&mut self) -> BasicBlockId {
        let id = next_block_id();

        let bb = BasicBlock {
            id,
            instructions: vec![],
            sealed: false,
            terminator: None,
            predecessors: HashSet::new(),
            phis: HashMap::new(),
        };

        let decl = self
            .program
            .declarations
            .get_mut(&self.context.func_id)
            .unwrap_or_else(|| {
                panic!(
                    "INTERNAL COMPILER ERROR: Expected function with DeclarationId({}) \
                     to exist.",
                    self.context.func_id.0
                )
            });

        let func = match decl {
            CheckedDeclaration::Function(f) => f,
            _ => panic!("INTERNAL COMPILER ERROR: Declaration is not a function"),
        };

        func.blocks.insert(id, bb);

        id
    }
}

```

`src/hir/builders/mod.rs`:

```rs
use std::collections::{HashMap, HashSet};

use crate::{
    ast::{DeclarationId, IdentifierNode, ModulePath, Span},
    hir::{
        errors::SemanticError,
        instructions::{Instruction, Terminator},
        types::{
            checked_declaration::{CheckedDeclaration, CheckedParam, FunctionEffects},
            checked_type::Type,
        },
        utils::{points_to::PointsToGraph, scope::Scope},
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

#[derive(Debug, Clone)]
pub struct TypePredicate {
    pub decl_id: DeclarationId,
    pub on_true_type: Option<Type>,
    pub on_false_type: Option<Type>,
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
    pub ptg: PointsToGraph,
    pub param_decl_ids: Vec<DeclarationId>,
    pub effects: FunctionEffects,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PhiSource {
    pub from: BasicBlockId,
    pub value: ValueId,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BasicBlockId,
    pub instructions: Vec<Instruction>,
    pub terminator: Option<Terminator>,
    pub predecessors: HashSet<BasicBlockId>,
    pub phis: HashMap<ValueId, HashSet<PhiSource>>,
    pub sealed: bool,
}

pub trait BuilderContext {}
pub struct Builder<'a, C: BuilderContext> {
    pub context: C,
    pub program: &'a mut Program,

    pub errors: &'a mut Vec<SemanticError>,
    pub current_scope: Scope,

    pub type_predicates: &'a mut HashMap<ValueId, Vec<TypePredicate>>,

    pub current_defs: &'a mut HashMap<BasicBlockId, HashMap<DeclarationId, ValueId>>,
    pub incomplete_phis:
        &'a mut HashMap<BasicBlockId, Vec<(ValueId, DeclarationId, Span)>>,

    pub ptg: &'a mut PointsToGraph,
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

```

`src/hir/builders/module.rs`:

```rs
use crate::hir::builders::{Builder, InGlobal, InModule, Module};

impl<'a> Builder<'a, InModule> {
    pub fn as_program(&mut self) -> Builder<'_, InGlobal> {
        Builder {
            program: self.program,
            errors: self.errors,
            current_scope: self.current_scope.clone(),
            context: InGlobal,
            current_defs: self.current_defs,
            incomplete_phis: self.incomplete_phis,
            type_predicates: self.type_predicates,
            ptg: self.ptg,
        }
    }

    pub fn module(&mut self) -> &mut Module {
        self.program.modules.get_mut(&self.context.path).unwrap()
    }
}

```

`src/hir/builders/program.rs`:

```rs
use std::collections::HashMap;

use crate::{
    ast::{
        decl::{Declaration, FnDecl},
        expr::ExprKind,
        stmt::StmtKind,
        ModulePath, Position,
    },
    compile::ParallelParseResult,
    hir::{
        builders::{BasicBlockId, Builder, Function, InGlobal, InModule, Module},
        types::checked_declaration::{CheckedDeclaration, FunctionEffects},
        utils::{
            check_type::{check_params, check_type_annotation, TypeCheckerContext},
            points_to::PointsToGraph,
            scope::ScopeKind,
        },
    },
};

impl<'a> Builder<'a, InGlobal> {
    pub fn build(&mut self, mut modules: Vec<ParallelParseResult>) {
        for m in &modules {
            let module_scope = self
                .current_scope
                .enter(ScopeKind::File, Position::default());

            self.program.modules.insert(
                m.path.clone(),
                Module {
                    path: m.path.clone(),
                    root_scope: module_scope,
                },
            );
        }

        for m in &modules {
            let mut module_builder = self.as_module(m.path.clone());

            for decl in &m.declarations {
                match decl {
                    Declaration::TypeAlias(alias) => {
                        module_builder.build_type_alias_decl(
                            alias.clone(),
                            alias.identifier.span.clone(),
                        );
                    }
                    Declaration::Fn(f) => {
                        module_builder.register_fn_signature(f);
                    }
                }
            }
        }

        for m in std::mem::take(&mut modules) {
            let mut module_builder = self.as_module(m.path.clone());

            for stmt in m.statements {
                match stmt.kind {
                    StmtKind::From { path, identifiers } => {
                        module_builder.build_from_stmt(path, identifiers, stmt.span);
                    }
                    StmtKind::Expression(expr) => {
                        if let ExprKind::Fn(f) = expr.kind {
                            if let Err(e) = module_builder.build_fn_body(*f) {
                                module_builder.errors.push(e);
                            };
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    fn as_module(&mut self, path: ModulePath) -> Builder<'_, InModule> {
        let scope = self.program.modules.get(&path).unwrap().root_scope.clone();
        Builder {
            context: InModule { path },
            program: self.program,
            errors: self.errors,
            current_scope: scope,
            current_defs: self.current_defs,
            incomplete_phis: self.incomplete_phis,
            type_predicates: self.type_predicates,
            ptg: self.ptg,
        }
    }
}

impl<'a> Builder<'a, InModule> {
    fn register_fn_signature(&mut self, f: &FnDecl) {
        let mut type_ctx = TypeCheckerContext {
            scope: self.current_scope.clone(),
            declarations: &self.program.declarations,
            errors: self.errors,
        };

        let checked_params = check_params(&mut type_ctx, &f.params);
        let checked_return_type = check_type_annotation(&mut type_ctx, &f.return_type);

        let function = Function {
            id: f.id,
            identifier: f.identifier.clone(),
            params: checked_params,
            return_type: checked_return_type,
            is_exported: f.is_exported,
            entry_block: BasicBlockId(0),
            blocks: HashMap::new(),
            value_definitions: HashMap::new(),
            ptg: PointsToGraph::new(),
            param_decl_ids: Vec::new(),
            effects: FunctionEffects::default(),
        };

        self.program
            .declarations
            .insert(f.id, CheckedDeclaration::Function(function));
        self.current_scope.map_name_to_decl(f.identifier.name, f.id);
    }
}

```

`src/hir/errors.rs`:

```rs
use std::collections::HashSet;

use crate::{
    ast::{IdentifierNode, ModulePath, Span},
    compile::interner::StringId,
    hir::{types::checked_type::Type, utils::points_to::PathSegment},
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

```

`src/hir/expressions/access.rs`:

```rs
use crate::{
    ast::{expr::Expr, IdentifierNode},
    hir::builders::{Builder, InBlock, ValueId},
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_access_expr(&mut self, left: Expr, field: IdentifierNode) -> ValueId {
        let base_val = self.build_expr(left);
        self.emit_read_struct_field(base_val, field)
    }
}

```

`src/hir/expressions/and.rs`:

```rs
use crate::{
    ast::expr::Expr,
    hir::builders::{Builder, InBlock, ValueId},
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_and_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        let left_span = left.span.clone();
        let left_id = self.build_expr(left);

        self.emit_logical_and(left_id, left_span, |builder| builder.build_expr(right))
    }
}

```

`src/hir/expressions/binary_op.rs`:

```rs
use crate::{
    ast::{expr::Expr, Span},
    hir::builders::{Builder, InBlock, ValueId},
};

impl<'a> Builder<'a, InBlock> {
    fn build_binary_op<F>(&mut self, left: Expr, right: Expr, emit_fn: F) -> ValueId
    where
        F: FnOnce(&mut Self, ValueId, Span, ValueId, Span) -> ValueId,
    {
        let left_span = left.span.clone();
        let left_value = self.build_expr(left);

        let right_span = right.span.clone();
        let right_value = self.build_expr(right);

        emit_fn(self, left_value, left_span, right_value, right_span)
    }

    pub fn build_add_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        self.build_binary_op(left, right, Self::emit_add)
    }

    pub fn build_sub_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        self.build_binary_op(left, right, Self::emit_sub)
    }

    pub fn build_mul_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        self.build_binary_op(left, right, Self::emit_mul)
    }

    pub fn build_div_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        self.build_binary_op(left, right, Self::emit_div)
    }

    pub fn build_mod_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        self.build_binary_op(left, right, Self::emit_rem)
    }

    pub fn build_eq_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        self.build_binary_op(left, right, Self::emit_eq)
    }

    pub fn build_neq_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        self.build_binary_op(left, right, Self::emit_neq)
    }

    pub fn build_lt_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        self.build_binary_op(left, right, Self::emit_lt)
    }

    pub fn build_lte_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        self.build_binary_op(left, right, Self::emit_lte)
    }

    pub fn build_gt_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        self.build_binary_op(left, right, Self::emit_gt)
    }

    pub fn build_gte_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        self.build_binary_op(left, right, Self::emit_gte)
    }
}

```

`src/hir/expressions/codeblock.rs`:

```rs
use crate::{
    ast::{expr::BlockContents, Span},
    hir::{
        builders::{Builder, InBlock, ValueId},
        utils::scope::ScopeKind,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_codeblock_expr(&mut self, codeblock: BlockContents) -> (ValueId, Span) {
        let mut final_expr_span = Span {
            start: codeblock.span.end,
            end: codeblock.span.end,
            path: codeblock.span.path.clone(),
        };

        self.current_scope = self
            .current_scope
            .enter(ScopeKind::CodeBlock, codeblock.span.start);

        self.build_statements(codeblock.statements);
        let result_id = if let Some(final_expr) = codeblock.final_expr {
            final_expr_span = final_expr.span.clone();
            self.build_expr(*final_expr)
        } else {
            self.emit_const_void()
        };

        self.current_scope = self
            .current_scope
            .exit(codeblock.span.end)
            .expect("INTERNAL COMPILER ERROR: Scope stack mismatch in codeblock");

        (result_id, final_expr_span)
    }
}

```

`src/hir/expressions/fn.rs`:

```rs
use std::collections::{HashMap, HashSet};

use crate::{
    ast::{decl::FnDecl, DeclarationId, Span},
    globals::{next_block_id, next_declaration_id},
    hir::{
        builders::{BasicBlock, Builder, Function, InBlock, InModule, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        instructions::Terminator,
        types::{
            checked_declaration::{
                CheckedDeclaration, CheckedParam, CheckedVarDecl, FunctionEffects,
                ParamMutation,
            },
            checked_type::Type,
        },
        utils::{
            adjustments::check_assignable,
            check_type::{check_params, check_type_annotation, TypeCheckerContext},
            points_to::PointsToGraph,
            scope::ScopeKind,
        },
    },
};

impl<'a> Builder<'a, InModule> {
    pub fn build_fn_body(&mut self, fn_decl: FnDecl) -> Result<(), SemanticError> {
        if !self.current_scope.is_file_scope() {
            return Err(SemanticError {
                kind: SemanticErrorKind::ClosuresNotSupportedYet,
                span: fn_decl.identifier.span.clone(),
            });
        }

        let FnDecl {
            id: decl_id,
            identifier,
            params,
            return_type,
            body,
            is_exported,
            ..
        } = fn_decl;

        let mut type_ctx = TypeCheckerContext {
            scope: self.current_scope.clone(),
            declarations: &self.program.declarations,
            errors: self.errors,
        };

        let checked_params = check_params(&mut type_ctx, &params);
        let checked_return_type = check_type_annotation(&mut type_ctx, &return_type);

        let entry_block_id = next_block_id();
        let function = Function {
            id: decl_id,
            identifier: identifier.clone(),
            params: checked_params.clone(),
            return_type: checked_return_type.clone(),
            is_exported,
            entry_block: entry_block_id,
            blocks: HashMap::new(),
            value_definitions: HashMap::new(),
            ptg: PointsToGraph::new(),
            param_decl_ids: Vec::new(),
            effects: FunctionEffects::default(),
        };

        self.program
            .declarations
            .insert(decl_id, CheckedDeclaration::Function(function));

        let mut fn_builder = Builder {
            context: InBlock {
                path: self.context.path.clone(),
                func_id: decl_id,
                block_id: entry_block_id,
            },
            program: self.program,
            errors: self.errors,
            current_scope: self
                .current_scope
                .enter(ScopeKind::FunctionBody, body.span.start),
            current_defs: self.current_defs,
            incomplete_phis: self.incomplete_phis,
            type_predicates: self.type_predicates,
            ptg: self.ptg,
        };

        let entry_bb = BasicBlock {
            id: entry_block_id,
            instructions: vec![],
            terminator: None,
            predecessors: HashSet::new(),
            phis: HashMap::new(),
            sealed: true,
        };
        fn_builder.get_fn().blocks.insert(entry_block_id, entry_bb);

        let mut param_decl_ids = Vec::with_capacity(checked_params.len());

        for param in &checked_params {
            let identity_id = fn_builder.new_value_id(param.ty.clone());

            let param_decl_id = next_declaration_id();
            let decl = CheckedVarDecl {
                id: param_decl_id,
                identifier: param.identifier.clone(),
                documentation: None,
                constraint: param.ty.clone(),
            };

            fn_builder.write_variable(param_decl_id, entry_block_id, identity_id);

            fn_builder
                .program
                .declarations
                .insert(param_decl_id, CheckedDeclaration::Var(decl));

            fn_builder
                .current_scope
                .map_name_to_decl(param.identifier.name, param_decl_id);

            param_decl_ids.push(param_decl_id);
        }

        fn_builder.get_fn().param_decl_ids = param_decl_ids.clone();

        let (final_value, final_value_span) = fn_builder.build_codeblock_expr(body);
        let final_value_type = fn_builder.get_value_type(final_value).clone();

        if !check_assignable(&final_value_type, &checked_return_type, false) {
            return Err(SemanticError {
                span: final_value_span,
                kind: SemanticErrorKind::ReturnTypeMismatch {
                    expected: checked_return_type.clone(),
                    received: final_value_type,
                },
            });
        }

        fn_builder.emit_return(final_value);

        let effects = fn_builder.compute_effects(
            &checked_params,
            &param_decl_ids,
            &identifier.span,
        );
        fn_builder.get_fn().effects = effects;

        Ok(())
    }
}

impl<'a> Builder<'a, InBlock> {
    pub fn build_fn_expr(&mut self, fn_decl: FnDecl) -> ValueId {
        let id = fn_decl.id;
        match self.as_module().build_fn_body(fn_decl) {
            Ok(v) => v,
            Err(e) => self.errors.push(e),
        };
        self.emit_const_fn(id)
    }

    fn compute_effects(
        &mut self,
        checked_params: &[CheckedParam],
        param_decl_ids: &[DeclarationId],
        fn_span: &Span,
    ) -> FunctionEffects {
        let func = self.get_fn();
        let return_block_ids: Vec<_> = func
            .blocks
            .iter()
            .filter_map(|(id, bb)| {
                if matches!(bb.terminator, Some(Terminator::Return { .. })) {
                    Some(*id)
                } else {
                    None
                }
            })
            .collect();

        if return_block_ids.is_empty() {
            return FunctionEffects::default();
        }

        let mut mutations = Vec::new();

        for (param_index, (param, &param_decl_id)) in
            checked_params.iter().zip(param_decl_ids.iter()).enumerate()
        {
            let declared_type = &param.ty;

            let mut exit_types: Vec<Type> = Vec::new();
            let mut any_changed = false;

            for &block_id in &return_block_ids {
                let val = self.read_variable(param_decl_id, block_id, fn_span.clone());
                let ty = self.get_value_type(val).clone();

                if ty != *declared_type {
                    any_changed = true;
                }

                exit_types.push(ty);
            }

            if any_changed {
                let exit_type = Type::make_union(exit_types);
                mutations.push(ParamMutation {
                    param_index,
                    exit_type,
                });
            }
        }

        FunctionEffects { mutations }
    }
}

```

`src/hir/expressions/fn_call.rs`:

```rs
use crate::{
    ast::{
        expr::{Expr, ExprKind},
        DeclarationId, Span,
    },
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        instructions::{CastInstr, Instruction},
        types::{
            checked_declaration::{CheckedDeclaration, CheckedParam},
            checked_type::Type,
        },
        utils::adjustments::check_assignable,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_fn_call_expr(
        &mut self,
        left: Expr,
        args: Vec<Expr>,
        span: Span,
    ) -> ValueId {
        let callee_decl_id = match &left.kind {
            ExprKind::Identifier(ident) => self.current_scope.lookup(ident.name),
            _ => None,
        };

        let func_id = self.build_expr(left);
        let func_type = self.get_value_type(func_id).clone();

        let (params, return_type) = match func_type {
            Type::Fn(f) => (f.params, *f.return_type),
            Type::Unknown => return self.new_value_id(Type::Unknown),
            _ => {
                return self.report_error_and_get_poison(SemanticError {
                    kind: SemanticErrorKind::CannotCall(func_type),
                    span,
                });
            }
        };

        if args.len() != params.len() {
            return self.report_error_and_get_poison(SemanticError {
                kind: SemanticErrorKind::FnArgumentCountMismatch {
                    expected: params.len(),
                    received: args.len(),
                },
                span,
            });
        }

        let evaluated = self.evaluate_call_args(&args);

        if let Err(e) = self.check_argument_aliasing(&evaluated) {
            return self.report_error_and_get_poison(e);
        }

        let final_args = match self.validate_call_args(&evaluated, &params) {
            Ok(args) => args,
            Err(e) => return self.report_error_and_get_poison(e),
        };

        let result = self.emit_call(func_id, final_args, return_type);

        if let Some(decl_id) = callee_decl_id {
            self.apply_callee_effects(decl_id, &args, &evaluated);
        }

        result
    }

    fn evaluate_call_args(&mut self, args: &[Expr]) -> Vec<(ValueId, Span)> {
        let mut evaluated = Vec::with_capacity(args.len());

        for arg_expr in args {
            let span = arg_expr.span.clone();
            let val_id = self.build_expr(arg_expr.clone());
            evaluated.push((val_id, span));
        }

        evaluated
    }

    fn check_argument_aliasing(
        &self,
        args: &[(ValueId, Span)],
    ) -> Result<(), SemanticError> {
        let val_ids: Vec<ValueId> = args.iter().map(|(v, _)| *v).collect();

        if let Some(conflict) = self.ptg.check_aliasing(&val_ids) {
            return Err(SemanticError {
                kind: SemanticErrorKind::ArgumentAliasing {
                    passed_arg_span: args[conflict.arg_i].1.clone(),
                    passed_path: conflict.path_i,
                    aliased_arg_span: args[conflict.arg_j].1.clone(),
                    aliased_path: conflict.path_j,
                },
                span: args[conflict.arg_i].1.clone(),
            });
        }

        Ok(())
    }

    fn validate_call_args(
        &self,
        args: &[(ValueId, Span)],
        params: &[CheckedParam],
    ) -> Result<Vec<ValueId>, SemanticError> {
        let mut final_args = Vec::with_capacity(args.len());

        for (i, (val_id, span)) in args.iter().enumerate() {
            let val_type = self.get_value_type(*val_id);

            if !check_assignable(val_type, &params[i].ty, false) {
                return Err(SemanticError {
                    kind: SemanticErrorKind::TypeMismatch {
                        expected: params[i].ty.clone(),
                        received: val_type.clone(),
                    },
                    span: span.clone(),
                });
            }

            final_args.push(*val_id);
        }

        Ok(final_args)
    }

    fn apply_callee_effects(
        &mut self,
        callee_decl_id: DeclarationId,
        arg_exprs: &[Expr],
        evaluated: &[(ValueId, Span)],
    ) {
        let effects = match self.program.declarations.get(&callee_decl_id) {
            Some(CheckedDeclaration::Function(f)) => f.effects.clone(),
            _ => return,
        };

        for mutation in &effects.mutations {
            let arg_expr = &arg_exprs[mutation.param_index];
            let arg_span = &evaluated[mutation.param_index].1;

            if let Some((decl_id, Some(new_type), _)) = self.resolve_narrow_target(
                arg_expr,
                Some(mutation.exit_type.clone()),
                None,
            ) {
                self.apply_effect_mutation(decl_id, new_type, arg_span.clone());
            }
        }
    }

    fn apply_effect_mutation(
        &mut self,
        decl_id: DeclarationId,
        new_type: Type,
        span: Span,
    ) {
        let current_val = self.read_variable(decl_id, self.context.block_id, span);
        let current_ty = self.get_value_type(current_val).clone();

        if current_ty == new_type {
            return;
        }

        let new_val = self.new_value_id(new_type);
        self.push_instruction(Instruction::Cast(CastInstr {
            src: current_val,
            dest: new_val,
        }));

        self.write_variable(decl_id, self.context.block_id, new_val);
    }
}

```

`src/hir/expressions/identifier.rs`:

```rs
use crate::{
    ast::IdentifierNode,
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_declaration::CheckedDeclaration,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_identifier_expr(&mut self, identifier: IdentifierNode) -> ValueId {
        let decl_id = match self.current_scope.lookup(identifier.name) {
            Some(id) => id,
            None => {
                return self.report_error_and_get_poison(SemanticError {
                    span: identifier.span.clone(),
                    kind: SemanticErrorKind::UndeclaredIdentifier(identifier),
                });
            }
        };

        let decl = self.program.declarations.get(&decl_id).unwrap();

        match decl {
            CheckedDeclaration::Function(_) => self.emit_const_fn(decl_id),
            CheckedDeclaration::Var(_) => {
                self.read_variable(decl_id, self.context.block_id, identifier.span)
            }
            CheckedDeclaration::TypeAlias(_) => {
                self.report_error_and_get_poison(SemanticError {
                    span: identifier.span.clone(),
                    kind: SemanticErrorKind::CannotUseTypeDeclarationAsValue,
                })
            }
        }
    }
}

```

`src/hir/expressions/if.rs`:

```rs
use std::collections::HashSet;

use crate::{
    ast::{
        expr::{BlockContents, Expr},
        Span,
    },
    hir::{
        builders::{BasicBlockId, Builder, InBlock, PhiSource, TypePredicate, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        instructions::{CastInstr, Instruction},
        types::checked_type::Type,
        utils::adjustments::check_assignable,
    },
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum IfContext {
    /// The `if` is used to produce a value
    Expression,
    /// The `if` is used for control flow, its value is discarded
    Statement,
}

impl<'a> Builder<'a, InBlock> {
    pub fn build_if(
        &mut self,
        branches: Vec<(Box<Expr>, BlockContents)>,
        else_branch: Option<BlockContents>,
        context: IfContext,
    ) -> ValueId {
        let expr_span = branches.first().unwrap().0.span.clone();

        if context == IfContext::Expression && else_branch.is_none() {
            return self.report_error_and_get_poison(SemanticError {
                kind: SemanticErrorKind::IfExpressionMissingElse,
                span: expr_span,
            });
        }

        let merge_block_id = self.as_fn().new_bb();
        let mut branch_results: Vec<(BasicBlockId, ValueId, Span)> = Vec::new();
        let mut current_cond_block_id = self.context.block_id;

        for (condition, body) in branches {
            self.use_basic_block(current_cond_block_id);

            let condition_span = condition.span.clone();
            let cond_id = self.build_expr(*condition);
            let cond_ty = self.get_value_type(cond_id);

            if !check_assignable(cond_ty, &Type::Bool, false) {
                return self.report_error_and_get_poison(SemanticError {
                    span: condition_span,
                    kind: SemanticErrorKind::TypeMismatch {
                        expected: Type::Bool,
                        received: cond_ty.clone(),
                    },
                });
            }

            let then_block_id = self.as_fn().new_bb();
            let next_cond_block_id = self.as_fn().new_bb();

            self.emit_cond_jmp(cond_id, then_block_id, next_cond_block_id);

            self.seal_block(then_block_id);
            self.use_basic_block(then_block_id);

            if let Some(preds) = self.type_predicates.get(&cond_id).cloned() {
                self.apply_predicate_list(&preds, true, &condition_span);
            }

            let (then_val, then_val_span) = self.build_codeblock_expr(body);

            if self.bb().terminator.is_none() {
                branch_results.push((self.context.block_id, then_val, then_val_span));
                self.emit_jmp(merge_block_id);
            }

            self.use_basic_block(next_cond_block_id);

            if let Some(preds) = self.type_predicates.get(&cond_id).cloned() {
                self.apply_predicate_list(&preds, false, &condition_span);
            }

            current_cond_block_id = next_cond_block_id;
        }

        self.use_basic_block(current_cond_block_id);

        if let Some(else_body) = else_branch {
            let (else_val, else_val_span) = self.build_codeblock_expr(else_body);

            if self.bb().terminator.is_none() {
                branch_results.push((self.context.block_id, else_val, else_val_span));
                self.emit_jmp(merge_block_id);
            }
        } else {
            self.emit_jmp(merge_block_id);
        }

        self.seal_block(current_cond_block_id);

        self.seal_block(merge_block_id);
        self.use_basic_block(merge_block_id);

        if context == IfContext::Expression {
            if branch_results.is_empty() {
                return self.new_value_id(Type::Never);
            }

            let type_entries: Vec<Type> = branch_results
                .iter()
                .map(|(_, val, _)| self.get_value_type(*val).clone())
                .collect();

            let result_type = Type::make_union(type_entries);

            let phi_id = self.new_value_id(result_type);
            let phi_sources: HashSet<PhiSource> = branch_results
                .into_iter()
                .map(|(block, value, _)| PhiSource { from: block, value })
                .collect();
            self.insert_phi(merge_block_id, phi_id, phi_sources);
            phi_id
        } else {
            self.emit_const_void()
        }
    }

    pub fn apply_type_predicate(
        &mut self,
        pred: &TypePredicate,
        new_type: Type,
        span: Span,
    ) {
        let current_val = self.read_variable(pred.decl_id, self.context.block_id, span);
        let current_ty = self.get_value_type(current_val).clone();

        if current_ty == new_type {
            return;
        }

        let narrowed = self.new_value_id(new_type);
        self.push_instruction(Instruction::Cast(CastInstr {
            src: current_val,
            dest: narrowed,
        }));

        self.write_variable(pred.decl_id, self.context.block_id, narrowed);
    }

    pub fn apply_predicate_list(
        &mut self,
        preds: &[TypePredicate],
        use_true_side: bool,
        span: &Span,
    ) {
        for pred in preds {
            let ty = if use_true_side {
                &pred.on_true_type
            } else {
                &pred.on_false_type
            };

            if let Some(ty) = ty {
                self.apply_type_predicate(pred, ty.clone(), span.clone());
            }
        }
    }
}

```

`src/hir/expressions/is_type.rs`:

```rs
use crate::{
    ast::{
        expr::{Expr, ExprKind},
        type_annotation::TypeAnnotation,
        DeclarationId, Span,
    },
    compile::interner::StringId,
    hir::{
        builders::{Builder, InBlock, TypePredicate, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::{checked_declaration::CheckedParam, checked_type::Type},
        utils::{
            adjustments::check_assignable,
            check_type::{check_type_annotation, TypeCheckerContext},
        },
    },
};

impl<'a> Builder<'a, InBlock> {
    fn emit_is_one_of_the_variants(
        &mut self,
        union: ValueId,
        matching_variants: &[Type],
        total_variants: usize,
        span: Span,
    ) -> ValueId {
        if matching_variants.is_empty() {
            return self.emit_const_bool(false);
        }

        if matching_variants.len() == total_variants {
            return self.emit_const_bool(true);
        }

        let mut iter = matching_variants.iter();
        let first_variant = iter.next().unwrap();

        let mut result_id = self.emit_test_variant(union, first_variant);

        for variant in iter {
            let variant_clone = variant.clone();
            result_id = self.emit_logical_or(result_id, span.clone(), |builder| {
                builder.emit_test_variant(union, &variant_clone)
            });
        }

        result_id
    }

    pub fn replace_field_type(
        struct_ty: &Type,
        field: StringId,
        new_field_ty: Type,
    ) -> Type {
        if let Type::Struct(fields) = struct_ty {
            let new_fields = fields
                .iter()
                .map(|f| {
                    if f.identifier.name == field {
                        CheckedParam {
                            identifier: f.identifier.clone(),
                            ty: new_field_ty.clone(),
                        }
                    } else {
                        f.clone()
                    }
                })
                .collect();
            Type::Struct(new_fields)
        } else {
            struct_ty.clone()
        }
    }

    /// Walk a narrowable expression up to the root variable, lifting the
    /// narrowed leaf types into the root's struct type at each level.
    pub fn resolve_narrow_target(
        &mut self,
        expr: &Expr,
        on_true: Option<Type>,
        on_false: Option<Type>,
    ) -> Option<(DeclarationId, Option<Type>, Option<Type>)> {
        match &expr.kind {
            ExprKind::Identifier(ident) => {
                let decl_id = self.current_scope.lookup(ident.name)?;
                Some((decl_id, on_true, on_false))
            }
            ExprKind::Access { left, field } => {
                let parent_val = self.build_expr(*left.clone());
                let parent_ty = self.get_value_type(parent_val).clone();

                let lifted_true =
                    on_true.map(|t| Self::replace_field_type(&parent_ty, field.name, t));
                let lifted_false =
                    on_false.map(|t| Self::replace_field_type(&parent_ty, field.name, t));

                self.resolve_narrow_target(left, lifted_true, lifted_false)
            }
            _ => None,
        }
    }

    pub fn build_is_type_expr(&mut self, left: Expr, ty: TypeAnnotation) -> ValueId {
        let span = left.span.clone();

        let current_val = self.build_expr(left.clone());
        let current_ty = self.get_value_type(current_val).clone();

        let variants = match current_ty.as_union_variants() {
            Some(v) => v,
            None => {
                return self.report_error_and_get_poison(SemanticError {
                    span: span.clone(),
                    kind: SemanticErrorKind::CannotNarrowNonUnion(current_ty.clone()),
                });
            }
        };

        let mut type_ctx = TypeCheckerContext {
            scope: self.current_scope.clone(),
            declarations: &self.program.declarations,
            errors: self.errors,
        };
        let target_type = check_type_annotation(&mut type_ctx, &ty);

        if target_type.as_union_variants().is_some() {
            return self.report_error_and_get_poison(SemanticError {
                kind: SemanticErrorKind::UnsupportedUnionNarrowing,
                span: ty.span.clone(),
            });
        }

        let mut matching_variants = Vec::new();
        let mut non_matching_variants = Vec::new();

        for variant in variants {
            if check_assignable(variant, &target_type, false) {
                matching_variants.push(variant.clone());
            } else {
                non_matching_variants.push(variant.clone());
            }
        }

        let result_id = self.emit_is_one_of_the_variants(
            current_val,
            &matching_variants,
            variants.len(),
            span.clone(),
        );

        let true_type = if !matching_variants.is_empty()
            && matching_variants.len() < variants.len()
        {
            Some(Type::make_union(matching_variants))
        } else {
            None
        };

        let false_type = if !non_matching_variants.is_empty()
            && non_matching_variants.len() < variants.len()
        {
            Some(Type::make_union(non_matching_variants))
        } else {
            None
        };

        if true_type.is_some() || false_type.is_some() {
            if let Some((decl_id, lifted_true, lifted_false)) =
                self.resolve_narrow_target(&left, true_type, false_type)
            {
                self.type_predicates.insert(
                    result_id,
                    vec![TypePredicate {
                        decl_id,
                        on_true_type: lifted_true,
                        on_false_type: lifted_false,
                    }],
                );
            }
        }

        result_id
    }
}

```

`src/hir/expressions/list_literal.rs`:

```rs
use crate::{
    ast::{expr::Expr, Span},
    hir::{
        builders::{Builder, InBlock, ValueId},
        types::checked_type::Type,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_list_literal_expr(
        &mut self,
        items: Vec<Expr>,
        _expr_span: Span,
    ) -> ValueId {
        let mut item_values = Vec::with_capacity(items.len());
        let mut element_types = Vec::with_capacity(items.len());

        for item in items {
            let val_id = self.build_expr(item);
            let ty = self.get_value_type(val_id).clone();

            item_values.push(val_id);
            element_types.push(ty);
        }

        let element_type = Type::make_union(element_types);
        self.emit_list_init(element_type, item_values)
    }
}

```

`src/hir/expressions/mod.rs`:

```rs
pub mod access;
pub mod and;
pub mod binary_op;
pub mod codeblock;
pub mod r#fn;
pub mod fn_call;
pub mod identifier;
pub mod r#if;
pub mod is_type;
pub mod list_literal;
pub mod or;
pub mod static_access;
pub mod string;
pub mod struct_init;
pub mod typecast;
pub mod unary_op;

use crate::{
    ast::expr::{Expr, ExprKind},
    hir::{
        builders::{Builder, InBlock, ValueId},
        expressions::r#if::IfContext,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_expr(&mut self, expr: Expr) -> ValueId {
        let span = expr.span.clone();
        match expr.kind {
            ExprKind::Not { right } => self.build_not_expr(*right),
            ExprKind::Neg { right } => self.build_neg_expr(*right),

            ExprKind::Add { left, right } => self.build_add_expr(*left, *right),
            ExprKind::Subtract { left, right } => self.build_sub_expr(*left, *right),
            ExprKind::Multiply { left, right } => self.build_mul_expr(*left, *right),
            ExprKind::Divide { left, right } => self.build_div_expr(*left, *right),
            ExprKind::Modulo { left, right } => self.build_mod_expr(*left, *right),
            ExprKind::LessThan { left, right } => self.build_lt_expr(*left, *right),
            ExprKind::LessThanOrEqual { left, right } => {
                self.build_lte_expr(*left, *right)
            }
            ExprKind::GreaterThan { left, right } => self.build_gt_expr(*left, *right),
            ExprKind::GreaterThanOrEqual { left, right } => {
                self.build_gte_expr(*left, *right)
            }
            ExprKind::Equal { left, right } => self.build_eq_expr(*left, *right),
            ExprKind::NotEqual { left, right } => self.build_neq_expr(*left, *right),

            ExprKind::And { left, right } => self.build_and_expr(*left, *right),
            ExprKind::Or { left, right } => self.build_or_expr(*left, *right),

            ExprKind::BoolLiteral(value) => self.emit_const_bool(value),
            ExprKind::Number(number_kind) => self.emit_const_number(number_kind),
            ExprKind::String(string_node) => self.build_string_literal(string_node),

            ExprKind::Struct(fields) => self.build_struct_init_expr(fields),
            ExprKind::List(items) => self.build_list_literal_expr(items, span),

            ExprKind::Access { left, field } => self.build_access_expr(*left, field),
            ExprKind::StaticAccess { left, field } => {
                self.build_static_access_expr(*left, field)
            }
            ExprKind::If {
                branches,
                else_branch,
            } => self.build_if(branches, else_branch, IfContext::Expression),

            ExprKind::CodeBlock(block_contents) => {
                self.build_codeblock_expr(block_contents).0
            }

            ExprKind::Fn(fn_decl) => self.build_fn_expr(*fn_decl),
            ExprKind::FnCall { left, args } => self.build_fn_call_expr(*left, args, span),

            ExprKind::Identifier(identifier_node) => {
                self.build_identifier_expr(identifier_node)
            }
            ExprKind::TypeCast { left, target } => {
                self.build_typecast_expr(*left, target)
            }
            ExprKind::IsType { left, ty } => self.build_is_type_expr(*left, ty),
        }
    }
}

```

`src/hir/expressions/or.rs`:

```rs
use crate::{
    ast::expr::Expr,
    hir::builders::{Builder, InBlock, ValueId},
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_or_expr(&mut self, left: Expr, right: Expr) -> ValueId {
        let left_span = left.span.clone();
        let left_id = self.build_expr(left);

        self.emit_logical_or(left_id, left_span, |builder| builder.build_expr(right))
    }
}

```

`src/hir/expressions/static_access.rs`:

```rs
use crate::{
    ast::{expr::Expr, IdentifierNode},
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_static_access_expr(
        &mut self,
        left: Expr,
        field: IdentifierNode,
    ) -> ValueId {
        let span = field.span.clone();

        let left_id = self.build_expr(left);
        let left_type = self.get_value_type(left_id).clone();

        self.report_error_and_get_poison(SemanticError {
            kind: SemanticErrorKind::CannotStaticAccess(left_type),
            span,
        })
    }
}

```

`src/hir/expressions/string.rs`:

```rs
use crate::{
    ast::StringNode,
    globals::next_constant_id,
    hir::builders::{Builder, InBlock, ValueId},
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_string_literal(&mut self, node: StringNode) -> ValueId {
        let constant_id = next_constant_id();
        self.program
            .constant_data
            .insert(constant_id, node.value.as_bytes().to_vec());

        self.emit_const_string(constant_id)
    }
}

```

`src/hir/expressions/struct_init.rs`:

```rs
use std::collections::HashSet;

use crate::{
    ast::{expr::Expr, IdentifierNode},
    compile::interner::StringId,
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_struct_init_expr(
        &mut self,
        fields: Vec<(IdentifierNode, Expr)>,
    ) -> ValueId {
        let mut field_values: Vec<(IdentifierNode, ValueId)> =
            Vec::with_capacity(fields.len());
        let mut seen_names: HashSet<StringId> = HashSet::new();

        for (field_name, field_expr) in fields {
            if !seen_names.insert(field_name.name) {
                self.errors.push(SemanticError {
                    kind: SemanticErrorKind::DuplicateStructFieldInitializer(
                        field_name.clone(),
                    ),
                    span: field_name.span.clone(),
                });
            }

            let val_id = self.build_expr(field_expr);
            field_values.push((field_name, val_id));
        }

        self.emit_struct_init(field_values)
    }
}

```

`src/hir/expressions/typecast.rs`:

```rs
use crate::{
    ast::{expr::Expr, type_annotation::TypeAnnotation},
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        instructions::{CastInstr, Instruction},
        utils::{
            adjustments::check_assignable,
            check_type::{check_type_annotation, TypeCheckerContext},
        },
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_typecast_expr(&mut self, left: Expr, target: TypeAnnotation) -> ValueId {
        let source_span = left.span.clone();
        let source = self.build_expr(left);
        let source_type = self.get_value_type(source).clone();

        let mut type_ctx = TypeCheckerContext {
            scope: self.current_scope.clone(),
            declarations: &self.program.declarations,
            errors: self.errors,
        };
        let target_type = check_type_annotation(&mut type_ctx, &target);

        if source_type == target_type {
            return source;
        }

        if !check_assignable(&source_type, &target_type, true) {
            return self.report_error_and_get_poison(SemanticError {
                kind: SemanticErrorKind::CannotCastType {
                    source_type,
                    target_type,
                },
                span: source_span,
            });
        }

        let dest = self.new_value_id(target_type);
        self.push_instruction(Instruction::Cast(CastInstr { src: source, dest }));
        dest
    }
}

```

`src/hir/expressions/unary_op.rs`:

```rs
use crate::{
    ast::expr::Expr,
    hir::builders::{Builder, InBlock, TypePredicate, ValueId},
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_not_expr(&mut self, expr: Expr) -> ValueId {
        let span = expr.span.clone();
        let operand_id = self.build_expr(expr);

        let result_id = self.emit_not(operand_id, span);

        if let Some(preds) = self.type_predicates.get(&operand_id).cloned() {
            let flipped: Vec<TypePredicate> = preds
                .into_iter()
                .map(|pred| TypePredicate {
                    decl_id: pred.decl_id,
                    on_true_type: pred.on_false_type,
                    on_false_type: pred.on_true_type,
                })
                .collect();

            self.type_predicates.insert(result_id, flipped);
        }

        result_id
    }

    pub fn build_neg_expr(&mut self, expr: Expr) -> ValueId {
        let span = expr.span.clone();
        let operand_id = self.build_expr(expr);

        self.emit_neg(operand_id, span)
    }
}

```

`src/hir/instructions.rs`:

```rs
use std::collections::BTreeSet;

use crate::{
    ast::DeclarationId,
    compile::interner::StringId,
    hir::{
        builders::{BasicBlockId, ConstantId, ValueId},
        types::checked_type::Type,
    },
    tokenize::NumberKind,
};

#[derive(Clone, Debug)]
pub enum ConstInstr {
    ConstNumber {
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
}

#[derive(Clone, Debug)]
pub enum UnaryInstr {
    Neg { dest: ValueId, src: ValueId },
    Not { dest: ValueId, src: ValueId },
}

#[derive(Clone, Debug)]
pub enum BinaryInstr {
    Add {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    Sub {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    Mul {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    Div {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    Rem {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
}

#[derive(Clone, Debug)]
pub enum CompInstr {
    Eq {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    Neq {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    Lt {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    Lte {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    Gt {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    Gte {
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
}

#[derive(Clone, Debug)]
pub enum StructInstr {
    Construct {
        dest: ValueId,
        fields: Vec<(StringId, ValueId)>,
    },
    ReadField {
        dest: ValueId,
        base: ValueId,
        field: StringId,
    },
    UpdateField {
        dest: ValueId,
        base: ValueId,
        field: StringId,
        value: ValueId,
    },
}

#[derive(Clone, Debug)]
pub enum UnionInstr {
    WrapInUnion {
        dest: ValueId,
        src: ValueId,
        target_variants: BTreeSet<Type>,
    },
    UnwrapUnion {
        dest: ValueId,
        src: ValueId,
        variant_type: Type,
    },
    TestVariant {
        dest: ValueId,
        src: ValueId,
        variant_type: Type,
    },
    WidenUnion {
        dest: ValueId,
        src: ValueId,
    },
    NarrowUnion {
        dest: ValueId,
        src: ValueId,
    },
}

#[derive(Clone, Debug)]
pub enum ListInstr {
    Init {
        dest: ValueId,
        element_type: Type,
        items: Vec<ValueId>,
    },
    Get {
        dest: ValueId,
        list: ValueId,
        index: ValueId,
    },
    Set {
        dest: ValueId,
        list: ValueId,
        index: ValueId,
        value: ValueId,
    },
    Len {
        dest: ValueId,
        list: ValueId,
    },
}

#[derive(Clone, Debug)]
pub struct CastInstr {
    pub src: ValueId,
    pub dest: ValueId,
}

#[derive(Clone, Debug)]
pub struct CallInstr {
    pub dest: ValueId,
    pub func: ValueId,
    pub args: Vec<ValueId>,
}

#[derive(Clone, Debug)]
pub struct SelectInstr {
    pub dest: ValueId,
    pub cond: ValueId,
    pub true_val: ValueId,
    pub false_val: ValueId,
}

#[derive(Clone, Debug)]
pub enum Instruction {
    Binary(BinaryInstr),
    Unary(UnaryInstr),
    Const(ConstInstr),
    Comp(CompInstr),
    Struct(StructInstr),
    Union(UnionInstr),
    List(ListInstr),
    Cast(CastInstr),
    Call(CallInstr),
    Select(SelectInstr),
}

#[derive(Clone, Debug)]
pub enum Terminator {
    Jump {
        target: BasicBlockId,
    },
    CondJump {
        condition: ValueId,
        true_target: BasicBlockId,
        false_target: BasicBlockId,
    },
    Return {
        value: ValueId,
    },
}

```

`src/hir/mod.rs`:

```rs
pub mod builders;
pub mod errors;
pub mod expressions;
pub mod instructions;
pub mod statements;
pub mod types;
pub mod utils;

```

`src/hir/statements/assignment.rs`:

```rs
use crate::{
    ast::{
        expr::{Expr, ExprKind},
        Span,
    },
    hir::{
        builders::{Builder, InBlock, ValueId},
        errors::{SemanticError, SemanticErrorKind},
        types::{checked_declaration::CheckedDeclaration, checked_type::Type},
        utils::adjustments::check_assignable,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn write_expr(
        &mut self,
        target: &Expr,
        value: ValueId,
        span: Span,
    ) -> Result<(), SemanticError> {
        match &target.kind {
            ExprKind::Identifier(ident) => {
                let decl_id = self.current_scope.lookup(ident.name).ok_or_else(|| {
                    SemanticError {
                        kind: SemanticErrorKind::UndeclaredIdentifier(ident.clone()),
                        span: span.clone(),
                    }
                })?;

                self.write_variable(decl_id, self.context.block_id, value);
                Ok(())
            }
            ExprKind::Access { left, field } => {
                let base_val = self.build_expr(*left.clone());
                let new_base_val =
                    self.emit_update_struct_field(base_val, field.clone(), value);
                self.write_expr(left, new_base_val, span)
            }
            _ => Err(SemanticError {
                kind: SemanticErrorKind::InvalidLValue,
                span,
            }),
        }
    }

    pub fn build_assignment_stmt(&mut self, target: Expr, value: Expr) {
        let value_span = value.span.clone();
        let target_span = target.span.clone();

        let value_id = self.build_expr(value);
        let value_type = self.get_value_type(value_id).clone();

        let constraint = match self.get_constraint_type(&target) {
            Some(c) => c,
            None => {
                self.errors.push(SemanticError {
                    kind: SemanticErrorKind::InvalidLValue,
                    span: target_span,
                });
                return;
            }
        };

        if !check_assignable(&value_type, &constraint, false) {
            self.errors.push(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: constraint,
                    received: value_type,
                },
                span: value_span,
            });
            return;
        }

        if let Err(e) = self.write_expr(&target, value_id, target_span) {
            self.errors.push(e);
        }
    }

    pub fn get_constraint_type(&self, expr: &Expr) -> Option<Type> {
        match &expr.kind {
            ExprKind::Identifier(ident) => {
                let decl_id = self.current_scope.lookup(ident.name)?;
                match self.program.declarations.get(&decl_id)? {
                    CheckedDeclaration::Var(v) => Some(v.constraint.clone()),
                    _ => None,
                }
            }
            ExprKind::Access { left, field } => {
                let parent_constraint = self.get_constraint_type(left)?;
                parent_constraint.get_field(&field.name).map(|(_, ty)| ty)
            }
            _ => None,
        }
    }
}

```

`src/hir/statements/from.rs`:

```rs
use crate::{
    ast::{IdentifierNode, ModulePath, Span, StringNode},
    hir::{
        builders::{Builder, InModule},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_declaration::CheckedDeclaration,
    },
};
use std::path::PathBuf;
use std::sync::Arc;

impl<'a> Builder<'a, InModule> {
    pub fn build_from_stmt(
        &mut self,
        path: StringNode,
        identifiers: Vec<(IdentifierNode, Option<IdentifierNode>)>,
        span: Span,
    ) {
        if !self.current_scope.is_file_scope() {
            self.errors.push(SemanticError {
                kind: SemanticErrorKind::FromStatementMustBeDeclaredAtTopLevel,
                span,
            });
            return;
        }

        let mut target_path_buf = PathBuf::from(&*self.context.path.0);
        target_path_buf.pop();
        target_path_buf.push(&path.value);

        let canonical_path = match target_path_buf.canonicalize() {
            Ok(p) => ModulePath(Arc::new(p)),
            Err(_) => {
                self.errors.push(SemanticError {
                    kind: SemanticErrorKind::ModuleNotFound(ModulePath(Arc::new(
                        target_path_buf,
                    ))),
                    span: path.span,
                });

                return;
            }
        };

        let target_module = match self.program.modules.get(&canonical_path) {
            Some(m) => m,
            None => {
                self.errors.push(SemanticError {
                    kind: SemanticErrorKind::ModuleNotFound(canonical_path),
                    span: path.span,
                });

                return;
            }
        };

        for (imported_ident, alias) in identifiers {
            let not_exported_err = SemanticError {
                span: imported_ident.span.clone(),
                kind: SemanticErrorKind::SymbolNotExported {
                    module_path: canonical_path.clone(),
                    symbol: imported_ident.clone(),
                },
            };

            if let Some(decl_id) = target_module.root_scope.lookup(imported_ident.name) {
                let is_exported = match self.program.declarations.get(&decl_id) {
                    Some(CheckedDeclaration::Function(f)) => f.is_exported,
                    Some(CheckedDeclaration::TypeAlias(t)) => t.is_exported,
                    _ => false,
                };

                if is_exported {
                    let name_node = alias.unwrap_or(imported_ident);
                    self.current_scope.map_name_to_decl(name_node.name, decl_id);
                } else {
                    self.errors.push(not_exported_err);
                    continue;
                }
            } else {
                self.errors.push(not_exported_err);
                continue;
            }
        }
    }
}

```

`src/hir/statements/mod.rs`:

```rs
pub mod assignment;
pub mod from;
pub mod r#return;
pub mod type_alias_decl;
pub mod var_decl;
pub mod r#while;

use crate::{
    ast::{
        expr::ExprKind,
        stmt::{Stmt, StmtKind},
    },
    hir::{
        builders::{Builder, InBlock},
        errors::{SemanticError, SemanticErrorKind},
        expressions::r#if::IfContext,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_statements(&mut self, statements: Vec<Stmt>) {
        for statement in statements {
            if self.bb().terminator.is_some() {
                self.errors.push(SemanticError {
                    kind: SemanticErrorKind::UnreachableCode,
                    span: statement.span,
                });
                break;
            }

            match statement.kind {
                StmtKind::Expression(expr) => {
                    if let ExprKind::If {
                        branches,
                        else_branch,
                    } = expr.kind
                    {
                        self.build_if(branches, else_branch, IfContext::Statement);
                    } else {
                        self.build_expr(expr);
                    }
                }
                StmtKind::TypeAliasDecl(decl) => {
                    self.as_module().build_type_alias_decl(decl, statement.span)
                }
                StmtKind::VarDecl(var_decl) => self.build_var_decl(var_decl),
                StmtKind::Return { value } => {
                    self.build_return_stmt(value, statement.span)
                }
                StmtKind::Assignment { target, value } => {
                    self.build_assignment_stmt(target, value)
                }
                StmtKind::From { path, identifiers } => {
                    self.as_module()
                        .build_from_stmt(path, identifiers, statement.span);
                }
                StmtKind::While { condition, body } => {
                    self.build_while_stmt(*condition, body)
                }
                StmtKind::Break => {
                    if let Some(targets) = self.current_scope.within_loop_body() {
                        self.emit_jmp(targets.on_break);
                    } else {
                        self.errors.push(SemanticError {
                            kind: SemanticErrorKind::BreakKeywordOutsideLoop,
                            span: statement.span,
                        })
                    }
                }
                StmtKind::Continue => {
                    if let Some(targets) = self.current_scope.within_loop_body() {
                        self.emit_jmp(targets.on_continue);
                    } else {
                        self.errors.push(SemanticError {
                            kind: SemanticErrorKind::ContinueKeywordOutsideLoop,
                            span: statement.span,
                        })
                    }
                }
            }
        }
    }
}

```

`src/hir/statements/return.rs`:

```rs
use crate::{
    ast::{expr::Expr, Span},
    hir::{
        builders::{Builder, InBlock},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_declaration::CheckedDeclaration,
        utils::adjustments::check_assignable,
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_return_stmt(&mut self, value: Expr, span: Span) {
        let value_span = value.span.clone();
        let func_id = self.context.func_id;
        let expected_return_type = match self.program.declarations.get(&func_id) {
            Some(CheckedDeclaration::Function(f)) => f.return_type.clone(),
            _ => {
                self.errors.push(SemanticError {
                    kind: SemanticErrorKind::ReturnKeywordOutsideFunction,
                    span: span.clone(),
                });
                return;
            }
        };

        let val_id = self.build_expr(value);
        let actual_type = self.get_value_type(val_id).clone();

        if !check_assignable(&actual_type, &expected_return_type, false) {
            self.errors.push(SemanticError {
                kind: SemanticErrorKind::ReturnTypeMismatch {
                    expected: expected_return_type,
                    received: actual_type,
                },
                span: value_span,
            });
            return;
        }

        self.emit_return(val_id);
    }
}

```

`src/hir/statements/type_alias_decl.rs`:

```rs
use crate::{
    ast::{decl::TypeAliasDecl, Span},
    hir::{
        builders::{Builder, InModule},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_declaration::{CheckedDeclaration, CheckedTypeAliasDecl},
        utils::check_type::{check_type_annotation, TypeCheckerContext},
    },
};

impl<'a> Builder<'a, InModule> {
    pub fn build_type_alias_decl(&mut self, type_alias_decl: TypeAliasDecl, span: Span) {
        if !self.current_scope.is_file_scope() {
            self.errors.push(SemanticError {
                kind: SemanticErrorKind::TypeAliasMustBeDeclaredAtTopLevel,
                span,
            });
            return;
        }

        let mut type_ctx = TypeCheckerContext {
            scope: self.current_scope.clone(),
            declarations: &self.program.declarations,
            errors: self.errors,
        };

        let resolved_type = check_type_annotation(&mut type_ctx, &type_alias_decl.value);

        let checked_type_alias_decl = CheckedTypeAliasDecl {
            id: type_alias_decl.id,
            documentation: type_alias_decl.documentation,
            identifier: type_alias_decl.identifier.clone(),
            span,
            value: Box::new(resolved_type),
            is_exported: type_alias_decl.is_exported,
        };

        self.program.declarations.insert(
            type_alias_decl.id,
            CheckedDeclaration::TypeAlias(checked_type_alias_decl),
        );

        self.current_scope
            .map_name_to_decl(type_alias_decl.identifier.name, type_alias_decl.id);
    }
}

```

`src/hir/statements/var_decl.rs`:

```rs
use crate::{
    ast::decl::VarDecl,
    hir::{
        builders::{Builder, InBlock},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_declaration::{CheckedDeclaration, CheckedVarDecl},
        utils::{
            adjustments::check_assignable,
            check_type::{check_type_annotation, TypeCheckerContext},
        },
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_var_decl(&mut self, var_decl: VarDecl) {
        if self.current_scope.is_file_scope() {
            self.errors.push(SemanticError {
                kind: SemanticErrorKind::CannotDeclareGlobalVariable,
                span: var_decl.identifier.span.clone(),
            });
            return;
        }

        let value_span = var_decl.value.span.clone();

        let val_id = self.build_expr(var_decl.value);
        let val_type = self.get_value_type(val_id).clone();

        let constraint = if let Some(annotation) = &var_decl.constraint {
            let mut type_ctx = TypeCheckerContext {
                scope: self.current_scope.clone(),
                declarations: &self.program.declarations,
                errors: self.errors,
            };

            check_type_annotation(&mut type_ctx, annotation)
        } else {
            val_type.clone()
        };

        if !check_assignable(&val_type, &constraint, false) {
            self.errors.push(SemanticError {
                kind: SemanticErrorKind::TypeMismatch {
                    expected: constraint.clone(),
                    received: val_type,
                },
                span: value_span,
            });
        }

        self.write_variable(var_decl.id, self.context.block_id, val_id);

        let checked_var_decl = CheckedVarDecl {
            id: var_decl.id,
            identifier: var_decl.identifier.clone(),
            documentation: var_decl.documentation,
            constraint,
        };

        self.program
            .declarations
            .insert(var_decl.id, CheckedDeclaration::Var(checked_var_decl));

        self.current_scope
            .map_name_to_decl(var_decl.identifier.name, var_decl.id);
    }
}

```

`src/hir/statements/while.rs`:

```rs
use crate::{
    ast::expr::{BlockContents, Expr},
    hir::{
        builders::{Builder, InBlock},
        errors::{SemanticError, SemanticErrorKind},
        types::checked_type::Type,
        utils::{adjustments::check_assignable, scope::ScopeKind},
    },
};

impl<'a> Builder<'a, InBlock> {
    pub fn build_while_stmt(&mut self, condition: Expr, body: BlockContents) {
        let header_block_id = self.as_fn().new_bb();
        let body_block_id = self.as_fn().new_bb();
        let exit_block_id = self.as_fn().new_bb();

        self.emit_jmp(header_block_id);
        self.use_basic_block(header_block_id);

        let condition_span = condition.span.clone();
        let cond_id = self.build_expr(condition);
        let cond_ty = self.get_value_type(cond_id);

        if !check_assignable(cond_ty, &Type::Bool, false) {
            self.errors.push(SemanticError {
                span: condition_span,
                kind: SemanticErrorKind::TypeMismatch {
                    expected: Type::Bool,
                    received: cond_ty.clone(),
                },
            });
        }

        self.emit_cond_jmp(cond_id, body_block_id, exit_block_id);

        self.seal_block(body_block_id);
        self.use_basic_block(body_block_id);

        self.current_scope = self.current_scope.enter(
            ScopeKind::WhileBody {
                break_target: exit_block_id,
                continue_target: header_block_id,
            },
            body.span.start,
        );

        self.build_statements(body.statements);
        if let Some(final_expr) = body.final_expr {
            self.build_expr(*final_expr);
        }

        self.current_scope = self
            .current_scope
            .exit(body.span.end)
            .expect("INTERNAL COMPILER ERROR: Scope mismatch");

        if self.bb().terminator.is_none() {
            self.emit_jmp(header_block_id);
        }

        self.seal_block(header_block_id);

        self.use_basic_block(exit_block_id);
        self.seal_block(exit_block_id);
    }
}

```

`src/hir/types/checked_declaration.rs`:

```rs
use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
};

use crate::{
    ast::{DeclarationId, IdentifierNode, Span},
    hir::{builders::Function, types::checked_type::Type},
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

#[derive(Debug, Clone)]
pub struct ParamMutation {
    pub param_index: usize,
    pub exit_type: Type,
}

#[derive(Debug, Clone, Default)]
pub struct FunctionEffects {
    pub mutations: Vec<ParamMutation>,
}

```

`src/hir/types/checked_type.rs`:

```rs
use crate::{
    compile::interner::StringId,
    hir::types::{
        checked_declaration::{CheckedParam, FnType},
        ordered_number_kind::OrderedNumberKind,
    },
};
use std::{collections::BTreeSet, hash::Hash};

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum LiteralType {
    Number(OrderedNumberKind),
    Bool(bool),
    String(StringId),
}

// TODO: make cheaper to clone
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Type {
    Void,
    Bool,
    U8,
    U16,
    U32,
    U64,
    USize,
    ISize,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Null,
    Literal(LiteralType),
    Struct(Vec<CheckedParam>),
    Union(BTreeSet<Type>),
    List(Box<Type>),
    String,
    Fn(FnType),
    Unknown,
    Never,
}

impl Type {
    pub fn make_union(types: impl IntoIterator<Item = Type>) -> Type {
        let mut flat_set = BTreeSet::new();

        for ty in types {
            if matches!(ty, Type::Never) {
                continue;
            }

            if let Type::Union(variants) = ty {
                flat_set.extend(variants);
                continue;
            }

            flat_set.insert(ty);
        }

        match flat_set.len() {
            0 => Type::Never,
            1 => flat_set.into_iter().next().unwrap(),
            _ => Type::Union(flat_set),
        }
    }

    pub fn union(self, other: Type) -> Type {
        Type::make_union(vec![self, other])
    }

    pub fn intersect(self, other: Type) -> Type {
        let s1 = self.into_set();
        let s2 = other.into_set();
        let result = s1.intersection(&s2).cloned();

        Type::make_union(result)
    }

    pub fn subtract(self, other: Type) -> Type {
        let mut s1 = self.into_set();
        let s2 = other.into_set();

        for t in s2 {
            s1.remove(&t);
        }

        Type::make_union(s1)
    }

    fn into_set(self) -> BTreeSet<Type> {
        if matches!(self, Type::Never) {
            return BTreeSet::new();
        }

        if let Type::Union(variants) = self {
            return variants;
        }

        BTreeSet::from([self])
    }

    pub fn as_union_variants(&self) -> Option<&BTreeSet<Type>> {
        match self {
            Type::Union(variants) => Some(variants),
            _ => None,
        }
    }

    /// Maps a struct field name -> (Index, Type)
    pub fn get_field(&self, name: &StringId) -> Option<(usize, Type)> {
        match self {
            Type::Struct(fields) => fields
                .iter()
                .enumerate()
                .find(|(_, param)| &param.identifier.name == name)
                .map(|(index, param)| (index, param.ty.clone())),
            _ => None,
        }
    }
}

```

`src/hir/types/mod.rs`:

```rs
pub mod checked_declaration;
pub mod checked_type;
pub mod ordered_number_kind;

```

`src/hir/types/ordered_number_kind.rs`:

```rs
use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
};

use crate::tokenize::NumberKind;

/// A wrapper around NumberKind that implements Eq, Hash, and Ord
/// by treating floats as raw bits.
#[derive(Debug, Clone, Copy)]
pub struct OrderedNumberKind(pub NumberKind);

impl OrderedNumberKind {
    fn variant_index(&self) -> u8 {
        match self.0 {
            NumberKind::I64(_) => 0,
            NumberKind::I32(_) => 1,
            NumberKind::I16(_) => 2,
            NumberKind::I8(_) => 3,
            NumberKind::F32(_) => 4,
            NumberKind::F64(_) => 5,
            NumberKind::U64(_) => 6,
            NumberKind::U32(_) => 7,
            NumberKind::U16(_) => 8,
            NumberKind::U8(_) => 9,
            NumberKind::ISize(_) => 10,
            NumberKind::USize(_) => 11,
        }
    }
}

impl PartialEq for OrderedNumberKind {
    fn eq(&self, other: &Self) -> bool {
        match (self.0, other.0) {
            (NumberKind::I64(a), NumberKind::I64(b)) => a == b,
            (NumberKind::I32(a), NumberKind::I32(b)) => a == b,
            (NumberKind::I16(a), NumberKind::I16(b)) => a == b,
            (NumberKind::I8(a), NumberKind::I8(b)) => a == b,
            (NumberKind::U64(a), NumberKind::U64(b)) => a == b,
            (NumberKind::U32(a), NumberKind::U32(b)) => a == b,
            (NumberKind::U16(a), NumberKind::U16(b)) => a == b,
            (NumberKind::U8(a), NumberKind::U8(b)) => a == b,
            (NumberKind::ISize(a), NumberKind::ISize(b)) => a == b,
            (NumberKind::USize(a), NumberKind::USize(b)) => a == b,
            (NumberKind::F32(a), NumberKind::F32(b)) => a.to_bits() == b.to_bits(),
            (NumberKind::F64(a), NumberKind::F64(b)) => a.to_bits() == b.to_bits(),
            _ => false,
        }
    }
}

impl Eq for OrderedNumberKind {}

impl Hash for OrderedNumberKind {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(&self.0).hash(state);
        match self.0 {
            NumberKind::I64(v) => v.hash(state),
            NumberKind::I32(v) => v.hash(state),
            NumberKind::I16(v) => v.hash(state),
            NumberKind::I8(v) => v.hash(state),
            NumberKind::U64(v) => v.hash(state),
            NumberKind::U32(v) => v.hash(state),
            NumberKind::U16(v) => v.hash(state),
            NumberKind::U8(v) => v.hash(state),
            NumberKind::ISize(v) => v.hash(state),
            NumberKind::USize(v) => v.hash(state),
            NumberKind::F32(v) => v.to_bits().hash(state),
            NumberKind::F64(v) => v.to_bits().hash(state),
        }
    }
}

impl PartialOrd for OrderedNumberKind {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for OrderedNumberKind {
    fn cmp(&self, other: &Self) -> Ordering {
        let self_idx = self.variant_index();
        let other_idx = other.variant_index();

        match self_idx.cmp(&other_idx) {
            Ordering::Equal => match (self.0, other.0) {
                (NumberKind::I64(a), NumberKind::I64(b)) => a.cmp(&b),
                (NumberKind::I32(a), NumberKind::I32(b)) => a.cmp(&b),
                (NumberKind::I16(a), NumberKind::I16(b)) => a.cmp(&b),
                (NumberKind::I8(a), NumberKind::I8(b)) => a.cmp(&b),
                (NumberKind::U64(a), NumberKind::U64(b)) => a.cmp(&b),
                (NumberKind::U32(a), NumberKind::U32(b)) => a.cmp(&b),
                (NumberKind::U16(a), NumberKind::U16(b)) => a.cmp(&b),
                (NumberKind::U8(a), NumberKind::U8(b)) => a.cmp(&b),
                (NumberKind::ISize(a), NumberKind::ISize(b)) => a.cmp(&b),
                (NumberKind::USize(a), NumberKind::USize(b)) => a.cmp(&b),
                (NumberKind::F32(a), NumberKind::F32(b)) => a.to_bits().cmp(&b.to_bits()),
                (NumberKind::F64(a), NumberKind::F64(b)) => a.to_bits().cmp(&b.to_bits()),
                _ => unreachable!(
                    "OrderedNumberKind::cmp: variant indices matched but types differed"
                ),
            },
            other_order => other_order,
        }
    }
}

```

`src/hir/utils/adjustments.rs`:

```rs
use crate::{
    ast::Span,
    hir::{
        errors::{SemanticError, SemanticErrorKind},
        types::checked_type::Type,
        utils::numeric::{get_numeric_type_rank, is_float, is_integer, is_signed},
    },
};

/// Checks if `source` can be assigned/cast to `target`
///
/// `is_explicit = false`: implicit conversions only (assignments, args, returns)
///   - Numeric widening (i32 → i64, f32 → f64, integer → float)
///   - Union coercion (i32 → i32 | string)
///   - Struct field coercion ({ value: i32 } → { value: i32 | string })
///
/// `is_explicit = true`: adds explicit conversions (typecast `as`)
///   - Numeric narrowing (i64 → i32, float → integer)
///   - Union unwrapping (i32 | string → i32)
pub fn check_assignable(source: &Type, target: &Type, is_explicit: bool) -> bool {
    check_assignable_recursive(source, target, is_explicit)
}

fn check_assignable_recursive(source: &Type, target: &Type, is_explicit: bool) -> bool {
    if source == target {
        return true;
    }

    if matches!(source, Type::Never) {
        return true;
    }

    if matches!(target, Type::Unknown) || matches!(source, Type::Unknown) {
        return true;
    }

    // Integer → Integer
    if is_integer(source) && is_integer(target) {
        let s_rank = get_numeric_type_rank(source).unwrap();
        let t_rank = get_numeric_type_rank(target).unwrap();
        if t_rank > s_rank || (t_rank < s_rank && is_explicit) {
            return true;
        }
    }

    // Float → Float
    if is_float(source) && is_float(target) {
        let s_rank = get_numeric_type_rank(source).unwrap();
        let t_rank = get_numeric_type_rank(target).unwrap();
        if t_rank > s_rank || (t_rank < s_rank && is_explicit) {
            return true;
        }
    }

    // Integer → Float
    if is_integer(source) && is_float(target) {
        return true;
    }

    // Float → Integer (explicit only)
    if is_float(source) && is_integer(target) && is_explicit {
        return true;
    }

    // Unions
    match (source.as_union_variants(), target.as_union_variants()) {
        (None, Some(_)) => todo!(),
        (Some(_), None) => todo!(),
        (Some(_), Some(_)) => todo!(),
        (None, None) => todo!(),
    }

    // Struct → Struct (field-level assignability)
    if let (Type::Struct(s_fields), Type::Struct(t_fields)) = (source, target) {
        if s_fields.len() == t_fields.len() {
            return s_fields.iter().zip(t_fields.iter()).all(|(sf, tf)| {
                sf.identifier.name == tf.identifier.name
                    && check_assignable_recursive(&sf.ty, &tf.ty, is_explicit)
            });
        }
    }

    if let (Type::List(s_elem), Type::List(t_elem)) = (source, target) {
        return check_assignable_recursive(s_elem, t_elem, is_explicit);
    }

    if let (Type::Fn(s_fn), Type::Fn(t_fn)) = (source, target) {
        if s_fn.params.len() != t_fn.params.len() {
            return false;
        }
        let params_ok = s_fn
            .params
            .iter()
            .zip(t_fn.params.iter())
            .all(|(sp, tp)| check_assignable_recursive(&sp.ty, &tp.ty, is_explicit));
        return params_ok
            && check_assignable_recursive(
                &s_fn.return_type,
                &t_fn.return_type,
                is_explicit,
            );
    }

    false
}

pub fn type_mismatch_error(source: &Type, target: &Type) -> SemanticErrorKind {
    SemanticErrorKind::TypeMismatch {
        expected: target.clone(),
        received: source.clone(),
    }
}

pub fn arithmetic_supertype(
    left: &Type,
    left_span: Span,
    right: &Type,
    right_span: Span,
) -> Result<Type, SemanticError> {
    let span = Span {
        start: left_span.start,
        end: right_span.end,
        path: left_span.path.clone(),
    };

    let left_type = if is_float(left) || is_integer(left) {
        left
    } else {
        return Err(SemanticError {
            kind: SemanticErrorKind::ExpectedANumericOperand,
            span: left_span,
        });
    };

    let right_type = if is_float(right) || is_integer(right) {
        right
    } else {
        return Err(SemanticError {
            kind: SemanticErrorKind::ExpectedANumericOperand,
            span: right_span,
        });
    };

    if (is_float(left_type) && is_integer(right_type))
        || (is_integer(left_type) && is_float(right_type))
    {
        return Err(SemanticError {
            kind: SemanticErrorKind::MixedFloatAndInteger,
            span,
        });
    }

    if is_signed(left_type) != is_signed(right_type) {
        return Err(SemanticError {
            kind: SemanticErrorKind::MixedSignedAndUnsigned,
            span,
        });
    }

    if right_type == left_type {
        return Ok(left_type.clone());
    }

    let left_rank = get_numeric_type_rank(left_type);
    let right_rank = get_numeric_type_rank(right_type);

    if left_rank > right_rank {
        Ok(left_type.clone())
    } else {
        Ok(right_type.clone())
    }
}

```

`src/hir/utils/check_type.rs`:

```rs
use std::collections::HashMap;

use crate::{
    ast::{
        decl::Param,
        type_annotation::{TypeAnnotation, TypeAnnotationKind},
        DeclarationId, IdentifierNode,
    },
    hir::{
        errors::{SemanticError, SemanticErrorKind},
        types::{
            checked_declaration::{CheckedDeclaration, CheckedParam, FnType},
            checked_type::Type,
        },
        utils::scope::Scope,
    },
};

pub struct TypeCheckerContext<'a> {
    pub scope: Scope,
    pub declarations: &'a HashMap<DeclarationId, CheckedDeclaration>,
    pub errors: &'a mut Vec<SemanticError>,
}

pub fn check_params(ctx: &mut TypeCheckerContext, params: &[Param]) -> Vec<CheckedParam> {
    params
        .iter()
        .map(|p| CheckedParam {
            ty: check_type_annotation(ctx, &p.constraint),
            identifier: p.identifier.clone(),
        })
        .collect()
}

pub fn check_type_identifier_annotation(
    ctx: &mut TypeCheckerContext,
    id: IdentifierNode,
) -> Type {
    ctx.scope
        .lookup(id.name)
        .map(|entry| {
            match ctx.declarations.get(&entry).unwrap_or_else(|| {
                panic!(
                    "INTERNAL COMPILER ERROR: Expected declarations to contain \
                     DeclarationId({}) key",
                    entry.0
                )
            }) {
                CheckedDeclaration::TypeAlias(decl) => (*decl.value).clone(),
                CheckedDeclaration::Function(_) => {
                    ctx.errors.push(SemanticError {
                        kind: SemanticErrorKind::CannotUseFunctionDeclarationAsType,
                        span: id.span.clone(),
                    });

                    Type::Unknown
                }
                CheckedDeclaration::Var(_) => {
                    ctx.errors.push(SemanticError {
                        kind: SemanticErrorKind::CannotUseVariableDeclarationAsType,
                        span: id.span.clone(),
                    });

                    Type::Unknown
                }
            }
        })
        .unwrap_or_else(|| {
            ctx.errors.push(SemanticError {
                span: id.span.clone(),
                kind: SemanticErrorKind::UndeclaredType(id),
            });

            Type::Unknown
        })
}

pub fn check_type_annotation(
    ctx: &mut TypeCheckerContext,
    annotation: &TypeAnnotation,
) -> Type {
    match &annotation.kind {
        TypeAnnotationKind::Void => Type::Void,
        TypeAnnotationKind::Bool => Type::Bool,
        TypeAnnotationKind::U8 => Type::U8,
        TypeAnnotationKind::U16 => Type::U16,
        TypeAnnotationKind::U32 => Type::U32,
        TypeAnnotationKind::U64 => Type::U64,
        TypeAnnotationKind::I8 => Type::I8,
        TypeAnnotationKind::I16 => Type::I16,
        TypeAnnotationKind::I32 => Type::I32,
        TypeAnnotationKind::I64 => Type::I64,
        TypeAnnotationKind::F32 => Type::F32,
        TypeAnnotationKind::F64 => Type::F64,
        TypeAnnotationKind::Identifier(id) => {
            check_type_identifier_annotation(ctx, id.clone())
        }
        TypeAnnotationKind::FnType {
            params,
            return_type,
        } => {
            let checked_params = check_params(ctx, params);
            let checked_return_type = check_type_annotation(ctx, return_type);

            Type::Fn(FnType {
                params: checked_params,
                return_type: Box::new(checked_return_type),
            })
        }
        TypeAnnotationKind::Literal(t) => Type::Literal(t.clone()),
        TypeAnnotationKind::Union(variants) => {
            let mut checked_variants = Vec::new();

            for v in variants {
                checked_variants.push(check_type_annotation(ctx, v));
            }

            Type::make_union(checked_variants)
        }
        TypeAnnotationKind::String => Type::String,
        TypeAnnotationKind::List(item_type) => {
            let checked_item_type = check_type_annotation(ctx, item_type);
            Type::List(Box::new(checked_item_type))
        }
        TypeAnnotationKind::Struct(items) => {
            let checked_field_types = check_params(ctx, items);
            Type::Struct(checked_field_types)
        }
        TypeAnnotationKind::Null => Type::Null,
    }
}

```

`src/hir/utils/dump.rs`:

```rs
use crate::{
    globals::STRING_INTERNER,
    hir::{
        builders::{BasicBlockId, Function, Program, ValueId},
        instructions::{
            BinaryInstr, CallInstr, CompInstr, ConstInstr, Instruction, ListInstr,
            SelectInstr, StructInstr, Terminator, UnaryInstr, UnionInstr,
        },
        types::checked_declaration::CheckedDeclaration,
        utils::type_to_string::type_to_string,
    },
    tokenize::number_kind_to_suffix,
};
use std::{collections::VecDeque, fmt::Write};

fn get_vt(p: &Program, vid: &ValueId) -> String {
    type_to_string(&p.value_types[vid])
}

fn find_blocks(f: &Function) -> Vec<BasicBlockId> {
    let mut blocks = Vec::new();
    let mut queue = VecDeque::new();
    let mut expanded = std::collections::HashSet::new();

    queue.push_back(f.entry_block);

    while let Some(bid) = queue.pop_front() {
        blocks.retain(|&id| id != bid);
        blocks.push(bid);

        if expanded.insert(bid) {
            if let Some(bb) = f.blocks.get(&bid) {
                if let Some(terminator) = &bb.terminator {
                    match terminator {
                        Terminator::Jump { target, .. } => {
                            queue.push_back(*target);
                        }
                        Terminator::CondJump {
                            true_target,
                            false_target,
                            ..
                        } => {
                            queue.push_back(*true_target);
                            queue.push_back(*false_target);
                        }
                        _ => {}
                    }
                }
            }
        }
    }
    blocks
}

pub fn dump_program(program: &Program) {
    let mut out = String::new();
    writeln!(out, "========== HIR DUMP START ==========").unwrap();
    for (_, decl) in program.declarations.iter() {
        if let CheckedDeclaration::Function(f) = decl {
            dump_function(f, program, &mut out);
        }
    }
    writeln!(out, "====================================").unwrap();
    println!("{}", out);
}

fn dump_function(f: &Function, p: &Program, out: &mut String) {
    let fn_name = STRING_INTERNER.resolve(f.identifier.name);
    let return_type = type_to_string(&f.return_type);
    writeln!(out, "fn {fn_name} -> {return_type}:").unwrap();
    let block_ids = find_blocks(f);

    for bid in block_ids {
        dump_block(&bid, f, p, out);
    }
}

pub fn dump_block(block_id: &BasicBlockId, f: &Function, p: &Program, out: &mut String) {
    let bb = f.blocks.get(block_id).unwrap();
    writeln!(out, "  block_{}:", bb.id.0).unwrap();

    writeln!(out, "    predecessors {{ ").unwrap();
    for p in &bb.predecessors {
        writeln!(out, "      block_{}", p.0).unwrap();
    }
    writeln!(out, "    }} ").unwrap();

    writeln!(out, "    phis {{ ").unwrap();
    for (dest, operands) in &bb.phis {
        let ops_str = operands
            .iter()
            .map(|phi| format!("v{} from block_{}", phi.value.0, phi.from.0))
            .collect::<Vec<_>>()
            .join(", ");
        writeln!(
            out,
            "      v{}: {} = phi [ {} ];",
            dest.0,
            get_vt(p, dest),
            ops_str
        )
        .unwrap();
    }
    writeln!(out, "    }} ").unwrap();

    writeln!(out).unwrap();

    dump_instructions(&bb.instructions, p, out);

    if let Some(term) = bb.terminator.clone() {
        match term {
            Terminator::Jump { target } => {
                writeln!(out, "    jmp block_{}", target.0).unwrap();
            }
            Terminator::CondJump {
                condition,
                true_target,
                false_target,
            } => {
                writeln!(
                    out,
                    "    cond_jmp v{} ? block_{} : block_{}\n",
                    condition.0, true_target.0, false_target.0
                )
                .unwrap();
            }
            Terminator::Return { value } => {
                writeln!(out, "    ret v{}\n", value.0).unwrap();
            }
        }
    }
}

pub fn dump_instructions(instrs: &[Instruction], p: &Program, out: &mut String) {
    let get_binary_sign = |instr: &BinaryInstr| match instr {
        BinaryInstr::Add { .. } => "+",
        BinaryInstr::Sub { .. } => "-",
        BinaryInstr::Mul { .. } => "*",
        BinaryInstr::Div { .. } => "/",
        BinaryInstr::Rem { .. } => "%",
    };

    let get_comp_sign = |instr: &CompInstr| match instr {
        CompInstr::Eq { .. } => "==",
        CompInstr::Neq { .. } => "!=",
        CompInstr::Lt { .. } => "<",
        CompInstr::Lte { .. } => "<=",
        CompInstr::Gt { .. } => ">",
        CompInstr::Gte { .. } => ">=",
    };

    for instruction in instrs {
        write!(out, "    ").unwrap();
        match instruction {
            Instruction::Const(kind) => match kind {
                ConstInstr::ConstNumber { dest, val } => {
                    writeln!(
                        out,
                        "v{}: {} = {};",
                        dest.0,
                        number_kind_to_suffix(val),
                        val.to_string()
                    )
                    .unwrap();
                }
                ConstInstr::ConstBool { dest, val } => {
                    writeln!(out, "v{}: bool = {};", dest.0, val).unwrap();
                }
                ConstInstr::ConstString { dest, constant_id } => {
                    let literal = String::from_utf8(
                        p.constant_data.get(constant_id).unwrap().clone(),
                    )
                    .unwrap();
                    writeln!(out, "v{}: string = \"{}\";", dest.0, literal).unwrap();
                }
                ConstInstr::ConstVoid { dest } => {
                    writeln!(out, "v{}: void = void;", dest.0).unwrap();
                }
                ConstInstr::ConstFn { dest, decl_id } => {
                    let decl = p.declarations.get(decl_id).unwrap_or_else(|| {
                        panic!(
                            "INTERNAL COMPILER ERROR: No corresponding for \
                             DeclarationId({})",
                            decl_id.0
                        )
                    });
                    let fn_identifier = if let CheckedDeclaration::Function(f) = decl {
                        f.identifier.clone()
                    } else {
                        panic!(
                            "INTERNAL COMPILER ERROR: Expected declaration id to \
                             correspond to a function"
                        )
                    };

                    writeln!(
                        out,
                        "v{}: {} = <function {} from {}>;",
                        dest.0,
                        get_vt(p, dest),
                        STRING_INTERNER.resolve(fn_identifier.name),
                        fn_identifier.span.path.0.display()
                    )
                    .unwrap();
                }
            },
            Instruction::Unary(kind) => match kind {
                UnaryInstr::Neg { dest, src } => {
                    writeln!(out, "v{}: {} = -{};", dest.0, get_vt(p, dest), src.0)
                        .unwrap();
                }
                UnaryInstr::Not { dest, src } => {
                    writeln!(out, "v{}: {} = !{};", dest.0, get_vt(p, dest), src.0)
                        .unwrap();
                }
            },
            Instruction::Binary(kind) => match kind {
                BinaryInstr::Add { dest, lhs, rhs }
                | BinaryInstr::Sub { dest, lhs, rhs }
                | BinaryInstr::Mul { dest, lhs, rhs }
                | BinaryInstr::Div { dest, lhs, rhs }
                | BinaryInstr::Rem { dest, lhs, rhs } => {
                    writeln!(
                        out,
                        "v{}: {} = v{} {} v{};",
                        dest.0,
                        get_vt(p, dest),
                        lhs.0,
                        get_binary_sign(kind),
                        rhs.0
                    )
                    .unwrap();
                }
            },
            Instruction::Comp(kind) => match kind {
                CompInstr::Eq { dest, lhs, rhs }
                | CompInstr::Neq { dest, lhs, rhs }
                | CompInstr::Lt { dest, lhs, rhs }
                | CompInstr::Lte { dest, lhs, rhs }
                | CompInstr::Gt { dest, lhs, rhs }
                | CompInstr::Gte { dest, lhs, rhs } => {
                    writeln!(
                        out,
                        "v{}: {} = v{} {} v{};",
                        dest.0,
                        get_vt(p, dest),
                        lhs.0,
                        get_comp_sign(kind),
                        rhs.0
                    )
                    .unwrap();
                }
            },
            Instruction::Select(SelectInstr {
                dest,
                cond,
                true_val,
                false_val,
            }) => {
                writeln!(
                    out,
                    "v{}: {} = v{} ? v{} : v{};",
                    dest.0,
                    get_vt(p, dest),
                    cond.0,
                    true_val.0,
                    false_val.0
                )
                .unwrap();
            }
            Instruction::Call(CallInstr { dest, func, args }) => {
                let args = args
                    .iter()
                    .map(|a| format!("v{}", a.0))
                    .collect::<Vec<String>>()
                    .join(", ");

                writeln!(
                    out,
                    "v{}: {} = call v{}({});",
                    dest.0,
                    get_vt(p, dest),
                    func.0,
                    args
                )
                .unwrap();
            }
            Instruction::Struct(struct_instr) => match struct_instr {
                StructInstr::Construct { dest, fields } => {
                    let fields_str = fields
                        .iter()
                        .map(|(name, val)| {
                            format!("{}: v{}", STRING_INTERNER.resolve(*name), val.0)
                        })
                        .collect::<Vec<String>>()
                        .join(", ");
                    writeln!(
                        out,
                        "v{}: {} = struct {{ {} }};",
                        dest.0,
                        get_vt(p, dest),
                        fields_str
                    )
                    .unwrap();
                }
                StructInstr::ReadField { dest, base, field } => {
                    writeln!(
                        out,
                        "v{}: {} = v{}.{};",
                        dest.0,
                        get_vt(p, dest),
                        base.0,
                        STRING_INTERNER.resolve(*field)
                    )
                    .unwrap();
                }
                StructInstr::UpdateField {
                    dest,
                    base,
                    field,
                    value,
                } => {
                    writeln!(
                        out,
                        "v{}: {} = update v{} {{ {}: v{} }};",
                        dest.0,
                        get_vt(p, dest),
                        base.0,
                        STRING_INTERNER.resolve(*field),
                        value.0
                    )
                    .unwrap();
                }
            },
            Instruction::Union(union_instr) => match union_instr {
                UnionInstr::WrapInUnion {
                    dest,
                    src,
                    target_variants: _,
                } => {
                    writeln!(
                        out,
                        "v{}: {} = wrap_union v{};",
                        dest.0,
                        get_vt(p, dest),
                        src.0
                    )
                    .unwrap();
                }
                UnionInstr::UnwrapUnion {
                    dest,
                    src,
                    variant_type,
                } => {
                    writeln!(
                        out,
                        "v{}: {} = unwrap_union v{} as {};",
                        dest.0,
                        get_vt(p, dest),
                        src.0,
                        type_to_string(variant_type)
                    )
                    .unwrap();
                }
                UnionInstr::TestVariant {
                    dest,
                    src,
                    variant_type,
                } => {
                    writeln!(
                        out,
                        "v{}: {} = test_variant v{} is {};",
                        dest.0,
                        get_vt(p, dest),
                        src.0,
                        type_to_string(variant_type)
                    )
                    .unwrap();
                }
                UnionInstr::WidenUnion { dest, src } => {
                    writeln!(
                        out,
                        "v{}: {} = widen_union v{};",
                        dest.0,
                        get_vt(p, dest),
                        src.0
                    )
                    .unwrap();
                }
                UnionInstr::NarrowUnion { dest, src } => {
                    writeln!(
                        out,
                        "v{}: {} = narrow_union v{};",
                        dest.0,
                        get_vt(p, dest),
                        src.0
                    )
                    .unwrap();
                }
            },
            Instruction::List(list_instr) => match list_instr {
                ListInstr::Init {
                    dest,
                    element_type: _,
                    items,
                } => {
                    let items_str = items
                        .iter()
                        .map(|v| format!("v{}", v.0))
                        .collect::<Vec<String>>()
                        .join(", ");
                    writeln!(out, "v{}: {} = [{}];", dest.0, get_vt(p, dest), items_str)
                        .unwrap();
                }
                ListInstr::Get { dest, list, index } => {
                    writeln!(
                        out,
                        "v{}: {} = v{}[{}];",
                        dest.0,
                        get_vt(p, dest),
                        list.0,
                        index.0
                    )
                    .unwrap();
                }
                ListInstr::Set {
                    dest,
                    list,
                    index,
                    value,
                } => {
                    writeln!(
                        out,
                        "v{}: {} = setListItem(v{}[{}] to v{});",
                        dest.0,
                        get_vt(p, dest),
                        list.0,
                        index.0,
                        value.0
                    )
                    .unwrap();
                }
                ListInstr::Len { dest, list } => {
                    writeln!(out, "v{}: {} = len(v{});", dest.0, get_vt(p, dest), list.0)
                        .unwrap();
                }
            },
            Instruction::Cast(cast_instr) => {
                let dest_type_str = get_vt(p, &cast_instr.dest);
                writeln!(
                    out,
                    "v{}: {} = v{}::as({})",
                    cast_instr.dest.0, dest_type_str, cast_instr.src.0, dest_type_str,
                )
                .unwrap();
            }
        }
    }
}

```

`src/hir/utils/get_poison.rs`:

```rs
use crate::hir::{
    builders::{Builder, InBlock, ValueId},
    errors::SemanticError,
    types::checked_type::Type,
};

impl<'a> Builder<'a, InBlock> {
    pub fn report_error_and_get_poison(&mut self, error: SemanticError) -> ValueId {
        self.errors.push(error);
        self.new_value_id(Type::Unknown)
    }
}

```

`src/hir/utils/layout.rs`:

```rs
use crate::{
    globals::STRING_INTERNER,
    hir::types::{checked_declaration::CheckedParam, checked_type::Type},
};

#[derive(Debug, Clone, Copy)]
pub struct Layout {
    pub size: usize,
    pub alignment: usize,
}

impl Layout {
    pub fn new(size: usize, alignment: usize) -> Self {
        Self { size, alignment }
    }
}

// Assuming 64-bit architecture
const PTR_SIZE: usize = 8;
const PTR_ALIGN: usize = 8;

pub fn get_layout_of(ty: &Type) -> Layout {
    match ty {
        Type::Void | Type::Never | Type::Unknown | Type::Literal(_) | Type::Null => {
            Layout::new(0, 1)
        }

        Type::Bool | Type::U8 | Type::I8 => Layout::new(1, 1),
        Type::U16 | Type::I16 => Layout::new(2, 2),
        Type::U32 | Type::I32 | Type::F32 => Layout::new(4, 4),

        Type::U64 | Type::I64 | Type::F64 | Type::USize | Type::ISize => {
            Layout::new(8, 8)
        }

        Type::String => Layout::new(PTR_SIZE * 2, PTR_ALIGN), // (ptr, len)
        Type::List(_) => Layout::new(PTR_SIZE * 3, PTR_ALIGN), // (ptr, len, cap)
        Type::Fn(_) => Layout::new(PTR_SIZE, PTR_ALIGN),      // function pointer

        Type::Struct(fields) => {
            let field_types: Vec<&Type> = fields.iter().map(|p| &p.ty).collect();
            calculate_fields_layout(&field_types)
        }

        Type::Union(variants) => {
            let mut max_size = 0;
            let mut max_align = 1;

            for v in variants {
                let layout = get_layout_of(v);
                max_size = max_size.max(layout.size);
                max_align = max_align.max(layout.alignment);
            }

            let discriminant_size = 2;
            let discriminant_align = 2;

            let total_align = max_align.max(discriminant_align);

            let padding_before_payload =
                (max_align - (discriminant_size % max_align)) % max_align;

            let raw_size = discriminant_size + padding_before_payload + max_size;

            let padding_end = (total_align - (raw_size % total_align)) % total_align;

            let total_size = raw_size + padding_end;

            Layout::new(total_size, total_align)
        }
    }
}

pub fn get_alignment_of(ty: &Type) -> usize {
    get_layout_of(ty).alignment
}

fn calculate_fields_layout(field_types: &[&Type]) -> Layout {
    let mut current_offset = 0;
    let mut max_alignment = 1;

    for ty in field_types {
        let field_layout = get_layout_of(ty);

        max_alignment = std::cmp::max(max_alignment, field_layout.alignment);

        let padding = (field_layout.alignment
            - (current_offset % field_layout.alignment))
            % field_layout.alignment;

        current_offset += padding;
        current_offset += field_layout.size;
    }

    let padding_end = (max_alignment - (current_offset % max_alignment)) % max_alignment;
    let total_size = current_offset + padding_end;

    Layout::new(total_size, max_alignment)
}

pub fn pack_struct(mut fields: Vec<CheckedParam>) -> Vec<CheckedParam> {
    fields.sort_by(|field_a, field_b| {
        let align_a = get_alignment_of(&field_a.ty);
        let align_b = get_alignment_of(&field_b.ty);

        align_b.cmp(&align_a).then_with(|| {
            let name_a = STRING_INTERNER.resolve(field_a.identifier.name);
            let name_b = STRING_INTERNER.resolve(field_b.identifier.name);
            name_a.cmp(&name_b)
        })
    });

    fields
}

```

`src/hir/utils/mod.rs`:

```rs
pub mod adjustments;
pub mod check_type;
pub mod dump;
pub mod get_poison;
pub mod layout;
pub mod numeric;
pub mod points_to;
pub mod scope;
pub mod type_to_string;
pub mod union;

```

`src/hir/utils/numeric.rs`:

```rs
use crate::hir::types::checked_type::Type;

pub fn get_numeric_type_rank(ty: &Type) -> Option<i32> {
    use Type::*;
    match &ty {
        I8 | U8 => Some(1),
        I16 | U16 => Some(2),
        I32 | U32 | ISize | USize => Some(3),
        I64 | U64 => Some(4),
        F32 => Some(5),
        F64 => Some(6),
        _ => None,
    }
}

pub fn is_float(ty: &Type) -> bool {
    use Type::*;
    matches!(ty, F32 | F64)
}

pub fn is_integer(ty: &Type) -> bool {
    use Type::*;
    matches!(
        ty,
        I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64 | ISize | USize
    )
}

pub fn is_signed(ty: &Type) -> bool {
    use Type::*;
    matches!(ty, I8 | I16 | I32 | I64 | ISize | F32 | F64)
}

```

`src/hir/utils/points_to.rs`:

```rs
use std::collections::{HashMap, HashSet, VecDeque};

use crate::{
    compile::interner::StringId, globals::STRING_INTERNER, hir::builders::ValueId,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct AllocId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum PathSegment {
    Field(StringId),
    Index,
}

pub struct AliasConflict {
    pub arg_i: usize,
    pub arg_j: usize,
    pub path_i: Vec<PathSegment>,
    pub path_j: Vec<PathSegment>,
}

#[derive(Default, Debug, Clone)]
pub struct PointsToGraph {
    next_alloc_id: usize,
    /// Maps an SSA ValueId to the set of allocations it directly points to
    pub value_locations: HashMap<ValueId, HashSet<AllocId>>,
    /// The Heap Graph: Maps an Allocation and a PathSegment to the allocations it points to
    pub heap_edges: HashMap<AllocId, HashMap<PathSegment, HashSet<AllocId>>>,
}

impl PointsToGraph {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn new_alloc(&mut self) -> AllocId {
        let id = AllocId(self.next_alloc_id);
        self.next_alloc_id += 1;
        id
    }

    pub fn bind_value_to_alloc(&mut self, val: ValueId, alloc: AllocId) {
        self.value_locations.entry(val).or_default().insert(alloc);
    }

    pub fn add_heap_edge(
        &mut self,
        base: AllocId,
        segment: PathSegment,
        target: AllocId,
    ) {
        self.heap_edges
            .entry(base)
            .or_default()
            .entry(segment)
            .or_default()
            .insert(target);
    }

    pub fn merge_values(&mut self, target: ValueId, sources: &[ValueId]) {
        let mut merged = HashSet::new();
        for src in sources {
            if let Some(allocs) = self.value_locations.get(src) {
                merged.extend(allocs.iter().copied());
            }
        }
        if !merged.is_empty() {
            self.value_locations.insert(target, merged);
        }
    }

    pub fn read_path(&mut self, target: ValueId, base: ValueId, segment: PathSegment) {
        let mut targets = HashSet::new();
        if let Some(base_allocs) = self.value_locations.get(&base) {
            for alloc in base_allocs {
                if let Some(edges) = self.heap_edges.get(alloc) {
                    if let Some(segment_targets) = edges.get(&segment) {
                        targets.extend(segment_targets.iter().copied());
                    }
                }
            }
        }
        if !targets.is_empty() {
            self.value_locations.insert(target, targets);
        }
    }

    pub fn update_path(&mut self, base: ValueId, segment: PathSegment, value: ValueId) {
        let base_allocs = self.value_locations.get(&base).cloned().unwrap_or_default();
        let value_allocs = self
            .value_locations
            .get(&value)
            .cloned()
            .unwrap_or_default();

        for b_alloc in base_allocs {
            for &v_alloc in &value_allocs {
                self.add_heap_edge(b_alloc, segment, v_alloc);
            }
        }
    }

    pub fn reachable_from(&self, val: ValueId) -> HashSet<AllocId> {
        let mut reachable = HashSet::new();
        let mut queue = VecDeque::new();

        if let Some(roots) = self.value_locations.get(&val) {
            for &root in roots {
                if reachable.insert(root) {
                    queue.push_back(root);
                }
            }
        }

        while let Some(current) = queue.pop_front() {
            if let Some(edges) = self.heap_edges.get(&current) {
                for targets in edges.values() {
                    for &target in targets {
                        if reachable.insert(target) {
                            queue.push_back(target);
                        }
                    }
                }
            }
        }

        reachable
    }

    pub fn find_path(&self, start: ValueId, target: AllocId) -> Option<Vec<PathSegment>> {
        let mut queue = VecDeque::new();
        let mut visited = HashSet::new();

        if let Some(roots) = self.value_locations.get(&start) {
            for &root in roots {
                if root == target {
                    return Some(vec![]);
                }
                visited.insert(root);
                queue.push_back((root, vec![]));
            }
        }

        while let Some((current, path)) = queue.pop_front() {
            if let Some(edges) = self.heap_edges.get(&current) {
                for (&segment, next_allocs) in edges {
                    for &next_alloc in next_allocs {
                        let mut next_path = path.clone();
                        next_path.push(segment);

                        if next_alloc == target {
                            return Some(next_path);
                        }

                        if visited.insert(next_alloc) {
                            queue.push_back((next_alloc, next_path));
                        }
                    }
                }
            }
        }
        None
    }

    pub fn check_aliasing(&self, args: &[ValueId]) -> Option<AliasConflict> {
        let reaches: Vec<_> = args.iter().map(|&v| self.reachable_from(v)).collect();

        for i in 0..args.len() {
            for j in (i + 1)..args.len() {
                if let Some(&shared_alloc) = reaches[i].intersection(&reaches[j]).next() {
                    let path_i =
                        self.find_path(args[i], shared_alloc).unwrap_or_default();
                    let path_j =
                        self.find_path(args[j], shared_alloc).unwrap_or_default();

                    return Some(AliasConflict {
                        arg_i: i,
                        arg_j: j,
                        path_i,
                        path_j,
                    });
                }
            }
        }

        None
    }

    pub fn format_path(arg_name: &str, path: &[PathSegment]) -> String {
        let mut s = arg_name.to_string();
        for seg in path {
            match seg {
                PathSegment::Field(name) => {
                    s.push('.');
                    s.push_str(&STRING_INTERNER.resolve(*name));
                }
                PathSegment::Index => {
                    s.push_str("[index]");
                }
            }
        }
        s
    }
}

```

`src/hir/utils/scope.rs`:

```rs
use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    rc::{Rc, Weak},
};

use crate::{
    ast::{DeclarationId, Position, Span},
    compile::interner::StringId,
    hir::builders::{BasicBlockId, LoopJumpTargets},
};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ScopeKind {
    FunctionBody,
    WhileBody {
        break_target: BasicBlockId,
        continue_target: BasicBlockId,
    },
    CodeBlock,
    File,
    GenericParams, // Not used for now
    Global,
}

#[derive(Debug)]
struct ScopeData {
    kind: ScopeKind,
    symbols: HashMap<StringId, DeclarationId>,
    parent: Option<Weak<RefCell<ScopeData>>>,
    children: Vec<Scope>,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct Scope(Rc<RefCell<ScopeData>>);

impl Scope {
    pub fn new_root(kind: ScopeKind, span: Span) -> Self {
        Self(Rc::new(RefCell::new(ScopeData {
            kind,
            symbols: HashMap::new(),
            parent: None,
            children: Vec::new(),
            span,
        })))
    }

    pub fn enter(&self, kind: ScopeKind, start_position: Position) -> Scope {
        let child = Scope(Rc::new(RefCell::new(ScopeData {
            kind,
            symbols: HashMap::new(),
            parent: Some(Rc::downgrade(&self.0)),
            span: Span {
                start: start_position,
                end: Position::default(),
                path: self.0.borrow().span.path.clone(),
            },
            children: Vec::new(),
        })));

        self.0.borrow_mut().children.push(child.clone());

        child
    }

    pub fn exit(&self, exit_position: Position) -> Option<Scope> {
        {
            let mut data = self.0.borrow_mut();
            data.span.end = exit_position;
        }
        self.parent()
    }

    pub fn parent(&self) -> Option<Scope> {
        self.0.borrow().parent.as_ref()?.upgrade().map(Scope)
    }

    pub fn lookup(&self, name: StringId) -> Option<DeclarationId> {
        if let Some(id) = self.0.borrow().symbols.get(&name) {
            return Some(*id);
        }

        self.parent()?.lookup(name)
    }

    pub fn map_name_to_decl(&self, name: StringId, id: DeclarationId) {
        let mut data = self.0.borrow_mut();
        if let Entry::Vacant(e) = data.symbols.entry(name) {
            e.insert(id);
        } else {
            panic!(
                "INTERNAL COMPILER ERROR: tried to re-map identifier to declaration, \
                 this should have been avoided at declaration site"
            );
        }
    }

    pub fn within_function_body(&self) -> bool {
        let kind = self.0.borrow().kind;

        if let ScopeKind::FunctionBody = kind {
            return true;
        }

        if matches!(kind, ScopeKind::CodeBlock | ScopeKind::WhileBody { .. }) {
            return self
                .parent()
                .is_some_and(|parent| parent.within_function_body());
        }

        false
    }

    pub fn within_loop_body(&self) -> Option<LoopJumpTargets> {
        let kind = self.0.borrow().kind;
        if let ScopeKind::WhileBody {
            break_target,
            continue_target,
        } = kind
        {
            return Some(LoopJumpTargets {
                on_break: break_target,
                on_continue: continue_target,
            });
        }

        if let ScopeKind::CodeBlock = kind {
            return self.parent().and_then(|parent| parent.within_loop_body());
        }

        None
    }

    pub fn is_file_scope(&self) -> bool {
        matches!(self.kind(), ScopeKind::File)
    }

    pub fn kind(&self) -> ScopeKind {
        self.0.borrow().kind
    }

    pub fn find_innermost_at(&self, byte_offset: usize) -> Option<Scope> {
        let data = self.0.borrow();

        if !data.span.contains(byte_offset) {
            return None;
        }

        for child in &data.children {
            if let Some(inner) = child.find_innermost_at(byte_offset) {
                return Some(inner);
            }
        }

        drop(data);
        Some(self.clone())
    }
}

```

`src/hir/utils/type_to_string.rs`:

```rs
use std::collections::{BTreeSet, HashSet};

use crate::{
    globals::STRING_INTERNER,
    hir::types::{
        checked_declaration::{CheckedParam, FnType},
        checked_type::{LiteralType, Type},
    },
    tokenize::TokenKind,
};

pub fn token_kind_to_string(kind: &TokenKind) -> String {
    match kind {
        TokenKind::Identifier(id) => STRING_INTERNER.resolve(*id).to_string(),
        TokenKind::Punctuation(punctuation_kind) => punctuation_kind.to_string(),
        TokenKind::Keyword(keyword_kind) => keyword_kind.to_string(),
        TokenKind::String(value) => value.to_owned(),
        TokenKind::Number(number_kind) => number_kind.to_string(),
        TokenKind::Doc(value) => format!("---\n{}\n---", value),
    }
}

pub fn type_to_string(ty: &Type) -> String {
    let mut visited_set = HashSet::new();
    type_to_string_recursive(ty, &mut visited_set)
}

pub fn type_to_string_recursive(ty: &Type, visited_set: &mut HashSet<Type>) -> String {
    if !visited_set.insert(ty.clone()) {
        return "...".to_string();
    }

    let result = match ty {
        Type::Void => String::from("void"),
        Type::Bool => String::from("bool"),
        Type::U8 => String::from("u8"),
        Type::U16 => String::from("u16"),
        Type::U32 => String::from("u32"),
        Type::U64 => String::from("u64"),
        Type::USize => String::from("usize"),
        Type::ISize => String::from("isize"),
        Type::I8 => String::from("i8"),
        Type::I16 => String::from("i16"),
        Type::I32 => String::from("i32"),
        Type::I64 => String::from("i64"),
        Type::F32 => String::from("f32"),
        Type::F64 => String::from("f64"),
        Type::String => String::from("string"),
        Type::Null => String::from("null"),
        Type::Unknown => String::from("unknown"),
        Type::Literal(literal) => literal_to_string(literal),
        Type::Never => String::from("never"),
        Type::Struct(s) => struct_to_string(s, visited_set),
        Type::Union(variants) => union_variants_to_string(variants, visited_set),
        Type::Fn(fn_type) => fn_signature_to_string(fn_type, visited_set),
        Type::List(item_type) => list_to_string(item_type, visited_set),
    };

    visited_set.remove(ty);

    result
}

pub fn literal_to_string(literal: &LiteralType) -> String {
    match literal {
        LiteralType::Number(ordered_number_kind) => ordered_number_kind.0.to_string(),
        LiteralType::Bool(value) => value.to_string(),
        LiteralType::String(id) => STRING_INTERNER.resolve(*id).to_string(),
    }
}

fn struct_to_string(fields: &[CheckedParam], visited_set: &mut HashSet<Type>) -> String {
    let fields_str = fields
        .iter()
        .map(|f| {
            format!(
                "{}: {}",
                STRING_INTERNER.resolve(f.identifier.name),
                type_to_string_recursive(&f.ty, visited_set)
            )
        })
        .collect::<Vec<String>>()
        .join(", ");

    format!("{{ {} }}", fields_str)
}

fn union_variants_to_string(
    variants: &BTreeSet<Type>,
    visited_set: &mut HashSet<Type>,
) -> String {
    variants
        .iter()
        .map(|tag| type_to_string_recursive(tag, visited_set))
        .collect::<Vec<String>>()
        .join(" | ")
}

pub fn list_to_string(item_type: &Type, visited_set: &mut HashSet<Type>) -> String {
    let item_type_string = type_to_string_recursive(item_type, visited_set);
    format!("{}[]", item_type_string)
}

fn fn_signature_to_string(fn_type: &FnType, visited_set: &mut HashSet<Type>) -> String {
    let params_str = fn_type
        .params
        .iter()
        .map(|p| {
            format!(
                "{}: {}",
                STRING_INTERNER.resolve(p.identifier.name),
                type_to_string_recursive(&p.ty, visited_set)
            )
        })
        .collect::<Vec<String>>()
        .join(", ");

    let return_type_str = type_to_string_recursive(&fn_type.return_type, visited_set);

    format!("fn({}): {}", params_str, return_type_str)
}

```

`src/hir/utils/union.rs`:

```rs
use std::collections::BTreeSet;

use crate::{compile::interner::StringId, hir::types::checked_type::Type};

fn get_type_at_path(mut ty: &Type, path: &[StringId]) -> Option<Type> {
    for field_name in path {
        match ty {
            Type::Struct(fields) => {
                let field = fields.iter().find(|f| f.identifier.name == *field_name)?;
                ty = &field.ty;
            }
            _ => return None,
        }
    }
    Some(ty.clone())
}

pub fn get_matching_variant_indices(
    union_variants: &BTreeSet<Type>,
    path: &[StringId],
    target_type: &Type,
) -> Vec<u16> {
    let mut matching_indices = Vec::new();

    for (index, variant) in union_variants.iter().enumerate() {
        let index = index as u16;

        if let Some(field_type) = get_type_at_path(variant, path) {
            if &field_type == target_type {
                matching_indices.push(index);
            }
        }
    }

    matching_indices
}

pub fn get_non_matching_variant_indices(
    union_variants: &BTreeSet<Type>,
    path: &[StringId],
    target_type: &Type,
) -> Vec<u16> {
    let matching = get_matching_variant_indices(union_variants, path, target_type);

    let total_variants = union_variants.len();

    let mut non_matching = Vec::with_capacity(total_variants - matching.len());

    let mut match_iter = matching.iter().peekable();

    for i in 0..(total_variants as u16) {
        if match_iter.peek() == Some(&&i) {
            match_iter.next();
        } else {
            non_matching.push(i);
        }
    }

    non_matching
}

```

`src/lib.rs`:

```rs
#![allow(clippy::result_large_err)]
#![allow(clippy::inherent_to_string)]
#![allow(clippy::redundant_pattern_matching)]

pub mod ast;
pub mod codegen;
pub mod compile;
pub mod globals;
pub mod hir;
pub mod parse;
pub mod tokenize;

```

`src/main.rs`:

```rs
use lilac::compile::Compiler;
use std::path::PathBuf;

fn main() {
    let file_path = std::env::args()
        .nth(1)
        .expect("\nExpected file path to the program entry\n");

    let mut compiler = Compiler::default();
    compiler.compile(PathBuf::from(file_path));
}

```

`src/parse/expressions/mod.rs`:

```rs
pub mod parse_codeblock_expr;
pub mod parse_fn_call_expr;
pub mod parse_fn_expr;
pub mod parse_if_expr;
pub mod parse_list_literal_expr;
pub mod parse_parenthesized_expr;
pub mod parse_struct_init_expr;

use crate::{
    ast::{
        expr::{Expr, ExprKind},
        Span,
    },
    globals::STRING_INTERNER,
    tokenize::{KeywordKind, PunctuationKind, TokenKind},
};

use super::{Parser, ParsingError, ParsingErrorKind};

fn prefix_bp(token_kind: &TokenKind) -> Option<((), u8)> {
    use PunctuationKind::*;
    use TokenKind::*;

    let priority = match token_kind {
        Punctuation(Minus) | Punctuation(Not) => ((), 13),
        _ => return None,
    };

    Some(priority)
}

fn infix_bp(token_kind: &TokenKind) -> Option<(u8, u8)> {
    use PunctuationKind::*;
    use TokenKind::*;

    let priority = match token_kind {
        Punctuation(DoubleOr) => (1, 2),
        Punctuation(DoubleAnd) => (3, 4),
        Punctuation(DoubleEq) | Punctuation(NotEq) => (5, 6),
        Punctuation(Lt) | Punctuation(Lte) | Punctuation(Gt) | Punctuation(Gte) => (7, 8),
        Punctuation(Plus) | Punctuation(Minus) => (9, 10),
        Punctuation(Star) | Punctuation(Slash) | Punctuation(Percent) => (11, 12),
        _ => return None,
    };

    Some(priority)
}

fn suffix_bp(token_kind: &TokenKind) -> Option<(u8, ())> {
    use PunctuationKind::*;
    use TokenKind::*;

    let priority = match token_kind {
        Punctuation(LParen) => (14, ()),    // fn call
        Punctuation(Dot) => (14, ()),       // member access
        Punctuation(DoubleCol) => (14, ()), // static member accesses
        _ => return None,
    };

    Some(priority)
}

pub fn is_start_of_expr(token_kind: &TokenKind) -> bool {
    match token_kind {
        TokenKind::Identifier(_)
        | TokenKind::Number(_)
        | TokenKind::String(_)
        | TokenKind::Keyword(KeywordKind::Fn)
        | TokenKind::Keyword(KeywordKind::True)
        | TokenKind::Keyword(KeywordKind::False)
        | TokenKind::Keyword(KeywordKind::If)
        | TokenKind::Punctuation(PunctuationKind::Hash)   // Tag expr
        | TokenKind::Punctuation(PunctuationKind::LParen)   // Parenthesized expr
        | TokenKind::Punctuation(PunctuationKind::LBrace)   // Codeblock or Struct expr
        | TokenKind::Punctuation(PunctuationKind::LBracket) // List literal
        | TokenKind::Punctuation(PunctuationKind::Minus)    // Negation
        | TokenKind::Punctuation(PunctuationKind::Not)      // Logical NOT
          => true,
        _ => false,
    }
}

impl Parser {
    pub fn parse_expr(&mut self, min_prec: u8) -> Result<Expr, ParsingError> {
        let token = self.current().ok_or(self.unexpected_end_of_input())?;

        let token_span = token.span.clone();

        let mut lhs = match token.kind {
            TokenKind::Identifier(_) => {
                let identifier = self.consume_identifier()?;
                Expr {
                    kind: ExprKind::Identifier(identifier),
                    span: token_span,
                }
            }
            TokenKind::Number(_) => {
                let number = self.consume_number()?;
                Expr {
                    kind: ExprKind::Number(number),
                    span: token_span,
                }
            }
            TokenKind::Keyword(KeywordKind::Fn) => self.parse_fn_expr()?,
            TokenKind::Punctuation(PunctuationKind::LParen) => {
                let start_offset = self.offset;
                let result = self.parse_parenthesized_expr()?;
                let span = self.get_span(start_offset, self.offset - 1)?;

                Expr {
                    kind: result.kind,
                    span,
                }
            }
            TokenKind::Punctuation(PunctuationKind::LBrace) => {
                self.place_checkpoint();

                self.parse_struct_init_expr()
                    .or_else(|struct_parsing_error| {
                        let struct_parsing_error_offset = self.offset;
                        self.goto_checkpoint();
                        self.parse_codeblock_expr()
                            .map(|codeblock| Expr {
                                span: codeblock.span.clone(),
                                kind: ExprKind::CodeBlock(codeblock),
                            })
                            .map_err(|codeblock_parsing_error| {
                                let codeblock_parsing_error_offset = self.offset;
                                if codeblock_parsing_error_offset
                                    > struct_parsing_error_offset
                                {
                                    codeblock_parsing_error
                                } else {
                                    struct_parsing_error
                                }
                            })
                    })?
            }
            TokenKind::Punctuation(PunctuationKind::LBracket) => {
                self.parse_list_literal_expr()?
            }
            TokenKind::Punctuation(PunctuationKind::Minus) => {
                let ((), r_bp) =
                    prefix_bp(&TokenKind::Punctuation(PunctuationKind::Minus)).expect(
                        "INTERNAL COMPILER ERROR: expected the minus \'-\' symbol to \
                         have a corresponding prefix binding power",
                    );
                let start_offset = self.offset;

                self.consume_punctuation(PunctuationKind::Minus)?;
                let expr = self.parse_expr(r_bp)?;
                Expr {
                    kind: ExprKind::Neg {
                        right: Box::new(expr),
                    },
                    span: self.get_span(start_offset, self.offset - 1)?,
                }
            }
            TokenKind::Punctuation(PunctuationKind::Not) => {
                let ((), r_bp) = prefix_bp(&TokenKind::Punctuation(PunctuationKind::Not))
                    .expect(
                        "INTERNAL COMPILER ERROR: expected the not \'!\' symbol to have \
                         a corresponding prefix binding power",
                    );
                let start_offset = self.offset;

                self.consume_punctuation(PunctuationKind::Not)?;
                let expr = self.parse_expr(r_bp)?;
                Expr {
                    kind: ExprKind::Not {
                        right: Box::new(expr),
                    },
                    span: self.get_span(start_offset, self.offset - 1)?,
                }
            }
            TokenKind::Keyword(KeywordKind::If) => self.parse_if_expr()?,
            TokenKind::Keyword(
                variant @ KeywordKind::True | variant @ KeywordKind::False,
            ) => {
                let start_offset = self.offset;
                self.consume_keyword(variant)?;
                let is_true = matches!(variant, KeywordKind::True);
                Expr {
                    kind: ExprKind::BoolLiteral(is_true),
                    span: self.get_span(start_offset, self.offset - 1)?,
                }
            }
            TokenKind::String(_) => {
                let value = self.consume_string()?;
                Expr {
                    span: value.span.clone(),
                    kind: ExprKind::String(value),
                }
            }
            _ => {
                return Err(ParsingError {
                    kind: ParsingErrorKind::ExpectedAnExpressionButFound(token.clone()),
                    span: token_span,
                })
            }
        };

        while let Some(op) = self.current().cloned() {
            if let Some((left_prec, ())) = suffix_bp(&op.kind) {
                if left_prec < min_prec {
                    break;
                }
                let lhs_clone = lhs.clone();

                let new_lhs = match op.kind {
                    TokenKind::Punctuation(PunctuationKind::Dot) => {
                        self.consume_punctuation(PunctuationKind::Dot)?;

                        let start_pos = lhs_clone.span.start;
                        let field = self.consume_identifier()?;
                        let end_pos = field.span.end;
                        Some(Expr {
                            kind: ExprKind::Access {
                                left: Box::new(lhs_clone),
                                field,
                            },
                            span: Span {
                                start: start_pos,
                                end: end_pos,
                                path: self.path.clone(),
                            },
                        })
                    }
                    TokenKind::Punctuation(PunctuationKind::DoubleCol) => {
                        let start_offset = self.offset;
                        self.consume_punctuation(PunctuationKind::DoubleCol)?;
                        let field = self.consume_identifier()?;
                        let field_name = STRING_INTERNER.resolve(field.name);

                        let new_lhs = if field_name == "is" {
                            // lhs::is(i32)
                            self.consume_punctuation(PunctuationKind::LParen)?;
                            let type_ann = self.parse_type_annotation(0)?;
                            self.consume_punctuation(PunctuationKind::RParen)?;

                            Expr {
                                kind: ExprKind::IsType {
                                    left: Box::new(lhs.clone()),
                                    ty: type_ann,
                                },
                                span: self.get_span(start_offset, self.offset - 1)?,
                            }
                        } else if field_name == "as" {
                            // lhs::as(Type)
                            self.consume_punctuation(PunctuationKind::LParen)?;
                            let target_type = self.parse_type_annotation(0)?;
                            self.consume_punctuation(PunctuationKind::RParen)?;

                            Expr {
                                kind: ExprKind::TypeCast {
                                    left: Box::new(lhs.clone()),
                                    target: target_type,
                                },
                                span: self.get_span(start_offset, self.offset - 1)?,
                            }
                        } else {
                            Expr {
                                kind: ExprKind::StaticAccess {
                                    left: Box::new(lhs.clone()),
                                    field,
                                },
                                span: self.get_span(start_offset, self.offset - 1)?,
                            }
                        };

                        Some(new_lhs)
                    }
                    TokenKind::Punctuation(PunctuationKind::LParen) => {
                        Some(self.parse_fn_call_expr(lhs.clone())?)
                    }
                    _ => {
                        return Err(ParsingError {
                            span: op.span.clone(),
                            kind: ParsingErrorKind::InvalidSuffixOperator(op.clone()),
                        })
                    }
                };

                if let Some(expr) = new_lhs {
                    lhs = expr;
                    continue;
                }
            }

            if let Some((left_prec, right_prec)) = infix_bp(&op.kind) {
                if left_prec < min_prec {
                    break;
                }

                let start_pos = lhs.span.start;

                self.advance();

                let rhs = self.parse_expr(right_prec)?;

                let end_pos = rhs.span.end;

                let expr_kind = match op.kind {
                    TokenKind::Punctuation(PunctuationKind::Plus) => ExprKind::Add {
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                    },
                    TokenKind::Punctuation(PunctuationKind::Minus) => {
                        ExprKind::Subtract {
                            left: Box::new(lhs),
                            right: Box::new(rhs),
                        }
                    }
                    TokenKind::Punctuation(PunctuationKind::Star) => ExprKind::Multiply {
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                    },
                    TokenKind::Punctuation(PunctuationKind::Slash) => ExprKind::Divide {
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                    },
                    TokenKind::Punctuation(PunctuationKind::Percent) => {
                        ExprKind::Modulo {
                            left: Box::new(lhs),
                            right: Box::new(rhs),
                        }
                    }
                    TokenKind::Punctuation(PunctuationKind::Lt) => ExprKind::LessThan {
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                    },
                    TokenKind::Punctuation(PunctuationKind::Lte) => {
                        ExprKind::LessThanOrEqual {
                            left: Box::new(lhs),
                            right: Box::new(rhs),
                        }
                    }
                    TokenKind::Punctuation(PunctuationKind::Gt) => {
                        ExprKind::GreaterThan {
                            left: Box::new(lhs),
                            right: Box::new(rhs),
                        }
                    }
                    TokenKind::Punctuation(PunctuationKind::Gte) => {
                        ExprKind::GreaterThanOrEqual {
                            left: Box::new(lhs),
                            right: Box::new(rhs),
                        }
                    }
                    TokenKind::Punctuation(PunctuationKind::DoubleEq) => {
                        ExprKind::Equal {
                            left: Box::new(lhs),
                            right: Box::new(rhs),
                        }
                    }
                    TokenKind::Punctuation(PunctuationKind::NotEq) => {
                        ExprKind::NotEqual {
                            left: Box::new(lhs),
                            right: Box::new(rhs),
                        }
                    }
                    TokenKind::Punctuation(PunctuationKind::DoubleAnd) => ExprKind::And {
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                    },
                    TokenKind::Punctuation(PunctuationKind::DoubleOr) => ExprKind::Or {
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                    },
                    _ => break,
                };

                lhs = Expr {
                    kind: expr_kind,
                    span: Span {
                        start: start_pos,
                        end: end_pos,
                        path: self.path.clone(),
                    },
                };

                continue;
            }

            break;
        }

        Ok(lhs)
    }
}

```

`src/parse/expressions/parse_codeblock_expr.rs`:

```rs
use crate::{
    ast::{
        expr::{BlockContents, Expr},
        stmt::{Stmt, StmtKind},
        Span,
    },
    parse::{statements::is_start_of_stmt, Parser, ParsingError, ParsingErrorKind},
    tokenize::{PunctuationKind, TokenKind},
};

use super::is_start_of_expr;

impl Parser {
    pub fn parse_codeblock_expr(&mut self) -> Result<BlockContents, ParsingError> {
        let start_offset = self.offset;
        self.consume_punctuation(PunctuationKind::LBrace)?;

        let mut statements = Vec::new();
        let mut final_expr: Option<Box<Expr>> = None;

        loop {
            if self.match_token(0, TokenKind::Punctuation(PunctuationKind::RBrace)) {
                break;
            }

            let current_token = self
                .current()
                .ok_or_else(|| self.unexpected_end_of_input())?;

            if is_start_of_stmt(&current_token.kind) {
                if let Some(old_expr) = final_expr.take() {
                    statements.push(Stmt {
                        span: old_expr.span.clone(),
                        kind: StmtKind::Expression(*old_expr),
                    })
                }

                statements.push(self.parse_stmt()?);
            } else if is_start_of_expr(&current_token.kind) {
                if let Some(old_expr) = final_expr.take() {
                    statements.push(Stmt {
                        span: old_expr.span.clone(),
                        kind: StmtKind::Expression(*old_expr),
                    })
                }

                let expr = self.parse_expr(0)?;

                if self.match_token(0, TokenKind::Punctuation(PunctuationKind::Eq))
                    && !self.match_token(1, TokenKind::Punctuation(PunctuationKind::Eq))
                {
                    let stmt = self.parse_assignment_stmt(expr)?;
                    statements.push(stmt);
                } else if self
                    .match_token(0, TokenKind::Punctuation(PunctuationKind::SemiCol))
                {
                    let semi_token = self.current().unwrap();
                    let stmt = Stmt {
                        span: Span {
                            start: expr.span.start,
                            end: semi_token.span.end,
                            path: self.path.clone(),
                        },
                        kind: StmtKind::Expression(expr),
                    };
                    statements.push(stmt);
                    self.advance();
                } else {
                    final_expr = Some(Box::new(expr));
                }
            } else {
                return Err(ParsingError {
                    kind: ParsingErrorKind::ExpectedStatementOrExpression {
                        found: current_token.clone(),
                    },
                    span: current_token.span.clone(),
                });
            }
        }

        self.consume_punctuation(PunctuationKind::RBrace)?;

        let span = self.get_span(start_offset, self.offset - 1)?;

        Ok(BlockContents {
            statements,
            final_expr,
            span,
        })
    }
}

```

`src/parse/expressions/parse_fn_call_expr.rs`:

```rs
use crate::{
    ast::expr::{Expr, ExprKind},
    parse::{Parser, ParsingError},
    tokenize::{PunctuationKind, TokenKind},
};

impl Parser {
    pub fn parse_fn_call_args(&mut self) -> Result<Vec<Expr>, ParsingError> {
        self.consume_punctuation(PunctuationKind::LParen)?;
        let args = self.comma_separated(
            |p| p.parse_expr(0),
            |p| p.match_token(0, TokenKind::Punctuation(PunctuationKind::RParen)),
        )?;
        self.consume_punctuation(PunctuationKind::RParen)?;
        Ok(args)
    }

    pub fn parse_fn_call_expr(&mut self, left: Expr) -> Result<Expr, ParsingError> {
        let start_offset = self.offset;

        let args = self.parse_fn_call_args()?;
        let mut span = left.span.clone();
        let end = self.get_span(start_offset, self.offset - 1)?;
        span.end = end.end;

        Ok(Expr {
            kind: ExprKind::FnCall {
                left: Box::new(left),
                args,
            },
            span,
        })
    }
}

```

`src/parse/expressions/parse_fn_expr.rs`:

```rs
use crate::{
    ast::{
        decl::{FnDecl, Param},
        expr::{Expr, ExprKind},
        type_annotation::{TypeAnnotation, TypeAnnotationKind},
    },
    globals::next_declaration_id,
    parse::{Parser, ParsingError},
    tokenize::{KeywordKind, PunctuationKind, TokenKind},
};

impl Parser {
    pub fn parse_fn_expr(&mut self) -> Result<Expr, ParsingError> {
        let documentation = self.consume_optional_doc();

        let start_offset = self.offset;

        let is_exported = if self.match_token(0, TokenKind::Keyword(KeywordKind::Export))
        {
            self.consume_keyword(KeywordKind::Export)?;
            true
        } else {
            false
        };

        self.consume_keyword(KeywordKind::Fn)?;
        let identifier = self.consume_identifier()?;
        self.consume_punctuation(PunctuationKind::LParen)?;
        let params = self.comma_separated(
            |p| {
                let identifier = p.consume_identifier()?;
                p.consume_punctuation(PunctuationKind::Col)?;
                let constraint = p.parse_type_annotation(0)?;

                Ok(Param {
                    constraint,
                    identifier,
                })
            },
            |p| p.match_token(0, TokenKind::Punctuation(PunctuationKind::RParen)),
        )?;
        self.consume_punctuation(PunctuationKind::RParen)?;

        let return_type =
            if self.match_token(0, TokenKind::Punctuation(PunctuationKind::Col)) {
                self.consume_punctuation(PunctuationKind::Col)?;
                self.parse_type_annotation(0)?
            } else {
                TypeAnnotation {
                    kind: TypeAnnotationKind::Void,
                    span: self.get_span(start_offset, self.offset - 1)?,
                }
            };

        let body = self.parse_codeblock_expr()?;

        let id = next_declaration_id();

        Ok(Expr {
            kind: ExprKind::Fn(Box::new(FnDecl {
                id,
                identifier,
                params,
                return_type,
                body,
                documentation,
                is_exported,
            })),
            span: self.get_span(start_offset, self.offset - 1)?,
        })
    }
}

```

`src/parse/expressions/parse_if_expr.rs`:

```rs
use crate::{
    ast::expr::{BlockContents, Expr, ExprKind},
    parse::{Parser, ParsingError},
    tokenize::{KeywordKind, TokenKind},
};

impl Parser {
    pub fn parse_if_expr(&mut self) -> Result<Expr, ParsingError> {
        let start_offset = self.offset;
        let mut branches: Vec<(Box<Expr>, BlockContents)> = Vec::new();

        self.consume_keyword(KeywordKind::If)?;
        let condition = self.parse_expr(0)?;
        let then_branch = self.parse_codeblock_expr()?;
        branches.push((Box::new(condition), then_branch));

        while self.match_token(0, TokenKind::Keyword(KeywordKind::Else))
            && self.match_token(1, TokenKind::Keyword(KeywordKind::If))
        {
            self.advance();
            self.advance();

            let else_if_condition = self.parse_expr(0)?;
            let else_if_body = self.parse_codeblock_expr()?;
            branches.push((Box::new(else_if_condition), else_if_body));
        }

        let else_branch = if self.match_token(0, TokenKind::Keyword(KeywordKind::Else)) {
            self.advance();

            let else_body = self.parse_codeblock_expr()?;
            Some(else_body)
        } else {
            None
        };

        Ok(Expr {
            kind: ExprKind::If {
                branches,
                else_branch,
            },
            span: self.get_span(start_offset, self.offset - 1)?,
        })
    }
}

```

`src/parse/expressions/parse_list_literal_expr.rs`:

```rs
use crate::{
    ast::expr::{Expr, ExprKind},
    parse::{Parser, ParsingError},
    tokenize::{PunctuationKind, TokenKind},
};

impl Parser {
    pub fn parse_list_literal_expr(&mut self) -> Result<Expr, ParsingError> {
        let start_offset = self.offset;
        self.consume_punctuation(PunctuationKind::LBracket)?;
        let items: Vec<Expr> = self.comma_separated(
            |p| p.parse_expr(0),
            |p| p.match_token(0, TokenKind::Punctuation(PunctuationKind::RBracket)),
        )?;
        self.consume_punctuation(PunctuationKind::RBracket)?;

        let span = self.get_span(start_offset, self.offset - 1)?;

        Ok(Expr {
            kind: ExprKind::List(items),
            span,
        })
    }
}

```

`src/parse/expressions/parse_parenthesized_expr.rs`:

```rs
use crate::{
    ast::expr::Expr,
    parse::{Parser, ParsingError},
    tokenize::PunctuationKind,
};

impl Parser {
    pub fn parse_parenthesized_expr(&mut self) -> Result<Expr, ParsingError> {
        let start_offset = self.offset;

        self.consume_punctuation(PunctuationKind::LParen)?;
        let expr = self.parse_expr(0)?;
        self.consume_punctuation(PunctuationKind::RParen)?;

        let span = self.get_span(start_offset, self.offset - 1)?;

        Ok(Expr {
            kind: expr.kind,
            span,
        })
    }
}

```

`src/parse/expressions/parse_struct_init_expr.rs`:

```rs
use crate::{
    ast::expr::{Expr, ExprKind},
    parse::{Parser, ParsingError},
    tokenize::{PunctuationKind, TokenKind},
};

impl Parser {
    pub fn parse_struct_init_expr(&mut self) -> Result<Expr, ParsingError> {
        let start_offset = self.offset;
        self.consume_punctuation(PunctuationKind::LBrace)?;
        let fields = self.comma_separated(
            |p| {
                let name = p.consume_identifier()?;
                p.consume_punctuation(PunctuationKind::Col)?;
                let value = p.parse_expr(0)?;
                Ok((name, value))
            },
            |p| p.match_token(0, TokenKind::Punctuation(PunctuationKind::RBrace)),
        )?;
        self.consume_punctuation(PunctuationKind::RBrace)?;

        let span = self.get_span(start_offset, self.offset - 1)?;

        Ok(Expr {
            kind: ExprKind::Struct(fields),
            span,
        })
    }
}

```

`src/parse/mod.rs`:

```rs
macro_rules! matches_token {
    ($parser:expr, $index:expr, $pattern:pat $(if $guard:expr)?) => {
        $parser.tokens.get($parser.offset + $index).map_or(false, |token| {
            matches!(token.kind, $pattern $(if $guard)?)
        })
    };
}

mod expressions;
mod statements;
mod type_annotations;

pub struct Parser {
    pub offset: usize,
    pub tokens: Vec<Token>,
    pub checkpoint_offset: usize,
    pub path: ModulePath,
}

use unicode_segmentation::UnicodeSegmentation;

use crate::{
    ast::{
        stmt::Stmt, type_annotation::TypeAnnotation, IdentifierNode, ModulePath,
        Position, Span, StringNode,
    },
    tokenize::{KeywordKind, NumberKind, PunctuationKind, Token, TokenKind},
};

#[derive(Debug, Clone, PartialEq)]
pub enum ParsingErrorKind {
    ExpectedATagTypeButFound(TypeAnnotation),
    DocMustBeFollowedByDeclaration,
    ExpectedAnExpressionButFound(Token),
    ExpectedATypeButFound(Token),
    InvalidSuffixOperator(Token),
    UnexpectedEndOfInput,
    ExpectedAnIdentifier,
    ExpectedAPunctuationMark(PunctuationKind),
    ExpectedAKeyword(KeywordKind),
    ExpectedAStringValue,
    ExpectedANumericValue,
    UnknownStaticMethod(IdentifierNode),
    UnexpectedStatementAfterFinalExpression,
    ExpectedStatementOrExpression { found: Token },
    UnexpectedTokenAfterFinalExpression { found: Token },
    ExpectedToBeFollowedByOneOfTheTokens(Vec<Token>),
}

impl ParsingErrorKind {
    pub fn code(&self) -> usize {
        match self {
            ParsingErrorKind::DocMustBeFollowedByDeclaration => 1,
            ParsingErrorKind::ExpectedAnExpressionButFound(..) => 2,
            ParsingErrorKind::ExpectedATypeButFound(..) => 3,
            ParsingErrorKind::InvalidSuffixOperator(..) => 4,
            ParsingErrorKind::UnexpectedEndOfInput => 15,
            ParsingErrorKind::ExpectedAnIdentifier => 16,
            ParsingErrorKind::ExpectedAPunctuationMark(..) => 17,
            ParsingErrorKind::ExpectedAKeyword(..) => 18,
            ParsingErrorKind::ExpectedAStringValue => 19,
            ParsingErrorKind::ExpectedANumericValue => 20,
            ParsingErrorKind::UnknownStaticMethod(..) => 21,
            ParsingErrorKind::UnexpectedStatementAfterFinalExpression => 22,
            ParsingErrorKind::ExpectedStatementOrExpression { .. } => 23,
            ParsingErrorKind::UnexpectedTokenAfterFinalExpression { .. } => 24,
            ParsingErrorKind::ExpectedATagTypeButFound(..) => 25,
            ParsingErrorKind::ExpectedToBeFollowedByOneOfTheTokens(..) => 26,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsingError {
    pub kind: ParsingErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DocAnnotation {
    message: String,
    span: Span,
}

impl Parser {
    fn match_token(&self, index: usize, kind: TokenKind) -> bool {
        if let Some(token) = self.tokens.get(self.offset + index) {
            return token.kind == kind;
        }

        false
    }

    fn advance(&mut self) {
        self.offset += 1;
    }

    fn current(&self) -> Option<&Token> {
        self.tokens.get(self.offset)
    }

    fn unexpected_end_of_input(&self) -> ParsingError {
        // TODO: fix this
        let first_token_span = Span {
            start: Position {
                line: 1,
                col: 1,
                byte_offset: 0,
            },
            end: Position {
                line: 1,
                col: 1,
                byte_offset: 0,
            },
            path: self.path.clone(),
        };

        let last_token_span = self
            .tokens
            .last()
            .map(|t| &t.span)
            .unwrap_or(&first_token_span)
            .clone();

        ParsingError {
            kind: ParsingErrorKind::UnexpectedEndOfInput,
            span: last_token_span,
        }
    }

    fn get_span(
        &self,
        start_offset: usize,
        end_offset: usize,
    ) -> Result<Span, ParsingError> {
        let start = self
            .tokens
            .get(start_offset)
            .ok_or(self.unexpected_end_of_input())?;

        let end = self
            .tokens
            .get(end_offset)
            .ok_or(self.unexpected_end_of_input())?;

        Ok(Span {
            start: start.span.start,
            end: end.span.end,
            path: self.path.clone(),
        })
    }

    fn place_checkpoint(&mut self) {
        self.checkpoint_offset = self.offset;
    }

    fn goto_checkpoint(&mut self) {
        self.offset = self.checkpoint_offset;
    }

    pub fn consume_string(&mut self) -> Result<StringNode, ParsingError> {
        if let Some(t) = self.current() {
            let span = t.span.clone();
            match &t.kind {
                TokenKind::String(value) => {
                    let len = value.graphemes(true).count();
                    let owned_value = value.to_string();
                    self.advance();

                    Ok(StringNode {
                        span,
                        len,
                        value: owned_value,
                    })
                }
                _ => Err(ParsingError {
                    kind: ParsingErrorKind::ExpectedAStringValue,
                    span,
                }),
            }
        } else {
            Err(self.unexpected_end_of_input())
        }
    }

    pub fn consume_punctuation(
        &mut self,
        expected: PunctuationKind,
    ) -> Result<(), ParsingError> {
        if let Some(token) = self.current() {
            match &token.kind {
                TokenKind::Punctuation(pk) if *pk == expected => {
                    self.advance();
                    Ok(())
                }
                _ => Err(ParsingError {
                    kind: ParsingErrorKind::ExpectedAPunctuationMark(expected),
                    span: token.span.clone(),
                }),
            }
        } else {
            Err(self.unexpected_end_of_input())
        }
    }

    pub fn consume_number(&mut self) -> Result<NumberKind, ParsingError> {
        if let Some(token) = self.current() {
            match token.kind {
                TokenKind::Number(number_kind) => {
                    self.advance();
                    return Ok(number_kind);
                }
                _ => {
                    return Err(ParsingError {
                        kind: ParsingErrorKind::ExpectedANumericValue,
                        span: token.span.clone(),
                    })
                }
            }
        }

        Err(self.unexpected_end_of_input())
    }

    pub fn consume_keyword(
        &mut self,
        expected: KeywordKind,
    ) -> Result<Span, ParsingError> {
        if let Some(token) = self.current() {
            let span = token.span.clone();
            match token.kind {
                TokenKind::Keyword(keyword_kind) if keyword_kind == expected => {
                    self.advance();
                    Ok(span)
                }
                _ => Err(ParsingError {
                    kind: ParsingErrorKind::ExpectedAKeyword(expected),
                    span,
                }),
            }
        } else {
            Err(self.unexpected_end_of_input())
        }
    }

    pub fn consume_identifier(&mut self) -> Result<IdentifierNode, ParsingError> {
        if let Some(token) = self.current() {
            match token.kind {
                TokenKind::Identifier(name) => {
                    let span = token.span.clone();
                    self.advance();
                    Ok(IdentifierNode { name, span })
                }
                _ => Err(ParsingError {
                    kind: ParsingErrorKind::ExpectedAnIdentifier,
                    span: token.span.clone(),
                }),
            }
        } else {
            Err(self.unexpected_end_of_input())
        }
    }

    pub fn consume_optional_doc(&mut self) -> Option<DocAnnotation> {
        let result = if let Some(Token {
            kind: TokenKind::Doc(doc),
            span,
        }) = self.current()
        {
            Some(DocAnnotation {
                span: span.clone(),
                message: doc.clone(),
            })
        } else {
            None
        };

        if result.is_some() {
            self.advance();
        };

        result
    }

    pub fn comma_separated<F, T, E>(
        &mut self,
        mut parser: F,
        is_end: E,
    ) -> Result<Vec<T>, ParsingError>
    where
        F: FnMut(&mut Self) -> Result<T, ParsingError>,
        E: Fn(&Self) -> bool,
    {
        let mut items = Vec::new();

        if is_end(self) {
            return Ok(items);
        }

        let first_item = parser(self)?;
        items.push(first_item);

        loop {
            if is_end(self) {
                break;
            }

            self.consume_punctuation(PunctuationKind::Comma)?;

            if is_end(self) {
                break;
            }

            let item = parser(self)?;
            items.push(item);
        }

        Ok(items)
    }

    pub fn parse(tokens: Vec<Token>, path: ModulePath) -> (Vec<Stmt>, Vec<ParsingError>) {
        let mut state = Parser {
            offset: 0,
            checkpoint_offset: 0,
            tokens,
            path,
        };

        let mut statements: Vec<Stmt> = vec![];
        let mut errors: Vec<ParsingError> = vec![];

        while state.current().is_some() {
            let stmt = state.parse_stmt();
            match stmt {
                Ok(s) => {
                    statements.push(s);
                }
                Err(e) => {
                    errors.push(e);
                }
            }
        }

        (statements, errors)
    }
}

```

`src/parse/statements/mod.rs`:

```rs
pub mod parse_assignment_stmt;
pub mod parse_break_stmt;
pub mod parse_continue_stmt;
pub mod parse_expr_stmt;
pub mod parse_from_stmt;
pub mod parse_return_stmt;
pub mod parse_type_alias_decl;
pub mod parse_var_decl;
pub mod parse_while_stmt;

use crate::{
    ast::{
        stmt::{Stmt, StmtKind},
        Span,
    },
    parse::{Parser, ParsingErrorKind},
    tokenize::{KeywordKind, PunctuationKind, TokenKind},
};

use super::ParsingError;

pub fn is_start_of_stmt(token_kind: &TokenKind) -> bool {
    matches!(
        token_kind,
        TokenKind::Keyword(
            KeywordKind::From
                | KeywordKind::While
                | KeywordKind::Return
                | KeywordKind::Break
                | KeywordKind::Continue
                | KeywordKind::Type
                | KeywordKind::Let
                | KeywordKind::Export
        ) | TokenKind::Doc(_)
    )
}

impl Parser {
    pub fn parse_stmt(&mut self) -> Result<Stmt, ParsingError> {
        let result = self.parse_stmt_no_sync();

        if result.is_err() {
            self.synchronize_stmt();
        }

        result
    }

    pub fn parse_stmt_no_sync(&mut self) -> Result<Stmt, ParsingError> {
        let mut lookahead_index = 0;

        let has_doc = if matches_token!(self, lookahead_index, TokenKind::Doc(_)) {
            lookahead_index += 1;
            true
        } else {
            false
        };

        let has_export = if matches_token!(
            self,
            lookahead_index,
            TokenKind::Keyword(KeywordKind::Export)
        ) {
            lookahead_index += 1;
            true
        } else {
            false
        };

        if matches_token!(self, lookahead_index, TokenKind::Keyword(KeywordKind::Type)) {
            return self.parse_type_alias_decl();
        }

        if matches_token!(self, lookahead_index, TokenKind::Keyword(KeywordKind::Let)) {
            return self.parse_var_decl();
        }

        if matches_token!(self, lookahead_index, TokenKind::Keyword(KeywordKind::Fn)) {
            let expr = self.parse_fn_expr()?;
            return Ok(Stmt {
                span: expr.span.clone(),
                kind: StmtKind::Expression(expr),
            });
        }

        if has_export {
            let invalid_token = self
                .tokens
                .get(self.offset + lookahead_index)
                .unwrap_or_else(|| self.tokens.last().unwrap());

            return Err(ParsingError {
                kind: ParsingErrorKind::ExpectedStatementOrExpression {
                    found: invalid_token.clone(),
                },
                span: invalid_token.span.clone(),
            });
        }

        if has_doc {
            return Err(ParsingError {
                kind: ParsingErrorKind::DocMustBeFollowedByDeclaration,
                span: self.tokens.get(self.offset).unwrap().span.clone(),
            });
        }

        if matches_token!(self, 0, TokenKind::Keyword(KeywordKind::From)) {
            return self.parse_from_stmt();
        }
        if matches_token!(self, 0, TokenKind::Keyword(KeywordKind::While)) {
            return self.parse_while_stmt();
        }
        if matches_token!(self, 0, TokenKind::Keyword(KeywordKind::Return)) {
            return self.parse_return_stmt();
        }
        if matches_token!(self, 0, TokenKind::Keyword(KeywordKind::Break)) {
            return self.parse_break_stmt();
        }
        if matches_token!(self, 0, TokenKind::Keyword(KeywordKind::Continue)) {
            return self.parse_continue_stmt();
        }

        let lhs = self.parse_expr(0)?;

        if matches_token!(self, 0, TokenKind::Punctuation(PunctuationKind::Eq))
            && !matches_token!(self, 1, TokenKind::Punctuation(PunctuationKind::Eq))
        {
            self.parse_assignment_stmt(lhs)
        } else {
            let mut end_span = lhs.span.clone();
            if matches_token!(self, 0, TokenKind::Punctuation(PunctuationKind::SemiCol)) {
                end_span = self.current().unwrap().span.clone();
                self.advance();
            }

            Ok(Stmt {
                span: Span {
                    start: lhs.span.start,
                    end: end_span.end,
                    path: self.path.clone(),
                },
                kind: StmtKind::Expression(lhs),
            })
        }
    }

    pub fn synchronize_stmt(&mut self) {
        loop {
            match self.current() {
                Some(token) => {
                    if token.kind == TokenKind::Punctuation(PunctuationKind::SemiCol) {
                        self.advance();
                        return;
                    }

                    self.advance();
                }
                None => return,
            }
        }
    }
}

```

`src/parse/statements/parse_assignment_stmt.rs`:

```rs
use crate::{
    ast::{
        expr::Expr,
        stmt::{Stmt, StmtKind},
        Span,
    },
    parse::{Parser, ParsingError},
    tokenize::PunctuationKind,
};

impl Parser {
    pub fn parse_assignment_stmt(&mut self, lhs: Expr) -> Result<Stmt, ParsingError> {
        let start_offset = self.offset;
        self.consume_punctuation(PunctuationKind::Eq)?;
        let value = self.parse_expr(0)?;
        self.consume_punctuation(PunctuationKind::SemiCol)?;
        let span_end = self.get_span(start_offset, self.offset - 1)?;
        Ok(Stmt {
            span: Span {
                start: lhs.span.start,
                end: span_end.end,
                path: self.path.clone(),
            },
            kind: StmtKind::Assignment { target: lhs, value },
        })
    }
}

```

`src/parse/statements/parse_break_stmt.rs`:

```rs
use crate::{
    ast::stmt::{Stmt, StmtKind},
    parse::{Parser, ParsingError},
    tokenize::KeywordKind,
};

impl Parser {
    pub fn parse_break_stmt(&mut self) -> Result<Stmt, ParsingError> {
        let start_offset = self.offset;
        self.consume_keyword(KeywordKind::Break)?;
        let span = self.get_span(start_offset, self.offset - 1)?;

        Ok(Stmt {
            kind: StmtKind::Break,
            span,
        })
    }
}

```

`src/parse/statements/parse_continue_stmt.rs`:

```rs
use crate::{
    ast::stmt::{Stmt, StmtKind},
    parse::{Parser, ParsingError},
    tokenize::KeywordKind,
};

impl Parser {
    pub fn parse_continue_stmt(&mut self) -> Result<Stmt, ParsingError> {
        let start_offset = self.offset;
        self.consume_keyword(KeywordKind::Continue)?;
        let span = self.get_span(start_offset, self.offset - 1)?;

        Ok(Stmt {
            kind: StmtKind::Continue,
            span,
        })
    }
}

```

`src/parse/statements/parse_expr_stmt.rs`:

```rs
use crate::{
    ast::{
        expr::Expr,
        stmt::{Stmt, StmtKind},
    },
    parse::{Parser, ParsingError},
};

impl Parser {
    pub fn parse_expr_stmt(&mut self, lhs: Expr) -> Result<Stmt, ParsingError> {
        Ok(Stmt {
            span: lhs.span.clone(),
            kind: StmtKind::Expression(lhs),
        })
    }
}

```

`src/parse/statements/parse_from_stmt.rs`:

```rs
use crate::{
    ast::stmt::{Stmt, StmtKind},
    parse::{Parser, ParsingError},
    tokenize::{KeywordKind, PunctuationKind, TokenKind},
};

impl Parser {
    pub fn parse_from_stmt(&mut self) -> Result<Stmt, ParsingError> {
        let start_offset = self.offset;

        self.consume_keyword(KeywordKind::From)?;
        let path = self.consume_string()?;

        self.consume_punctuation(PunctuationKind::LBrace)?;
        let identifiers = self.comma_separated(
            |p| {
                let identifier = p.consume_identifier()?;
                let alias =
                    if p.match_token(0, TokenKind::Punctuation(PunctuationKind::Col)) {
                        p.advance();
                        Some(p.consume_identifier()?)
                    } else {
                        None
                    };

                Ok((identifier, alias))
            },
            |p| p.match_token(0, TokenKind::Punctuation(PunctuationKind::RBrace)),
        )?;
        self.consume_punctuation(PunctuationKind::RBrace)?;

        let span = self.get_span(start_offset, self.offset - 1)?;

        Ok(Stmt {
            kind: StmtKind::From { path, identifiers },
            span,
        })
    }
}

```

`src/parse/statements/parse_return_stmt.rs`:

```rs
use crate::{
    ast::stmt::{Stmt, StmtKind},
    parse::{Parser, ParsingError},
    tokenize::{KeywordKind, PunctuationKind},
};

impl Parser {
    pub fn parse_return_stmt(&mut self) -> Result<Stmt, ParsingError> {
        let start_offset = self.offset;

        self.consume_keyword(KeywordKind::Return)?;
        let value = self.parse_expr(0)?;
        self.consume_punctuation(PunctuationKind::SemiCol)?;

        let span = self.get_span(start_offset, self.offset - 1)?;

        Ok(Stmt {
            kind: StmtKind::Return { value },
            span,
        })
    }
}

```

`src/parse/statements/parse_type_alias_decl.rs`:

```rs
use crate::{
    ast::{
        decl::TypeAliasDecl,
        stmt::{Stmt, StmtKind},
    },
    globals::next_declaration_id,
    parse::{Parser, ParsingError},
    tokenize::{KeywordKind, PunctuationKind, TokenKind},
};

impl Parser {
    pub fn parse_type_alias_decl(&mut self) -> Result<Stmt, ParsingError> {
        let documentation = self.consume_optional_doc();

        let start_offset = self.offset;

        let is_exported = if self.match_token(0, TokenKind::Keyword(KeywordKind::Export))
        {
            self.consume_keyword(KeywordKind::Export)?;
            true
        } else {
            false
        };

        self.consume_keyword(KeywordKind::Type)?;

        let name = self.consume_identifier()?;

        self.consume_punctuation(PunctuationKind::Eq)?;

        let ty = self.parse_type_annotation(0)?;

        self.consume_punctuation(PunctuationKind::SemiCol)?;

        let span = self.get_span(start_offset, self.offset - 1)?;

        let id = next_declaration_id();

        Ok(Stmt {
            kind: StmtKind::TypeAliasDecl(TypeAliasDecl {
                id,
                identifier: name,
                documentation,
                value: ty,
                is_exported,
            }),
            span,
        })
    }
}

```

`src/parse/statements/parse_var_decl.rs`:

```rs
use crate::{
    ast::{
        decl::VarDecl,
        stmt::{Stmt, StmtKind},
    },
    globals::next_declaration_id,
    parse::{Parser, ParsingError},
    tokenize::{KeywordKind, PunctuationKind, TokenKind},
};

impl Parser {
    pub fn parse_var_decl(&mut self) -> Result<Stmt, ParsingError> {
        let documentation = self.consume_optional_doc();

        let start_offset = self.offset;

        self.consume_keyword(KeywordKind::Let)?;

        let name = self.consume_identifier()?;

        let constraint =
            if self.match_token(0, TokenKind::Punctuation(PunctuationKind::Col)) {
                self.advance();
                Some(self.parse_type_annotation(0)?)
            } else {
                None
            };

        self.consume_punctuation(PunctuationKind::Eq)?;

        let value = self.parse_expr(0)?;

        self.consume_punctuation(PunctuationKind::SemiCol)?;

        let span = self.get_span(start_offset, self.offset - 1)?;

        let id = next_declaration_id();

        Ok(Stmt {
            kind: StmtKind::VarDecl(VarDecl {
                id,
                documentation,
                identifier: name,
                constraint,
                value,
            }),
            span,
        })
    }
}

```

`src/parse/statements/parse_while_stmt.rs`:

```rs
use crate::{
    ast::stmt::{Stmt, StmtKind},
    parse::{Parser, ParsingError},
    tokenize::KeywordKind,
};

impl Parser {
    pub fn parse_while_stmt(&mut self) -> Result<Stmt, ParsingError> {
        let start_offset = self.offset;

        self.consume_keyword(KeywordKind::While)?;
        let condition = Box::new(self.parse_expr(0)?);
        let body = self.parse_codeblock_expr()?;

        let span = self.get_span(start_offset, self.offset - 1)?;

        Ok(Stmt {
            kind: StmtKind::While { condition, body },
            span,
        })
    }
}

```

`src/parse/type_annotations/mod.rs`:

```rs
pub mod parse_fn_type_annotation;
pub mod parse_parenthesized_type_annotation;
pub mod parse_struct_type_annotation;

use super::{Parser, ParsingError, ParsingErrorKind};
use crate::{
    ast::{
        type_annotation::{TypeAnnotation, TypeAnnotationKind},
        Span,
    },
    globals::STRING_INTERNER,
    hir::types::{checked_type::LiteralType, ordered_number_kind::OrderedNumberKind},
    tokenize::{KeywordKind, PunctuationKind, TokenKind},
};

fn suffix_bp(token_kind: &TokenKind) -> Option<(u8, ())> {
    use PunctuationKind::*;
    use TokenKind::*;

    let priority = match token_kind {
        Punctuation(LBracket) => (3, ()),
        _ => return None,
    };

    Some(priority)
}

fn infix_bp(token_kind: &TokenKind) -> Option<(u8, u8)> {
    use PunctuationKind::*;
    use TokenKind::*;

    let priority = match token_kind {
        Punctuation(Or) => (1, 2),
        _ => return None,
    };

    Some(priority)
}

impl Parser {
    pub fn parse_type_annotation(
        &mut self,
        min_prec: u8,
    ) -> Result<TypeAnnotation, ParsingError> {
        let token = self.current().ok_or(self.unexpected_end_of_input())?;

        let mut lhs = match token.kind {
            TokenKind::Keyword(KeywordKind::Void) => {
                let start_offset = self.offset;

                self.consume_keyword(KeywordKind::Void)?;
                let span = self.get_span(start_offset, self.offset - 1)?;
                TypeAnnotation {
                    kind: TypeAnnotationKind::Void,
                    span,
                }
            }
            TokenKind::Keyword(KeywordKind::Bool) => {
                let start_offset = self.offset;

                self.consume_keyword(KeywordKind::Bool)?;
                let span = self.get_span(start_offset, self.offset - 1)?;
                TypeAnnotation {
                    kind: TypeAnnotationKind::Bool,
                    span,
                }
            }
            TokenKind::Keyword(KeywordKind::String) => {
                let start_offset = self.offset;
                self.consume_keyword(KeywordKind::String)?;
                let span = self.get_span(start_offset, self.offset - 1)?;
                TypeAnnotation {
                    kind: TypeAnnotationKind::String,
                    span,
                }
            }
            TokenKind::Keyword(KeywordKind::U8) => {
                let start_offset = self.offset;

                self.consume_keyword(KeywordKind::U8)?;
                let span = self.get_span(start_offset, self.offset - 1)?;
                TypeAnnotation {
                    kind: TypeAnnotationKind::U8,
                    span,
                }
            }
            TokenKind::Keyword(KeywordKind::U16) => {
                let start_offset = self.offset;

                self.consume_keyword(KeywordKind::U16)?;
                let span = self.get_span(start_offset, self.offset - 1)?;
                TypeAnnotation {
                    kind: TypeAnnotationKind::U16,
                    span,
                }
            }
            TokenKind::Keyword(KeywordKind::U32) => {
                let start_offset = self.offset;

                self.consume_keyword(KeywordKind::U32)?;
                let span = self.get_span(start_offset, self.offset - 1)?;
                TypeAnnotation {
                    kind: TypeAnnotationKind::U32,
                    span,
                }
            }
            TokenKind::Keyword(KeywordKind::U64) => {
                let start_offset = self.offset;

                self.consume_keyword(KeywordKind::U64)?;
                let span = self.get_span(start_offset, self.offset - 1)?;
                TypeAnnotation {
                    kind: TypeAnnotationKind::U64,
                    span,
                }
            }
            TokenKind::Keyword(KeywordKind::I8) => {
                let start_offset = self.offset;

                self.consume_keyword(KeywordKind::I8)?;
                let span = self.get_span(start_offset, self.offset - 1)?;
                TypeAnnotation {
                    kind: TypeAnnotationKind::I8,
                    span,
                }
            }
            TokenKind::Keyword(KeywordKind::I16) => {
                let start_offset = self.offset;

                self.consume_keyword(KeywordKind::I16)?;
                let span = self.get_span(start_offset, self.offset - 1)?;
                TypeAnnotation {
                    kind: TypeAnnotationKind::I16,
                    span,
                }
            }
            TokenKind::Keyword(KeywordKind::I32) => {
                let start_offset = self.offset;

                self.consume_keyword(KeywordKind::I32)?;
                let span = self.get_span(start_offset, self.offset - 1)?;
                TypeAnnotation {
                    kind: TypeAnnotationKind::I32,
                    span,
                }
            }
            TokenKind::Keyword(KeywordKind::I64) => {
                let start_offset = self.offset;

                self.consume_keyword(KeywordKind::I64)?;
                let span = self.get_span(start_offset, self.offset - 1)?;
                TypeAnnotation {
                    kind: TypeAnnotationKind::I64,
                    span,
                }
            }
            TokenKind::Keyword(KeywordKind::F32) => {
                let start_offset = self.offset;

                self.consume_keyword(KeywordKind::F32)?;
                let span = self.get_span(start_offset, self.offset - 1)?;
                TypeAnnotation {
                    kind: TypeAnnotationKind::F32,
                    span,
                }
            }
            TokenKind::Keyword(KeywordKind::F64) => {
                let start_offset = self.offset;

                self.consume_keyword(KeywordKind::F64)?;
                let span = self.get_span(start_offset, self.offset - 1)?;
                TypeAnnotation {
                    kind: TypeAnnotationKind::F64,
                    span,
                }
            }
            TokenKind::Keyword(KeywordKind::Null) => {
                let span = token.span.clone();
                self.advance();
                TypeAnnotation {
                    kind: TypeAnnotationKind::Null,
                    span,
                }
            }
            TokenKind::Number(num_kind) => {
                let span = token.span.clone();
                self.advance();
                TypeAnnotation {
                    kind: TypeAnnotationKind::Literal(LiteralType::Number(
                        OrderedNumberKind(num_kind),
                    )),
                    span,
                }
            }
            TokenKind::String(ref s) => {
                let span = token.span.clone();
                let id = STRING_INTERNER.intern(s);
                self.advance();
                TypeAnnotation {
                    kind: TypeAnnotationKind::Literal(LiteralType::String(id)),
                    span,
                }
            }
            TokenKind::Keyword(KeywordKind::True) => {
                let span = token.span.clone();
                self.advance();
                TypeAnnotation {
                    kind: TypeAnnotationKind::Literal(LiteralType::Bool(true)),
                    span,
                }
            }
            TokenKind::Keyword(KeywordKind::False) => {
                let span = token.span.clone();
                self.advance();
                TypeAnnotation {
                    kind: TypeAnnotationKind::Literal(LiteralType::Bool(false)),
                    span,
                }
            }
            TokenKind::Punctuation(PunctuationKind::LParen) => {
                self.parse_parenthesized_type_annotation()?
            }
            TokenKind::Punctuation(PunctuationKind::LBrace) => {
                self.parse_struct_type_annotation()?
            }
            TokenKind::Keyword(KeywordKind::Fn) => self.parse_fn_type_annotation()?,
            TokenKind::Identifier(_) => {
                let identifier = self.consume_identifier()?;
                TypeAnnotation {
                    span: identifier.span.clone(),
                    kind: TypeAnnotationKind::Identifier(identifier),
                }
            }
            _ => {
                return Err(ParsingError {
                    kind: ParsingErrorKind::ExpectedATypeButFound(token.clone()),
                    span: token.span.clone(),
                })
            }
        };

        while let Some(op) = self.current() {
            if let Some((left_prec, ())) = suffix_bp(&op.kind) {
                if left_prec < min_prec {
                    break;
                }

                lhs = match op.kind {
                    TokenKind::Punctuation(PunctuationKind::LBracket) => {
                        self.consume_punctuation(PunctuationKind::LBracket)?;
                        self.consume_punctuation(PunctuationKind::RBracket)?;

                        let span =
                            self.get_span(lhs.span.start.byte_offset, self.offset - 1)?;
                        TypeAnnotation {
                            kind: TypeAnnotationKind::List(Box::new(lhs.clone())),
                            span,
                        }
                    }
                    _ => {
                        panic!(
                            "INTERNAL COMPILER ERROR: Unexpected suffix type-annotation \
                             operator"
                        )
                    }
                };

                continue;
            }

            if let Some((left_prec, right_prec)) = infix_bp(&op.kind) {
                if left_prec < min_prec {
                    break;
                }

                let op_kind = op.kind.clone();
                self.advance();

                let rhs = self.parse_type_annotation(right_prec)?;

                let start_pos = lhs.span.start;
                let end_pos = rhs.span.end;
                let combined_span = Span {
                    start: start_pos,
                    end: end_pos,
                    path: self.path.clone(),
                };

                lhs = match op_kind {
                    TokenKind::Punctuation(PunctuationKind::Or) => {
                        let mut variants = match lhs.kind {
                            TypeAnnotationKind::Union(v) => v,
                            _ => vec![lhs],
                        };

                        match rhs.kind {
                            TypeAnnotationKind::Union(v) => variants.extend(v),
                            _ => variants.push(rhs),
                        }

                        TypeAnnotation {
                            kind: TypeAnnotationKind::Union(variants),
                            span: combined_span,
                        }
                    }

                    _ => unreachable!(
                        "Operator found in infix_bp but not handled in match: {:?}",
                        op_kind
                    ),
                };

                continue;
            }

            break;
        }

        Ok(lhs)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{
            type_annotation::{TypeAnnotation, TypeAnnotationKind},
            ModulePath, Span,
        },
        globals::reset_globals,
        parse::Parser,
    };

    #[test]
    fn parses_primitive_types() {
        use crate::ast::Position;
        use crate::tokenize::Tokenizer;
        use pretty_assertions::assert_eq;

        reset_globals();

        let path = ModulePath::default();

        let test_cases = vec![
            (
                "i8",
                TypeAnnotation {
                    kind: TypeAnnotationKind::I8,
                    span: Span {
                        start: Position {
                            line: 1,
                            col: 1,
                            byte_offset: 0,
                        },
                        end: Position {
                            line: 1,
                            col: 3,
                            byte_offset: 2,
                        },
                        path: path.clone(),
                    },
                },
            ),
            (
                "i16",
                TypeAnnotation {
                    kind: TypeAnnotationKind::I16,
                    span: Span {
                        start: Position {
                            line: 1,
                            col: 1,
                            byte_offset: 0,
                        },
                        end: Position {
                            line: 1,
                            col: 4,
                            byte_offset: 3,
                        },
                        path: path.clone(),
                    },
                },
            ),
            (
                "i32",
                TypeAnnotation {
                    kind: TypeAnnotationKind::I32,
                    span: Span {
                        start: Position {
                            line: 1,
                            col: 1,
                            byte_offset: 0,
                        },
                        end: Position {
                            line: 1,
                            col: 4,
                            byte_offset: 3,
                        },
                        path: path.clone(),
                    },
                },
            ),
            (
                "i64",
                TypeAnnotation {
                    kind: TypeAnnotationKind::I64,
                    span: Span {
                        start: Position {
                            line: 1,
                            col: 1,
                            byte_offset: 0,
                        },
                        end: Position {
                            line: 1,
                            col: 4,
                            byte_offset: 3,
                        },
                        path: path.clone(),
                    },
                },
            ),
            (
                "f32",
                TypeAnnotation {
                    kind: TypeAnnotationKind::F32,
                    span: Span {
                        start: Position {
                            line: 1,
                            col: 1,
                            byte_offset: 0,
                        },
                        end: Position {
                            line: 1,
                            col: 4,
                            byte_offset: 3,
                        },
                        path: path.clone(),
                    },
                },
            ),
            (
                "f64",
                TypeAnnotation {
                    kind: TypeAnnotationKind::F64,
                    span: Span {
                        start: Position {
                            line: 1,
                            col: 1,
                            byte_offset: 0,
                        },
                        end: Position {
                            line: 1,
                            col: 4,
                            byte_offset: 3,
                        },
                        path: path.clone(),
                    },
                },
            ),
            (
                "u8",
                TypeAnnotation {
                    kind: TypeAnnotationKind::U8,
                    span: Span {
                        start: Position {
                            line: 1,
                            col: 1,
                            byte_offset: 0,
                        },
                        end: Position {
                            line: 1,
                            col: 3,
                            byte_offset: 2,
                        },
                        path: path.clone(),
                    },
                },
            ),
            (
                "u16",
                TypeAnnotation {
                    kind: TypeAnnotationKind::U16,
                    span: Span {
                        start: Position {
                            line: 1,
                            col: 1,
                            byte_offset: 0,
                        },
                        end: Position {
                            line: 1,
                            col: 4,
                            byte_offset: 3,
                        },
                        path: path.clone(),
                    },
                },
            ),
            (
                "u32",
                TypeAnnotation {
                    kind: TypeAnnotationKind::U32,
                    span: Span {
                        start: Position {
                            line: 1,
                            col: 1,
                            byte_offset: 0,
                        },
                        end: Position {
                            line: 1,
                            col: 4,
                            byte_offset: 3,
                        },
                        path: path.clone(),
                    },
                },
            ),
            (
                "u64",
                TypeAnnotation {
                    kind: TypeAnnotationKind::U64,
                    span: Span {
                        start: Position {
                            line: 1,
                            col: 1,
                            byte_offset: 0,
                        },
                        end: Position {
                            line: 1,
                            col: 4,
                            byte_offset: 3,
                        },
                        path: path.clone(),
                    },
                },
            ),
            (
                "void",
                TypeAnnotation {
                    kind: TypeAnnotationKind::Void,
                    span: Span {
                        start: Position {
                            line: 1,
                            col: 1,
                            byte_offset: 0,
                        },
                        end: Position {
                            line: 1,
                            col: 5,
                            byte_offset: 4,
                        },
                        path: path.clone(),
                    },
                },
            ),
            (
                "bool",
                TypeAnnotation {
                    kind: TypeAnnotationKind::Bool,
                    span: Span {
                        start: Position {
                            line: 1,
                            col: 1,
                            byte_offset: 0,
                        },
                        end: Position {
                            line: 1,
                            col: 5,
                            byte_offset: 4,
                        },
                        path: path.clone(),
                    },
                },
            ),
            (
                "string",
                TypeAnnotation {
                    kind: TypeAnnotationKind::String,
                    span: Span {
                        start: Position {
                            line: 1,
                            col: 1,
                            byte_offset: 0,
                        },
                        end: Position {
                            line: 1,
                            col: 7,
                            byte_offset: 6,
                        },
                        path: path.clone(),
                    },
                },
            ),
        ];

        for (input, expected) in test_cases {
            let (tokens, _) = Tokenizer::tokenize(input, path.clone());
            let mut parser = Parser {
                offset: 0,
                checkpoint_offset: 0,
                tokens,
                path: path.clone(),
            };
            let result = parser.parse_type_annotation(0);

            assert_eq!(result, Ok(expected))
        }
    }
}

```

`src/parse/type_annotations/parse_fn_type_annotation.rs`:

```rs
use crate::{
    ast::{
        decl::Param,
        type_annotation::{TypeAnnotation, TypeAnnotationKind},
    },
    parse::ParsingError,
    tokenize::{KeywordKind, PunctuationKind, TokenKind},
};

use super::Parser;

impl Parser {
    pub fn parse_fn_type_annotation(&mut self) -> Result<TypeAnnotation, ParsingError> {
        let start_offset = self.offset;

        self.consume_keyword(KeywordKind::Fn)?;
        self.consume_punctuation(PunctuationKind::LParen)?;
        let params = self.comma_separated(
            |p| {
                let identifier = p.consume_identifier()?;
                p.consume_punctuation(PunctuationKind::Col)?;
                let constraint = p.parse_type_annotation(0)?;

                Ok(Param {
                    constraint,
                    identifier,
                })
            },
            |p| p.match_token(0, TokenKind::Punctuation(PunctuationKind::RParen)),
        )?;
        self.consume_punctuation(PunctuationKind::RParen)?;
        self.consume_punctuation(PunctuationKind::Col)?;
        let return_type = self.parse_type_annotation(0)?;

        let span = self.get_span(start_offset, self.offset - 1)?;

        Ok(TypeAnnotation {
            kind: TypeAnnotationKind::FnType {
                params,
                return_type: Box::new(return_type),
            },
            span,
        })
    }
}

```

`src/parse/type_annotations/parse_parenthesized_type_annotation.rs`:

```rs
use crate::{
    ast::type_annotation::TypeAnnotation, parse::ParsingError, tokenize::PunctuationKind,
};

use super::Parser;

impl Parser {
    pub fn parse_parenthesized_type_annotation(
        &mut self,
    ) -> Result<TypeAnnotation, ParsingError> {
        self.consume_punctuation(PunctuationKind::LParen)?;
        let item = self.parse_type_annotation(0)?;
        self.consume_punctuation(PunctuationKind::RParen)?;

        Ok(item)
    }
}

```

`src/parse/type_annotations/parse_struct_type_annotation.rs`:

```rs
use crate::{
    ast::{
        decl::Param,
        type_annotation::{TypeAnnotation, TypeAnnotationKind},
    },
    parse::{Parser, ParsingError},
    tokenize::{PunctuationKind, TokenKind},
};

impl Parser {
    pub fn parse_struct_type_annotation(
        &mut self,
    ) -> Result<TypeAnnotation, ParsingError> {
        let start_offset = self.offset;
        self.consume_punctuation(PunctuationKind::LBrace)?;
        let fields = self.comma_separated(
            |p| {
                let identifier = p.consume_identifier()?;
                p.consume_punctuation(PunctuationKind::Col)?;
                let constraint = p.parse_type_annotation(0)?;

                Ok(Param {
                    identifier,
                    constraint,
                })
            },
            |p| p.match_token(0, TokenKind::Punctuation(PunctuationKind::RBrace)),
        )?;
        self.consume_punctuation(PunctuationKind::RBrace)?;

        let span = self.get_span(start_offset, self.offset - 1)?;

        Ok(TypeAnnotation {
            kind: TypeAnnotationKind::Struct(fields),
            span,
        })
    }
}

```

`src/tokenize/mod.rs`:

```rs
use unicode_segmentation::UnicodeSegmentation;

pub mod tokenize_documentation;
pub mod tokenize_identifier;
pub mod tokenize_number;
pub mod tokenize_punctuation;
pub mod tokenize_string;

use crate::{
    ast::{ModulePath, Position, Span},
    compile::interner::StringId,
    globals::STRING_INTERNER,
};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenizationErrorKind {
    UnknownToken(String),
    UnknownEscapeSequence,
    InvalidFloatingNumber,
    InvalidIntegerNumber,
    UnterminatedString,
    UnterminatedDoc,
}

impl TokenizationErrorKind {
    pub fn code(&self) -> usize {
        match self {
            TokenizationErrorKind::UnknownToken { .. } => 1,
            TokenizationErrorKind::UnknownEscapeSequence => 2,
            TokenizationErrorKind::InvalidFloatingNumber => 3,
            TokenizationErrorKind::InvalidIntegerNumber => 4,
            TokenizationErrorKind::UnterminatedString => 5,
            TokenizationErrorKind::UnterminatedDoc => 6,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenizationError {
    pub kind: TokenizationErrorKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum PunctuationKind {
    DoubleCol,
    DoubleOr,
    DoubleAnd,
    DoubleEq,
    Col,
    SemiCol,
    Lt,
    Gt,
    Lte,
    Gte,
    Or,
    And,
    Not,
    Dot,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Eq,
    NotEq,
    Plus,
    Minus,
    Slash,
    Star,
    Percent,
    Comma,
    Dollar,
    Question,
    Hash,
}

impl PunctuationKind {
    pub fn to_string(&self) -> String {
        String::from(match self {
            PunctuationKind::DoubleCol => "::",
            PunctuationKind::DoubleOr => "||",
            PunctuationKind::DoubleAnd => "&&",
            PunctuationKind::DoubleEq => "==",
            PunctuationKind::Col => ":",
            PunctuationKind::SemiCol => ";",
            PunctuationKind::Lt => "<",
            PunctuationKind::Gt => ">",
            PunctuationKind::Lte => "<=",
            PunctuationKind::Gte => ">=",
            PunctuationKind::Or => "|",
            PunctuationKind::And => "&",
            PunctuationKind::Not => "!",
            PunctuationKind::Dot => ".",
            PunctuationKind::LParen => "(",
            PunctuationKind::RParen => ")",
            PunctuationKind::LBracket => "[",
            PunctuationKind::RBracket => "]",
            PunctuationKind::LBrace => "{",
            PunctuationKind::RBrace => "}",
            PunctuationKind::Eq => "=",
            PunctuationKind::NotEq => "!=",
            PunctuationKind::Plus => "+",
            PunctuationKind::Minus => "-",
            PunctuationKind::Slash => "/",
            PunctuationKind::Star => "*",
            PunctuationKind::Percent => "%",
            PunctuationKind::Comma => ",",
            PunctuationKind::Dollar => "$",
            PunctuationKind::Question => "?",
            PunctuationKind::Hash => "#",
        })
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum KeywordKind {
    Let,
    Return,
    If,
    Else,
    While,
    Break,
    Continue,
    Type,
    From,
    Void,
    True,
    False,
    Export,
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Fn,
    String,
    Null,
}

impl KeywordKind {
    pub fn to_string(&self) -> String {
        String::from(match self {
            KeywordKind::Let => "let",
            KeywordKind::Return => "return",
            KeywordKind::If => "if",
            KeywordKind::Else => "else",
            KeywordKind::While => "while",
            KeywordKind::Break => "break",
            KeywordKind::Continue => "continue",
            KeywordKind::Type => "type",
            KeywordKind::From => "from",
            KeywordKind::Void => "void",
            KeywordKind::True => "true",
            KeywordKind::False => "false",
            KeywordKind::Export => "export",
            KeywordKind::Bool => "bool",
            KeywordKind::I8 => "i8",
            KeywordKind::I16 => "i16",
            KeywordKind::I32 => "i32",
            KeywordKind::I64 => "i64",
            KeywordKind::U8 => "u8",
            KeywordKind::U16 => "u16",
            KeywordKind::U32 => "u32",
            KeywordKind::U64 => "u64",
            KeywordKind::F32 => "f32",
            KeywordKind::F64 => "f64",
            KeywordKind::Fn => "fn",
            KeywordKind::String => "string",
            KeywordKind::Null => "null",
        })
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum NumberKind {
    I64(i64),
    I32(i32),
    I16(i16),
    I8(i8),
    F32(f32),
    F64(f64),
    U64(u64),
    U32(u32),
    U16(u16),
    U8(u8),
    ISize(isize),
    USize(usize),
}

pub fn number_kind_to_suffix(kind: &NumberKind) -> String {
    match kind {
        NumberKind::I64(_) => "i64".to_owned(),
        NumberKind::I32(_) => "i32".to_owned(),
        NumberKind::I16(_) => "i16".to_owned(),
        NumberKind::I8(_) => "i8".to_owned(),
        NumberKind::F32(_) => "f32".to_owned(),
        NumberKind::F64(_) => "f64".to_owned(),
        NumberKind::U64(_) => "u64".to_owned(),
        NumberKind::U32(_) => "u32".to_owned(),
        NumberKind::U16(_) => "u16".to_owned(),
        NumberKind::U8(_) => "u8".to_owned(),
        NumberKind::ISize(_) => "isize".to_owned(),
        NumberKind::USize(_) => "usize".to_owned(),
    }
}

impl NumberKind {
    pub fn to_string(&self) -> String {
        match self {
            NumberKind::I64(v) => format!("{}i64", v),
            NumberKind::I32(v) => format!("{}i32", v),
            NumberKind::I16(v) => format!("{}i16", v),
            NumberKind::I8(v) => format!("{}i8", v),
            NumberKind::F32(v) => format!("{}f32", v),
            NumberKind::F64(v) => format!("{}f64", v),
            NumberKind::U64(v) => format!("{}u64", v),
            NumberKind::U32(v) => format!("{}u32", v),
            NumberKind::U16(v) => format!("{}u16", v),
            NumberKind::U8(v) => format!("{}u8", v),
            NumberKind::ISize(v) => format!("{}isize", v),
            NumberKind::USize(v) => format!("{}usize", v),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Identifier(StringId),
    Punctuation(PunctuationKind),
    Keyword(KeywordKind),
    String(String),
    Number(NumberKind),
    Doc(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

#[derive(Debug)]
pub struct Tokenizer<'a> {
    input: &'a str,
    byte_offset: usize,
    grapheme_offset: usize,
    line: usize,
    col: usize,
    path: ModulePath,
}

impl<'a> Tokenizer<'a> {
    fn current(&self) -> Option<&'a str> {
        self.input.graphemes(true).nth(self.grapheme_offset)
    }

    fn consume(&mut self) {
        if let Some(c) = self.current() {
            if c == "\n" {
                self.byte_offset += c.len();
                self.line += 1;
                self.col = 1;
            } else {
                self.byte_offset += c.len();
                self.col += 1;
            }
            self.grapheme_offset += 1;
        }
    }

    fn peek(&self, i: usize) -> Option<&'a str> {
        self.input.graphemes(true).nth(self.grapheme_offset + i)
    }

    fn slice(&self, start: usize, end: usize) -> &'a str {
        let grapheme_indices: Vec<(usize, &str)> =
            self.input.grapheme_indices(true).collect();

        let start_idx = grapheme_indices[start].0;
        let end_idx = if end < grapheme_indices.len() {
            grapheme_indices[end].0
        } else {
            self.input.len()
        };

        &self.input[start_idx..end_idx]
    }

    fn synchronize(&mut self) {
        while let Some(ch) = self.current() {
            let is_whitespace = ch.chars().all(|c| c.is_whitespace());

            if is_whitespace || ch == ";" {
                self.consume();
                break;
            } else {
                self.consume();
            }
        }
    }

    fn skip_ignored(&mut self) {
        loop {
            let start_offset = self.grapheme_offset;
            self.skip_whitespace();
            self.skip_comment();

            if self.grapheme_offset == start_offset {
                break;
            }
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current() {
            let is_whitespace = ch.chars().all(|c| c.is_whitespace());

            if is_whitespace {
                self.consume();
            } else {
                break;
            }
        }
    }

    fn skip_comment(&mut self) {
        if self.peek(0) == Some("/") && self.peek(1) == Some("/") {
            while let Some(c) = self.current() {
                if c == "\n" || c == "\r" {
                    break;
                }
                self.consume();
            }
        }
    }

    pub fn tokenize(
        input: &'a str,
        path: ModulePath,
    ) -> (Vec<Token>, Vec<TokenizationError>) {
        let mut state = Tokenizer {
            input,
            byte_offset: 0,
            grapheme_offset: 0,
            line: 1,
            col: 1,
            path: path.clone(),
        };
        let mut tokens: Vec<Token> = vec![];
        let mut errors: Vec<TokenizationError> = vec![];

        loop {
            state.skip_ignored();

            let start_pos = Position {
                line: state.line,
                col: state.col,
                byte_offset: state.byte_offset,
            };

            match state.current() {
                Some(letter) if is_letter(letter) => {
                    let identifier = state.tokenize_identifier();
                    let keyword = is_keyword(identifier);
                    let kind = if let Some(keyword_kind) = keyword {
                        TokenKind::Keyword(keyword_kind)
                    } else {
                        let id = STRING_INTERNER.intern(identifier);
                        TokenKind::Identifier(id)
                    };
                    let end_pos = Position {
                        line: state.line,
                        col: state.col,
                        byte_offset: state.byte_offset,
                    };

                    tokens.push(Token {
                        span: Span {
                            start: start_pos,
                            end: end_pos,
                            path: state.path.clone(),
                        },
                        kind,
                    });
                }
                Some("\"") => match state.string() {
                    Ok(value) => {
                        let end_pos = Position {
                            line: state.line,
                            col: state.col,
                            byte_offset: state.byte_offset,
                        };
                        tokens.push(Token {
                            span: Span {
                                start: start_pos,
                                end: end_pos,
                                path: state.path.clone(),
                            },
                            kind: TokenKind::String(value.to_string()),
                        })
                    }
                    Err(kind) => {
                        let end_pos = Position {
                            line: state.line,
                            col: state.col,
                            byte_offset: state.byte_offset,
                        };
                        errors.push(TokenizationError {
                            kind,
                            span: Span {
                                start: start_pos,
                                end: end_pos,
                                path: state.path.clone(),
                            },
                        });
                        state.synchronize();
                    }
                },
                Some(digit) if is_digit(digit) => match state.tokenize_number() {
                    Ok(number_kind) => {
                        let end_pos = Position {
                            line: state.line,
                            col: state.col,
                            byte_offset: state.byte_offset,
                        };
                        tokens.push(Token {
                            kind: TokenKind::Number(number_kind),
                            span: Span {
                                start: start_pos,
                                end: end_pos,
                                path: state.path.clone(),
                            },
                        })
                    }
                    Err(kind) => {
                        let end_pos = Position {
                            line: state.line,
                            col: state.col,
                            byte_offset: state.byte_offset,
                        };
                        errors.push(TokenizationError {
                            kind,
                            span: Span {
                                start: start_pos,
                                end: end_pos,
                                path: state.path.clone(),
                            },
                        });
                        state.synchronize();
                    }
                },
                Some("-") if state.peek(1) == Some("-") && state.peek(2) == Some("-") => {
                    match state.tokenize_documentation() {
                        Ok(content) => {
                            let end_pos = Position {
                                line: state.line,
                                col: state.col,
                                byte_offset: state.byte_offset,
                            };
                            tokens.push(Token {
                                kind: TokenKind::Doc(content.to_string()),
                                span: Span {
                                    start: start_pos,
                                    end: end_pos,
                                    path: state.path.clone(),
                                },
                            })
                        }
                        Err(kind) => {
                            let end_pos = Position {
                                line: state.line,
                                col: state.col,
                                byte_offset: state.byte_offset,
                            };
                            errors.push(TokenizationError {
                                kind,
                                span: Span {
                                    start: start_pos,
                                    end: end_pos,
                                    path: state.path.clone(),
                                },
                            });
                            state.synchronize();
                        }
                    }
                }
                Some(punct) => match state.tokenize_punctuation(punct) {
                    Some(kind) => {
                        let end_pos = Position {
                            line: state.line,
                            col: state.col,
                            byte_offset: state.byte_offset,
                        };
                        tokens.push(Token {
                            kind: TokenKind::Punctuation(kind),
                            span: Span {
                                start: start_pos,
                                end: end_pos,
                                path: state.path.clone(),
                            },
                        })
                    }
                    None => {
                        let end_pos = Position {
                            line: state.line,
                            col: state.col,
                            byte_offset: state.byte_offset,
                        };
                        errors.push(TokenizationError {
                            kind: TokenizationErrorKind::UnknownToken(punct.to_string()),
                            span: Span {
                                start: start_pos,
                                end: end_pos,
                                path: state.path.clone(),
                            },
                        });
                        state.synchronize();
                    }
                },
                None => break,
            };
        }

        (tokens, errors)
    }
}

fn is_letter(value: &str) -> bool {
    value.graphemes(true).count() == 1 && value.chars().all(char::is_alphabetic)
}

fn is_digit(value: &str) -> bool {
    value.graphemes(true).count() == 1 && value.chars().all(|x| char::is_ascii_digit(&x))
}

fn is_alphanumeric(value: &str) -> bool {
    value.graphemes(true).count() == 1 && value.chars().all(char::is_alphanumeric)
}

fn is_keyword(identifier: &str) -> Option<KeywordKind> {
    match identifier {
        "fn" => Some(KeywordKind::Fn),
        "let" => Some(KeywordKind::Let),
        "return" => Some(KeywordKind::Return),
        "if" => Some(KeywordKind::If),
        "else" => Some(KeywordKind::Else),
        "while" => Some(KeywordKind::While),
        "break" => Some(KeywordKind::Break),
        "continue" => Some(KeywordKind::Continue),
        "type" => Some(KeywordKind::Type),
        "from" => Some(KeywordKind::From),
        "void" => Some(KeywordKind::Void),
        "true" => Some(KeywordKind::True),
        "false" => Some(KeywordKind::False),
        "export" => Some(KeywordKind::Export),
        "bool" => Some(KeywordKind::Bool),
        "i8" => Some(KeywordKind::I8),
        "i16" => Some(KeywordKind::I16),
        "i32" => Some(KeywordKind::I32),
        "i64" => Some(KeywordKind::I64),
        "u8" => Some(KeywordKind::U8),
        "u16" => Some(KeywordKind::U16),
        "u32" => Some(KeywordKind::U32),
        "u64" => Some(KeywordKind::U64),
        "f32" => Some(KeywordKind::F32),
        "f64" => Some(KeywordKind::F64),
        "string" => Some(KeywordKind::String),
        "null" => Some(KeywordKind::Null),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{ModulePath, Position, Span},
        globals::{reset_globals, STRING_INTERNER},
        tokenize::{
            KeywordKind, NumberKind, PunctuationKind, Token, TokenKind, Tokenizer,
        },
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn test_skip_single_line_comment() {
        reset_globals();

        let input = "// This is a comment\nlet x = 10;";
        let path = ModulePath::default();

        let x_id = STRING_INTERNER.intern("x");

        let (tokens, _) = Tokenizer::tokenize(input, path.clone());

        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: TokenKind::Keyword(KeywordKind::Let),
                    span: Span {
                        start: Position {
                            line: 2,
                            col: 1,
                            byte_offset: 21
                        },
                        end: Position {
                            line: 2,
                            col: 4,
                            byte_offset: 24
                        },
                        path: path.clone()
                    }
                },
                Token {
                    kind: TokenKind::Identifier(x_id),
                    span: Span {
                        start: Position {
                            line: 2,
                            col: 5,
                            byte_offset: 25
                        },
                        end: Position {
                            line: 2,
                            col: 6,
                            byte_offset: 26
                        },
                        path: path.clone()
                    }
                },
                Token {
                    kind: TokenKind::Punctuation(PunctuationKind::Eq),
                    span: Span {
                        start: Position {
                            line: 2,
                            col: 7,
                            byte_offset: 27
                        },
                        end: Position {
                            line: 2,
                            col: 8,
                            byte_offset: 28
                        },
                        path: path.clone()
                    }
                },
                Token {
                    kind: TokenKind::Number(NumberKind::I64(10)),
                    span: Span {
                        start: Position {
                            line: 2,
                            col: 9,
                            byte_offset: 29
                        },
                        end: Position {
                            line: 2,
                            col: 11,
                            byte_offset: 31
                        },
                        path: path.clone()
                    }
                },
                Token {
                    kind: TokenKind::Punctuation(PunctuationKind::SemiCol),
                    span: Span {
                        start: Position {
                            line: 2,
                            col: 11,
                            byte_offset: 31
                        },
                        end: Position {
                            line: 2,
                            col: 12,
                            byte_offset: 32
                        },
                        path: path.clone()
                    }
                }
            ]
        );
    }

    #[test]
    fn test_skip_multiple_single_line_comments() {
        reset_globals();

        let input = "// Comment 1\n// Comment 2\nlet x = 10;";
        let path = ModulePath::default();

        let (tokens, _) = Tokenizer::tokenize(input, path);

        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0].kind, TokenKind::Keyword(KeywordKind::Let));
    }

    #[test]
    fn test_comment_at_end_of_input() {
        reset_globals();

        let input = "let x = 10; // Comment at the end";
        let path = ModulePath::default();

        let (tokens, _) = Tokenizer::tokenize(input, path);

        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0].kind, TokenKind::Keyword(KeywordKind::Let));
    }

    #[test]
    fn test_no_comments() {
        reset_globals();

        let input = "let x = 10;";
        let path = ModulePath::default();

        let (tokens, _) = Tokenizer::tokenize(input, path);

        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0].kind, TokenKind::Keyword(KeywordKind::Let));
    }

    #[test]
    fn test_only_comments() {
        reset_globals();

        let input = "// Only a comment";
        let path = ModulePath::default();

        let (tokens, _) = Tokenizer::tokenize(input, path);
        assert_eq!(tokens.len(), 0);
    }
}

```

`src/tokenize/tokenize_documentation.rs`:

```rs
use super::{TokenizationErrorKind, Tokenizer};

impl<'a> Tokenizer<'a> {
    pub fn tokenize_documentation(&mut self) -> Result<&'a str, TokenizationErrorKind> {
        self.consume();
        self.consume();
        self.consume();

        let start = self.grapheme_offset;
        while let Some(c) = self.current() {
            if c == "-" && self.peek(1) == Some("-") && self.peek(2) == Some("-") {
                let doc_content = self.slice(start, self.grapheme_offset);
                self.consume();
                self.consume();
                self.consume();
                return Ok(doc_content);
            }
            self.consume();
        }

        Err(TokenizationErrorKind::UnterminatedDoc)
    }
}

```

`src/tokenize/tokenize_identifier.rs`:

```rs
use super::{is_alphanumeric, Tokenizer};

impl<'a> Tokenizer<'a> {
    pub fn tokenize_identifier(&mut self) -> &'a str {
        let start = self.grapheme_offset;
        while let Some(c) = self.current() {
            if is_alphanumeric(c) || c == "_" {
                self.consume();
            } else {
                break;
            }
        }

        self.slice(start, self.grapheme_offset)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{ModulePath, Position, Span},
        globals::{reset_globals, STRING_INTERNER},
        tokenize::{Token, TokenKind, Tokenizer},
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn tokenizes_simple_identifiers() {
        reset_globals();

        let hello_id = STRING_INTERNER.intern("hello");
        let path = ModulePath::default();

        let (tokens, _) = Tokenizer::tokenize("hello", path.clone());

        assert_eq!(
            tokens,
            vec![Token {
                kind: TokenKind::Identifier(hello_id),
                span: Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0
                    },
                    end: Position {
                        line: 1,
                        col: 6,
                        byte_offset: 5
                    },
                    path: path.clone()
                }
            }]
        )
    }

    #[test]
    fn tokenizes_sequence_as_identifier() {
        reset_globals();

        let structhello_id = STRING_INTERNER.intern("structhello");
        let path = ModulePath::default();

        let (tokens, _) = Tokenizer::tokenize("\nstructhello", path.clone());

        assert_eq!(
            tokens,
            vec![Token {
                kind: TokenKind::Identifier(structhello_id),
                span: Span {
                    start: Position {
                        line: 2,
                        col: 1,
                        byte_offset: 1
                    },
                    end: Position {
                        line: 2,
                        col: 12,
                        byte_offset: 12
                    },
                    path: path.clone()
                }
            }]
        )
    }
}

```

`src/tokenize/tokenize_number.rs`:

```rs
use super::{is_digit, is_letter, NumberKind, TokenizationErrorKind, Tokenizer};

impl<'a> Tokenizer<'a> {
    pub fn tokenize_number(&mut self) -> Result<NumberKind, TokenizationErrorKind> {
        let start = self.grapheme_offset;
        let mut has_dot = false;

        while let Some(c) = self.current() {
            if is_digit(c) {
                self.consume();
            } else if c == "." && !has_dot {
                has_dot = true;
                self.consume();
            } else if c == "." && has_dot {
                return Err(TokenizationErrorKind::InvalidFloatingNumber);
            } else if is_letter(c) {
                self.consume();
            } else {
                break;
            }
        }

        let number_str = self.slice(start, self.grapheme_offset);

        parse_number(number_str)
    }
}

const SUFFIX_INFOS: [(&str, bool); 10] = [
    ("f64", true),
    ("f32", true),
    ("u64", false),
    ("u32", false),
    ("u16", false),
    ("u8", false),
    ("i64", false),
    ("i32", false),
    ("i16", false),
    ("i8", false),
];

fn parse_number(full_number_str: &str) -> Result<NumberKind, TokenizationErrorKind> {
    for (suffix_str, is_float_suffix) in SUFFIX_INFOS {
        if let Some(numeric_part) = full_number_str.strip_suffix(suffix_str) {
            if !is_float_suffix && numeric_part.contains('.') {
                // e.g. "1.0u8"
                return Err(TokenizationErrorKind::InvalidIntegerNumber);
            }
            if is_float_suffix && numeric_part == "." {
                // e.g. ".f32"
                return Err(TokenizationErrorKind::InvalidFloatingNumber);
            }

            let result: Result<NumberKind, _> = match suffix_str {
                "f64" => numeric_part
                    .parse::<f64>()
                    .map(NumberKind::F64)
                    .or(Err(TokenizationErrorKind::InvalidFloatingNumber)),
                "f32" => numeric_part
                    .parse::<f32>()
                    .map(NumberKind::F32)
                    .or(Err(TokenizationErrorKind::InvalidFloatingNumber)),
                "usize" => numeric_part
                    .parse::<usize>()
                    .map(NumberKind::USize)
                    .or(Err(TokenizationErrorKind::InvalidIntegerNumber)),
                "u64" => numeric_part
                    .parse::<u64>()
                    .map(NumberKind::U64)
                    .or(Err(TokenizationErrorKind::InvalidIntegerNumber)),
                "u32" => numeric_part
                    .parse::<u32>()
                    .map(NumberKind::U32)
                    .or(Err(TokenizationErrorKind::InvalidIntegerNumber)),
                "u16" => numeric_part
                    .parse::<u16>()
                    .map(NumberKind::U16)
                    .or(Err(TokenizationErrorKind::InvalidIntegerNumber)),
                "u8" => numeric_part
                    .parse::<u8>()
                    .map(NumberKind::U8)
                    .or(Err(TokenizationErrorKind::InvalidIntegerNumber)),
                "isize" => numeric_part
                    .parse::<isize>()
                    .map(NumberKind::ISize)
                    .or(Err(TokenizationErrorKind::InvalidIntegerNumber)),
                "i64" => numeric_part
                    .parse::<i64>()
                    .map(NumberKind::I64)
                    .or(Err(TokenizationErrorKind::InvalidIntegerNumber)),
                "i32" => numeric_part
                    .parse::<i32>()
                    .map(NumberKind::I32)
                    .or(Err(TokenizationErrorKind::InvalidIntegerNumber)),
                "i16" => numeric_part
                    .parse::<i16>()
                    .map(NumberKind::I16)
                    .or(Err(TokenizationErrorKind::InvalidIntegerNumber)),
                "i8" => numeric_part
                    .parse::<i8>()
                    .map(NumberKind::I8)
                    .or(Err(TokenizationErrorKind::InvalidIntegerNumber)),
                _ => unreachable!("Suffix matched in loop but not in match block"),
            };

            return result;
        }
    }

    if full_number_str.contains('.') {
        full_number_str
            .parse::<f64>()
            .map(NumberKind::F64)
            .or(Err(TokenizationErrorKind::InvalidFloatingNumber))
    } else {
        full_number_str
            .parse::<i32>()
            .map(NumberKind::I32)
            .or(Err(TokenizationErrorKind::InvalidIntegerNumber))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{ModulePath, Position, Span},
        globals::reset_globals,
        tokenize::{NumberKind, Token, TokenKind, Tokenizer},
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn tokenizes_numbers_with_suffixes() {
        reset_globals();
        let path = ModulePath::default();

        let test_cases = vec![
            (
                "1.",
                NumberKind::F64(1.0f64),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 3,
                        byte_offset: 2,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1.5",
                NumberKind::F64(1.5f64),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 4,
                        byte_offset: 3,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1",
                NumberKind::I64(1i64),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 2,
                        byte_offset: 1,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1.5f64",
                NumberKind::F64(1.5f64),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 7,
                        byte_offset: 6,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1.5f32",
                NumberKind::F32(1.5f32),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 7,
                        byte_offset: 6,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1f64",
                NumberKind::F64(1f64),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 5,
                        byte_offset: 4,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1f32",
                NumberKind::F32(1f32),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 5,
                        byte_offset: 4,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1u8",
                NumberKind::U8(1u8),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 4,
                        byte_offset: 3,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1u16",
                NumberKind::U16(1u16),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 5,
                        byte_offset: 4,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1u32",
                NumberKind::U32(1u32),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 5,
                        byte_offset: 4,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1u64",
                NumberKind::U64(1u64),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 5,
                        byte_offset: 4,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1i8",
                NumberKind::I8(1i8),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 4,
                        byte_offset: 3,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1i16",
                NumberKind::I16(1i16),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 5,
                        byte_offset: 4,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1i32",
                NumberKind::I32(1i32),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 5,
                        byte_offset: 4,
                    },
                    path: path.clone(),
                },
            ),
            (
                "1i64",
                NumberKind::I64(1i64),
                Span {
                    start: Position {
                        line: 1,
                        col: 1,
                        byte_offset: 0,
                    },
                    end: Position {
                        line: 1,
                        col: 5,
                        byte_offset: 4,
                    },
                    path: path.clone(),
                },
            ),
        ];

        for (input, expected_kind, span) in test_cases {
            let (tokens, errors) = Tokenizer::tokenize(input, path.clone());

            assert_eq!(errors, vec![]);

            assert_eq!(
                tokens,
                vec![Token {
                    span,
                    kind: TokenKind::Number(expected_kind),
                }]
            );
        }
    }
}

```

`src/tokenize/tokenize_punctuation.rs`:

```rs
use super::{PunctuationKind, Tokenizer};

impl<'a> Tokenizer<'a> {
    pub fn tokenize_punctuation(&mut self, punct: &str) -> Option<PunctuationKind> {
        match punct {
            ":" => match self.peek(1) {
                Some(":") => {
                    self.consume();
                    self.consume();
                    Some(PunctuationKind::DoubleCol)
                }
                _ => {
                    self.consume();
                    Some(PunctuationKind::Col)
                }
            },
            "|" => match self.peek(1) {
                Some("|") => {
                    self.consume();
                    self.consume();
                    Some(PunctuationKind::DoubleOr)
                }
                _ => {
                    self.consume();
                    Some(PunctuationKind::Or)
                }
            },
            "&" => match self.peek(1) {
                Some("&") => {
                    self.consume();
                    self.consume();
                    Some(PunctuationKind::DoubleAnd)
                }
                _ => {
                    self.consume();
                    Some(PunctuationKind::And)
                }
            },
            "=" => match self.peek(1) {
                Some("=") => {
                    self.consume();
                    self.consume();
                    Some(PunctuationKind::DoubleEq)
                }
                _ => {
                    self.consume();
                    Some(PunctuationKind::Eq)
                }
            },
            "<" => match self.peek(1) {
                Some("=") => {
                    self.consume();
                    self.consume();
                    Some(PunctuationKind::Lte)
                }
                _ => {
                    self.consume();
                    Some(PunctuationKind::Lt)
                }
            },
            ">" => match self.peek(1) {
                Some("=") => {
                    self.consume();
                    self.consume();
                    Some(PunctuationKind::Gte)
                }
                _ => {
                    self.consume();
                    Some(PunctuationKind::Gt)
                }
            },
            "!" => match self.peek(1) {
                Some("=") => {
                    self.consume();
                    self.consume();
                    Some(PunctuationKind::NotEq)
                }
                _ => {
                    self.consume();
                    Some(PunctuationKind::Not)
                }
            },
            ";" => {
                self.consume();
                Some(PunctuationKind::SemiCol)
            }
            "." => {
                self.consume();
                Some(PunctuationKind::Dot)
            }
            "(" => {
                self.consume();
                Some(PunctuationKind::LParen)
            }
            ")" => {
                self.consume();
                Some(PunctuationKind::RParen)
            }
            "[" => {
                self.consume();
                Some(PunctuationKind::LBracket)
            }
            "]" => {
                self.consume();
                Some(PunctuationKind::RBracket)
            }
            "{" => {
                self.consume();
                Some(PunctuationKind::LBrace)
            }
            "}" => {
                self.consume();
                Some(PunctuationKind::RBrace)
            }
            "+" => {
                self.consume();
                Some(PunctuationKind::Plus)
            }
            "-" => {
                self.consume();
                Some(PunctuationKind::Minus)
            }
            "*" => {
                self.consume();
                Some(PunctuationKind::Star)
            }
            "/" => {
                self.consume();
                Some(PunctuationKind::Slash)
            }
            "%" => {
                self.consume();
                Some(PunctuationKind::Percent)
            }
            "," => {
                self.consume();
                Some(PunctuationKind::Comma)
            }
            "$" => {
                self.consume();
                Some(PunctuationKind::Dollar)
            }
            "?" => {
                self.consume();
                Some(PunctuationKind::Question)
            }
            "#" => {
                self.consume();
                Some(PunctuationKind::Hash)
            }
            _ => None,
        }
    }
}

```

`src/tokenize/tokenize_string.rs`:

```rs
use super::{TokenizationErrorKind, Tokenizer};

impl<'a> Tokenizer<'a> {
    pub fn string(&mut self) -> Result<&'a str, TokenizationErrorKind> {
        self.consume();
        let literal_start = self.grapheme_offset;

        while let Some(c) = self.current() {
            match c {
                "\"" => {
                    let value = self.slice(literal_start, self.grapheme_offset);
                    self.consume();
                    return Ok(value);
                }
                "\\" => {
                    self.consume();
                    if let Some(next_char) = self.current() {
                        match next_char {
                            "\"" | "\\" | "$" | "{" | "}" | "n" | "r" | "t" => {
                                self.consume();
                            }
                            _ => {
                                return Err(TokenizationErrorKind::UnknownEscapeSequence);
                            }
                        }
                    } else {
                        return Err(TokenizationErrorKind::UnterminatedString);
                    }
                }
                _ => self.consume(),
            }
        }

        Err(TokenizationErrorKind::UnterminatedString)
    }
}

```