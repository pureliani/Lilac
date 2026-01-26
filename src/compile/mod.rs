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
        let mut aliases = HashMap::new();

        let mut program = Program {
            constant_data: HashMap::new(),
            declarations: HashMap::new(),
            modules: HashMap::new(),
            value_types: HashMap::new(),
        };

        let global_scope = Scope::new_root(ScopeKind::Global, Span::default());

        let mut program_builder = Builder {
            context: InGlobal,
            current_scope: global_scope,
            errors: &mut builder_errors,
            program: &mut program,
            current_defs: &mut current_defs,
            incomplete_phis: &mut incomplete_phis,
            aliases: &mut aliases,
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
