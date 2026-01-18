use std::{
    collections::HashSet,
    fs,
    path::{Path, PathBuf},
    sync::{atomic::AtomicUsize, Arc, Mutex},
};

pub mod file_cache;
pub mod interner;
pub mod report_errors;

use crate::{
    ast::{
        decl::Declaration,
        expr::{Expr, ExprKind},
        stmt::{Stmt, StmtKind},
    },
    compile::{
        file_cache::FileCache,
        interner::{Interners, SharedStringInterner, SharedTagInterner},
    },
    hir::{errors::SemanticError, ProgramBuilder},
    parse::{Parser, ParsingError},
    tokenize::{TokenizationError, Tokenizer},
};

pub struct Compiler {
    interners: Interners,
    files: Arc<Mutex<FileCache>>,
    errors: Vec<CompilationError>,
    decl_id_counter: Arc<AtomicUsize>,
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            interners: Interners {
                string_interner: Arc::new(SharedStringInterner::default()),
                tag_interner: Arc::new(SharedTagInterner::default()),
            },
            files: Arc::new(Mutex::new(FileCache::default())),
            errors: Vec::new(),
            decl_id_counter: Arc::new(AtomicUsize::new(0)),
        }
    }
}

#[derive(Debug)]
pub enum CompilationError {
    CouldNotReadFile {
        path: PathBuf,
        error: std::io::Error,
    },
    ModuleNotFound {
        importing_module: PathBuf,
        target_path: PathBuf,
        error: std::io::Error,
    },
    Tokenization {
        path: PathBuf,
        errors: Vec<TokenizationError>,
    },
    Parsing {
        path: PathBuf,
        errors: Vec<ParsingError>,
    },
    Semantic {
        path: PathBuf,
        errors: Vec<SemanticError>,
    },
}

#[derive(Debug)]
pub struct ParallelParseResult {
    pub path: PathBuf,
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
                Err(e) => {
                    self.errors.push(e);
                }
                Ok(module) => {
                    let has_tokenization_errors = !module.tokenization_errors.is_empty();
                    let has_parsing_errors = !module.parsing_errors.is_empty();

                    if has_tokenization_errors {
                        self.errors.push(CompilationError::Tokenization {
                            path: module.path.clone(),
                            errors: module.tokenization_errors.clone(),
                        });
                    }

                    if has_parsing_errors {
                        self.errors.push(CompilationError::Parsing {
                            path: module.path.clone(),
                            errors: module.parsing_errors.clone(),
                        });
                    }

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

        let mut program_builder = ProgramBuilder::new(
            self.interners.string_interner.clone(),
            self.interners.tag_interner.clone(),
        );

        program_builder.build(modules_to_compile);

        for (path, mb) in program_builder.modules {
            if !mb.errors.is_empty() {
                self.errors.push(CompilationError::Semantic {
                    path,
                    errors: mb.errors,
                });
            }
        }

        if !program_builder.errors.is_empty() {
            // These are global program errors
            self.errors.push(CompilationError::Semantic {
                path: PathBuf::from("Global"),
                errors: program_builder.errors,
            });
        }

        if !self.errors.is_empty() {
            self.report_errors();
            return;
        }

        println!("Compilation successful (HIR generated)");
    }

    pub fn parallel_parse_modules(
        &self,
        main_path: PathBuf,
    ) -> Vec<Result<ParallelParseResult, CompilationError>> {
        let canonical_main = main_path
            .canonicalize()
            .expect("Could not find the main module");

        let visited = Arc::new(Mutex::new(HashSet::new()));
        let all_results = Arc::new(Mutex::new(Vec::new()));

        rayon::scope(|s| {
            fn parse_recursive(
                path: PathBuf,
                s: &rayon::Scope,
                interners: Interners,
                files: Arc<Mutex<FileCache>>,
                visited: Arc<Mutex<HashSet<PathBuf>>>,
                all_results: Arc<
                    Mutex<Vec<Result<ParallelParseResult, CompilationError>>>,
                >,
                decl_id_counter: Arc<AtomicUsize>,
            ) {
                let path = path.canonicalize().expect("Could not find a module");

                {
                    let mut visited_guard = visited.lock().unwrap();
                    if !visited_guard.insert(path.clone()) {
                        return;
                    }
                }

                let source_code = match fs::read_to_string(&path) {
                    Ok(sc) => sc,
                    Err(e) => {
                        all_results.lock().unwrap().push(Err(
                            CompilationError::CouldNotReadFile { path, error: e },
                        ));
                        return;
                    }
                };

                let (tokens, tokenization_errors) =
                    Tokenizer::tokenize(&source_code, interners.string_interner.clone());
                let (statements, parsing_errors) = Parser::parse(
                    tokens,
                    interners.string_interner.clone(),
                    decl_id_counter.clone(),
                );

                let (dependencies, dependency_errors, declarations) =
                    find_dependencies(&path, &statements);

                for dep_path in dependencies {
                    let cloned_interners = interners.clone();
                    let decl_id_counter = decl_id_counter.clone();
                    let files = Arc::clone(&files);
                    let visited = Arc::clone(&visited);
                    let all_results = Arc::clone(&all_results);

                    s.spawn(move |s| {
                        parse_recursive(
                            dep_path,
                            s,
                            cloned_interners,
                            files,
                            visited,
                            all_results,
                            decl_id_counter,
                        );
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
                canonical_main,
                s,
                self.interners.clone(),
                self.files.clone(),
                visited,
                all_results.clone(),
                self.decl_id_counter.clone(),
            );
        });

        Arc::try_unwrap(all_results)
            .expect("Arc unwrap failed")
            .into_inner()
            .expect("Mutex into_inner failed")
    }
}

fn find_dependencies(
    current_module_path: &Path,
    statements: &[Stmt],
) -> (HashSet<PathBuf>, Vec<CompilationError>, Vec<Declaration>) {
    let mut dependencies = HashSet::new();
    let mut errors = vec![];
    let mut declarations: Vec<Declaration> = vec![];

    for stmt in statements {
        match &stmt.kind {
            StmtKind::From { path, .. } => {
                let relative_path_str = &path.value;
                let mut target_path = current_module_path.to_path_buf();
                target_path.pop();
                target_path.push(relative_path_str);

                match fs::canonicalize(target_path.clone()) {
                    Ok(canonical_path) => {
                        dependencies.insert(canonical_path);
                    }
                    Err(e) => {
                        errors.push(CompilationError::ModuleNotFound {
                            importing_module: current_module_path.to_path_buf(),
                            target_path,
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
