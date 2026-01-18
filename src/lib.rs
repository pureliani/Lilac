#![allow(clippy::result_large_err)]
#![allow(clippy::inherent_to_string)]
#![allow(clippy::redundant_pattern_matching)]

use std::{path::PathBuf, sync::Arc};

pub mod ast;
pub mod codegen;
pub mod compile;
pub mod globals;
pub mod hir;
pub mod parse;
pub mod tokenize;

#[derive(Default, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ModulePath(Arc<PathBuf>);

impl From<ModulePath> for PathBuf {
    fn from(value: ModulePath) -> Self {
        value.0.to_path_buf()
    }
}
