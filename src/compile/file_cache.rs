use ariadne::{Cache, Source};
use std::collections::HashMap;

use crate::ast::ModulePath;

#[derive(Default)]
pub struct FileCache {
    sources: HashMap<ModulePath, Source>,
}

impl Cache<ModulePath> for FileCache {
    type Storage = String;

    fn fetch(
        &mut self,
        id: &ModulePath,
    ) -> Result<
        &ariadne::Source<<Self as ariadne::Cache<ModulePath>>::Storage>,
        impl std::fmt::Debug,
    > {
        self.sources
            .get(id)
            .ok_or_else(|| Box::new("File not found") as Box<dyn std::fmt::Debug>)
    }

    fn display<'a>(&self, id: &'a ModulePath) -> Option<impl std::fmt::Display + 'a> {
        Some(id.0.display())
    }
}

impl FileCache {
    pub fn insert(
        &mut self,
        path: ModulePath,
        source: String,
    ) -> Option<self::Source<String>> {
        self.sources.insert(path, Source::from(source))
    }
}
