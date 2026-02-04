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
