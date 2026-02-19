use std::{collections::HashMap, sync::RwLock};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct StringId(pub usize);

struct StringInterner {
    forward: HashMap<String, usize>,
    backward: Vec<String>,
}

impl StringInterner {
    pub fn new() -> Self {
        Self {
            forward: HashMap::new(),
            backward: Vec::new(),
        }
    }

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

pub struct SharedStringInterner {
    interner: RwLock<StringInterner>,
}

impl SharedStringInterner {
    pub fn new() -> Self {
        Self {
            interner: RwLock::new(StringInterner::new()),
        }
    }

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
