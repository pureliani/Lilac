use std::collections::{BTreeSet, HashSet};

use crate::{
    compile::interner::TypeId,
    globals::STRING_INTERNER,
    mir::{
        builders::{Builder, BuilderContext},
        types::{
            checked_declaration::{CheckedParam, FnType},
            checked_type::{StructKind, Type},
        },
    },
    tokenize::TokenKind,
};
