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
