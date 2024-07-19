use std::collections::hash_map::Entry;

use fxhash::FxHashMap;

use crate::{exp::Exp, pure::EGraph};

use super::{CTreeChunk, Heap, HeapChunk};

// Mergeable

// trait Mergeable<T = Self> {
//     fn merge(&mut self, other: T, egraph: &mut EGraph);
// }

// impl<T: Mergeable> Mergeable for Option<T> {
//     fn merge(&mut self, other: Self, egraph: &mut EGraph) {
//         if let Some(other) = other {
//             match self {
//                 Some(self_) => self_.merge(other, egraph),
//                 None => *self = Some(other),
//             }
//         }
//     }
// }

// impl CTreeChunk {
//     pub(super) fn for_roots(&mut self, f: &mut impl FnMut(&mut HeapChunk)) {
//         if let Some(chunk) = &mut self.chunk {
//             f(chunk);
//         } else {
//             for chunk in self.conditional.values_mut() {
//                 chunk.for_roots(f);
//             }
//         }
//     }
// }

impl CTreeChunk {
    fn inhale(&mut self, other: HeapChunk, egraph: &mut EGraph) {
        for ct_chunk in self.conditional.values_mut() {
            ct_chunk.inhale(other, egraph);
        }
        self.insert_root(other, egraph);
    }

    fn insert_root(&mut self, other: HeapChunk, egraph: &mut EGraph) {
        match &mut self.chunk {
            Some(self_) => self_.merge(other, egraph),
            None => {
                self.chunk = Some(other);
            }
        }
    }
    fn insert_subtree(&mut self, idx: egg::Id, ct_chunk: CTreeChunk, egraph: &mut EGraph) {
        match self.conditional.entry(idx) {
            Entry::Vacant(v) => {
                v.insert(ct_chunk);
            }
            Entry::Occupied(o) =>
                o.into_mut().insert_tree(ct_chunk, egraph),
        }
    }
    fn insert_tree(&mut self, other: Self, egraph: &mut EGraph) {
        if let Some(chunk) = other.chunk {
            self.insert_root(chunk, egraph);
        }
        for (idx, ct_chunk) in other.conditional {
            self.insert_subtree(idx, ct_chunk, egraph);
        }
    }
}

impl HeapChunk {
    fn merge(&mut self, other: HeapChunk, egraph: &mut EGraph) {
        egraph.equate(self.symbolic_value, other.symbolic_value, "heap merge");
        self.permission = egraph.add(Exp::BinOp(silver_oxide::ast::BinOp::Plus, [self.permission, other.permission]));
    }
}

// Normalisable

impl Heap {
    pub fn normalise(&mut self, egraph: &mut EGraph) {
        let true_ = egraph.normalise(egraph.true_());
        let false_ = egraph.normalise(egraph.false_());
        self.0.normalise(egraph, true, true_, false_);
        egraph.rebuild();
    }
}

impl CTreeChunk {
    fn normalise(&mut self, egraph: &mut EGraph, is_heap: bool, true_: egg::Id, false_: egg::Id) {
        // TODO: check if any of the `self.conditional`s imply any other of the
        // `self.conditional`s and rebuild the tree if so (double implication
        // leads to merge).
        let mut normalised = CTreeChunk::default();
        self.conditional.retain(|&idx, chunk| {
            let nidx = egraph.normalise(idx);
            if !is_heap && nidx == true_ {
                let mut chunk = std::mem::take(chunk);
                chunk.pop_up_true_conds(egraph, true_);
                normalised.insert_tree(chunk, egraph);
                false
            } else if !is_heap && nidx == false_ {
                false
            } else if idx != nidx {
                let chunk = std::mem::take(chunk);
                normalised.insert_subtree(nidx, chunk, egraph);
                false
            } else {
                true
            }
        });
        if normalised.chunk.is_none() && normalised.conditional.is_empty() {
            return;
        }
        // TODO: could switch from pairwise (the above merges) to n-way merging
        self.insert_tree(normalised, egraph);

        for chunk in self.conditional.values_mut() {
            chunk.normalise(egraph, false, true_, false_);
        }
    }

    fn pop_up_true_conds(&mut self, egraph: &mut EGraph, true_: egg::Id) {
        let mut true_conds = CTreeChunk::default();
        self.conditional.retain(|&idx, chunk| {
            let nidx = egraph.normalise(idx);
            if nidx == true_ {
                let mut chunk = std::mem::take(chunk);
                chunk.pop_up_true_conds(egraph, true_);
                true_conds.insert_tree(chunk, egraph);
                false
            } else {
                true
            }
        });
        if true_conds.chunk.is_some() || !true_conds.conditional.is_empty() {
            self.insert_tree(true_conds, egraph);
        }
    }
}
