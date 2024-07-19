use crate::pure::EGraph;

use super::{CTreeChunk, Heap};

impl Heap {
    pub fn normalise(&mut self, egraph: &mut EGraph) {
        // TODO: merging
        for chunk in self.0.values_mut() {
            chunk.normalise(egraph);
        }
    }
}

impl CTreeChunk {
    pub fn normalise(&mut self, egraph: &mut EGraph) {
        for update in &mut self.updates {
            if egraph.is_true(update.condition) {}
        }
    }
}
