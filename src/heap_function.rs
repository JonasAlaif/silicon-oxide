use crate::{HashMap, HashSet};

#[derive(Debug, Clone, Default)]
pub struct HeapFunctions {
    // TODO: could potentially look into deduplicating the `Vec` if we have the
    // same call multiple times (would need to normalise `egg::Id`s though).
    pub functions: FxHashMap<Vec<egg::Id>, Vec<HeapFunction>>,
}

impl HeapFunctions {
    pub fn add(&mut self, mut heap_args: Vec<egg::Id>, call: egg::Id, name: silver_oxide::parse::Ident, args: Vec<egg::Id>) {
        let function = HeapFunction { call, name, args };
        heap_args.sort();
        heap_args.dedup();
        self.functions.entry(heap_args).or_default().push(function);
    }
}

#[derive(Debug, Clone)]
pub struct HeapFunction {
    pub call: egg::Id,
    pub name: silver_oxide::parse::Ident,
    pub args: Vec<egg::Id>,
}
