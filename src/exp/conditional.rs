use fxhash::FxHashMap;

use crate::{path_condition::PathCondition, pure::EGraph, exp::BinOp};

pub struct ConditionalExp {
    /// Invariant that `exps.keys().fold(pc.negative, |acc, &k| acc || k)` is always true.
    pub pc: PathCondition,
    pub default: Option<egg::Id>,
    pub exps: FxHashMap<egg::Id, egg::Id>,
}

impl ConditionalExp {
    pub fn new(pc: PathCondition, exp: egg::Id) -> Self {
        Self { pc, default: Some(exp), exps: Default::default() }
    }

    // pub fn cross_with(self, other: Self, pc: PathCondition, binop: BinOp, egraph: &mut EGraph) -> Self {
    //     let new = Self { pc, default: None, exps: Default::default() };
    //     if let (Some(self, ))
    // }

    // pub fn 
}
