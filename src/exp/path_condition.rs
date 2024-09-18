use crate::{exp::{BinOp, Exp, UnOp}, pure::EGraph};


#[derive(Debug, Clone, Copy)]
pub struct PathCondition {
    pub pos_neg: [egg::Id; 2],
}

// pub struct Assertion

impl PathCondition {
    pub fn new(egraph: &EGraph) -> Self {
        Self::new_with(egraph.true_(), egraph.false_())
    }
    fn new_with(positive: egg::Id, negative: egg::Id) -> Self {
        Self { pos_neg: [positive, negative] }
    }

    pub fn positive(&self) -> egg::Id {
        self.pos_neg[0]
    }

    pub fn negative(&self) -> egg::Id {
        self.pos_neg[1]
    }

    pub fn add(self, egraph: &mut EGraph, condition: egg::Id) -> Self {
        let positive = egraph.add(Exp::BinOp(BinOp::And, [self.positive(), condition]));
        let negative = egraph.add(Exp::UnOp(UnOp::Not, positive));
        let negative = egraph.add(Exp::BinOp(BinOp::Or, [self.negative(), negative]));
        Self::new_with(positive, negative)
    }
    pub fn add_negate(self, egraph: &mut EGraph, condition: egg::Id) -> Self {
        let positive = egraph.add(Exp::UnOp(UnOp::Not, condition));
        let positive = egraph.add(Exp::BinOp(BinOp::And, [self.positive(), positive]));
        let negative = egraph.add(Exp::BinOp(BinOp::Or, [self.negative(), condition]));
        Self::new_with(positive, negative)
    }

    pub fn merge(self, egraph: &mut EGraph, other: Self) -> Result<bool, (Self, egg::Id)> {
        let def_true = egraph.add(Exp::BinOp(BinOp::Or, [self.negative(), other.positive()]));
        let def_false = egraph.add(Exp::BinOp(BinOp::Or, [self.negative(), other.negative()]));
        egraph.saturate();
        if egraph.is_true(def_true) {
            Ok(true)
        } else if egraph.is_true(def_false) {
            Ok(false)
        } else {
            let positive = egraph.add(Exp::BinOp(BinOp::And, [self.positive(), other.positive()]));
            Err((Self::new_with(positive, def_false), def_true))
        }
    }

    pub fn condition(&self, egraph: &mut EGraph, assertion: egg::Id) -> egg::Id {
        egraph.add(Exp::BinOp(BinOp::Or, [self.negative(), assertion]))
    }
    pub fn assert_lite(&self, egraph: &mut EGraph, assertion: egg::Id) -> Result<(), egg::Id> {
        if egraph.is_true(assertion) {
            return Ok(());
        }
        let assertion = self.condition(egraph, assertion);
        if egraph.is_true(assertion) {
            return Ok(());
        }
        egraph.saturate();
        egraph.is_true(assertion).then(|| ()).ok_or(assertion)
    }
    pub fn assert(&self, egraph: &mut EGraph, assertion: egg::Id) -> Result<(), egg::Id> {
        let Err(assertion) = self.assert_lite(egraph, assertion) else {
            return Ok(());
        };
        // if egraph.z3_assert(assertion) {
        //     return Ok(());
        // }
        Err(egraph.normalise(assertion))
        // // TODO: check how often this happens
        // let mut egraph_branch = egraph.clone();
        // egraph_branch.assume(self.positive(), "branch check");
        // egraph_branch.saturate();
        // egraph_branch.is_true(assertion).then(|| {
        //     eprintln!("assertion only succeeded after cloning and assuming the pc!");
        //     ()
        // }).ok_or_else(|| {
        //     let norm_assertion = egraph.normalise(assertion);
        //     eprintln!("assertion FAILED after cloning and assuming the pc: {norm_assertion}");
        //     let path_buf = "lseg/egraph_branch".parse().unwrap();
        //     egraph_branch.dot(path_buf, None, None);
        //     norm_assertion
        // })
    }

    pub fn assume(&self, egraph: &mut EGraph, assertion: egg::Id, reason: impl Into<egg::Symbol>) {
        let assertion = self.condition(egraph, assertion);
        egraph.assume(assertion, reason);
        egraph.rebuild();
    }
}
