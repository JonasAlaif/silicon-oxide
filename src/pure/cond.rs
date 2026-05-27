use silver_oxide::parse::{BinOp, UnOp};

use crate::translator::{resource::{EggExpCond, EggExpConds}, VerificationState};

use super::{EGraph, EggExpKind, LogicalOp};

#[derive(Debug, Clone, Default)]
pub struct PathCondition(EggExpConds);

impl PathCondition {
    fn is_bool(egraph: &EGraph, c: EggExpCond, value: bool) -> bool {
        egraph.is_bool(c.cond, c.neg != value)
    }

    pub fn is_true(&self, egraph: &EGraph) -> bool {
        self.conds().is_some_and(|mut c| c.all(|c| Self::is_bool(egraph, c, true)))
    }

    pub fn is_false(&self, egraph: &EGraph) -> bool {
        self.conds().is_none_or(|mut c| c.any(|c| Self::is_bool(egraph, c, false)))
    }

    fn conds(&self) -> Option<impl Iterator<Item = EggExpCond> + '_> {
        self.0.iter().map(|c| c.copied())
    }

    fn conds_egg<'a, 'tcx>(&'a self, egraph: &'a mut EGraph<'tcx>, positive: bool) -> Option<impl Iterator<Item = egg::Id> + use<'a, 'tcx>> {
        self.conds().map(move |c| c.map(move |c| if c.neg == positive {
            egraph.add_bool(EggExpKind::UnOp(UnOp::Not, c.cond))
        } else {
            c.cond
        }))
    }

    /// Calculates conjunction of all conditions (i.e. a single node representing the truth value)
    fn conjoin(&self, egraph: &mut EGraph) -> egg::Id {
        if self.is_false(egraph) {
            egraph.false_()
        } else if self.is_true(egraph) {
            egraph.true_()
        } else {
            let conj = self.conds_egg(egraph, true).unwrap().collect();
            egraph.add_bool(EggExpKind::Logical(LogicalOp::And, conj))
        }
    }

    // pub fn positive(&self) -> egg::Id {
    //     self.pos_neg[0]
    // }

    // pub fn negative(&self) -> egg::Id {
    //     self.pos_neg[1]
    // }

    // pub fn add(self, egraph: &mut EGraph, condition: egg::Id) -> Self {
    //     let positive = egraph.add(Exp::BinOp(BinOp::And, [self.positive(), condition]));
    //     let negative = egraph.add(Exp::UnOp(UnOp::Not, positive));
    //     let negative = egraph.add(Exp::BinOp(BinOp::Or, [self.negative(), negative]));
    //     Self::new_with(positive, negative)
    // }
    // pub fn add_negate(self, egraph: &mut EGraph, condition: egg::Id) -> Self {
    //     let positive = egraph.add(Exp::UnOp(UnOp::Not, condition));
    //     let positive = egraph.add(Exp::BinOp(BinOp::And, [self.positive(), positive]));
    //     let negative = egraph.add(Exp::BinOp(BinOp::Or, [self.negative(), condition]));
    //     Self::new_with(positive, negative)
    // }

    /// Returns `Ok(true)` if `self ==> other`, `Ok(false)` if `!other`, `Err(id)`
    /// if neither can be proved. The `id` points to the `self ==> other` exp.
    pub fn should_merge(&self, egraph: &mut EGraph, other: &Self) -> Result<bool, egg::Id> {
        if self.is_false(egraph) || other.is_true(egraph) {
            return Ok(true);
        } else if other.is_false(egraph) {
            // TODO: is it ok to skip the `self.is_true(egraph) &&` check in
            // this case? We are essentially treating `(b ==> false) == false`
            return Ok(false);
        }

        // eprintln!("should_merge: self: {:?}, other: {:?}", self, other);
        let other_conjoin = other.conjoin(egraph);
        let def_true = self.condition(egraph, other_conjoin);
        egraph.saturate();
        if egraph.is_bool(def_true, true) {
            // Captures `self.is_false(egraph) || other.is_true(egraph)`
            return Ok(true);
        } else if egraph.is_bool(other_conjoin, false) {
            // TODO: same as above, is it ok to ignore the self here?
            return Ok(false);
        }
        // `Some(true)` is the first case above and `Some(false)` should've been
        // caught by the second case.
        assert!(egraph.as_bool(def_true).is_none());
        Err(def_true)
    }

    pub fn merge(&mut self, egraph: &EGraph, other: &Self) {
        self.0.extend(other.0.iter().unwrap().copied());
        self.0.retain(|&c| {
            assert!(!Self::is_bool(egraph, c, false));
            !Self::is_bool(egraph, c, true)
        });
    }

    /// Calculates `self ==> assertion`
    pub fn condition(&self, egraph: &mut EGraph, assertion: egg::Id) -> egg::Id {
        if self.is_false(egraph) {
            egraph.true_()
        } else if self.is_true(egraph) {
            assertion
        } else {
            let self_conjoin = self.conjoin(egraph);
            egraph.add_bool(EggExpKind::BinOp(BinOp::Implies, [self_conjoin, assertion]))
        }
    }

    pub fn assert_lite(&self, egraph: &mut EGraph, assertion: egg::Id) -> Result<bool, egg::Id> {
        if egraph.is_bool(assertion, true) || self.is_false(egraph) {
            return Ok(true);
        } else if egraph.is_bool(assertion, false) && self.is_true(egraph) {
            return Ok(false);
        }
        let assertion = self.condition(egraph, assertion);
        egraph.saturate();
        egraph.as_bool(assertion).ok_or(assertion)
    }
    pub fn assert(&self, egraph: &mut EGraph, assertion: egg::Id, reason: &str, decls: &VerificationState<'_>) -> Result<(), egg::Id> {
        let Err(assertion) = self.assert_lite(egraph, assertion) else {
            return Ok(());
        };
        // egraph.expand_heap_fns(decls);
        egraph.log_pure("_pc/z3_assert", Some(egraph.normalise(assertion).to_string()));
        panic!()
        // if egraph.z3_assert(assertion, reason, decls) {
        //     println!("assertion succeeded after z3_assert {:?}", egraph.normalise(assertion));
        //     egraph.assume(assertion, "smt");
        //     egraph.log_pure("_pc/z3_pass_post", Some(egraph.normalise(assertion).to_string()));
        //     return Ok(());
        // }
        // Err(egraph.normalise(assertion))

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

impl From<EggExpConds> for PathCondition {
    fn from(conds: EggExpConds) -> Self {
        Self(conds)
    }
}
