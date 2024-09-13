use crate::{egg::rules, exp::{BinOp, Exp, SymbolicValue}, meaning::{Constant, Meaning}};

pub type EClass = egg::EClass<Exp, Option<Constant>>;

#[derive(Debug, Clone, Copy)]
pub struct Constants {
    pub true_: egg::Id,
    pub false_: egg::Id,
    /// Also equal to the integer 0.
    pub none: egg::Id,
    /// Also equal to the integer 1.
    pub write: egg::Id,
}

impl Constants {
    pub fn new<N: egg::Analysis<Exp>>(egraph: &mut egg::EGraph<Exp, N>) -> Self {
        let true_ = egraph.add(Exp::Const(silver_oxide::ast::Const::Bool(true)));
        let false_ = egraph.add(Exp::Const(silver_oxide::ast::Const::Bool(false)));
        // let not_true_ = egraph.add(Exp::Not(true_));
        // let not_false_ = egraph.add(Exp::Not(false_));
        // egraph.union_trusted(true_, not_false_, "true is not false");
        // egraph.union_trusted(false_, not_true_, "false is not true");

        let none = egraph.add(Exp::Const(silver_oxide::ast::Const::None));
        let write = egraph.add(Exp::Const(silver_oxide::ast::Const::Write));
        Self { true_, false_, none, write }
    }
}

#[derive(Debug, Clone)]
pub struct EGraph {
    pub egraph: egg::EGraph<Exp, Meaning>,
    pub next_symbolic_value: u64,

    constants: Constants,
}

impl Default for EGraph {
    fn default() -> Self {
        let mut egraph = egg::EGraph::default();
        let constants = Constants::new(&mut egraph);
        Self { egraph, next_symbolic_value: 0, constants }
    }
}

impl EGraph {
    pub fn true_(&self) -> egg::Id {
        self.constants.true_
    }
    pub fn false_(&self) -> egg::Id {
        self.constants.false_
    }
    pub fn none(&self) -> egg::Id {
        self.constants.none
    }
    pub fn write(&self) -> egg::Id {
        self.constants.write
    }
    pub fn next_symbolic_value(&mut self, name: Option<String>) -> egg::Id {
        let id = self.egraph.add(Exp::SymbolicValue(SymbolicValue(self.next_symbolic_value, name)));
        self.next_symbolic_value += 1;
        id
    }

    pub fn has_inconsistency(&self) -> Option<&EClass> {
        self.egraph.classes().find(|class| class.data == Some(Constant::Inconsistent))
    }
    pub fn has_type_error(&self) -> Option<&EClass> {
        self.egraph.classes().find(|class| class.data == Some(Constant::TypeError))
    }

    pub fn add(&mut self, exp: Exp) -> egg::Id {
        self.egraph.add(exp)
    }
    pub fn add_binop(&mut self, op: silver_oxide::ast::BinOp, lhs: egg::Id, rhs: egg::Id) -> egg::Id {
        BinOp::translate(op, lhs, rhs, self)
    }
    pub fn add_snapshot(&mut self, snapshot: Vec<egg::Id>) -> egg::Id {
        let snap = self.add(Exp::Snapshot(snapshot.clone()));
        for (i, child) in snapshot.into_iter().enumerate() {
            let i = self.add(Exp::Project(snap, i));
            self.equate(child, i, "snap-inj");
        }
        snap
    }

    pub fn fold(&mut self, op: BinOp, default: egg::Id, mut items: impl Iterator<Item = egg::Id>) -> egg::Id {
        let first = items.next();
        first.map(|first| {
            items.fold(first, |acc, item| {
                self.add(Exp::BinOp(op, [acc, item]))
            })
        }).unwrap_or(default)
    }

    pub fn equate(&mut self, id1: egg::Id, id2: egg::Id, reason: impl Into<egg::Symbol>) {
        self.egraph.union_trusted(id1, id2, reason);
    }
    pub fn assume(&mut self, id: egg::Id, reason: impl Into<egg::Symbol>) {
        self.egraph.union_trusted(id, self.true_(), reason);
    }
    pub fn rebuild(&mut self) {
        if !self.egraph.clean {
            self.egraph.rebuild();
        }
    }
    pub fn saturate(&mut self) {
        let egraph = std::mem::replace(&mut self.egraph, egg::EGraph::default());
        let runner = egg::Runner::default().with_egraph(egraph);

        let runner = runner.run(rules(self.constants));

        self.egraph = runner.egraph;
    }

    pub fn normalise(&self, id: egg::Id) -> egg::Id {
        self.egraph.find(id)
    }

    pub fn is_true(&self, exp: egg::Id) -> bool {
        self.egraph[exp].data == Some(Constant::Bool(true))
    }
    pub fn is_false(&self, exp: egg::Id) -> bool {
        self.egraph[exp].data == Some(Constant::Bool(false))
    }
}
