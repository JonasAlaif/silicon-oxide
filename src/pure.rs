use crate::{egg::rules, exp::{BinOp, Exp, SymbolicValue}, meaning::{Constant, Meaning}};

pub type EClass = egg::EClass<Exp, Option<Constant>>;

#[derive(Debug, Clone)]
pub struct EGraph {
    pub egraph: egg::EGraph<Exp, Meaning>,
    pub next_symbolic_value: u64,

    pub true_: egg::Id,
    pub false_: egg::Id,
    pub none: egg::Id,
    pub write: egg::Id,
}

impl Default for EGraph {
    fn default() -> Self {
        let mut egraph = egg::EGraph::default();
        let true_ = egraph.add(Exp::Const(silver_oxide::ast::Const::True));
        let false_ = egraph.add(Exp::Const(silver_oxide::ast::Const::False));
        let none = egraph.add(Exp::Const(silver_oxide::ast::Const::None));
        let write = egraph.add(Exp::Const(silver_oxide::ast::Const::Write));
        Self { egraph, next_symbolic_value: 0, true_, false_, none, write }
    }
}

impl EGraph {
    pub fn true_(&self) -> egg::Id {
        self.true_
    }
    pub fn false_(&self) -> egg::Id {
        self.false_
    }
    pub fn none(&self) -> egg::Id {
        self.none
    }
    pub fn write(&self) -> egg::Id {
        self.write
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

    pub fn negate_pc(&mut self, pc: &[egg::Id]) -> egg::Id {
        pc.split_first().map(|(first, rest)| {
            let first_neg = self.add(Exp::Not(*first));
            rest.iter().fold(first_neg, |acc, pc| {
                let neg = self.add(Exp::Not(*pc));
                self.add(Exp::BinOp(BinOp::Or, [acc, neg]))
            })
        }).unwrap_or(self.false_())
    }

    pub fn equate(&mut self, id1: egg::Id, id2: egg::Id, reason: impl Into<egg::Symbol>) {
        self.egraph.union_trusted(id1, id2, reason);
    }
    pub fn assume(&mut self, id: egg::Id, reason: impl Into<egg::Symbol>) {
        self.egraph.union_trusted(id, self.true_, reason);
    }
    pub fn rebuild(&mut self) {
        if !self.egraph.clean {
            self.egraph.rebuild();
        }
    }
    pub fn saturate(&mut self) {
        let egraph = std::mem::replace(&mut self.egraph, egg::EGraph::default());
        let runner = egg::Runner::default().with_egraph(egraph);

        let runner = runner.run(rules());

        self.egraph = runner.egraph;
    }

    pub fn normalise(&self, id: egg::Id) -> egg::Id {
        self.egraph.find(id)
    }

    pub fn is_true(&mut self, exp: egg::Id) -> bool {
        self.egraph[exp].data == Some(Constant::Bool(true))
    }
    pub fn is_false(&mut self, exp: egg::Id) -> bool {
        self.egraph[exp].data == Some(Constant::Bool(false))
    }
}
