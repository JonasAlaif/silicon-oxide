use super::EGraph;
use fxhash::FxHashMap;
use z3::*;

impl EGraph {
    pub fn z3_assert(&self, assertion: egg::Id) -> bool {
        let cfg = Config::new();
        let ctx = Context::new(&cfg);
        let solver = self.translate_to_z3(&ctx);

        let sat = solver.check();
        println!("sat: {sat:?}");
        matches!(sat, SatResult::Unsat)
    }

    fn translate_to_z3<'z>(&self, ctx: &'z Context) -> Solver<'z> {
        let solver = Solver::new(&ctx);
        let b = ast::Bool::new_const(&ctx, "a");
        solver
    }
}

struct EGraphToZ3<'z> {
    ctx: &'z Context,
    nodes: FxHashMap<egg::Id, ast::Dynamic<'z>>
}

impl<'z> EGraphToZ3<'z> {
    fn new(ctx: &'z Context) -> Self {
        Self { ctx, nodes: Default::default() }
    }

    fn translate(&mut self, egraph: &EGraph) -> Solver<'z> {
        let mut solver = Solver::new(&self.ctx);
        for eclass in egraph.egraph.classes() {
            let id = eclass.id;
            // let z3node = ast::Dynamic::
            // self.nodes
        }
        solver
    }

}
