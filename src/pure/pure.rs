use silver_oxide::{parse::ConstKind, translate::{Const, Ty, TyKind}, TyCtxt};

use crate::egg::rewrites;

use super::{EggAnalysis, EggEGraph, EggExp, EggExpKind, LeafKind, SConst, STy};

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
    pub fn new(egraph: &mut EggEGraph) -> Self {
        let bool_ = egraph.analysis.types.bool_;

        let true_ = egraph.analysis.interner.mk_const(ConstKind::Bool(true));
        let true_ = EggExp { ty: bool_, kind: EggExpKind::Leaf(LeafKind::Const(true_)) };
        let true_ = egraph.add(true_);

        let false_ = egraph.analysis.interner.mk_const(ConstKind::Bool(false));
        let false_ = EggExp { ty: bool_, kind: EggExpKind::Leaf(LeafKind::Const(false_)) };
        let false_ = egraph.add(false_);
        
        let real_ = egraph.analysis.types.real_;

        let none = egraph.analysis.interner.mk_const(ConstKind::none());
        let none = EggExp { ty: real_, kind: EggExpKind::Leaf(LeafKind::Const(none)) };
        let none = egraph.add(none);
        
        let write = egraph.analysis.interner.mk_const(ConstKind::write());
        let write = EggExp { ty: real_, kind: EggExpKind::Leaf(LeafKind::Const(write)) };
        let write = egraph.add(write);

        Self { true_, false_, none, write }
    }
}

#[derive(Clone, Debug)]
pub struct EGraph<'tcx> {
    egraph: EggEGraph,
    constants: Constants,
    tcx: core::marker::PhantomData<&'tcx TyCtxt<'tcx>>,

    // TODO: is this necessary?
    next_heap_value: usize,
}

impl<'tcx> EGraph<'tcx> {
    pub fn new(tcx: &'tcx TyCtxt<'tcx>) -> Self {
        let (stcx, tcx) = unsafe { tcx.as_static() };
        let analysis = EggAnalysis::new(stcx);
        let mut egraph = EggEGraph::new(analysis);
        let constants = Constants::new(&mut egraph);
        let next_heap_value = 0;
        Self { egraph, constants, tcx, next_heap_value }
    }

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

    pub fn next_heap_value(&mut self, resource: egg::Id) -> egg::Id {
        let ty = self.egraph[resource].data.ty.deref();
        let kind = EggExpKind::Leaf(LeafKind::HeapValue(self.next_heap_value));
        self.next_heap_value += 1;
        self.add(ty, kind)
    }

    pub fn next_heap_value_like(&mut self, existing_heap_value_ty: Ty<'tcx>) -> egg::Id {
        let kind = EggExpKind::Leaf(LeafKind::HeapValue(self.next_heap_value));
        self.next_heap_value += 1;
        self.add(existing_heap_value_ty, kind)
    }

    // pub fn next_symbolic_value2(&mut self, local: Local, ty: silver_oxide::program::Ty) -> egg::Id {
    //     // println!("next_symbolic_value({:?}, {:?})", name, ty);
    //     let ty = unsafe {
    //         std::mem::transmute::<silver_oxide::program::Ty, silver_oxide::program::Ty>(ty)
    //     };
    //     let id = self.egraph.add(Exp::SymbolicValue2(SymbolicValue2(self.next_symbolic_value, local, ty)));
    //     self.next_symbolic_value += 1;
    //     id
    // }

    // pub fn egraph_union(&mut self, mut exp: egg::RecExpr<EggExp>, max_sv: u64, sv_to_ids: &[egg::Id]) -> egg::Id {
    //     assert!(sv_to_ids.len() <= max_sv as usize);
    //     let old_sv = self.next_symbolic_value;
    //     self.next_symbolic_value += max_sv;

    //     for i in 0..exp.as_ref().len() {
    //         let node = &mut exp[egg::Id::from(i)];
    //         let Exp::SymbolicValue(sv) = node else { continue };
    //         assert!(sv.0 < max_sv);
    //         sv.0 += old_sv;
    //     }

    //     let eid = self.egraph.add_expr(&exp);
    //     for (i, id) in sv_to_ids.iter().copied().enumerate() {
    //         let sv = Exp::SymbolicValue(SymbolicValue(i as u64 + old_sv, None, TyKind::default()));
    //         if let Some(sv_id) = self.egraph.lookup(sv) {
    //             self.equate(id, sv_id, "union_argument");
    //         }
    //     }
    //     eid
    // }

    // pub fn has_type_error(&self) -> Option<&EClass> {
    //     self.egraph.classes().find(|class| class.data.is_error())
    // }

    pub fn add<T>(&mut self, ty: Ty<'tcx>, kind: T) -> egg::Id where EggExpKind: From<T> {
        let ty = unsafe { core::mem::transmute::<Ty<'tcx>, STy>(ty) };
        self.egraph.add(EggExp { ty, kind: kind.into() })
    }

    pub fn add_const(&mut self, ty: Ty<'tcx>, const_: Const<'tcx>) -> egg::Id {
        let const_ = unsafe { core::mem::transmute::<Const<'tcx>, SConst>(const_) };
        let kind = EggExpKind::Leaf(LeafKind::Const(const_));
        self.add(ty, kind)
    }

    pub fn add_bool<T>(&mut self, kind: T) -> egg::Id where EggExpKind: From<T> {
        self.add(self.types.bool_, kind)
    }

    pub fn add_real<T>(&mut self, kind: T) -> egg::Id where EggExpKind: From<T> {
        self.add(self.types.real_, kind)
    }

    // pub fn add_binop(&mut self, op: silver_oxide::parse::BinOp, lhs: egg::Id, rhs: egg::Id) -> egg::Id {
    //     BinOp::translate(op, lhs, rhs, self)
    // }
    // pub fn add_snapshot(&mut self, snapshot: Vec<egg::Id>, eid: ResId) -> egg::Id {
    //     let snap = self.add(Exp::Snapshot(snapshot.clone(), eid));
    //     for (i, child) in snapshot.into_iter().enumerate() {
    //         let ty = self.egraph[child].data.kind();
    //         let i = self.add(Exp::Project(snap, i, ty));
    //         self.equate(child, i, "snap-inj");
    //     }
    //     snap
    // }

    // pub fn fold(&mut self, op: BinOp, default: egg::Id, mut items: impl Iterator<Item = egg::Id>) -> egg::Id {
    //     let first = items.next();
    //     first.map(|first| {
    //         items.fold(first, |acc, item| {
    //             self.add(Exp::BinOp(op, [acc, item]))
    //         })
    //     }).unwrap_or(default)
    // }

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
        let analysis = EggAnalysis::new(self.egraph.analysis.tcx());
        let src = egg::EGraph::new(analysis.clone());
        let egraph = std::mem::replace(&mut self.egraph, src);
        let runner = egg::Runner::<_, _, ()>::new(analysis).with_egraph(egraph);

        // todo:
        let rewrites = rewrites(self.constants);
        let runner = runner.run(rewrites);

        self.egraph = runner.egraph;
    }

    pub fn normalise(&self, id: egg::Id) -> egg::Id {
        self.egraph.find(id)
    }

    pub fn ty(&self, exp: egg::Id) -> Ty<'tcx> {
        self.egraph[exp].data.ty
    }

    pub fn as_const(&self, exp: egg::Id) -> Option<SConst> {
        self.egraph[exp].data.as_const()
    }

    pub fn as_bool(&self, exp: egg::Id) -> Option<bool> {
        self.as_const(exp).and_then(SConst::as_bool)
    }

    pub fn as_int(&self, exp: egg::Id) -> Option<&'tcx num::BigInt> {
        self.as_const(exp).and_then(SConst::as_int)
    }

    pub fn as_real(&self, exp: egg::Id) -> Option<&'tcx num::BigRational> {
        self.as_const(exp).and_then(SConst::as_real)
    }

    pub fn is_bool(&self, exp: egg::Id, value: bool) -> bool {
        self.as_bool(exp).map(|b| b == value).unwrap_or_default()
    }

    pub(crate) fn egraph_for_log(&self) -> EggEGraph {
        self.egraph.clone()
    }
}

impl<'tcx> core::ops::Deref for EGraph<'tcx> {
    type Target = TyCtxt<'tcx>;
    fn deref(&self) -> &Self::Target {
        self.egraph.analysis.tcx()
    }
}
