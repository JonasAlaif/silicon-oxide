use core::fmt;

use silver_oxide::{parse::ConstKind, HashSet, TyCtxt};

use crate::pure::LogicalOp;

use super::{EggExp, EggExpKind, LeafKind, SConst, STy};

#[derive(Debug, Clone)]
pub struct ConstValue {
    pub ty: STy,
    pub kind: ConstValueKind,
}

#[derive(Debug, Clone)]
pub enum ConstValueKind {
    Unknown(Option<(egg::Id, usize)>),
    Snapshot(Vec<egg::Id>),
    Const(SConst),
}

impl ConstValue {
    pub fn as_snapshot(&self) -> Option<&[egg::Id]> {
        match &self.kind {
            ConstValueKind::Snapshot(s) => Some(s),
            ConstValueKind::Unknown(..) => None,
            _ => unreachable!(),
        }
    }

    pub fn as_const(&self) -> Option<SConst> {
        match &self.kind {
            ConstValueKind::Const(c) => Some(*c),
            ConstValueKind::Unknown(..) => None,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone)]
pub struct EggAnalysis {
    tcx: &'static TyCtxt<'static>,
    to_merge: HashSet<(egg::Id, egg::Id)>,
}

impl EggAnalysis {
    pub fn new(tcx: &'static TyCtxt<'static>) -> Self {
        Self {
            tcx,
            to_merge: Default::default(),
        }
    }

    pub fn tcx(&self) -> &'static TyCtxt<'static> {
        self.tcx
    }
}

impl core::ops::Deref for EggAnalysis {
    type Target = TyCtxt<'static>;
    fn deref(&self) -> &Self::Target {
        self.tcx()
    }
}

impl egg::Analysis<EggExp> for EggAnalysis {
    type Data = ConstValue;

    fn make(egraph: &egg::EGraph<EggExp, Self>, enode: &EggExp) -> Self::Data {
        use EggExpKind::*;
        let as_const = |id| egraph[id].data.as_const();
        let kind = match enode.kind {
            Leaf(LeafKind::Const(c)) => ConstValueKind::Const(c),
            Leaf(_) => ConstValueKind::Unknown(None),
            Snapshot(_, ref ids) =>
                ConstValueKind::Snapshot(ids.clone()),
            Field(id, idx) =>
                egraph[id].data.as_snapshot()
                    .map(|s| egraph[s[idx]].data.kind.clone())
                    .unwrap_or(ConstValueKind::Unknown(Some((id, idx)))),
            Logical(op, ref ids) => {
                let is_and = matches!(op, LogicalOp::And);
                let consts = ids.iter().map(|id|
                    egraph[*id].data.as_const().map(|c| c.as_bool().unwrap() != is_and)
                );
                let (mut is_true, mut is_false) = (false, false);
                let (all_pos, one_neg) = if is_and {
                    (&mut is_true, &mut is_false)
                } else {
                    (&mut is_false, &mut is_true)
                };
                *all_pos = true;
                for c in consts {
                    *all_pos &= c.unwrap_or_default();
                    if c.is_some_and(|c| !c) {
                        *one_neg = true;
                        break;
                    }
                }
                assert!(!is_true || !is_false);
                if is_true || is_false {
                    ConstValueKind::Const(egraph.analysis.tcx.interner.mk_const(ConstKind::bool(is_true)))
                } else {
                    ConstValueKind::Unknown(None)
                }
            }
            Call(..) =>
                ConstValueKind::Unknown(None),
            UnOp(op, id) =>
                as_const(id)
                    .and_then(|c| egraph.analysis.eval_unop(op, c))
                    .map(|c| ConstValueKind::Const(c))
                    .unwrap_or(ConstValueKind::Unknown(None)),
            BinOp(op, [lhs, rhs]) =>
                as_const(lhs).zip(as_const(rhs)).and_then(|(lhs, rhs)|
                    egraph.analysis.eval_binop(op, lhs, rhs))
                    .map(|c| ConstValueKind::Const(c))
                    .unwrap_or(ConstValueKind::Unknown(None)),
            // TODO: recalculate this in `modify`?
            Ternary([c, t, e]) =>
                as_const(c).map(|c| {
                    let b = if c.as_bool().unwrap() {
                        t
                    } else {
                        e
                    };
                    egraph[b].data.kind.clone()
                }).unwrap_or(ConstValueKind::Unknown(None)),
        };
        ConstValue {
            ty: enode.ty,
            kind,
        }
    }

    fn merge(&mut self, a: &mut Self::Data, b: Self::Data) -> egg::DidMerge {
        assert_eq!(a.ty, b.ty);
        use ConstValueKind::*;
        match (&mut a.kind, b.kind) {
            (Snapshot(_), Const(_)) | (Const(_), Snapshot(_)) => unreachable!(),
            (Unknown(Some(a)), Unknown(b)) => {
                assert!(b.is_none_or(|b| *a == b));
                egg::DidMerge(false, false)
            }
            (Unknown(a@None), Unknown(b)) => {
                *a = b;
                egg::DidMerge(false, false)
            }
            (Unknown(..), other) => {
                a.kind = other;
                egg::DidMerge(true, false)
            }
            (_, Unknown(..)) => egg::DidMerge(false, true),
            (Snapshot(a), Snapshot(b)) => {
                assert_eq!(a.len(), b.len());
                for (a, b) in a.iter_mut().zip(b) {
                    if *a != b {
                        self.to_merge.insert((*a, b));
                    }
                }
                let req_modify = !self.to_merge.is_empty();
                egg::DidMerge(req_modify, req_modify)
            }
            (Const(a), Const(b)) => {
                assert_eq!(*a, b);
                egg::DidMerge(false, false)
            }
        }
    }

    fn modify(egraph: &mut egg::EGraph<EggExp, Self>, id: egg::Id) {
        while let Some((a, b)) = egraph.analysis.to_merge.pop() {
            egraph.union_trusted(a, b, "fields of eq snapshots are eq");
        }
        use ConstValueKind::*;
        let data = &egraph[id].data;
        let ty = data.ty;
        match data.kind {
            Unknown(None) => (),
            Unknown(Some((id, idx))) => {
                if let Some(s) = egraph[id].data.as_snapshot() {
                    let new_data = egraph[s[idx]].data.clone();
                    assert_eq!(ty, new_data.ty);
                    egraph.set_analysis_data(id, new_data);
                }
            }
            // TODO: do we want to do the same as `Const` here?
            Snapshot(_) => (),
            Const(c) => {
                let kind = EggExpKind::Leaf(LeafKind::Const(c));
                let c = egraph.add(EggExp { ty, kind });
                egraph.union_trusted(id, c, "const eval");
            }
        }
    }
}

// fmt

impl fmt::Display for ConstValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ConstValueKind::*;
        match &self.kind {
            Const(c) => write!(f, "# {c:?}"),
            _ => write!(f, "{:?}", self.ty),
        }
    }
}
