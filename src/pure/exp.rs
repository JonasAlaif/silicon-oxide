use core::fmt;

use silver_oxide::{parse::{BinOp, UnOp}, translate::{AsParens, CompoundId, DefId, Local, QuantLocal, Temporary}};

use super::{SConst, STy};

pub type Snapshot = egg::Id;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EggExp {
    pub ty: STy,
    pub kind: EggExpKind,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum EggExpKind {
    Leaf(LeafKind),
    Snapshot(CompoundId, Vec<egg::Id>),
    Field(egg::Id, usize),
    Logical(LogicalOp, Vec<egg::Id>),

    // TODO:
    // Quantifier(QuantifierKind, TyList<'tcx>, Vec<Vec<Exp<'tcx>>>, Exp<'tcx>),

    Call(DefId, Vec<egg::Id>),
    UnOp(UnOp, egg::Id),
    BinOp(BinOp, [egg::Id; 2]),
    Ternary([egg::Id; 3]),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LogicalOp {
    And,
    // Or,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LeafKind {
    Local(Local),
    Temporary(Temporary),
    Quantified(QuantLocal),
    Const(SConst),

    /// TODO: is this necessary?
    HeapValue(usize),
}

impl From<Local> for EggExpKind {
    fn from(local: Local) -> Self {
        EggExpKind::Leaf(LeafKind::Local(local))
    }
}

impl From<Temporary> for EggExpKind {
    fn from(temporary: Temporary) -> Self {
        EggExpKind::Leaf(LeafKind::Temporary(temporary))
    }
}

impl From<QuantLocal> for EggExpKind {
    fn from(quantified: QuantLocal) -> Self {
        EggExpKind::Leaf(LeafKind::Quantified(quantified))
    }
}

impl egg::Language for EggExp {
    fn matches(&self, other: &Self) -> bool {
        use EggExpKind::*;
        let same = match (&self.kind, &other.kind) {
            (Leaf(r1), Leaf(r2)) => r1 == r2,
            (Snapshot(id1, args1), Snapshot(id2, args2)) => if id1 == id2 {
                assert_eq!(args1.len(), args2.len());
                true
            } else {
                false
            },
            (Field(_, i1), Field(_, i2)) => return i1 == i2,
            (Logical(op1, args1), Logical(op2, args2)) =>
                op1 == op2 && args1.len() == args2.len(),
            (Call(d1, args1), Call(d2, args2)) => if d1 == d2 {
                assert_eq!(args1.len(), args2.len());
                true
            } else {
                false
            },
            (UnOp(op1, _), UnOp(op2, _)) => return op1 == op2,
            (BinOp(op1, _), BinOp(op2, _)) => return op1 == op2,
            (Ternary(_), Ternary(_)) => true,
            _ => false,
        };
        if same {
            assert_eq!(self.ty, other.ty, "Types should be equal");
        }
        same
    }

    fn children(&self) -> &[egg::Id] {
        use EggExpKind::*;
        match &self.kind {
            Leaf(_) => &[],
            Snapshot(_, children) => children,
            Field(id, _) => core::slice::from_ref(id),
            Logical(_, children) => children,
            Call(_, children) => children,
            UnOp(_, id) => core::slice::from_ref(id),
            BinOp(_, children) => children,
            Ternary(children) => children,
        }
    }

    fn children_mut(&mut self) -> &mut [egg::Id] {
        use EggExpKind::*;
        match &mut self.kind {
            Leaf(_) => &mut [],
            Snapshot(_, children) => children,
            Field(id, _) => core::slice::from_mut(id),
            Logical(_, children) => children,
            Call(_, children) => children,
            UnOp(_, id) => core::slice::from_mut(id),
            BinOp(_, children) => children,
            Ternary(children) => children,
        }
    }
}

// fmt

impl fmt::Debug for EggExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for EggExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl fmt::Display for EggExpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use EggExpKind::*;
        match self {
            Leaf(l) => write!(f, "{l}"),
            Snapshot(id, args) => write!(f, "{id:?}{:?}", args.parenthesised()),
            Field(id, i) => write!(f, "{id:?}[{i}]"),
            Logical(op, args) => write!(f, "{op:?}{args:?}"),
            Call(def_id, args) => write!(f, "{def_id:?}{:?}", args.parenthesised()),
            UnOp(op, id) => write!(f, "{op:?} {id:?}"),
            BinOp(op, [lhs, rhs]) => write!(f, "{lhs:?} {op:?} {rhs:?}"),
            Ternary([c, t, e]) => write!(f, "{c:?} ? {t:?} : {e:?}"),
        }
    }
}

impl fmt::Display for LeafKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use LeafKind::*;
        match self {
            Local(l) => write!(f, "{l:?}"),
            Temporary(t) => write!(f, "{t:?}"),
            Quantified(q) => write!(f, "{q:?}"),
            Const(c) => write!(f, "{c:?}"),
            HeapValue(h) => write!(f, "h{h}"),
        }
    }
}
