use std::fmt;

use crate::{error::Error, pure::{EGraph, Ty, TyKind}};

pub type Snapshot = egg::Id;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ExpG<T> {
    Const(silver_oxide::ast::Const),
    // Result,
    // Old(Option<Ident>, Box<Exp>),
    // Lhs(Box<Exp>),
    // Ascribe(Box<Exp>, Type),
    // Perm(Box<LocAccess>),
    // Unfolding(Box<AccExp>, Box<Exp>),
    // Folding(Box<AccExp>, Box<Exp>),
    // Applying(Box<Exp>, Box<Exp>),
    // Packaging(Box<Exp>, Box<Exp>),
    // Forall(Vec<(Ident, Type)>, Vec<Trigger>, Box<Exp>),
    // Exists(Vec<(Ident, Type)>, Vec<Trigger>, Box<Exp>),
    // SeqConstructor(SeqConstructor),
    // SetConstructor(SetConstructor),
    // MapConstructor(MapConstructor),
    // Abs(Box<Exp>),
    // LetIn(Ident, Box<Exp>, Box<Exp>),
    // ForPerm(Vec<(Ident, Type)>, Box<ResAccess>, Box<Exp>),
    // Acc(Box<AccExp>),
    FuncApp(silver_oxide::ast::Ident, Vec<T>, TyKind),
    /// Should never have parents!
    PredicateApp(silver_oxide::ast::Ident, Vec<T>),
    SymbolicValue(SymbolicValue),
    BinOp(BinOp, [T; 2]),
    Ternary([T; 3]),
    // Field(Box<Exp>, Ident),
    // Index(Box<Exp>, Box<IndexOp>),
    UnOp(UnOp, T),
    // InhaleExhale(Box<Exp>, Box<Exp>),
    Snapshot(Vec<T>),
    Project(T, usize),
    /// Going down to `TyKind::Snapshot -> self.1`
    Downcast(T, TyKind),
    /// Going up from `self.1 -> TyKind::Snapshot`
    Upcast(T, TyKind),
}

pub type Exp = ExpG<egg::Id>;
// pub struct ExpU(pub ExpG<Box<ExpU>>);

// pub trait Translate<T> {
//     fn translate(&self, egraph: &mut EGraph) -> T;
// }

// impl<T, U: Translate<T>> Translate<ExpG<T>> for ExpG<U> {
//     fn translate(&self, egraph: &mut EGraph) -> ExpG<T> {
//         match self {
//             Self::Const(c) => ExpG::Const(c.clone()),
//             Self::FuncApp(i, args) => ExpG::FuncApp(i.clone(), args.iter().map(|a| a.translate(egraph)).collect()),
//             Self::PredicateApp(i, args) => ExpG::PredicateApp(i.clone(), args.iter().map(|a| a.translate(egraph)).collect()),
//             Self::SymbolicValue(sv) => ExpG::SymbolicValue(sv.clone()),
//             Self::BinOp(op, [l, r]) => ExpG::BinOp(*op, [l.translate(egraph), r.translate(egraph)]),
//             Self::Ternary([c, t, e]) => ExpG::Ternary([c.translate(egraph), t.translate(egraph), e.translate(egraph)]),
//             Self::UnOp(op, e) => ExpG::UnOp(*op, e.translate(egraph)),
//             Self::Snapshot(es) => ExpG::Snapshot(es.iter().map(|e| e.translate(egraph)).collect()),
//             Self::Project(e, i) => ExpG::Project(e.translate(egraph), *i),
//         }
//     }
// }

// impl<T, U: Translate<T>> Translate<T> for Box<U> {
//     fn translate(&self, egraph: &mut EGraph) -> T {
//         self.as_ref().translate(egraph)
//     }
// }

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolicValue(pub u64, pub Option<String>, pub TyKind);

impl<T: fmt::Display> fmt::Display for ExpG<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Const(c) => write!(f, "{c:?}"),
            Self::FuncApp(i, args, _) | Self::PredicateApp(i, args) => {
                write!(f, "{}(", i.0)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "#{arg}")?;
                }
                write!(f, ")")
            }
            Self::SymbolicValue(SymbolicValue(sv, None, _)) => write!(f, "@{sv}"),
            Self::SymbolicValue(SymbolicValue(sv, Some(name), _)) => write!(f, "{name}@{sv}"),
            Self::BinOp(op, [l, r]) => write!(f, "(#{} {:?} #{})", l, op, r),
            Self::Ternary([c, t, e]) => write!(f, "(#{} ? #{} : #{})", c, t, e),
            Self::UnOp(op, e) => write!(f, "{op}#{}", e),
            Self::Snapshot(es) => {
                write!(f, "snap(")?;
                for (i, e) in es.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "#{e}")?;
                }
                write!(f, ")")
            }
            Self::Project(e, i) => write!(f, "#{e}[{i}]"),
            Self::Downcast(e, _) => write!(f, "#{e}⟱"),
            Self::Upcast(e, _) => write!(f, "#{e}⟰"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinOp {
    And,
    Or,
    Eq,
    Lt,
    Plus,
    Mult,
    Div,
    Mod,
    // TODO: Set/Seq/Map
}

impl BinOp {
    pub fn translate(op: silver_oxide::ast::BinOp, mut lhs: egg::Id, mut rhs: egg::Id, egraph: &mut EGraph) -> egg::Id {
        use silver_oxide::ast::BinOp::*;
        let op = match op {
            Implies => {
                lhs = egraph.add(Exp::UnOp(UnOp::Not, lhs));
                BinOp::Or
            }
            Iff => BinOp::Eq,
            And => BinOp::And,
            Or => BinOp::Or,
            Eq => BinOp::Eq,
            Neq => {
                let exp = egraph.add(Exp::BinOp(BinOp::Eq, [lhs, rhs]));
                return egraph.add(Exp::UnOp(UnOp::Not, exp));
            }
            Lt => BinOp::Lt,
            Le => {
                let lt = egraph.add(Exp::BinOp(BinOp::Lt, [lhs, rhs]));
                let eq = egraph.add(Exp::BinOp(BinOp::Eq, [lhs, rhs]));
                (lhs, rhs) = (lt, eq);
                BinOp::Or
            }
            Gt => {
                (lhs, rhs) = (rhs, lhs);
                BinOp::Lt
            }
            Ge => {
                (lhs, rhs) = (rhs, lhs);
                let lt = egraph.add(Exp::BinOp(BinOp::Lt, [lhs, rhs]));
                let eq = egraph.add(Exp::BinOp(BinOp::Eq, [lhs, rhs]));
                (lhs, rhs) = (lt, eq);
                BinOp::Or
            }
            Plus => BinOp::Plus,
            Minus => {
                rhs = egraph.add(Exp::UnOp(UnOp::Neg, rhs));
                BinOp::Plus
            }
            Mult => BinOp::Mult,
            Div => todo!(),
            Mod => todo!(),
            In => todo!(),
            PermDiv => todo!(),
            Union => todo!(),
            SetMinus => todo!(),
            Intersection => todo!(),
            Subset => todo!(),
            Concat => todo!(),
            MagicWand => todo!(),
        };
        egraph.add(Exp::BinOp(op, [lhs, rhs]))
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnOp {
    Neg,
    Not,
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnOp::Neg => write!(f, "-"),
            UnOp::Not => write!(f, "!"),
        }
    }
}
