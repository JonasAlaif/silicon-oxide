
use std::{slice::{from_mut, from_ref}, str::FromStr};

use egg::{FromOp, Language};

use crate::{exp::{BinOp, Exp, UnOp}, pure::TyKind};

impl FromOp for Exp {
    type Error = ();
    fn from_op(op: &str, children: Vec<egg::Id>) -> Result<Self, Self::Error> {
        match (op, children.as_slice()) {
            ("true", &[]) => Ok(Exp::Const(silver_oxide::parse::ConstKind::Bool(true))),
            ("false", &[]) => Ok(Exp::Const(silver_oxide::parse::ConstKind::Bool(false))),
            ("none", &[]) => Ok(Exp::Const(silver_oxide::parse::ConstKind::none())),
            ("write", &[]) => Ok(Exp::Const(silver_oxide::parse::ConstKind::write())),
            ("wild", &[]) => Ok(Exp::Const(silver_oxide::parse::ConstKind::Wildcard)),

            ("!", &[child]) => Ok(Exp::UnOp(UnOp::Not, child)),
            ("-", &[child]) => Ok(Exp::UnOp(UnOp::Neg, child)),

            ("+", &[lhs, rhs]) => Ok(Exp::BinOp(BinOp::Plus, [lhs, rhs])),
            ("<", &[lhs, rhs]) => Ok(Exp::BinOp(BinOp::Lt, [lhs, rhs])),
            ("&&", &[lhs, rhs]) => Ok(Exp::BinOp(BinOp::And, [lhs, rhs])),
            ("||", &[lhs, rhs]) => Ok(Exp::BinOp(BinOp::Or, [lhs, rhs])),
            ("==" | "eq", &[lhs, rhs]) => Ok(Exp::BinOp(BinOp::Eq, [lhs, rhs])),
            ("/", &[lhs, rhs]) => Ok(Exp::BinOp(BinOp::Div, [lhs, rhs])),
            ("*", &[lhs, rhs]) => Ok(Exp::BinOp(BinOp::Mult, [lhs, rhs])),
            ("%", &[lhs, rhs]) => Ok(Exp::BinOp(BinOp::Mod, [lhs, rhs])),

            ("?", &[cond, then, els]) => Ok(Exp::Ternary([cond, then, els])),
            ("itr", &[child]) => Ok(Exp::UnOp(UnOp::IntToReal, child)),
            // (op, &[child]) if op.starts_with('@') => {
            //     let op = op.strip_prefix('@').unwrap();
            //     let mut op = op.split('-');
            //     let from = op.next().unwrap().parse().unwrap();
            //     let to = op.next().unwrap().parse().unwrap();
            //     assert!(op.next().is_none());
            //     Ok(Exp::TyCast(child, from, to))
            // }
            (number, &[]) if number.parse::<num::BigUint>().is_ok() => {
                Ok(Exp::Const(silver_oxide::parse::ConstKind::Int(number.parse().unwrap())))
            }
            _ => Err(()),
        }
    }
}

impl Language for Exp {
    fn matches(&self, other: &Self) -> bool {
        match (self, other) {
            (Exp::Const(c1), Exp::Const(c2)) => c1 == c2,
            (Exp::FuncApp(f1, c1, t1), Exp::FuncApp(f2, c2, t2)) => f1 == f2 && c1.len() == c2.len() && t1 == t2,
            (Exp::SymbolicValue(sv1), Exp::SymbolicValue(sv2)) => sv1 == sv2,
            (Exp::BinOp(op1, _), Exp::BinOp(op2, _)) => op1 == op2,
            (Exp::Ternary(_), Exp::Ternary(_)) => true,
            (Exp::UnOp(op1, _), Exp::UnOp(op2, _)) => op1 == op2,
            (Exp::Snapshot(es1, eid1), Exp::Snapshot(es2, eid2)) =>
                es1.len() == es2.len() && eid1 == eid2,
            (Exp::Project(_, i1, t1), Exp::Project(_, i2, t2)) =>
                i1 == i2 && t1 == t2,
            // (Exp::TyCast(_, tf1, tt1), Exp::TyCast(_, tf2, tt2)) => tf1 == tf2 && tt1 == tt2,
            _ => false,
        }
    }

    fn children(&self) -> &[egg::Id] {
        match self {
            Exp::Const(_) => &[],
            Exp::FuncApp(_, children, _) => children,
            Exp::PredicateApp(_, children) => children,
            Exp::SymbolicValue(_) => &[],
            Exp::BinOp(_, children) => children,
            Exp::Ternary(children) => children,
            Exp::UnOp(_, children) => from_ref(children),
            Exp::Snapshot(es, _) => es,
            Exp::Project(e, _, _) => from_ref(e),
            _ => todo!(),
            // Exp::TyCast(e, _, _) => from_ref(e),
        }
    }

    fn children_mut(&mut self) -> &mut [egg::Id] {
        match self {
            Exp::Const(_) => &mut [],
            Exp::FuncApp(_, children, _) => children,
            Exp::PredicateApp(_, children) => children,
            Exp::SymbolicValue(_) => &mut [],
            Exp::BinOp(_, children) => children,
            Exp::Ternary(children) => children,
            Exp::UnOp(_, children) => from_mut(children),
            Exp::Snapshot(es, _) => es,
            Exp::Project(e, _, _) => from_mut(e),
            _ => todo!(),
            // Exp::TyCast(e, _, _) => from_mut(e),
        }
    }
}

// Bool(Bool),
// Integer(Integer),
// Real(Real),
// /// Data indicates if this is `Null`.
// Ref(Ref),
// Snapshot(Snapshot),
// PredicateId,
// #[default]
// TypeError,

impl FromStr for TyKind {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "bool" => Ok(TyKind::Bool(())),
            "int" => Ok(TyKind::Integer(())),
            "real" => Ok(TyKind::Real(())),
            "ref" => Ok(TyKind::Ref(())),
            // "snap" => Ok(TyKind::Snapshot(())),
            _ => Err(()),
        }
    }
}
