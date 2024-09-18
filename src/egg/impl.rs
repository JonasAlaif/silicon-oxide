
use std::slice::{from_mut, from_ref};

use egg::{FromOp, Language};

use crate::exp::{BinOp, Exp, UnOp};

impl FromOp for Exp {
    type Error = ();
    fn from_op(op: &str, children: Vec<egg::Id>) -> Result<Self, Self::Error> {
        match (op, children.as_slice()) {
            ("true", &[]) => Ok(Exp::Const(silver_oxide::ast::Const::Bool(true))),
            ("false", &[]) => Ok(Exp::Const(silver_oxide::ast::Const::Bool(false))),
            ("none", &[]) => Ok(Exp::Const(silver_oxide::ast::Const::None)),
            ("write", &[]) => Ok(Exp::Const(silver_oxide::ast::Const::Write)),

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
            (number, &[]) if number.parse::<num_bigint::BigUint>().is_ok() => {
                Ok(Exp::Const(silver_oxide::ast::Const::Int(number.parse().unwrap())))
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
            (Exp::Snapshot(es1), Exp::Snapshot(es2)) => es1.len() == es2.len(),
            (Exp::Project(_, i1), Exp::Project(_, i2)) => i1 == i2,
            (Exp::Downcast(_, t1), Exp::Downcast(_, t2)) => t1 == t2,
            (Exp::Upcast(_, t1), Exp::Upcast(_, t2)) => t1 == t2,
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
            Exp::Snapshot(es) => es,
            Exp::Project(e, _) => from_ref(e),
            Exp::Downcast(e, _) => from_ref(e),
            Exp::Upcast(e, _) => from_ref(e),
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
            Exp::Snapshot(es) => es,
            Exp::Project(e, _) => from_mut(e),
            Exp::Downcast(e, _) => from_mut(e),
            Exp::Upcast(e, _) => from_mut(e),
        }
    }
}
