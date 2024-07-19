
use std::slice::{from_mut, from_ref};

use egg::{FromOp, Language};

use crate::exp::Exp;

impl FromOp for Exp {
    type Error = ();
    fn from_op(op: &str, children: Vec<egg::Id>) -> Result<Self, Self::Error> {
        match (op, children.as_slice()) {
            ("true", &[]) => Ok(Exp::Const(silver_oxide::ast::Const::True)),
            ("false", &[]) => Ok(Exp::Const(silver_oxide::ast::Const::False)),
            ("none", &[]) => Ok(Exp::Const(silver_oxide::ast::Const::None)),
            ("write", &[]) => Ok(Exp::Const(silver_oxide::ast::Const::Write)),

            ("!", &[child]) => Ok(Exp::Not(child)),
            ("-", &[child]) => Ok(Exp::Neg(child)),

            ("+", &[lhs, rhs]) => Ok(Exp::BinOp(silver_oxide::ast::BinOp::Plus, [lhs, rhs])),
            ("-", &[lhs, rhs]) => Ok(Exp::BinOp(silver_oxide::ast::BinOp::Minus, [lhs, rhs])),
            (">", &[lhs, rhs]) => Ok(Exp::BinOp(silver_oxide::ast::BinOp::Gt, [lhs, rhs])),
            // TODO: remove the following 3 ops and instead simplify them on the fly
            (">=", &[lhs, rhs]) => Ok(Exp::BinOp(silver_oxide::ast::BinOp::Ge, [lhs, rhs])),
            ("<", &[lhs, rhs]) => Ok(Exp::BinOp(silver_oxide::ast::BinOp::Lt, [lhs, rhs])),
            ("<=", &[lhs, rhs]) => Ok(Exp::BinOp(silver_oxide::ast::BinOp::Le, [lhs, rhs])),
            ("&&", &[lhs, rhs]) => Ok(Exp::BinOp(silver_oxide::ast::BinOp::And, [lhs, rhs])),
            ("||", &[lhs, rhs]) => Ok(Exp::BinOp(silver_oxide::ast::BinOp::Or, [lhs, rhs])),
            ("==", &[lhs, rhs]) => Ok(Exp::BinOp(silver_oxide::ast::BinOp::Eq, [lhs, rhs])),
            ("!=", &[lhs, rhs]) => Ok(Exp::BinOp(silver_oxide::ast::BinOp::Neq, [lhs, rhs])),
            ("/", &[lhs, rhs]) => Ok(Exp::BinOp(silver_oxide::ast::BinOp::Div, [lhs, rhs])),
            ("*", &[lhs, rhs]) => Ok(Exp::BinOp(silver_oxide::ast::BinOp::Mult, [lhs, rhs])),
            ("%", &[lhs, rhs]) => Ok(Exp::BinOp(silver_oxide::ast::BinOp::Mod, [lhs, rhs])),

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
            (Exp::FuncApp(f1, c1), Exp::FuncApp(f2, c2)) => f1 == f2 && c1.len() == c2.len(),
            (Exp::SymbolicValue(sv1), Exp::SymbolicValue(sv2)) => sv1 == sv2,
            (Exp::BinOp(op1, _), Exp::BinOp(op2, _)) => op1 == op2,
            (Exp::Ternary(_), Exp::Ternary(_)) => true,
            (Exp::Neg(_), Exp::Neg(_)) => true,
            (Exp::Not(_), Exp::Not(_)) => true,
            _ => false,
        }
    }

    fn children(&self) -> &[egg::Id] {
        match self {
            Exp::Const(_) => &[],
            Exp::FuncApp(_, children) => children,
            Exp::SymbolicValue(_) => &[],
            Exp::BinOp(_, children) => children,
            Exp::Ternary(children) => children,
            Exp::Neg(children) => from_ref(children),
            Exp::Not(children) => from_ref(children),
        }
    }

    fn children_mut(&mut self) -> &mut [egg::Id] {
        match self {
            Exp::Const(_) => &mut [],
            Exp::FuncApp(_, children) => children,
            Exp::SymbolicValue(_) => &mut [],
            Exp::BinOp(_, children) => children,
            Exp::Ternary(children) => children,
            Exp::Neg(children) => from_mut(children),
            Exp::Not(children) => from_mut(children),
        }
    }
}
