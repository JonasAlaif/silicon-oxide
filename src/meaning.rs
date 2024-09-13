use std::{fmt, ops::{Add, BitAnd, BitOr, Div, Mul, Neg, Not}};

use egg::Analysis;
use num_bigint::BigInt;
use num_rational::BigRational;

use crate::exp::{BinOp, Exp, UnOp};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Meaning;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Constant {
    Bool(bool),
    Rational(BigRational),
    Inconsistent,
    TypeError,
}

impl Neg for &'_ Constant {
    type Output = Constant;
    fn neg(self) -> Self::Output {
        match self {
            Constant::Rational(r) => Constant::Rational(-r),
            Constant::Inconsistent => Constant::Inconsistent,
            _ => Constant::TypeError,
        }
    }
}

impl Not for &'_ Constant {
    type Output = Constant;
    fn not(self) -> Self::Output {
        match self {
            Constant::Bool(b) => Constant::Bool(!b),
            Constant::Inconsistent => Constant::Inconsistent,
            _ => Constant::TypeError,
        }
    }
}

macro_rules! impl_binop {
    ($trait:ident, $variant:ident, $method:ident, $op:tt$(, $d:tt)?) => {
        impl $trait for &'_ Constant {
            type Output = Constant;
            fn $method(self, other: Self) -> Self::Output {
                match (self, other) {
                    (Constant::$variant(a), Constant::$variant(b)) => Constant::$variant($($d)?a $op $($d)?b),
                    (Constant::Inconsistent, _) | (_, Constant::Inconsistent) => Constant::Inconsistent,
                    _ => Constant::TypeError,
                }
            }
        }
    };
}

impl_binop!(Add, Rational, add, +);
impl_binop!(Mul, Rational, mul, *);
impl_binop!(BitAnd, Bool, bitand, &&, *);
impl_binop!(BitOr, Bool, bitor, ||, *);

impl PartialOrd for Constant {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Constant::Rational(a), Constant::Rational(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}

impl Div for &'_ Constant {
    type Output = Constant;
    fn div(self, other: Self) -> Self::Output {
        match (self, other) {
            (Constant::Rational(a), Constant::Rational(b)) =>
                if b.numer() == &BigInt::from(0u8) { Constant::Inconsistent } else { Constant::Rational(a / b) },
            (Constant::Inconsistent, _) | (_, Constant::Inconsistent) => Constant::Inconsistent,
            _ => Constant::TypeError,
        }
    }
}

impl Analysis<Exp> for Meaning {
    type Data = Option<Constant>;

    fn make(egraph: &egg::EGraph<Exp, Self>, enode: &Exp) -> Self::Data {
        let data = match enode {
            Exp::Const(c) => match c {
                silver_oxide::ast::Const::Bool(b) => Constant::Bool(*b),
                silver_oxide::ast::Const::Int(n) => Constant::Rational(BigRational::from(BigInt::from(n.clone()))),
                _ => return None,
            },
            Exp::UnOp(UnOp::Neg, e) => - egraph[*e].data.as_ref()?,
            Exp::UnOp(UnOp::Not, e) => ! egraph[*e].data.as_ref()?,

            Exp::BinOp(BinOp::Plus, [a, b]) =>
                egraph[*a].data.as_ref()? + egraph[*b].data.as_ref()?,
            Exp::BinOp(BinOp::Mult, [a, b]) =>
                egraph[*a].data.as_ref()? * egraph[*b].data.as_ref()?,
            Exp::BinOp(BinOp::Div, [a, b]) =>
                egraph[*a].data.as_ref()? / egraph[*b].data.as_ref()?,
            Exp::BinOp(BinOp::Lt, [a, b]) =>
                Constant::Bool(egraph[*a].data.as_ref()?.partial_cmp(egraph[*b].data.as_ref()?)?.is_lt()),
            Exp::BinOp(BinOp::Eq, [a, b]) =>
                Constant::Bool(egraph[*a].data.as_ref()? == egraph[*b].data.as_ref()?),
            // Not strictly necessary as this should get simplified anyway
            Exp::BinOp(BinOp::And, [a, b]) =>
                egraph[*a].data.as_ref()? & egraph[*b].data.as_ref()?,
            Exp::BinOp(BinOp::Or, [a, b]) =>
                egraph[*a].data.as_ref()? | egraph[*b].data.as_ref()?,
            _ => return None,
        };
        Some(data)
    }

    fn merge(&mut self, a: &mut Self::Data, b: Self::Data) -> egg::DidMerge {
        egg::merge_option(a, b, Constant::merge_fn)
    }

    fn modify(egraph: &mut egg::EGraph<Exp, Self>, id: egg::Id) {
        match &egraph[id].data {
            Some(Constant::Bool(b)) => {
                let c = egraph.add(Exp::Const(silver_oxide::ast::Const::Bool(*b)));
                egraph.union_trusted(id, c, "same meaning");
            }
            Some(Constant::Rational(r)) => {
                let (numer, denom) = (r.numer().clone(), (!r.is_integer()).then(|| r.denom().clone()));
                let mut div = egraph.add(Exp::Const(silver_oxide::ast::Const::Int(numer)));
                if let Some(denom) = denom {
                    let denom = egraph.add(Exp::Const(silver_oxide::ast::Const::Int(denom)));
                    div = egraph.add(Exp::BinOp(BinOp::Div, [div, denom]));
                }
                egraph.union_trusted(id, div, "same meaning");
            }
            _ => (),
        }
    }
}

impl Constant {
    fn merge_fn(&mut self, other: Self) -> egg::DidMerge {
        if self == &other {
            return egg::DidMerge(false, false)
        }
        let same_type = std::mem::discriminant(self) == std::mem::discriminant(&other);
        let inconsistent = *self == Constant::Inconsistent || other == Constant::Inconsistent;
        let new = if same_type || inconsistent { Constant::Inconsistent } else { Constant::TypeError };

        let old_a = std::mem::replace(self, new);
        egg::DidMerge(old_a != *self, other != *self)
    }

    pub fn compare(&self, b: Option<&Self>) -> Option<bool> {
        let b = b?;
        match (self, b) {
            (Constant::Bool(a), Constant::Bool(b)) => Some(a == b),
            (Constant::Rational(a), Constant::Rational(b)) => Some(a == b),
            _ => None,
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Bool(b) => write!(f, "{b}"),
            Constant::Rational(r) => write!(f, "{r}"),
            Constant::Inconsistent => write!(f, "inconsistent"),
            Constant::TypeError => write!(f, "type error"),
        }
    }
}
