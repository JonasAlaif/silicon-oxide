use std::ops::{Add, Div, Mul, Neg, Not};

use egg::Analysis;
use num_bigint::BigInt;
use num_rational::BigRational;

use crate::exp::{BinOp, Exp};

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
    ($trait:ident, $method:ident, $op:tt) => {
        impl $trait for &'_ Constant {
            type Output = Constant;
            fn $method(self, other: Self) -> Self::Output {
                match (self, other) {
                    (Constant::Rational(a), Constant::Rational(b)) => Constant::Rational(a $op b),
                    (Constant::Inconsistent, _) | (_, Constant::Inconsistent) => Constant::Inconsistent,
                    _ => Constant::TypeError,
                }
            }
        }
    };
}

impl_binop!(Add, add, +);
impl_binop!(Mul, mul, *);

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
                silver_oxide::ast::Const::True => Constant::Bool(true),
                silver_oxide::ast::Const::False => Constant::Bool(false),
                silver_oxide::ast::Const::Int(n) => Constant::Rational(BigRational::from(BigInt::from(n.clone()))),
                _ => return None,
            },
            Exp::Neg(e) => - egraph[*e].data.as_ref()?,
            // Not strictly necessary as this should get simplified anyway
            Exp::Not(e) => ! egraph[*e].data.as_ref()?,
            Exp::BinOp(BinOp::Plus, [a, b]) =>
                egraph[*a].data.as_ref()? + egraph[*b].data.as_ref()?,
            Exp::BinOp(BinOp::Mult, [a, b]) =>
                egraph[*a].data.as_ref()? * egraph[*b].data.as_ref()?,
            Exp::BinOp(BinOp::Div, [a, b]) =>
                egraph[*a].data.as_ref()? / egraph[*b].data.as_ref()?,
            _ => return None,
        };
        Some(data)
    }

    fn merge(&mut self, a: &mut Self::Data, b: Self::Data) -> egg::DidMerge {
        match (a, b) {
            (Some(a), Some(b)) if a == &b => egg::DidMerge(false, false),
            (Some(a), Some(b)) => {
                let same_type = std::mem::discriminant(a) == std::mem::discriminant(&b);
                let inconsistent = *a == Constant::Inconsistent || b == Constant::Inconsistent;
                let new = if same_type || inconsistent { Constant::Inconsistent } else { Constant::TypeError };

                let old_a = std::mem::replace(a, new);
                egg::DidMerge(old_a != *a, b != *a)
            }
            (None, None) => egg::DidMerge(false, false),
            (Some(_), None) => egg::DidMerge(false, true),
            (a@None, Some(b)) => {
                *a = Some(b);
                egg::DidMerge(true, false)
            }
        }
    }
}
