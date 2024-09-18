use std::{fmt, ops::{Add, BitAnd, BitOr, Deref, Div, Mul, Neg, Not}};

use egg::Analysis;
use num_bigint::BigInt;
use num_rational::BigRational;

use crate::exp::{BinOp, Exp, UnOp};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Meaning;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TyG<Bool, Rational, Ref, Snapshot> {
    Bool(Bool),
    Rational(Rational),
    /// Data indicates if this is `Null`.
    Ref(Ref),
    Snapshot(Snapshot),
    PredicateId,
    #[default]
    TypeError,
}
// #[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub struct SnapConst(pub Vec<Ty>);

// impl Deref for SnapConst {
//     type Target = Vec<Ty>;
//     fn deref(&self) -> &Self::Target {
//         &self.0
//     }
// }
pub type SnapConst = usize;

pub type Ty = TyG<Option<bool>, Option<BigRational>, Option<()>, Option<SnapConst>>;
pub type TyKind = TyG<(), (), (), ()>;

impl<Bool, Rational, Ref, Snapshot> TyG<Bool, Rational, Ref, Snapshot> {
    pub fn is_bool(&self) -> bool {
        matches!(self, TyG::Bool(_))
    }
    pub fn is_rational(&self) -> bool {
        matches!(self, TyG::Rational(_))
    }
    pub fn is_ref(&self) -> bool {
        matches!(self, TyG::Ref(_))
    }
    pub fn is_snapshot(&self) -> bool {
        matches!(self, TyG::Snapshot(_))
    }
    pub fn is_predicate_id(&self) -> bool {
        matches!(self, TyG::PredicateId)
    }
    pub fn is_error(&self) -> bool {
        matches!(self, TyG::TypeError)
    }
    pub fn non_error(&self) -> Result<&Self, ()> {
        Some(self).filter(|s| !s.is_error()).ok_or(())
    }

    pub fn ty_default<Bool_: Default, Rational_: Default, Ref_: Default, Snapshot_: Default>(&self) -> TyG<Bool_, Rational_, Ref_, Snapshot_> {
        match self {
            TyG::Bool(_) => TyG::Bool(Default::default()),
            TyG::Rational(_) => TyG::Rational(Default::default()),
            TyG::Ref(_) => TyG::Ref(Default::default()),
            TyG::Snapshot(_) => TyG::Snapshot(Default::default()),
            TyG::PredicateId => TyG::PredicateId,
            TyG::TypeError => TyG::TypeError,
        }
    }

    pub fn matches(&self, other: &Self) -> Self where Bool: Default, Rational: Default, Ref: Default, Snapshot: Default {
        if std::mem::discriminant(self) == std::mem::discriminant(other) {
            self.ty_default()
        } else {
            TyG::TypeError
        }
    }
}

impl Ty {
    pub fn eq_(&self, other: &Self) -> Self {
        self.compare(other).map_or(Ty::TypeError, Ty::Bool)
    }
    pub fn compare(&self, other: &Self) -> Result<Option<bool>, ()> {
        match (self, other) {
            (Ty::Bool(a), Ty::Bool(b)) =>
                Ok(a.zip(*b).map(|(a, b)| a == b)),
            (Ty::Rational(a), Ty::Rational(b)) =>
                Ok(a.as_ref().zip(b.as_ref()).map(|(a, b)| a == b)),
            (Ty::Ref(a), Ty::Ref(b)) =>
                Ok(a.zip(*b).map(|(a, b)| a == b)),
            // (Ty::Snapshot(a), Ty::Snapshot(b)) =>
            //     Ok(a.as_ref().zip(b.as_ref()).map(|(a, b)| {
            //         if a.len() != b.len() {
            //             return Err(())
            //         }
            //         for (a, b) in a.iter().zip(b.iter()) {
            //             match a.compare(b)? {
            //                 None => return Ok(None),
            //                 Some(false) => return Ok(Some(false)),
            //                 Some(true) => (),
            //             }
            //         }
            //         Ok(Some(true))
            //     }).transpose()?.flatten()),
            (Ty::Snapshot(a), Ty::Snapshot(b)) => Ok(a.zip(*b).map(|(a, b)| a == b)),
            (Ty::PredicateId, Ty::PredicateId) => Ok(Some(true)),
            (Ty::TypeError, Ty::TypeError) => Err(()),
            (a, b) if std::mem::discriminant(a) == std::mem::discriminant(b) => unimplemented!("new variant"),
            _ => Err(()),
        }
    }

    fn kind(&self) -> TyKind {
        self.ty_default()
    }
}

impl Neg for &'_ Ty {
    type Output = Ty;
    fn neg(self) -> Self::Output {
        match self {
            Ty::Rational(r) =>
                Ty::Rational(r.as_ref().map(<&BigRational as Neg>::neg)),
            _ => Ty::TypeError,
        }
    }
}

impl Not for &'_ Ty {
    type Output = Ty;
    fn not(self) -> Self::Output {
        match self {
            Ty::Bool(b) => Ty::Bool(b.map(bool::not)),
            _ => Ty::TypeError,
        }
    }
}

fn rational_binop_fn<T>(a: &Ty, b: &Ty, op: fn(&BigRational, &BigRational) -> Result<T, ()>, to_ty: fn(Option<T>) -> Ty) -> Ty {
    match (a, b) {
        (Ty::Rational(a), Ty::Rational(b)) => {
            let r = a.as_ref().zip(b.as_ref()).map(|(a, b)| op(a, b));
            r.transpose().map_or(Ty::TypeError, to_ty)
        }
        _ => Ty::TypeError,
    }
}

macro_rules! impl_binop {
    ($trait:ident, $variant:ident, $method:ident, $op:tt$(, $d:tt)?) => {
        impl $trait for &'_ Ty {
            type Output = Ty;
            fn $method(self, other: Self) -> Self::Output {
                match (self, other) {
                    (Ty::$variant(a), Ty::$variant(b)) =>
                        Ty::$variant(a.as_ref().zip(b.as_ref()).map(|(a, b)| $($d)?a $op $($d)?b)),
                    _ => Ty::TypeError,
                }
            }
        }
    };
}

impl_binop!(Add, Rational, add, +);
impl_binop!(Mul, Rational, mul, *);
impl_binop!(BitAnd, Bool, bitand, &&, *);
impl_binop!(BitOr, Bool, bitor, ||, *);

// impl PartialOrd for Ty {
//     fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
//         match (self, other) {
//             (Ty::Rational(a), Ty::Rational(b)) => a.partial_cmp(b),
//             _ => None,
//         }
//     }
// }

impl Div for &'_ Ty {
    type Output = Ty;
    fn div(self, other: Self) -> Self::Output {
        rational_binop_fn(self, other, |a, b|
            (b.numer() != &BigInt::from(0u8)).then(|| a / b).ok_or(()),
            Ty::Rational
        )
    }
}

impl Analysis<Exp> for Meaning {
    type Data = Ty;

    fn make(egraph: &egg::EGraph<Exp, Self>, enode: &Exp) -> Self::Data {
        use silver_oxide::ast::Const;
        match enode {
            Exp::Const(c) => match c {
                Const::Bool(b) => Ty::Bool(Some(*b)),
                Const::Int(n) => Ty::Rational(Some(BigRational::from(BigInt::from(n.clone())))),
                Const::Epsilon | Const::Wildcard => Ty::Rational(None),
                Const::Null => Ty::Ref(Some(())),
                Const::None => Ty::Rational(Some(BigRational::from(BigInt::ZERO))),
                Const::Write => Ty::Rational(Some(BigRational::from(BigInt::from(1u8)))),
            },
            Exp::UnOp(unop, e) => match unop {
                UnOp::Neg =>  - &egraph[*e].data,
                UnOp::Not => ! &egraph[*e].data,
            },
            Exp::BinOp(binop, [a, b]) => match binop {
                BinOp::Plus => &egraph[*a].data + &egraph[*b].data,
                BinOp::Mult => &egraph[*a].data * &egraph[*b].data,
                BinOp::Div => &egraph[*a].data / &egraph[*b].data,
                BinOp::Lt => rational_binop_fn(&egraph[*a].data, &egraph[*b].data, |a, b| Ok(a < b), Ty::Bool),
                BinOp::Eq => egraph[*a].data.eq_(&egraph[*b].data),
                // Not strictly necessary as this should get simplified anyway
                BinOp::And => &egraph[*a].data & &egraph[*b].data,
                BinOp::Or => &egraph[*a].data | &egraph[*b].data,
                BinOp::Mod => todo!(),
            }
            Exp::Ternary([c, t, e]) => match (&egraph[*c].data, &egraph[*t].data, &egraph[*e].data) {
                (Ty::Bool(_), t, e) => t.matches(e),
                _ => Ty::TypeError,
            },
            Exp::Project(s, i) => match &egraph[*s].data {
                Ty::Snapshot(Some(n)) if i < n => Ty::Snapshot(None),
                Ty::Snapshot(None) => Ty::Snapshot(None),
                _ => Ty::TypeError,
            },
            Exp::Snapshot(vec) =>
                // vec.iter()
                //     .map(|id| egraph[*id].data.non_error().cloned())
                //     .collect::<Result<Vec<_>, ()>>()
                //     .map(SnapConst)
                //     .map(Some)
                //     .map_or(Ty::TypeError, Ty::Snapshot),
                Ty::Snapshot(Some(vec.len())),
            Exp::PredicateApp(..) => Ty::PredicateId,

            Exp::FuncApp(.., ty) => ty.ty_default(),
            Exp::SymbolicValue(symbolic_value) => symbolic_value.2.ty_default(),
            Exp::Downcast(s, ty) => match &egraph[*s].data {
                Ty::Snapshot(None) | Ty::Snapshot(Some(0)) => ty.ty_default(),
                _ => Ty::TypeError,
            },
            Exp::Upcast(v, ty) => if std::mem::discriminant(&egraph[*v].data.ty_default()) == std::mem::discriminant(ty) {
                Ty::Snapshot(Some(0))
            } else {
                Ty::TypeError
            },
        }
    }

    fn merge(&mut self, a: &mut Self::Data, b: Self::Data) -> egg::DidMerge {
        fn merge_option<T: Eq>(a: &mut Option<T>, b: Option<T>) -> Result<egg::DidMerge, ()> {
            match (&mut *a, b) {
                (None, None) => Ok(egg::DidMerge(false, false)),
                (Some(_), None) => Ok(egg::DidMerge(false, false)),
                (None, Some(b)) => {
                    *a = Some(b);
                    Ok(egg::DidMerge(true, false))
                }
                (Some(a), Some(b)) => {
                    (*a == b).then(|| egg::DidMerge(false, false)).ok_or(())
                }
            }
        }
        let (a_error, b_error) = (a.is_error(), b.is_error());
        let merge = if std::mem::discriminant(a) != std::mem::discriminant(&b) {
            Err(())
        } else {
            match (&mut *a, b) {
                (Ty::Bool(a), Ty::Bool(b)) =>
                    merge_option(a, b),
                (Ty::Rational(a), Ty::Rational(b)) =>
                    merge_option(a, b),
                (Ty::Ref(a), Ty::Ref(b)) =>
                    merge_option(a, b),
                (Ty::Snapshot(a), Ty::Snapshot(b)) =>
                    merge_option(a, b),
                (Ty::PredicateId, Ty::PredicateId) => Ok(egg::DidMerge(false, false)),
                (Ty::TypeError, Ty::TypeError) => Ok(egg::DidMerge(false, false)),
                _ => todo!("new variant"),
            }
        };
        if let Ok(merge) = merge {
            return merge;
        }
        *a = Ty::TypeError;
        egg::DidMerge(!a_error, !b_error)
    }

    fn modify(egraph: &mut egg::EGraph<Exp, Self>, id: egg::Id) {
        use silver_oxide::ast::Const;
        match &egraph[id].data {
            Ty::Bool(Some(b)) => {
                let c = egraph.add(Exp::Const(Const::Bool(*b)));
                egraph.union_trusted(id, c, "same meaning");
            }
            Ty::Rational(Some(r)) => {
                let (numer, denom) = (r.numer().clone(), (!r.is_integer()).then(|| r.denom().clone()));
                let mut div = egraph.add(Exp::Const(Const::Int(numer)));
                if let Some(denom) = denom {
                    let denom = egraph.add(Exp::Const(Const::Int(denom)));
                    div = egraph.add(Exp::BinOp(BinOp::Div, [div, denom]));
                }
                egraph.union_trusted(id, div, "same meaning");
            }
            Ty::Ref(Some(())) => {
                let c = egraph.add(Exp::Const(Const::Null));
                egraph.union_trusted(id, c, "same meaning");
            }
            _ => (),
        }
    }
}

// impl Ty {
//     fn merge_fn(&mut self, other: Self) -> egg::DidMerge {
//         if self == &other {
//             return egg::DidMerge(false, false)
//         }
//         let same_type = std::mem::discriminant(self) == std::mem::discriminant(&other);
//         let inconsistent = *self == Ty::Inconsistent || other == Ty::Inconsistent;
//         let new = if same_type || inconsistent { Ty::Inconsistent } else { Ty::TypeError };

//         let old_a = std::mem::replace(self, new);
//         egg::DidMerge(old_a != *self, other != *self)
//     }

//     pub fn compare(&self, b: Option<&Self>) -> Option<bool> {
//         let b = b?;
//         match (self, b) {
//             (Ty::Bool(a), Ty::Bool(b)) => Some(a == b),
//             (Ty::Rational(a), Ty::Rational(b)) => Some(a == b),
//             _ => None,
//         }
//     }
// }

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Bool(_) => write!(f, "bool"),
            Ty::Rational(_) => write!(f, "rat"),
            Ty::Ref(_) => write!(f, "ref"),
            Ty::Snapshot(_) => write!(f, "snap"),
            Ty::PredicateId => write!(f, "pred_id"),
            Ty::TypeError => write!(f, "error"),
        }?;
        match self {
            Ty::Bool(Some(b)) => write!(f, "{{{b}}}"),
            Ty::Rational(Some(r)) => write!(f, "{{{r}}}"),
            Ty::Ref(Some(())) => write!(f, "{{null}}"),
            Ty::Snapshot(Some(n)) => write!(f, "{{{n}}}"),
            _ => Ok(()),
        }
    }
}

impl<'a> From<&'a silver_oxide::ast::Type> for TyKind {
    fn from(value: &'a silver_oxide::ast::Type) -> Self {
        use silver_oxide::ast::Type;
        match value {
            Type::Int => TyKind::Rational(()),
            Type::Bool => TyKind::Bool(()),
            Type::Perm => TyKind::Rational(()),
            Type::Ref => TyKind::Ref(()),
            Type::Rational => TyKind::Rational(()),
            Type::Seq(_) => todo!(),
            Type::Set(_) => todo!(),
            Type::Map(_, _) => todo!(),
            Type::User(_, _) => todo!("{value:?}"),
        }
    }
}
