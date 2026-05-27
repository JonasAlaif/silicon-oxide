use std::{fmt, ops::{Add, BitAnd, BitOr, Deref, Div, Mul, Neg, Not}};

use egg::Analysis;
use num::BigInt;
use num::BigRational;
// use silver_oxide::intern::{ExpId, ResId};

use crate::{exp::{BinOp, Exp, UnOp}};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Meaning;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TyG<Bool, Integer, Real, Ref> {
    Bool(Bool),
    Integer(Integer),
    Real(Real),
    /// Data indicates if this is `Null`.
    Ref(Ref),
    Snapshot(()),
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

pub type Ty = TyG<Option<bool>, Option<BigInt>, Option<BigRational>, Option<()>>;
pub type TyKind = TyG<(), (), (), ()>;

impl fmt::Display for TyKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TyG::Bool(_) => write!(f, "bool"),
            TyG::Integer(_) => write!(f, "int"),
            TyG::Real(_) => write!(f, "real"),
            TyG::Ref(_) => write!(f, "ref"),
            TyG::Snapshot(eid) => write!(f, "snap_"),
            TyG::PredicateId => write!(f, "pred_id"),
            TyG::TypeError => write!(f, "error"),
        }
    }
}

impl<Bool, Integer, Real, Ref> TyG<Bool, Integer, Real, Ref> {
    pub fn type_error() -> Self {
        // panic!();
        Self::TypeError
    }
    pub fn is_bool(&self) -> bool {
        matches!(self, TyG::Bool(_))
    }
    pub fn is_int(&self) -> bool {
        matches!(self, TyG::Integer(_))
    }
    pub fn is_real(&self) -> bool {
        matches!(self, TyG::Real(_))
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
    pub fn non_error(self) -> Result<Self, ()> {
        Some(self).filter(|s| !s.is_error()).ok_or(())
    }

    fn ty_default<Bool_: Default, Integer_: Default, Rational_: Default, Ref_: Default>(&self) -> TyG<Bool_, Integer_, Rational_, Ref_> {
        match self {
            TyG::Bool(_) => TyG::Bool(Default::default()),
            TyG::Integer(_) => TyG::Integer(Default::default()),
            TyG::Real(_) => TyG::Real(Default::default()),
            TyG::Ref(_) => TyG::Ref(Default::default()),
            TyG::Snapshot(s) => TyG::Snapshot(*s),
            TyG::PredicateId => TyG::PredicateId,
            TyG::TypeError => TyG::type_error(),
        }
    }
}

impl Ty {
    pub fn eq_(&self, other: &Self) -> Self {
        self.compare(other).map_or_else(|()| {
            panic!("Expected same values for eq, got {self:?} and {other:?}");
            Ty::type_error()
        }, Ty::Bool)
    }
    pub fn compare(&self, other: &Self) -> Result<Option<bool>, ()> {
        // println!("Comparing {self:?} with {other:?}");
        match (self, other) {
            (Ty::Bool(a), Ty::Bool(b)) =>
                Ok(a.zip(*b).map(|(a, b)| a == b)),
            (Ty::Integer(a), Ty::Integer(b)) =>
                Ok(a.as_ref().zip(b.as_ref()).map(|(a, b)| a == b)),
            (Ty::Real(a), Ty::Real(b)) =>
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
            (Ty::Snapshot(a), Ty::Snapshot(b)) => Ok(Some(a == b)),
            (Ty::PredicateId, Ty::PredicateId) => Ok(Some(true)),
            (Ty::TypeError, Ty::TypeError) => Err(()),
            (a, b) if std::mem::discriminant(a) == std::mem::discriminant(b) => unimplemented!("new variant"),
            _ => Err(()),
        }
    }

    pub fn kind(&self) -> TyKind {
        self.ty_default()
    }

    pub fn is_type(&self, ty: TyKind) -> bool {
        std::mem::discriminant(&self.ty_default()) == std::mem::discriminant(&ty)
    }

    pub fn matches(&self, other: &Self) -> Self {
        if std::mem::discriminant(self) == std::mem::discriminant(other) {
            self.ty_default()
        } else {
            TyG::type_error()
        }
    }

    pub fn is_true(&self) -> bool {
        matches!(self, Ty::Bool(Some(true)))
    }
    pub fn is_false(&self) -> bool {
        matches!(self, Ty::Bool(Some(false)))
    }

    pub fn is_int_and(&self, f: impl FnOnce(&BigInt) -> bool) -> bool {
        matches!(self, Ty::Integer(Some(i)) if f(i))
    }

    pub fn is_real_and(&self, f: impl FnOnce(&BigRational) -> bool) -> bool {
        matches!(self, TyG::Real(Some(r)) if f(r))
    }
    pub fn is_none(&self) -> bool {
        self.is_real_and(|r| r == &BigRational::from(BigInt::from(0u8)))
    }
    pub fn is_write(&self) -> bool {
        self.is_real_and(|r| r == &BigRational::from(BigInt::from(1u8)))
    }

    pub fn is_null(&self) -> bool {
        matches!(self, Ty::Ref(Some(())))
    }
}

impl Neg for &'_ Ty {
    type Output = Ty;
    fn neg(self) -> Self::Output {
        match self {
            Ty::Integer(r) =>
                Ty::Integer(r.as_ref().map(<&BigInt as Neg>::neg)),
            Ty::Real(r) =>
                Ty::Real(r.as_ref().map(<&BigRational as Neg>::neg)),
            _ => Ty::type_error(),
        }
    }
}

impl Not for &'_ Ty {
    type Output = Ty;
    fn not(self) -> Self::Output {
        match self {
            Ty::Bool(b) => Ty::Bool(b.map(bool::not)),
            _ => Ty::type_error(),
        }
    }
}

fn number_binop_fn<Integer, Real>(op: &str, a: &Ty, b: &Ty,
        int_op: fn(&BigInt, &BigInt) -> Result<Integer, ()>,
        int_ty: fn(Option<Integer>) -> Ty,
        real_op: fn(&BigRational, &BigRational) -> Result<Real, ()>,
        real_ty: fn(Option<Real>) -> Ty,
    ) -> Ty {
    match (a, b) {
        (Ty::Integer(a), Ty::Integer(b)) => {
            let r = a.as_ref().zip(b.as_ref())
                .map(|(a, b)| int_op(a, b));
            r.transpose().map_or_else(|()| Ty::type_error(), int_ty)
        }
        (Ty::Real(a), Ty::Real(b)) => {
            let r = a.as_ref().zip(b.as_ref())
                .map(|(a, b)| real_op(a, b));
            r.transpose().map_or_else(|()| Ty::type_error(), real_ty)
        }
        _ => {
            panic!("Expected numbers for {op}, got {a:?} and {b:?}");
            Ty::type_error()
        }
    }
}

macro_rules! impl_binop {
    ($trait:ident, $method:ident, $op:tt$(($d:tt))?, $variant:ident) => {
        impl $trait for &'_ Ty {
            type Output = Ty;
            fn $method(self, other: Self) -> Self::Output {
                match (self, other) {
                    (Ty::$variant(a), Ty::$variant(b)) =>
                        Ty::$variant(a.as_ref().zip(b.as_ref()).map(|(a, b)| $($d)?a $op $($d)?b)),
                    _ => Ty::type_error(),
                }
            }
        }
    };
}

// impl_binop!(Add, add, +, Real);
// impl_binop!(Mul, mul, *, Real);
impl_binop!(BitAnd, bitand, &&(*), Bool);
impl_binop!(BitOr, bitor, ||(*), Bool);

impl Add for &'_ Ty {
    type Output = Ty;
    fn add(self, other: Self) -> Self::Output {
        number_binop_fn("add", self, other,
            |a, b| Ok(a + b),
            Ty::Integer,
            |a, b| Ok(a + b),
            Ty::Real,
        )
    }
}
impl Mul for &'_ Ty {
    type Output = Ty;
    fn mul(self, other: Self) -> Self::Output {
        number_binop_fn("mul", self, other,
            |a, b| Ok(a * b),
            Ty::Integer,
            |a, b| Ok(a * b),
            Ty::Real,
        )
    }
}

impl Div for &'_ Ty {
    type Output = Ty;
    fn div(self, other: Self) -> Self::Output {
        number_binop_fn("div", self, other,
            |a, b|
                (b != &BigInt::from(0u8)).then(||
                    BigRational::new_raw(a.clone(), b.clone())
                ).ok_or(()),
            Ty::Real,
            |a, b|
                (b.numer() != &BigInt::from(0u8)).then(|| a / b).ok_or(()),
            Ty::Real,
        )
    }
}

impl Analysis<Exp> for Meaning {
    type Data = Ty;

    fn make(egraph: &egg::EGraph<Exp, Self>, enode: &Exp) -> Self::Data {
        use silver_oxide::parse::ConstKind;
        match enode {
            Exp::Const(c) => match c {
                ConstKind::Bool(b) => Ty::Bool(Some(*b)),
                ConstKind::Real(r) => Ty::Real(Some(BigRational::from(r.clone()))),
                ConstKind::Int(n) => Ty::Integer(Some(BigInt::from(n.clone()))),
                ConstKind::Epsilon | ConstKind::Wildcard => Ty::Real(None),
                ConstKind::Null => Ty::Ref(Some(())),
                ConstKind::Heap(_) => todo!(),
                // ConstKind::None => Ty::Real(Some(BigRational::from(BigInt::ZERO))),
                // ConstKind::Write => Ty::Real(Some(BigRational::from(BigInt::from(1u8)))),
            },
            Exp::UnOp(unop, e) => match unop {
                UnOp::Neg =>  - &egraph[*e].data,
                UnOp::Not => ! &egraph[*e].data,
                UnOp::IntToReal => match &egraph[*e].data {
                    Ty::Integer(i) =>
                        Ty::Real(i.as_ref().map(|i| BigRational::from_integer(i.clone()))),
                    _ => Ty::type_error(),
                },
            },
            Exp::BinOp(binop, [a, b]) => match binop {
                BinOp::Plus => &egraph[*a].data + &egraph[*b].data,
                BinOp::Mult => &egraph[*a].data * &egraph[*b].data,
                BinOp::Div => &egraph[*a].data / &egraph[*b].data,
                BinOp::Lt => number_binop_fn("lt", &egraph[*a].data, &egraph[*b].data,
                    |a, b| Ok(a < b),
                    Ty::Bool,
                    |a, b| Ok(a < b),
                    Ty::Bool,
                ),
                BinOp::Eq => egraph[*a].data.eq_(&egraph[*b].data),
                // Not strictly necessary as this should get simplified anyway
                BinOp::And => &egraph[*a].data & &egraph[*b].data,
                BinOp::Or => &egraph[*a].data | &egraph[*b].data,
                BinOp::Mod => todo!(),
            }
            Exp::Ternary([c, t, e]) => match (&egraph[*c].data, &egraph[*t].data, &egraph[*e].data) {
                (Ty::Bool(_), t, e) => t.matches(e),
                _ => Ty::type_error(),
            },
            Exp::Project(s, _, ty) => match &egraph[*s].data {
                Ty::Snapshot(_) => ty.ty_default(),
                _ => Ty::type_error(),
            },
            Exp::Snapshot(_, eid) => Ty::Snapshot(*eid),
            Exp::PredicateApp(..) => Ty::PredicateId,

            Exp::FuncApp(.., ty) => ty.ty_default(),
            Exp::SymbolicValue(symbolic_value) => todo!(),//symbolic_value.2.ty_default(),
            // Exp::TyCast(v, from, to) => match (&egraph[*v].data, to) {
            //     (f, _)
            //         if std::mem::discriminant(&f.ty_default()) != std::mem::discriminant(from) => Ty::TypeError,
            //     (_, TyKind::Snapshot(())) => Ty::Snapshot(Some(0)),
            //     (Ty::Snapshot(_), to) => to.ty_default(),
            //     (Ty::Real(f), TyKind::Integer(()))
            //         => Ty::Integer(f.as_ref().map(|f| f.numer() / f.denom())),
            //     (Ty::Integer(i), TyKind::Real(()))
            //         => Ty::Real(i.as_ref().map(|i| BigRational::from_integer(i.clone()))),
            //     _ => Ty::TypeError,
            // }
            _ => todo!(),
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
                (Ty::Integer(a), Ty::Integer(b)) =>
                    merge_option(a, b),
                (Ty::Real(a), Ty::Real(b)) =>
                    merge_option(a, b),
                (Ty::Ref(a), Ty::Ref(b)) =>
                    merge_option(a, b),
                (Ty::Snapshot(a), Ty::Snapshot(b)) =>
                    (*a == b).then(|| egg::DidMerge(false, false)).ok_or(()),
                (Ty::PredicateId, Ty::PredicateId) => Ok(egg::DidMerge(false, false)),
                (Ty::TypeError, Ty::TypeError) => Ok(egg::DidMerge(false, false)),
                _ => todo!("new variant"),
            }
        };
        if let Ok(merge) = merge {
            return merge;
        }
        *a = Ty::type_error();
        egg::DidMerge(!a_error, !b_error)
    }

    fn modify(egraph: &mut egg::EGraph<Exp, Self>, id: egg::Id) {
        use silver_oxide::parse::ConstKind;
        match &egraph[id].data {
            Ty::Bool(Some(b)) => {
                let c = egraph.add(Exp::Const(ConstKind::Bool(*b)));
                egraph.union_trusted(id, c, "same meaning");
            }
            Ty::Integer(Some(i)) => {
                let i = egraph.add(Exp::Const(ConstKind::Int(i.clone())));
                egraph.union_trusted(id, i, "same meaning");
            }
            Ty::Real(Some(r)) => {
                let (numer, denom) = (r.numer().clone(), (!r.is_integer()).then(|| r.denom().clone()));
                let mut div = egraph.add(Exp::Const(ConstKind::Int(numer)));
                if let Some(denom) = denom {
                    let denom = egraph.add(Exp::Const(ConstKind::Int(denom)));
                    div = egraph.add(Exp::BinOp(BinOp::Div, [div, denom]));
                } else {
                    div = egraph.add(Exp::UnOp(UnOp::IntToReal, div));
                }
                egraph.union_trusted(id, div, "same meaning");
            }
            Ty::Ref(Some(())) => {
                let c = egraph.add(Exp::Const(ConstKind::Null));
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
//             (Ty::Real(a), Ty::Real(b)) => Some(a == b),
//             _ => None,
//         }
//     }
// }

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ty: TyKind = self.kind();
        write!(f, "{ty}")?;
        match self {
            Ty::Bool(Some(b)) => write!(f, "{{{b}}}"),
            Ty::Integer(Some(i)) => write!(f, "{{{i}}}"),
            Ty::Real(Some(r)) => write!(f, "{{{r}}}"),
            Ty::Ref(Some(())) => write!(f, "{{null}}"),
            _ => Ok(()),
        }
    }
}

impl<'a> From<&'a silver_oxide::parse::Type> for TyKind {
    fn from(value: &'a silver_oxide::parse::Type) -> Self {
        use silver_oxide::parse::Type;
        match value {
            Type::Bool => TyKind::Bool(()),
            Type::Int => TyKind::Integer(()),
            Type::Real => TyKind::Real(()),
            Type::Ref => TyKind::Ref(()),
            Type::Domain(_, _) => todo!("{value:?}"),
        }
    }
}

// impl<'a> From<&'a silver_oxide::intern::Ty> for TyKind {
//     fn from(value: &'a silver_oxide::intern::Ty) -> Self {
//         use silver_oxide::intern::{Ty, ResourceKind};
//         match value {
//             Ty::Bool => TyKind::Bool(()),
//             Ty::Int => TyKind::Integer(()),
//             Ty::Real => TyKind::Real(()),
//             Ty::Ref => TyKind::Ref(()),
//             Ty::Domain(_, _) => todo!("{value:?}"),
//             Ty::Resource(ResourceKind::Field(_)) => todo!(),
//             Ty::Resource(ResourceKind::Compound(rid)) => TyKind::Snapshot(*rid),
//         }
//     }
// }
