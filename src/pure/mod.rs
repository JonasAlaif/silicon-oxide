mod pure;
// mod meaning;
mod r#const;
// mod z3;
mod exp;
mod cond;

pub use pure::*;
// pub use meaning::*;
// pub use meaning2::*;
// pub use z3::*;
pub use exp::*;
pub use r#const::*;
pub use cond::*;

use silver_oxide::translate::{Const, Ty};

pub type EggEGraph = egg::EGraph<EggExp, EggAnalysis>;

/// An unsafe const that has lost the `'tcx` lifetime, only to be used when stored e.g. in an egraph
type SConst = Const<'static>;

/// An unsafe ty that has lost the `'tcx` lifetime, only to be used when stored e.g. in an egraph
type STy = Ty<'static>;
