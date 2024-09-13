use std::fmt;

use crate::{error::Error, pure::EGraph};

pub type Snapshot = egg::Id;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Exp {
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
    FuncApp(silver_oxide::ast::Ident, Vec<egg::Id>),
    /// Should never have parents!
    PredicateApp(silver_oxide::ast::Ident, Vec<egg::Id>),
    SymbolicValue(SymbolicValue),
    BinOp(BinOp, [egg::Id; 2]),
    Ternary([egg::Id; 3]),
    // Field(Box<Exp>, Ident),
    // Index(Box<Exp>, Box<IndexOp>),
    UnOp(UnOp, egg::Id),
    // InhaleExhale(Box<Exp>, Box<Exp>),
    Snapshot(Vec<egg::Id>),
    Project(egg::Id, usize),
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolicValue(pub u64, pub Option<String>);

impl fmt::Display for Exp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Exp::Const(c) => write!(f, "{c:?}"),
            Exp::FuncApp(i, args) | Exp::PredicateApp(i, args) => {
                write!(f, "{}(", i.0)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "#{arg}")?;
                }
                write!(f, ")")
            }
            Exp::SymbolicValue(SymbolicValue(sv, None)) => write!(f, "@{sv}"),
            Exp::SymbolicValue(SymbolicValue(sv, Some(name))) => write!(f, "{name}@{sv}"),
            Exp::BinOp(op, [l, r]) => write!(f, "(#{} {:?} #{})", l, op, r),
            Exp::Ternary([c, t, e]) => write!(f, "(#{} ? #{} : #{})", c, t, e),
            Exp::UnOp(op, e) => write!(f, "{op}#{}", e),
            Exp::Snapshot(es) => {
                write!(f, "snap(")?;
                for (i, e) in es.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "#{e}")?;
                }
                write!(f, ")")
            }
            Exp::Project(e, i) => write!(f, "#{e}[{i}]"),
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
