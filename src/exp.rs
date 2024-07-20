use std::fmt;

use crate::pure::EGraph;

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
    PredicateApp(silver_oxide::ast::Ident, Vec<egg::Id>),
    SymbolicValue(SymbolicValue),
    BinOp(BinOp, [egg::Id; 2]),
    Ternary([egg::Id; 3]),
    // Field(Box<Exp>, Ident),
    // Index(Box<Exp>, Box<IndexOp>),
    Neg(egg::Id),
    Not(egg::Id),
    // InhaleExhale(Box<Exp>, Box<Exp>),
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
            Exp::SymbolicValue(SymbolicValue(sv, Some(name))) => write!(f, "@{sv}@{name}"),
            Exp::BinOp(op, [l, r]) => write!(f, "(#{} {:?} #{})", l, op, r),
            Exp::Ternary([c, t, e]) => write!(f, "(#{} ? #{} : #{})", c, t, e),
            Exp::Neg(e) => write!(f, "-#{}", e),
            Exp::Not(e) => write!(f, "!#{}", e),
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
                lhs = egraph.add(Exp::Not(lhs));
                BinOp::Or
            }
            Iff => BinOp::Eq,
            And => BinOp::And,
            Or => BinOp::Or,
            Eq => BinOp::Eq,
            Neq => {
                let exp = egraph.add(Exp::BinOp(BinOp::Eq, [lhs, rhs]));
                return egraph.add(Exp::Not(exp));
            }
            Lt => BinOp::Lt,
            Le => {
                let lt = egraph.add(Exp::BinOp(BinOp::Lt, [lhs, rhs]));
                let eq = egraph.add(Exp::BinOp(BinOp::Eq, [lhs, rhs]));
                return egraph.add(Exp::BinOp(BinOp::Or, [lt, eq]));
            }
            Gt => {
                (lhs, rhs) = (rhs, lhs);
                BinOp::Lt
            }
            Ge => {
                (lhs, rhs) = (rhs, lhs);
                let lt = egraph.add(Exp::BinOp(BinOp::Lt, [lhs, rhs]));
                let eq = egraph.add(Exp::BinOp(BinOp::Eq, [lhs, rhs]));
                return egraph.add(Exp::BinOp(BinOp::Or, [lt, eq]));
            }
            Plus => BinOp::Plus,
            Minus => {
                rhs = egraph.add(Exp::Neg(rhs));
                BinOp::Plus
            }
            Mult => BinOp::Mult,
            Div => BinOp::Div,
            Mod => BinOp::Mod,
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
