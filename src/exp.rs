use std::fmt;

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
    SymbolicValue(SymbolicValue),
    BinOp(silver_oxide::ast::BinOp, [egg::Id; 2]),
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
            Exp::FuncApp(i, args) => {
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
