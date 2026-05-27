use std::{fmt, ops::Index, ptr::NonNull};

use crate::HashMap;
use silver_oxide::parse;

use crate::pure::TyKind;

use super::DefId;


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpId(pub(super) DefId, pub(super) u32);

impl fmt::Display for ExpId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}e{}", self.0, self.1)
    }
}

#[derive(Default, Debug)]
pub struct ExpData<'a> {
    exp_to_id: FxHashMap<NonNull<parse::Exp>, ExpId>,
    id_to_exp: FxHashMap<ExpId, &'a parse::Exp>,
    exp_to_perm_kind: FxHashMap<NonNull<parse::Exp>, TyKind>,
}

impl<'a> ExpData<'a> {
    pub fn get_perm_kind(&self, exp: &parse::Exp) -> Option<TyKind> {
        self.exp_to_perm_kind.get(&NonNull::from(exp)).copied()
    }

    pub fn get_eid(&self, exp: &parse::Exp) -> Option<ExpId> {
        self.exp_to_id.get(&NonNull::from(exp)).copied()
    }

    pub fn insert(&mut self, exp: &'a parse::Exp, id: ExpId) {
        self.exp_to_id.insert(NonNull::from(exp), id);
        self.id_to_exp.insert(id, exp);
    }

    pub fn init_perm_kind(&mut self, f: impl Fn(&Self, &'a parse::Exp) -> Option<(&'a parse::Exp, TyKind)>) {
        for e in self.id_to_exp.iter().map(|(_, &v)| v) {
            if let Some((e, kind)) = f(self, e) {
                self.exp_to_perm_kind.insert(NonNull::from(e), kind);
            }
        }
    }
}

impl Index<ExpId> for ExpData<'_> {
    type Output = parse::Exp;
    fn index(&self, index: ExpId) -> &Self::Output {
        self.id_to_exp[&index]
    }
}
