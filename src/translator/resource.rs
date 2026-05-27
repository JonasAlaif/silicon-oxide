use silver_oxide::{translate::{exp::{ExpCondAny, ExpCondsAny}, resource::ResourceExp, CompoundId, CompoundIdx, Local, Ty, TyKind}, TiVec};

use crate::{pure::{EggEGraph, PathCondition}, silicon::Silicon};

use super::VerificationState;

pub struct SelfFraming<'a, 'tcx> {
    pub resource: EggResourceExp<'tcx>,
    pub silicon: Silicon<'a, 'tcx>,
}

#[derive(Debug)]
pub struct EggResourceExp<'tcx> {
    pub locals: Option<TiVec<Local, Ty<'tcx>>>,
    pub resources: TiVec<CompoundIdx, EggResource>,
    pub pure: egg::Id,
    pub egraph: EggEGraph,
}

#[derive(Debug)]
pub struct EggResource {
    pub cond: PathCondition,
    pub loc: egg::Id,
    pub perm: egg::Id,
}

pub type EggExpConds = ExpCondsAny<egg::Id>;

pub type EggExpCond = ExpCondAny<egg::Id>;

impl<'tcx> VerificationState<'tcx> {
    pub fn verify_self_framing<'a>(&'a self, re: &ResourceExp<'tcx>, snap: Result<Option<Local>, CompoundId>) -> SelfFraming<'a, 'tcx> {
        eprintln!("Verifying self-framing:\n{re:?}");
        let mut silicon = Silicon::new(self);
        if let Some(locals) = &re.locals {
            silicon.register_locals(locals);
        }
        let snap = snap.map(|local| local.map(|l| silicon.value_state.locals[l]));
        let resource = silicon.translate_exp_resource(re, snap);
        SelfFraming { resource, silicon }
    }
}
