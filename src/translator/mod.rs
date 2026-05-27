pub mod function;
pub mod method;
pub mod predicate;
pub mod resource;

use std::{ops::{Deref, Index}, path::PathBuf};

use function::{FunctionVerified};
use method::{MethodVerified};
use predicate::{PredicateVerified};

use resource::{EggResourceExp, SelfFraming};
use silver_oxide::{translate::{CompoundId, DefId, LocalDefId, TyKind}, TiVec, TyCtxt};

pub struct VerificationState<'tcx> {
    tcx: &'tcx TyCtxt<'tcx>,
    analysis: TiVec<LocalDefId, Option<VerifiedMember<'tcx>>>,
}

impl<'tcx> Deref for VerificationState<'tcx> {
    type Target = TyCtxt<'tcx>;
    fn deref(&self) -> &Self::Target {
        self.tcx()
    }
}

impl<'tcx, Id> Index<Id> for VerificationState<'tcx> where DefId: From<Id> {
    type Output = Option<VerifiedMember<'tcx>>;
    fn index(&self, def_id: Id) -> &Self::Output {
        let Some(def_id) = DefId::from(def_id).as_local() else {
            return &None;
        };
        &self.analysis[def_id]
    }
}

#[derive(Debug)]
pub enum VerifiedMember<'tcx> {
    Function(FunctionVerified<'tcx>),
    Method(MethodVerified),
    Predicate(Option<EggResourceExp<'tcx>>),
    Other,
}

impl<'tcx> VerificationState<'tcx> {
    fn new(tcx: &'tcx TyCtxt<'tcx>) -> Self {
        VerificationState {
            tcx,
            analysis: tcx.mk_member_vec(|_| None),
        }
    }

    pub fn tcx(&self) -> &'tcx TyCtxt<'tcx> {
        self.tcx
    }

    pub fn verify<'a>(program: &'tcx silver_oxide::Silver<'tcx>) {
        let mut self_ = VerificationState::new(&program.tcx);
        for mref in self_.tcx.members_topo() {
            let Some(did) = mref.id.as_local() else {
                continue;
            };
            self_.verify_member(did, mref.kind);
        }
    }

    fn verify_member(&mut self, did: LocalDefId, body: bool) {
        let member = self.tcx.member(did);
        use silver_oxide::translate::member::Member::*;
        let vm = match member {
            Import | Define | Field => unreachable!(),
            Domain | DomainFunction(..) => VerifiedMember::Other,
            DomainAxiom(..) => todo!(),
            Predicate(re) => {
                let sf = re.as_ref().map(|re| {
                    let snap = Err(CompoundId { did: did.into(), contract: None });
                    let mut re = self.verify_self_framing(re, snap);
                    re.silicon.log_pure(self.tcx.item_name(did).unwrap().as_str(), None);
                    re.resource
                });
                VerifiedMember::Predicate(sf)
            }
            Function(pre, post, body) => {
                // TODO: improve
                let snap = pre.locals.as_ref().unwrap().last_key_value()
                    .filter(|(_, ty)| matches!(ty.kind(), TyKind::Compound(_)))
                    .map(|(local, _)| local);

                let mut pre = self.verify_self_framing(pre, Ok(snap));
                let post = pre.silicon.translate_exp(post).0;
                pre.silicon.log_pure(self.tcx.item_name(did).unwrap().as_str(), None);
                let body = body.as_ref().map(|b| pre.silicon.translate_exp(b).0);
                pre.silicon.log_pure(self.tcx.item_name(did).unwrap().as_str(), None);
                todo!()
            }
            Method(..) => todo!(),
        };
        self.analysis[did] = Some(vm);
    }

    // fn verify_declaration(&mut self, decl: &'a Declaration, log_dir: &PathBuf) -> CallableVerified<'a> {
    //     eprintln!("### Verifying `{:?}` ###", decl.idn_decl());
    //     match decl {
    //         Declaration::Function(f) => {
    //             let f = Function::verify(f, self, log_dir).unwrap();
    //             CallableVerified::Function(f)
    //         }
    //         Declaration::Method(m) => {
    //             let m = Method::verify(m, self, log_dir).unwrap();
    //             CallableVerified::Method(m)
    //         }
    //         Declaration::DomainElement(_) => CallableVerified::DomainFunction,
    //         Declaration::Predicate(p) => {
    //             let p = Predicate::verify(p, self, log_dir).unwrap();
    //             CallableVerified::Predicate(p)
    //         }
    //         _ => CallableVerified::Other,
    //     }
    // }

    // pub fn resolver(&self, def_id: DefIdC) -> &DefResolver {
    //     &self.resolved[def_id.def_id]
    // }
    // pub fn get_callable_with_sig(&self, def_id: DefIdC, ident: &Ident) -> (CallableDecl, &'a Signature) {
    //     self.silver.resolved.get_callable_with_sig(def_id.def_id, ident)
    // }

    pub fn get_fn_analysis(&self, callee: DefId) -> Option<&FunctionVerified<'tcx>> {
        self[callee].as_ref().and_then(|v| match v {
            VerifiedMember::Function(f) => Some(f),
            _ => None,
        })
    }
}
