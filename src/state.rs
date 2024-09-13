use std::path::PathBuf;

use fxhash::FxHashMap;
use silver_oxide::ast;

use crate::{declarations::{CallableDecl, Declarations}, error::Error, exp::{PathCondition, Snapshot}, heap::{Heap, Mutation}, pure::EGraph, translate::{TranslationMode}};

#[derive(Debug, Clone)]
pub struct ValueState {
    pub egraph: EGraph,
    pub heap: Heap,
    mutation_id: Mutation,
}

impl ValueState {
    pub fn new() -> Self {
        Self {
            egraph: EGraph::default(),
            heap: Heap::default(),
            mutation_id: Default::default(),
        }
    }

    // pub fn new_binding(&mut self, binding: &'e ast::Ident) {
    //     let id = self.egraph.next_symbolic_value(Some(binding.0.clone()));
    //     self.bindings.insert(Some(binding), id);
    // }
    pub fn mutation_id(&mut self) -> Mutation {
        let id = self.mutation_id;
        self.mutation_id += 1;
        id
    }

    // pub fn inhale(&mut self, exp: &'e ast::Exp, reason: impl Into<egg::Symbol> + Clone, pc: PathCondition) -> Result<Snapshot, Error<'e>> {
    //     self.inhale_many([exp].into_iter(), reason, pc)
    // }
    // pub fn inhale_many(&mut self, exp: impl Iterator<Item = &'e ast::Exp>, reason: impl Into<egg::Symbol> + Clone, pc: PathCondition) -> Result<Snapshot, Error<'e>> {
    //     self.inhale_inner(exp, reason, TranslationMode::Mutating { exhale: false, perm_mult: None }, pc, &Default::default())
    // }
    // pub fn inhale_with_args(&mut self, exp: &'e ast::Exp, reason: impl Into<egg::Symbol> + Clone, perm_mult: Option<egg::Id>, pc: PathCondition, args: &Args<'e>) -> Result<Snapshot, Error<'e>> {
    //     self.inhale_inner([exp].into_iter(), reason, TranslationMode::Mutating { exhale: false, perm_mult }, pc, args)
    // }

    // pub fn assume(&mut self, exp: &'e ast::Exp, reason: impl Into<egg::Symbol> + Clone, pc: PathCondition) -> Result<Snapshot, Error<'e>> {
    //     self.assume_with_args(exp, reason, pc, &Default::default())
    // }
    // pub fn assume_with_args(&mut self, exp: &'e ast::Exp, reason: impl Into<egg::Symbol> + Clone, pc: PathCondition, args: &Args<'e>) -> Result<Snapshot, Error<'e>> {
    //     self.assume_many_with_args([exp].into_iter(), reason, pc, args)
    // }
    // pub fn assume_many_with_args(&mut self, exp: impl Iterator<Item = &'e ast::Exp>, reason: impl Into<egg::Symbol> + Clone, pc: PathCondition, args: &Args<'e>) -> Result<Snapshot, Error<'e>> {
    //     self.inhale_inner(exp, reason, TranslationMode::Fact, pc, args)
    // }

    // fn inhale_inner(&mut self, exp: impl Iterator<Item = &'e ast::Exp>, reason: impl Into<egg::Symbol> + Clone, mode: TranslationMode, pc: PathCondition, args: &Args<'e>) -> Result<Snapshot, Error<'e>> {
    //     let mut data = TranslationData { pc, args, mutation: None, snapshot: &mut Default::default() };
    //     for exp in exp {
    //         let exp = self.translate_with_args(exp, mode, data.reborrow())?;
    //         let exp = pc.condition(&mut self.egraph, exp);
    //         self.egraph.assume(exp, reason.clone());
    //         self.egraph.rebuild();
    //     }
    //     Ok(data.snapshot(&mut self.egraph))
    // }

    // pub fn exhale(&mut self, exp: &'e ast::Exp, pc: PathCondition) -> Result<Snapshot, Error<'e>> {
    //     self.exhale_many([exp].into_iter(), pc)
    // }
    // pub fn exhale_many(&mut self, exp: impl Iterator<Item = &'e ast::Exp>, pc: PathCondition) -> Result<Snapshot, Error<'e>> {
    //     self.exhale_inner(exp, TranslationMode::Mutating { exhale: true, perm_mult: None }, pc, &Default::default())
    // }
    // pub fn exhale_with_args(&mut self, exp: &'e ast::Exp, perm_mult: Option<egg::Id>, pc: PathCondition, args: &Args<'e>) -> Result<Snapshot, Error<'e>> {
    //     self.exhale_inner([exp].into_iter(), TranslationMode::Mutating { exhale: true, perm_mult }, pc, args)
    // }

    // pub fn assert(&mut self, exp: &'e ast::Exp, pc: PathCondition) -> Result<Snapshot, Error<'e>> {
    //     self.assert_with_args(exp, pc, &Default::default())
    // }
    // pub fn assert_with_args(&mut self, exp: &'e ast::Exp, pc: PathCondition, args: &Args<'e>) -> Result<Snapshot, Error<'e>> {
    //     self.assert_many_with_args([exp].into_iter(), pc, args)
    // }
    // pub fn assert_many_with_args(&mut self, exp: impl Iterator<Item = &'e ast::Exp>, pc: PathCondition, args: &Args<'e>) -> Result<Snapshot, Error<'e>> {
    //     self.exhale_inner(exp, TranslationMode::Fact, pc, args)
    // }

    // fn exhale_inner(&mut self, exp: impl Iterator<Item = &'e ast::Exp>, mode: TranslationMode, pc: PathCondition, args: &Args<'e>) -> Result<Snapshot, Error<'e>> {
    //     let mutation = matches!(mode, TranslationMode::Mutating { exhale: true, .. }).then(|| self.mutation_id());
    //     let mut data = TranslationData { pc, args, mutation, snapshot: &mut Default::default() };
    //     for exp in exp {
    //         let assertion = self.translate_with_args(exp, mode, data.reborrow())?;
    //         pc.check(&mut self.egraph, assertion).map_err(|assertion| {
    //             let label = Some(format!("{}", self.egraph.normalise(assertion)));
    //             self.log_pure("exhale", label);
    //             Error::exhale(exp, assertion)
    //         })?;
    //     }
    //     Ok(data.snapshot(&mut self.egraph))
    // }

    // pub fn fold(&mut self, exp: &'e ast::Exp, pc: PathCondition) -> Result<(), Error<'e>> {
    //     let (args, perm_mult, body) = self.get_predicate(exp, pc)?;
    //     let exhaled_snap = self.exhale_with_args(body, perm_mult, pc, &args)?;
    //     let inhaled_snap = self.inhale(exp, "", pc)?.expect("internal error");
    //     self.equate_snapshots(exhaled_snap, inhaled_snap, "fold");
    //     Ok(())
    // }
    // pub fn unfold(&mut self, exp: &'e ast::Exp, pc: PathCondition) -> Result<(), Error<'e>> {
    //     let (args, perm_mult, body) = self.get_predicate(exp, pc)?;
    //     let exhaled_snap = self.exhale(exp, pc)?.expect("internal error");
    //     let inhaled_snap = self.inhale_with_args(body, "", perm_mult, pc, &args)?;
    //     self.equate_snapshots(inhaled_snap, exhaled_snap, "unfold");
    //     Ok(())
    // }
    // fn equate_snapshots(&mut self, snap1: Snapshot, snap2: egg::Id, reason: &str) {
    //     println!("EQUATE: {:#?} = {:#?}", snap1, snap2);
    //     let Some(snap1) = snap1 else {
    //         return;
    //     };
    //     self.egraph.equate(snap1, snap2, reason)
    // }

    // pub(super) fn get_predicate(&mut self, exp: &'e ast::Exp, pc: PathCondition) -> Result<(Args<'e>, Option<egg::Id>, &'e ast::Exp), Error<'e>> {
    //     let (loc, perm) = match exp {
    //         ast::Exp::Acc(acc) => (&acc.acc.loc, &acc.perm),
    //         _ => (exp, &None),
    //     };
    //     self.get_predicate_inner(loc, perm, pc)
    // }
    // pub(super) fn get_predicate_inner(&mut self, loc: &'e ast::Exp, perm: &'e Option<ast::Exp>, pc: PathCondition) -> Result<(Args<'e>, Option<egg::Id>, &'e ast::Exp), Error<'e>> {
    //     let ast::Exp::FuncApp(name, args) = loc else {
    //         panic!("{loc:?} not a predicate fn application")
    //     };
    //     let decl = self.declarations.callable.get(name).expect("undeclared function");
    //     let CallableDecl::Predicate(pred) = decl else {
    //         panic!("{name:?} not a predicate")
    //     };
    //     assert_eq!(args.len(), pred.signature.args.len());
    //     let args = pred.signature.args.iter().zip(args.iter()).map(|(arg, exp)| {
    //         let ast::ArgOrType::Arg((arg, _)) = arg else {
    //             panic!()
    //         };
    //         let id = self.translate_exp(exp, pc)?;
    //         Ok((Some(arg), id))
    //     }).collect::<Result<_, _>>()?;
    //     let perm_mult = perm.as_ref().map(|perm| self.translate_exp(perm, pc)).transpose()?;
    //     let body = pred.body.as_ref().expect("opaque predicate");
    //     Ok((args, perm_mult, body))
    // }
}
