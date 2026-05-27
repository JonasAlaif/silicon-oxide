use egg::Language;
use num::BigInt;
use silver_oxide::{intern::{CallableDeclKind, DefIdC, ExpId, ResId, TyId}, parse};

use crate::{error::Error, exp::{BinOp, Exp, PathCondition, UnOp}, heap::{Mutation, PermissionConfig, Temporary}, pure::{EGraph, Ty, TyKind}, silicon::{Bindings, Silicon, StmtState}, state::ValueState, translator::VerificationState};

impl<'a, 'e> StmtState<'a, 'e> {
    pub fn translator(&self, mode: TranslationMode) -> Translator<'_, 'e> {
        Translator::new(self.pc, mode, &self.bindings, self.decls)
    }
}

impl<'a, 'e> Silicon<'a, 'e> {
    pub fn translate_for_inhale(&mut self, exp: &'e parse::Exp) -> Result<TranslationResult, Error<'e>> {
        let eid = ResId::from(exp);
            let tys = &self.stmt_state.decls.resolved[eid];
            assert!(tys.len() > 0);
        let assert_snapshot = self.value_state.egraph.next_symbolic_value(Some("$INHALE".into()), TyKind::Snapshot(eid));
        let translator = self.stmt_state.translator(TranslationMode::Inhale)
            .set_assert_snapshot(assert_snapshot, true);
        translator.translate(exp, &mut self.value_state)
    }
    pub fn translate_for_exhale(&mut self, exp: &'e parse::Exp) -> Result<TranslationResult, Error<'e>> {
        let mutation = self.value_state.mutation_id();
        let translator = self.stmt_state.translator(TranslationMode::Exhale)
            .set_mutation(mutation);
        translator.translate(exp, &mut self.value_state)
    }

    pub fn translate_for_fact(&mut self, exp: &'e parse::Exp) -> Result<TranslationResult, Error<'e>> {
        let translator = self.stmt_state.translator(TranslationMode::Fact);
        translator.translate(exp, &mut self.value_state)
    }

    pub fn translate_for_unfold(&mut self, exp: &'e parse::AccExp) -> Result<egg::Id, Error<'e>> {
        let mutation = self.value_state.mutation_id();
        let translator = self.stmt_state.translator(TranslationMode::Exhale)
            .set_mutation(mutation);
        translator.unfold_predicate(exp, &mut self.value_state)
    }
    pub fn translate_for_fold(&mut self, exp: &'e parse::AccExp) -> Result<egg::Id, Error<'e>> {
        let mutation = self.value_state.mutation_id();
        let translator = self.stmt_state.translator(TranslationMode::Exhale)
            .set_mutation(mutation);
        translator.fold_predicate(exp, &mut self.value_state)
    }

    pub fn translate_exp(&mut self, exp: &'e parse::Exp) -> Result<egg::Id, Error<'e>> {
        let translator = self.stmt_state.translator(TranslationMode::Expression);
        Ok(translator.translate(exp, &mut self.value_state)?.expression)
    }
    pub fn translate_exp_pc(&mut self, exp: &'e parse::Exp, pc: PathCondition) -> Result<egg::Id, Error<'e>> {
        let mut translator = self.stmt_state.translator(TranslationMode::Expression);
        translator.pc = pc;
        Ok(translator.translate(exp, &mut self.value_state)?.expression)
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Permission(Option<egg::Id>);

#[derive(Debug, Clone, Copy)]
pub struct Translator<'a, 'e> {
    pc: PathCondition,
    mode: TranslationMode,
    binds: &'a Bindings<'e>,
    decls: &'a VerificationState<'e>,
    /// Used to mark changes in permission amounts in the heap to figure out
    /// what was mutating in the current translation.
    mutation: Option<Mutation>,
    temporary: Option<Temporary>,
    /// The generated expression will encode equality to this snapshot, useful
    /// for inhaling a predicate body. `bool` indicates if we should project.
    assert_snapshot: Option<(egg::Id, bool)>,
    /// Determines the multiplier for all permissions.
    perm_mult: Permission,
    block_fn_unfold: bool,
    def_id: DefIdC,
    debug: bool,
}

impl<'a, 'e> Translator<'a, 'e> {
    pub fn new(pc: PathCondition, mode: TranslationMode, binds: &'a Bindings<'e>, decls: &'a VerificationState<'e>) -> Self {
        Self { pc, mode, binds, decls, mutation: None, temporary: None, assert_snapshot: None, perm_mult: Permission::default(), block_fn_unfold: false, def_id: decls.def_id(), debug: false }
    }
    pub fn set_mutation(self, mutation: Mutation) -> Self {
        Self { mutation: Some(mutation), ..self }
    }
    pub fn set_assert_snapshot(self, snapshot: egg::Id, project: bool) -> Self {
        assert!(matches!(self.mode, TranslationMode::Inhale), "assert_snapshot is only valid in inhale mode");
        Self { assert_snapshot: Some((snapshot, project)), ..self }
    }
    pub fn set_perm_mult(self, perm_mult: Permission) -> Self {
        Self { perm_mult, ..self }
    }

    pub fn with_mode(self, mode: TranslationMode) -> Self {
        Self { mode, ..self }
    }
    pub fn with_condition(self, egraph: &mut EGraph, condition: egg::Id) -> Self {
        let pc = self.pc.add(egraph, condition);
        Self { pc, ..self }
    }
    pub fn with_condition_negate(self, egraph: &mut EGraph, condition: egg::Id) -> Self {
        let pc = self.pc.add_negate(egraph, condition);
        Self { pc, ..self }
    }
    pub fn with_params(self, params: &'a Bindings<'e>) -> Self {
        Self { binds: params, ..self }
    }
    pub fn with_def_id(self, def_id: DefIdC) -> Self {
        Self { def_id, ..self }
    }
    pub fn temporary_up(self) -> Self {
        let temporary = Some((true, self.temporary.map(|(_, t)| t + 1).unwrap_or_default()));
        Self { temporary, ..self }
    }
    pub fn temporary_down(self) -> Self {
        Self { temporary: Some((false, self.temporary.unwrap().1)), ..self }
    }

    fn next_assert_snapshot(self, egraph: &mut EGraph, translation_state: &mut TranslationState, ty: TyKind) -> Option<egg::Id> {
        self.assert_snapshot.map(|(snap, project)| {
            // println!("\n\nmk_snap({snap}, {})\n", translation_state.assert_snapshot_idx);
            // if format!("{snap}") == "81" {
            //     panic!()
            // }
            let snap = if project {
                egraph.add(Exp::Project(snap, translation_state.assert_snapshot_idx, ty))
            } else {
                assert_eq!(translation_state.assert_snapshot_idx, 0);
                snap
            };
            translation_state.assert_snapshot_idx += 1;
            snap
        })
    }
}

#[derive(Debug, Default)]
pub struct TranslationState {
    pub(super) collect_snapshot: Vec<egg::Id>,
    pub(super) assert_snapshot_idx: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct TranslationResultData<T> {
    pub(super) expression: egg::Id,
    pub(super) snapshot: egg::Id,
    pub(super) data: T,
}

pub type TranslationResult = TranslationResultData<()>;

impl<'a, 'e> Translator<'a, 'e> {
    // TODO: this is only used to extract the individual `collect_snapshot` ids
    // for function unfolding on predicate arguments. Remove this once the
    // function unfolding is done by looking at unfoldings in the body of the
    // fn, rather than just the args.
    fn with_data_translate<T>(self, exp: &'e parse::Exp, value_state: &mut ValueState, f: impl FnOnce(&Vec<egg::Id>) -> T) -> Result<TranslationResultData<T>, Error<'e>> {
        let mut translation_state = TranslationState::default();
        let expression = self.t_inner(exp, value_state, &mut translation_state)?;
        // assert!(!self.assert_snapshot.is_some_and(|(_, expected_children)|
        // expected_children != translation_state.assert_snapshot_idx));
        let data = f(&translation_state.collect_snapshot);

        let eid = ResId::from(exp);
        let tys = &self.decls.resolved[eid];
        assert_eq!(tys.len(), translation_state.collect_snapshot.len());
        assert!(tys.len() > 0);
        let snapshot = value_state.egraph.add_snapshot(translation_state.collect_snapshot, eid);
        if let Some((assert_snapshot, collect)) = self.assert_snapshot {
            assert!(collect);
            value_state.egraph.equate(assert_snapshot, snapshot, "assert_snapshot");
            // TODO: remove
            if let Some(e) = value_state.egraph.has_type_error() {
                eprintln!("Type error: {e:?}");
                return Err(Error::Type)
            }
        }
        Ok(TranslationResultData { expression, snapshot, data })
    }

    pub fn translate(self, exp: &'e parse::Exp, value_state: &mut ValueState) -> Result<TranslationResult, Error<'e>> {
        self.with_data_translate(exp, value_state, |_| ())
    }

    fn t_inner_as_exp(self, exp: &'e parse::Exp, value_state: &mut ValueState) -> Result<egg::Id, Error<'e>> {
        self.with_mode(TranslationMode::Expression).t_inner(exp, value_state, &mut Default::default())
    }
    fn t_inner(self, exp: &'e parse::Exp, value_state: &mut ValueState, translation_state: &mut TranslationState) -> Result<egg::Id, Error<'e>> {
        if self.debug {
            println!("#T-0: {exp:?}");
        }
        use parse::ExpKind::*;
        use parse::BinOp;
        // Hopefully this gets inlined
        let exp = match &**exp {
            Result => return self.binds.get(&None).copied().ok_or_else(|| panic!("Found `result` outside of postcondition of function call")),
            Const(c) => Exp::Const(c.clone()),
            Ident(i) => {
                return self.binds
                    .get(&Some(i))
                    .copied()
                    .ok_or(Error::undeclared_variable(i))
            }
            BinOp(BinOp::MagicWand, ..) => todo!(),
            BinOp(op, l, r) => {
                return self.translate_binop(*op, l, r, value_state)
            }
            Ternary(c, t, f) => {
                let c = self.t_inner(c, value_state, translation_state)?;
                let t = self.with_condition(&mut value_state.egraph, c)
                    .t_inner(t, value_state, translation_state)?;
                let f = self.with_condition_negate(&mut value_state.egraph, c)
                    .t_inner(f, value_state, translation_state)?;
                Exp::Ternary([c, t, f])
            }
            UnOp(op, exp) => return self.translate_unop(*op, exp, value_state),
            FuncApp(name, args) => {
                let (callable, sig) = self.decls.get_callable_with_sig(self.def_id, name);
                let decl = &self.decls.silver().program[callable.def_id];
                
                // let declaration = &self.decls.callable[def_id];
                match callable.kind {
                    CallableDeclKind::Function => {
                        let (mut args, mut params) = self.translate_params(args, &sig.args, value_state)?;
                        let parse::ArgOrType::Type(ret) = &sig.ret[0] else {
                            unreachable!()
                        };
                        let ty = TyKind::from(ret);
                        
                        let pre = &decl.contract().unwrap().precondition;
                        // TODO: which `pc`` to use here?
                        let mut translator = self.with_mode(TranslationMode::Fact)
                            .with_params(&params);
                        translator.assert_snapshot = None;
                        // translator.debug = true;
                        // println!("Checking precondition for call to {}: {pre:?}", name.0);
                        let pre = translator.with_data_translate(pre, value_state, |s| s.clone())?;
                        // value_state.egraph.log_pure(&format!("_pc/z3_pre_assert_{}", name.0), Some(value_state.egraph.normalise(pre.expression).to_string()));
                        self.pc.assert(&mut value_state.egraph, pre.expression, "call_pre", &self.decls).map_err(|assertion|
                            Error::function_precondition(exp, assertion)
                        )?;
                        args.push(pre.snapshot);
                        // Functions with no heap dependence will not be automatically unfolded
                        let heap_args = Some(pre.data).filter(|data| !data.is_empty());

                        let result = value_state.egraph.add(Exp::FuncApp(name.clone(), args.clone(), ty));
                        if let Some(Some(analysis)) = self.decls.get_fn_analysis(callable.def_id).map(|f| &**f) {
                            assert_eq!(analysis.args.len(), args.len());
                            let body = analysis.egraph.egraph.id_to_expr(analysis.body);
                            let body = value_state.egraph.egraph_union(body, analysis.egraph.next_symbolic_value, &args);
                            value_state.egraph.equate(result, body, "unfold_body");
                        }
                        // if !self.block_fn_unfold || heap_args.is_some() {
                        //     value_state.heap_functions.add(heap_args.unwrap_or_default(), result, name.clone(), args);
                        // }

                        // TODO: improve this hack to unfold fn defns only once
                        // if data.args.is_empty() {
                        //     if let Some(body) = &fun.body {
                        //         let data = TranslationData { pc: data.pc, args: &params, mutation: None, snapshot: &mut Default::default() };
                        //         let body = self.translate_with_args(body, TranslationMode::Expression, data).unwrap();
                        //         self.egraph.equate(result, body, "fn call body");
                        //     }
                        // }

                        params.insert(None, result);
                        let post = &decl.contract().unwrap().postcondition;
                        // TODO: which `pc` to use here?
                        let translator = Translator::new(self.pc, TranslationMode::Expression, &params, self.decls);
                        let post = translator.translate(post, value_state).expect("internal error");

                        // Note: this does not necessarily need to be under
                        // the PC, but this matches Viper behaviour and does
                        // not pollute other branches. Though this cases
                        // some awkwardness when using a function as a
                        // lemma, e.g. `b && lemma(...)` since the lemma
                        // postcondition will be under the PC and thus not
                        // apply everywhere!
                        self.pc.assume(&mut value_state.egraph, post.expression, "fn post");
                        return Ok(result);
                    }
                    CallableDeclKind::Predicate(..) | CallableDeclKind::Field(..) =>
                        return Ok(self.translate_predicate_target(name, args, value_state)?.0),
                    // TODO: domain functions
                    _ => todo!(),
                }
            }
            Acc(acc) => return self.translate_acc_exp(&acc.acc.loc, &acc.perm, value_state, translation_state),
            HeapUpdate(parse::HeapUpdateOp::Unfold, acc, exp) => {
                let Acc(acc) = &**acc else {
                    unreachable!()
                };
                let (body, params, loc, perm, def_id) = self.extract_predicate(acc, value_state)?;
                let self_ = self.temporary_up();
                let body = self_.unfold_inner(acc, body, &params, loc, perm, def_id, value_state)?;
                // See note above for function calls
                self.pc.assume(&mut value_state.egraph, body, "unfolding predicate body");
                let exp = self_.t_inner_as_exp(exp, value_state)?;

                let self_ = self_.temporary_down();
                let tmp = self_.temporary.unwrap().1;
                value_state.heap.kill_temporary(tmp);
                // self_.fold_inner(acc, body, &params, loc, perm, value_state)?;
                return Ok(exp);
            }
            _ => todo!("{exp:?}"),
        };
        Ok(value_state.egraph.add(exp))
    }

    fn translate_params(self, args: &'e Vec<parse::Exp>, formals: &'e Vec<parse::ArgOrType>, value_state: &mut ValueState) -> Result<(Vec<egg::Id>, Bindings<'e>), Error<'e>> {
        let formals = formals.iter().map(|a| {let parse::ArgOrType::Arg(a) = a else { unreachable!() }; &a.idn.0 });
        let args = args.iter()
            .map(|arg| self.t_inner_as_exp(arg, value_state))
            .collect::<core::result::Result<Vec<_>, _>>()?;
        let params: Bindings = formals.map(Some).zip(args.iter().copied()).collect();
        Ok((args, params))
    }

    // /// Translate an expression. This is the function to use if the expression
    // /// doesn't contain any permission (acc/predicate/wand) expressions.
    // pub fn translate_exp(&mut self, exp: &'e parse::Exp, pc: PathCondition) -> Result<egg::Id, Error<'e>> {
    //     self.translate(exp, TranslationMode::Expression, pc, None)
    // }
    // pub fn translate(&mut self, exp: &'e parse::Exp, mode: TranslationMode, pc: PathCondition, mutation: Option<Mutation>) -> Result<egg::Id, Error<'e>> {
    //     let data = TranslationData { pc, args: &Default::default(), mutation, snapshot: &mut Default::default() };
    //     self.translate_with_args(exp, mode, data)
    // }

    // /// The root function which will be called for any translation job.
    // pub fn translate_with_args(&mut self, exp: &'e parse::Exp, mode: TranslationMode, data: TranslationData) -> Result<egg::Id, Error<'e>> {
    //     assert_eq!(matches!(mode, TranslationMode::Mutating { exhale: true, .. }), data.mutation.is_some());
    //     self.t_inner(exp, mode, data)
    // }
    // pub fn translate_field_target(&mut self, r: &'e parse::Exp, field: &'e parse::Ident, pc: PathCondition) -> Result<egg::Id, Error<'e>> {
    //     let data = TranslationData { pc, mutation: None, args: &Default::default(), snapshot: &mut Default::default() };
    //     self.translate_field_target_inner(r, field, data)
    // }











    pub fn translate_predicate_target(self, name: &'e parse::Ident, args: &'e Vec<parse::Exp>, value_state: &mut ValueState) -> Result<(egg::Id, TyId), Error<'e>> {
        let cd = self.decls.resolver(self.def_id).get_callable(name);
        let ty = match cd.kind {
            CallableDeclKind::Predicate(ty) | CallableDeclKind::Field(ty) => ty,
            _ => unreachable!(),
        };

        let args = args.iter()
            .map(|arg| {
                // TODO: there is a bug in Egg where adding the
                // `Exp::PredicateApp` causes a panic at
                // `self.explainfind[usize::from(node1)].node.matches(&self.explainfind[usize::from(node2)].node)`
                // at `egg-0.9.5/src/explain.rs:1122:13`, so pre-normalise here.
                let arg = self.t_inner_as_exp(arg, value_state)?;
                Ok(value_state.egraph.normalise(arg))
            })
            .collect::<core::result::Result<_, _>>()?;
        Ok((value_state.egraph.add(Exp::PredicateApp(name.clone(), args)), ty))
    }

    fn permission_config<T: Copy>(self, t: impl FnOnce() -> T) -> PermissionConfig<T> {
        if let Some(temporary) = self.temporary {
            PermissionConfig::Temporary {
                temporary,
                mutation: self.mutation,
            }
        } else {
            PermissionConfig::Mutation(t())
        }
    }
    
    fn translate_acc_exp(self, eloc: &'e parse::Exp, perm: &'e parse::Exp, value_state: &mut ValueState, translation_state: &mut TranslationState) -> Result<egg::Id, Error<'e>> {
        self.mode.assert_expression(false, eloc)?;

        let (loc, ty) = match &**eloc {
            parse::ExpKind::FuncApp(name, args) => {
                self.translate_predicate_target(name, args, value_state)?
            }
            _ => unreachable!(),
        };
        let silver_oxide::intern::Ty::Resource(rk) = &self.decls.resolved[ty] else {
            unreachable!()
        };
        let ty = match *rk {
            silver_oxide::intern::ResourceKind::Field(ty) => TyKind::from(&self.decls.resolved[ty]),
            silver_oxide::intern::ResourceKind::Compound(res_id) => TyKind::Snapshot(res_id),
        };

        // Optimisation to return early if pc is false.
        let false_ = value_state.egraph.false_();
        if let Ok(()) = self.pc.assert_lite(&mut value_state.egraph, false_) {
            let snapshot = value_state.egraph.next_symbolic_value(Some("$MAGIC".into()), ty);
            translation_state.collect_snapshot.push(snapshot);
            return Ok(value_state.egraph.true_());
        }

        let perm = self.translate_permission(perm, value_state)?;
        if self.debug {
            println!("#T-1: {loc:?} / {perm:?}");
        }
        let res = self.translate_acc_exp_inner(loc, perm, ty, value_state, translation_state, eloc)?;
        translation_state.collect_snapshot.push(res.snapshot);
        Ok(res.expression)
    }
    fn translate_acc_exp_inner(self, loc: egg::Id, perm: Permission, ty: TyKind, value_state: &mut ValueState, translation_state: &mut TranslationState, eloc: &'e parse::Exp) -> Result<TranslationResult, Error<'e>> {
        // TODO:
        //self.decls.expd.get_perm_kind(eloc).unwrap();
        let perm_kind = ty;
        assert_eq!(perm_kind, ty);

        let perm = perm.0.unwrap_or(value_state.egraph.write());
        let result;
        let snapshot = match self.mode {
            TranslationMode::Exhale => {
                result = value_state.egraph.true_();
                let config = self.permission_config(|| self.mutation.unwrap());
                value_state.heap
                    .remove_permission(&mut value_state.egraph, loc, perm, self.pc, config, &self.decls)
                    .map_err(|kind| Error::expression(eloc, kind))
            }
            TranslationMode::Inhale => {
                result = value_state.egraph.true_();
                let value = self.next_assert_snapshot(&mut value_state.egraph, translation_state, ty);
                // let value = value.map(|value|
                //     field.map(|field| value_state.egraph.add(Exp::TyCast(value, TyKind::Snapshot(()), field))).unwrap_or(value)
                // );
                let bound = (!ty.is_snapshot()).then_some(value_state.egraph.write());
                let config = self.permission_config(|| ());
                Ok(value_state.heap
                    .add_permission(&mut value_state.egraph, loc, perm, self.pc, value, bound, config, ty, &self.decls)
                    .map_err(|kind| Error::expression(eloc, kind))?)
            }
            TranslationMode::Fact => {
                let (snapshot, curr_perm) = value_state.heap.get_permission(&mut value_state.egraph, loc, self.pc, self.mutation)
                    .map_err(|kind| Error::expression(eloc, kind))?;
                if self.debug {
                    println!("#T-2: {snapshot:?} / {curr_perm:?} / {:?}", self.pc);
                }
                result = value_state.egraph.add_binop(parse::BinOp::Le, perm, curr_perm);
                // value_state.egraph.saturate();
                // println!("___TESTTEST: {}\n{:?}\nC1:{:?}\nC2:{:?}\n2:{:?}\n____",
                //     value_state.egraph.normalise(result),
                //     value_state.egraph.egraph[result],
                //     value_state.egraph.egraph[result].nodes[0].children().iter().map(|&c| &value_state.egraph.egraph[c]).collect::<Vec<_>>(),
                //     value_state.egraph.egraph[result].nodes[0].children().get(0).map(|&c| value_state.egraph.egraph[c].nodes[0].children().iter().map(|&c| &value_state.egraph.egraph[c]).collect::<Vec<_>>()),
                //     value_state.egraph.egraph[egg::Id::from(2)],
                // );
                Ok(snapshot)
            }
            TranslationMode::Expression => unreachable!(),
        };
        let snapshot = match snapshot {
            Ok(snapshot) => snapshot,
            // if let Some(field) = field {
            //     value_state.egraph.add(Exp::TyCast(snapshot, field, TyKind::Snapshot(())))
            // } else {
            //     snapshot
            // },
            Err(err) => {
                // If PC is `false` this case can still happen, since the lookup
                // for the resource is in a hashmap and doesn't care about the
                // pc.
                let false_ = value_state.egraph.false_();
                // Do not error if PC is false
                self.pc.assert(&mut value_state.egraph, false_, "false_pc_check", &self.decls).map_err(|_| err)?;
                value_state.egraph.next_symbolic_value(Some("$MAGIC".into()), ty)
            }
        };
        Ok(TranslationResult { expression: result, snapshot, data: () })
    }

    fn translate_unop(
        self,
        op: parse::UnOp,
        exp: &'e parse::Exp,
        value_state: &mut ValueState,
    ) -> Result<egg::Id, Error<'e>> {
        // match op {
        //     silver_oxide::parse::UnOp::Neg => Exp::UnOp(UnOp::Neg, self.t_inner_as_exp(exp, value_state)?),
        //     silver_oxide::parse::UnOp::Not => Exp::UnOp(UnOp::Not, self.t_inner_as_exp(exp, value_state)?),
        // }
        let texp = self.t_inner_as_exp(exp, value_state)?;
        let op = match op {
            parse::UnOp::Not => UnOp::Not,
            parse::UnOp::Neg => UnOp::Neg,
            parse::UnOp::IntToReal => UnOp::IntToReal,
            parse::UnOp::Deref => {
                return value_state.heap
                    .get_symbolic_value(&mut value_state.egraph, texp, self.pc, self.mutation, &self.decls)
                    .map_err(|kind| Error::expression(exp, kind));
            }
            parse::UnOp::Abs => todo!(),
            parse::UnOp::Perm => todo!(),
        };
        Ok(value_state.egraph.add(Exp::UnOp(op, texp)))
    }

    pub(super) fn translate_binop(
        self,
        op: parse::BinOp,
        lhs: &'e parse::Exp,
        rhs: &'e parse::Exp,
        value_state: &mut ValueState,
    ) -> Result<egg::Id, Error<'e>> {
        let lhs = self.t_inner_as_exp(lhs, value_state)?;
        let rhs = self.t_inner_as_exp(rhs, value_state)?;
        Ok(BinOp::translate(op, lhs, rhs, &mut value_state.egraph))
    }

    pub fn unfold_predicate(self, acc: &'e parse::AccExp, value_state: &mut ValueState) -> Result<egg::Id, Error<'e>> {
        let (body, params, loc, perm, def_id) = self.extract_predicate(acc, value_state)?;
        self.unfold_inner(acc, body, &params, loc, perm, def_id, value_state)
    }
    fn unfold_inner(self, acc: &'e parse::AccExp, body: &'e parse::Exp, params: &Bindings<'e>, _loc: egg::Id, perm: Permission, def_id: DefIdC, value_state: &mut ValueState) -> Result<egg::Id, Error<'e>> {
        // Exhale
        let translator_in = self.with_mode(TranslationMode::Exhale);
        let mut ts = TranslationState::default();
        let _snap_in = translator_in.translate_acc_exp(&acc.acc.loc, &acc.perm, value_state, &mut ts)?;
        assert_eq!(ts.collect_snapshot.len(), 1);
        // Inhale
        let translator_out = self.with_mode(TranslationMode::Inhale)
            .with_params(params)
            .with_def_id(def_id)
            .set_perm_mult(perm)
            .set_assert_snapshot(ts.collect_snapshot[0], true);
        let output = translator_out.translate(body, value_state)?;
        Ok(output.expression)
    }
    pub fn fold_predicate(self, acc: &'e parse::AccExp, value_state: &mut ValueState) -> Result<egg::Id, Error<'e>> {
        let (body, params, loc, perm, def_id) = self.extract_predicate(acc, value_state)?;
        self.fold_inner(acc, body, &params, loc, perm, def_id, value_state)
    }
    fn fold_inner(self, acc: &'e parse::AccExp, body: &'e parse::Exp, params: &Bindings<'e>, _loc: egg::Id, perm: Permission, def_id: DefIdC, value_state: &mut ValueState) -> Result<egg::Id, Error<'e>> {
        // Exhale
        let translator_in = self.with_mode(TranslationMode::Exhale)
            .with_params(params)
            .with_def_id(def_id)
            .set_perm_mult(perm);
        let snap_in = translator_in.translate(body, value_state)?;
        // Inhale
        let translator_out = self.with_mode(TranslationMode::Inhale)
            .set_assert_snapshot(snap_in.snapshot, false);
        let mut ts = TranslationState::default();
        let _snap_out = translator_out.translate_acc_exp(&acc.acc.loc, &acc.perm, value_state, &mut ts)?;
        assert_eq!(ts.collect_snapshot.len(), 1);
        value_state.egraph.equate(snap_in.snapshot, ts.collect_snapshot[0], "fold");
        Ok(snap_in.expression)
    }

    fn extract_predicate(self, acc: &'e parse::AccExp, value_state: &mut ValueState) -> Result<(&'e parse::Exp, Bindings<'e>, egg::Id, Permission, DefIdC), Error<'e>> {
        use parse::ExpKind::*;
        let (name, args) = match &*acc.acc.loc {
            FuncApp(name, args) => (name, args),
            _ => panic!("Expected predicate, found {acc:?}"),
        };
        let cd = self.decls.resolver(self.def_id).get_callable(name);
        let silver_oxide::parse::Declaration::Predicate(p) = &self.decls.silver().program[cd.def_id] else {
            panic!("Expected predicate, found {acc:?}");
        };
        let Some(body) = &p.body else {
            panic!("{name:?} is opaque");
        };
        let (args, params) = self.translate_params(args, &p.signature.args, value_state)?;
        let loc = value_state.egraph.add(Exp::PredicateApp(name.clone(), args));
        let perm = self.translate_permission(&acc.perm, value_state)?;
        Ok((&body.0, params, loc, perm, DefIdC { contract: false, def_id: cd.def_id }))
    }

    fn translate_permission(self, perm: &'e parse::Exp, value_state: &mut ValueState) -> Result<Permission, Error<'e>> {
        // println!("translate_permission({perm:?})");
        let perm = self.t_inner_as_exp(perm, value_state)?;
        let perm = if value_state.ty(perm).is_int() {
            value_state.egraph.add(Exp::UnOp(UnOp::IntToReal, perm))
        } else { perm };
        let perm = if let Some(perm_mult) = self.perm_mult.0 {
            value_state.egraph.add(Exp::BinOp(BinOp::Mult, [perm, perm_mult]))
        } else { perm };
        let definitely_write = value_state.egraph.egraph[perm].data.is_write();
        // println!("translate_permission_data({:?}) -> {definitely_write}", value_state.egraph.egraph[perm].data);
        Ok(Permission((!definitely_write).then_some(perm)))
    }
}


// #[derive(Debug)]
// pub struct TranslationData<'e> {
//     pub(super) pc: PathCondition,
//     pub(super) args: &'e Args<'e>,
//     /// Used to mark changes in permission amounts in the heap to figure out
//     /// what was mutating in the current translation.
//     pub(super) mutation: Option<Mutation>,
//     pub(super) collect_snapshot: &'e mut Vec<egg::Id>,
//     pub(super) assert_snapshot: egg::Id,
//     pub(super) perm_mult: Option<egg::Id>,
// }
// impl<'e> TranslationData<'e> {
//     pub fn with_condition<'f>(&'f mut self, egraph: &mut EGraph, condition: egg::Id) -> TranslationData<'f> {
//         let pc = self.pc.add(egraph, condition);
//         TranslationData { pc, snapshot: self.snapshot, ..*self }
//     }
//     pub fn with_condition_negate<'f>(&'f mut self, egraph: &mut EGraph, condition: egg::Id) -> TranslationData<'f> {
//         let pc = self.pc.add_negate(egraph, condition);
//         TranslationData { pc, snapshot: self.snapshot, ..*self }
//     }
//     pub fn reborrow<'f>(&'f mut self) -> TranslationData<'f> {
//         TranslationData { snapshot: self.snapshot, ..*self }
//     }

//     pub fn snapshot(self, egraph: &mut EGraph) -> Snapshot {
//         let snapshot = std::mem::take(self.snapshot);
//         egraph.add_snapshot(snapshot)
//     }
// }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TranslationMode {
    /// Inhale/Exhale -> add/remove permissions when encountering `acc(x.f)`
    Inhale,
    Exhale,
    /// Assume/Assert -> assume/check permissions when encountering `acc(x.f)`
    Fact,
    /// For example, under negation -> error on encountering `acc(x.f)`
    Expression,
}

impl TranslationMode {
    pub fn under_bin_op(self, op: parse::BinOp) -> (Self, Self) {
        use parse::BinOp::*;
        match (self, op) {
            (Self::Expression, _) => (Self::Expression, Self::Expression),
            (other, And) => (other, other),
            (other, Or | Implies) => (Self::Expression, other),
            _ => (Self::Expression, Self::Expression),
        }
    }

    pub fn assert_expression<'e>(self, expect_expression: bool, exp: &'e parse::Exp) -> Result<(), Error<'e>> {
        if matches!(self, TranslationMode::Expression) == expect_expression {
            Ok(())
        } else {
            Err(Error::type_error(exp))
        }
    }
}
