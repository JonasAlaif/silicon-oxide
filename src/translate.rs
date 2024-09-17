use silver_oxide::ast;

use crate::{declarations::{CallableDecl, Declarations}, error::Error, exp::{BinOp, Exp, PathCondition, UnOp}, heap::{Mutation, PermissionConfig, Temporary}, pure::EGraph, silicon::{Bindings, Silicon, StmtState}, state::ValueState};

impl<'a, 'e, F, M> StmtState<'a, 'e, F, M> {
    pub fn translator(&self, mode: TranslationMode) -> Translator<'_, 'e, F, M> {
        Translator::new(self.pc, mode, &self.bindings, self.declarations)
    }
}

impl<'a, 'e, F, M> Silicon<'a, 'e, F, M> {
    pub fn translate_for_inhale(&mut self, exp: &'e ast::Exp) -> Result<TranslationResult, Error<'e>> {
        let assert_snapshot = self.value_state.egraph.next_symbolic_value(None);
        let translator = self.stmt_state.translator(TranslationMode::Inhale)
            .set_assert_snapshot(assert_snapshot);
        translator.translate(exp, &mut self.value_state)
    }
    pub fn translate_for_exhale(&mut self, exp: &'e ast::Exp) -> Result<TranslationResult, Error<'e>> {
        let mutation = self.value_state.mutation_id();
        let translator = self.stmt_state.translator(TranslationMode::Exhale)
            .set_mutation(mutation);
        translator.translate(exp, &mut self.value_state)
    }

    pub fn translate_for_fact(&mut self, exp: &'e ast::Exp) -> Result<TranslationResult, Error<'e>> {
        let translator = self.stmt_state.translator(TranslationMode::Fact);
        translator.translate(exp, &mut self.value_state)
    }

    pub fn translate_for_unfold(&mut self, exp: &'e ast::Exp) -> Result<egg::Id, Error<'e>> {
        let mutation = self.value_state.mutation_id();
        let translator = self.stmt_state.translator(TranslationMode::Exhale)
            .set_mutation(mutation);
        translator.unfold_predicate(exp, &mut self.value_state)
    }
    pub fn translate_for_fold(&mut self, exp: &'e ast::Exp) -> Result<egg::Id, Error<'e>> {
        let mutation = self.value_state.mutation_id();
        let translator = self.stmt_state.translator(TranslationMode::Exhale)
            .set_mutation(mutation);
        translator.fold_predicate(exp, &mut self.value_state)
    }

    pub fn translate_exp(&mut self, exp: &'e ast::Exp) -> Result<egg::Id, Error<'e>> {
        let translator = self.stmt_state.translator(TranslationMode::Expression);
        Ok(translator.translate(exp, &mut self.value_state)?.expression)
    }
    pub fn translate_exp_pc(&mut self, exp: &'e ast::Exp, pc: PathCondition) -> Result<egg::Id, Error<'e>> {
        let mut translator = self.stmt_state.translator(TranslationMode::Expression);
        translator.pc = pc;
        Ok(translator.translate(exp, &mut self.value_state)?.expression)
    }
}

#[derive(Debug)]
pub struct Translator<'a, 'e, F, M> {
    pc: PathCondition,
    mode: TranslationMode,
    binds: &'a Bindings<'e>,
    decls: &'a Declarations<'e, F, M>,
    /// Used to mark changes in permission amounts in the heap to figure out
    /// what was mutating in the current translation.
    mutation: Option<Mutation>,
    temporary: Option<Temporary>,
    /// The generated expression will encode equality to this snapshot, useful
    /// for inhaling a predicate body.
    assert_snapshot: Option<egg::Id>,
    /// Determines the multiplier for all permissions.
    perm_mult: Option<egg::Id>,
}

impl<F, M> Copy for Translator<'_, '_, F, M> {}
impl<F, M> Clone for Translator<'_, '_, F, M> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, 'e, F, M> Translator<'a, 'e, F, M> {
    pub fn new(pc: PathCondition, mode: TranslationMode, binds: &'a Bindings<'e>, decls: &'a Declarations<'e, F, M>) -> Self {
        Self { pc, mode, binds, decls, mutation: None, temporary: None, assert_snapshot: None, perm_mult: None }
    }
    pub fn set_mutation(self, mutation: Mutation) -> Self {
        Self { mutation: Some(mutation), ..self }
    }
    pub fn set_assert_snapshot(self, snapshot: egg::Id) -> Self {
        assert!(matches!(self.mode, TranslationMode::Inhale), "assert_snapshot is only valid in inhale mode");
        Self { assert_snapshot: Some(snapshot), ..self }
    }
    pub fn set_perm_mult(self, perm_mult: Option<egg::Id>) -> Self {
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
    pub fn temporary_up(self) -> Self {
        let temporary = Some((true, self.temporary.map(|(_, t)| t + 1).unwrap_or_default()));
        Self { temporary, ..self }
    }
    pub fn temporary_down(self) -> Self {
        Self { temporary: Some((false, self.temporary.unwrap().1)), ..self }
    }

    fn next_assert_snapshot(self, egraph: &mut EGraph, translation_state: &mut TranslationState) -> Option<egg::Id> {
        self.assert_snapshot.map(|snap| {
            let snap = egraph.add(Exp::Project(snap, translation_state.assert_snapshot_idx));
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
pub struct TranslationResult {
    pub(super) expression: egg::Id,
    pub(super) snapshot: egg::Id,
}

impl<'a, 'e, F, M> Translator<'a, 'e, F, M> {
    pub fn translate(self, exp: &'e ast::Exp, value_state: &mut ValueState) -> Result<TranslationResult, Error<'e>> {
        let mut translation_state = TranslationState::default();
        let expression = self.t_inner(exp, value_state, &mut translation_state)?;
        // assert!(!self.assert_snapshot.is_some_and(|(_, expected_children)| expected_children != translation_state.assert_snapshot_idx));
        let snapshot = value_state.egraph.add_snapshot(translation_state.collect_snapshot);
        Ok(TranslationResult { expression, snapshot })
    }

    fn t_inner_as_exp(self, exp: &'e ast::Exp, value_state: &mut ValueState) -> Result<egg::Id, Error<'e>> {
        self.with_mode(TranslationMode::Expression).t_inner(exp, value_state, &mut Default::default())
    }
    fn t_inner(self, exp: &'e ast::Exp, value_state: &mut ValueState, translation_state: &mut TranslationState) -> Result<egg::Id, Error<'e>> {
        use ast::Exp::*;
        use ast::BinOp;
        // Hopefully this gets inlined
        let exp = match exp {
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
                return self.translate_binop(*op, l, r, value_state, translation_state)
            }
            Ternary(c, t, f) => {
                let c = self.t_inner_as_exp(c, value_state)?;
                let t = self.with_condition(&mut value_state.egraph, c)
                    .t_inner(t, value_state, translation_state)?;
                let f = self.with_condition_negate(&mut value_state.egraph, c)
                    .t_inner(f, value_state, translation_state)?;
                Exp::Ternary([c, t, f])
            }
            Field(r, field) => {
                let resource = self.translate_field_target(r, field, value_state)?;
                let value = value_state.heap
                    .get_symbolic_value(&mut value_state.egraph, resource, self.pc, self.mutation)
                    .map_err(|kind| Error::expression(exp, kind))?;
                return Ok(value);
            },
            Neg(exp) =>
                Exp::UnOp(UnOp::Neg, self.t_inner_as_exp(exp, value_state)?),
            Not(exp) =>
                Exp::UnOp(UnOp::Not, self.t_inner_as_exp(exp, value_state)?),
            FuncApp(name, args) => {
                let declaration = self.decls.callable.get(name).ok_or_else(|| Error::undeclared_function(name))?;
                match declaration {
                    CallableDecl::Function(fun, _) => {
                        let (mut args, mut params) = self.translate_params(args, &fun.signature.args, value_state)?;
                        if let Some(pre) = &fun.contract.precondition {
                            // TODO: which `pc`` to use here?
                            let translator = Translator::new(self.pc, TranslationMode::Fact, &params, self.decls);
                            let pre = translator.translate(pre, value_state)?;
                            self.pc.assert(&mut value_state.egraph, pre.expression).map_err(|assertion|
                                Error::function_precondition(exp, assertion)
                            )?;
                            args.push(pre.snapshot);
                        }

                        let result = value_state.egraph.add(Exp::FuncApp(name.clone(), args));

                        // TODO: improve this hack to unfold fn defns only once
                        // if data.args.is_empty() {
                        //     if let Some(body) = &fun.body {
                        //         let data = TranslationData { pc: data.pc, args: &params, mutation: None, snapshot: &mut Default::default() };
                        //         let body = self.translate_with_args(body, TranslationMode::Expression, data).unwrap();
                        //         self.egraph.equate(result, body, "fn call body");
                        //     }
                        // }

                        params.insert(None, result);
                        if let Some(post) = &fun.contract.postcondition {
                            // TODO: which `pc`` to use here?
                            let translator = Translator::new(self.pc, TranslationMode::Expression, &params, self.decls);
                            let post = translator.translate(post, value_state)?;
                            self.pc.assume(&mut value_state.egraph, post.expression, "fn post");
                        }
                        return Ok(result);
                    }
                    CallableDecl::Predicate(_pred) => {
                        self.mode.assert_expression(false, exp)?;
                        let loc = self.translate_predicate_target(name, args, value_state)?;
                        let perm = self.translate_permission(None, value_state)?;
                        return self.translate_acc_exp(loc, perm, false, value_state, translation_state, exp)
                    }
                    // TODO: domain functions
                    _ => todo!(),
                }
            }
            Acc(acc) => {
                self.mode.assert_expression(false, exp)?;
                let perm = self.translate_permission(acc.perm.as_ref(), value_state)?;
                let (loc, bound) = match &acc.acc.loc {
                    ast::Exp::FuncApp(name, args) => {
                        let loc = self.translate_predicate_target(name, args, value_state)?;
                        (loc, false)
                    }
                    ast::Exp::Field(r, field) => {
                        let loc = self.translate_field_target(r, field, value_state)?;
                        (loc, true)
                    }
                    _ => unreachable!(),
                };
                return self.translate_acc_exp(loc, perm, bound, value_state, translation_state, exp)
            }
            Unfolding(acc, exp) => {
                let (body, params, loc, perm) = self.extract_predicate(acc, value_state)?;
                let self_ = self.temporary_up();
                let body = self_.unfold_inner(acc, body, &params, loc, perm, value_state)?;
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

    fn translate_params(self, args: &'e Vec<ast::Exp>, formals: &'e Vec<ast::ArgOrType>, value_state: &mut ValueState) -> Result<(Vec<egg::Id>, Bindings<'e>), Error<'e>> {
        let formals = formals.iter().map(|a| {let ast::ArgOrType::Arg(a) = a else { unreachable!() }; &a.0 });
        let args = args.iter()
            .map(|arg| self.t_inner_as_exp(arg, value_state))
            .collect::<core::result::Result<Vec<_>, _>>()?;
        let params: Bindings = formals.map(Some).zip(args.iter().copied()).collect();
        Ok((args, params))
    }

    // /// Translate an expression. This is the function to use if the expression
    // /// doesn't contain any permission (acc/predicate/wand) expressions.
    // pub fn translate_exp(&mut self, exp: &'e ast::Exp, pc: PathCondition) -> Result<egg::Id, Error<'e>> {
    //     self.translate(exp, TranslationMode::Expression, pc, None)
    // }
    // pub fn translate(&mut self, exp: &'e ast::Exp, mode: TranslationMode, pc: PathCondition, mutation: Option<Mutation>) -> Result<egg::Id, Error<'e>> {
    //     let data = TranslationData { pc, args: &Default::default(), mutation, snapshot: &mut Default::default() };
    //     self.translate_with_args(exp, mode, data)
    // }

    // /// The root function which will be called for any translation job.
    // pub fn translate_with_args(&mut self, exp: &'e ast::Exp, mode: TranslationMode, data: TranslationData) -> Result<egg::Id, Error<'e>> {
    //     assert_eq!(matches!(mode, TranslationMode::Mutating { exhale: true, .. }), data.mutation.is_some());
    //     self.t_inner(exp, mode, data)
    // }
    // pub fn translate_field_target(&mut self, r: &'e ast::Exp, field: &'e ast::Ident, pc: PathCondition) -> Result<egg::Id, Error<'e>> {
    //     let data = TranslationData { pc, mutation: None, args: &Default::default(), snapshot: &mut Default::default() };
    //     self.translate_field_target_inner(r, field, data)
    // }











    fn translate_predicate_target(self, name: &'e ast::Ident, args: &'e Vec<ast::Exp>, value_state: &mut ValueState) -> Result<egg::Id, Error<'e>> {
        let declaration = self.decls.callable.get(name).ok_or_else(|| Error::undeclared_function(name))?;
        assert!(matches!(declaration, CallableDecl::Predicate(_)));

        let args = args.iter()
            .map(|arg| self.t_inner_as_exp(arg, value_state))
            .collect::<core::result::Result<_, _>>()?;
        Ok(value_state.egraph.add(Exp::PredicateApp(name.clone(), args)))
    }
    pub fn translate_field_target(self, r: &'e ast::Exp, field: &'e ast::Ident, value_state: &mut ValueState) -> Result<egg::Id, Error<'e>> {
        self.decls.field.get(field).ok_or_else(|| Error::undeclared_field(field))?;

        let recv = self.t_inner_as_exp(r, value_state)?;
        Ok(value_state.egraph.add(Exp::PredicateApp(field.clone(), vec![recv])))
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
    pub(super) fn translate_acc_exp(self, loc: egg::Id, perm: Option<egg::Id>, bound: bool, value_state: &mut ValueState, translation_state: &mut TranslationState, exp: &'e ast::Exp) -> Result<egg::Id, Error<'e>> {
        let perm = perm.unwrap_or(value_state.egraph.none());
        let result;
        let snapshot = match self.mode {
            TranslationMode::Exhale => {
                result = value_state.egraph.true_();
                let config = self.permission_config(|| self.mutation.unwrap());
                value_state.heap
                    .remove_permission(&mut value_state.egraph, loc, perm, self.pc, config)
                    .map_err(|kind| Error::expression(exp, kind))
            }
            TranslationMode::Inhale => {
                result = value_state.egraph.true_();
                let value = self.next_assert_snapshot(&mut value_state.egraph, translation_state);
                let bound = bound.then_some(value_state.egraph.write());
                let config = self.permission_config(|| ());
                value_state.heap
                    .add_permission(&mut value_state.egraph, loc, perm, self.pc, value, bound, config)
                    .map_err(|kind| Error::expression(exp, kind))
            }
            TranslationMode::Fact => {
                let (snapshot, curr_perm) = value_state.heap.get_permission(&mut value_state.egraph, loc, self.pc)
                    .map_err(|kind| Error::expression(exp, kind))?;
                result = value_state.egraph.add_binop(ast::BinOp::Le, perm, curr_perm);
                Ok(snapshot)
            }
            TranslationMode::Expression => unreachable!(),
        };
        let snapshot = match snapshot {
            Ok(snapshot) => snapshot,
            Err(err) => {
                let false_ = value_state.egraph.false_();
                // Do not error if PC is false
                self.pc.assert(&mut value_state.egraph, false_).map_err(|_| err)?;
                value_state.egraph.next_symbolic_value(None)
            }
        };
        translation_state.collect_snapshot.push(snapshot);
        Ok(result)
    }

    pub(super) fn translate_binop(
        self,
        op: ast::BinOp,
        lhs: &'e ast::Exp,
        rhs: &'e ast::Exp,
        value_state: &mut ValueState,
        translation_state: &mut TranslationState,
    ) -> Result<egg::Id, Error<'e>> {
        use ast::BinOp::*;
        let lhs = if matches!(op, And) {
            self.t_inner(lhs, value_state, translation_state)
        } else {
            self.t_inner_as_exp(lhs, value_state)
        }?;

        let (lhs, op, translator_rhs) = match op {
            And => (lhs, BinOp::And, self.with_condition(&mut value_state.egraph, lhs)),
            Implies => {
                let not_lhs = value_state.egraph.add(Exp::UnOp(UnOp::Not, lhs));
                (not_lhs, BinOp::Or, self.with_condition(&mut value_state.egraph, lhs))
            }
            Or => (lhs, BinOp::Or, self.with_condition_negate(&mut value_state.egraph, lhs)),
            Div => todo!(),
            Mod => todo!(),
            // Need to check for the above two:
                // let neq_zero = egraph.add(Exp::BinOp(BinOp::Eq, [rhs, egraph.none()]));
            In => todo!(),
            PermDiv => todo!(),
            Union => todo!(),
            SetMinus => todo!(),
            Intersection => todo!(),
            Subset => todo!(),
            Concat => todo!(),
            MagicWand => todo!(),
            op => {
                let rhs = self.t_inner_as_exp(rhs, value_state)?;
                return Ok(BinOp::translate(op, lhs, rhs, &mut value_state.egraph));
            }
        };
        let rhs = translator_rhs.t_inner(rhs, value_state, translation_state)?;
        Ok(value_state.egraph.add(Exp::BinOp(op, [lhs, rhs])))

        // let (lhs, rhs) = mode.under_bin_op(*op);
        // let lhs = self.t_inner(l, lhs, data)?;
        // let data = match op {
        //     BinOp::And | BinOp::Implies => data.with_condition(&mut self.egraph, lhs),
        //     BinOp::Or => data.with_condition_negate(&mut self.egraph, lhs),
        //     _ => data,
        // };
        // let rhs = self.t_inner(r, rhs, data)?;
        // if matches!(op, ast::BinOp::Div) {
        //     let neq_zero = self.egraph.add_binop(BinOp::Neq, rhs, self.egraph.none());
        //     data.pc.check(&mut self.egraph, neq_zero).map_err(|assertion| Error::divide_by_zero(exp, assertion))?;
        // }
        // return Ok(self.egraph.add_binop(*op, lhs, rhs))
    }

    pub fn unfold_predicate(self, exp: &'e ast::Exp, value_state: &mut ValueState) -> Result<egg::Id, Error<'e>> {
        let (body, params, loc, perm) = self.extract_predicate(exp, value_state)?;
        self.unfold_inner(exp, body, &params, loc, perm, value_state)
    }
    fn unfold_inner(self, exp: &'e ast::Exp, body: &'e ast::Exp, params: &Bindings<'e>, _loc: egg::Id, perm: Option<egg::Id>, value_state: &mut ValueState) -> Result<egg::Id, Error<'e>> {
        // Exhale
        let translator_in = self.with_mode(TranslationMode::Exhale);
        let mut ts = TranslationState::default();
        let _snap_in = translator_in.t_inner(exp, value_state, &mut ts)?;
        assert_eq!(ts.collect_snapshot.len(), 1);
        // Inhale
        let translator_out = self.with_mode(TranslationMode::Inhale)
            .with_params(params)
            .set_perm_mult(perm)
            .set_assert_snapshot(ts.collect_snapshot[0]);
        let output = translator_out.translate(body, value_state)?;
        Ok(output.expression)
    }
    pub fn fold_predicate(self, exp: &'e ast::Exp, value_state: &mut ValueState) -> Result<egg::Id, Error<'e>> {
        let (body, params, loc, perm) = self.extract_predicate(exp, value_state)?;
        self.fold_inner(exp, body, &params, loc, perm, value_state)
    }
    fn fold_inner(self, exp: &'e ast::Exp, body: &'e ast::Exp, params: &Bindings<'e>, _loc: egg::Id, perm: Option<egg::Id>, value_state: &mut ValueState) -> Result<egg::Id, Error<'e>> {
        // Exhale
        let translator_in = self.with_mode(TranslationMode::Exhale)
            .with_params(params)
            .set_perm_mult(perm);
        let snap_in = translator_in.translate(body, value_state)?;
        // Inhale
        let translator_out = self.with_mode(TranslationMode::Inhale)
            .set_assert_snapshot(snap_in.snapshot);
        let mut ts = TranslationState::default();
        let _snap_out = translator_out.t_inner(exp, value_state, &mut ts)?;
        assert_eq!(ts.collect_snapshot.len(), 1);
        value_state.egraph.equate(snap_in.snapshot, ts.collect_snapshot[0], "fold");
        Ok(snap_in.expression)
    }

    fn extract_predicate(self, exp: &'e ast::Exp, value_state: &mut ValueState) -> Result<(&'e ast::Exp, Bindings<'e>, egg::Id, Option<egg::Id>), Error<'e>> {
        use ast::Exp::*;
        let (name, args, perm) = match exp {
            FuncApp(name, args) =>
                (name, args, None),
            Acc(acc) => if let ast::AccExp { acc: ast::LocAccess { loc: FuncApp(name, args) }, perm } = &**acc {
                (name, args, perm.as_ref())
            } else {
                panic!("Expected predicate, found {exp:?}");
            },
            _ => panic!("Expected predicate, found {exp:?}"),
        };
        let Some(CallableDecl::Predicate(predicate)) = self.decls.callable.get(name) else {
            panic!("{name:?} not found or not a predicate");
        };
        let Some(body) = &predicate.body else {
            panic!("{name:?} is opaque");
        };
        let (args, params) = self.translate_params(args, &predicate.signature.args, value_state)?;
        let loc = value_state.egraph.add(Exp::PredicateApp(name.clone(), args));
        let perm = self.translate_permission(perm, value_state)?;
        Ok((body, params, loc, perm))
    }

    fn translate_permission(self, perm: Option<&'e ast::Exp>, value_state: &mut ValueState) -> Result<Option<egg::Id>, Error<'e>> {
        let perm = perm.as_ref()
            .map(|perm| self.t_inner_as_exp(perm, value_state))
            .transpose()?;
        match (perm, self.perm_mult) {
            (Some(perm), Some(perm_mult)) =>
                Ok(Some(value_state.egraph.add(Exp::BinOp(BinOp::Mult, [perm, perm_mult])))),
            (Some(perm), None) => Ok(Some(perm)),
            (None, Some(perm_mult)) => Ok(Some(perm_mult)),
            (None, None) => Ok(None),
        }
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
    pub fn under_bin_op(self, op: ast::BinOp) -> (Self, Self) {
        use ast::BinOp::*;
        match (self, op) {
            (Self::Expression, _) => (Self::Expression, Self::Expression),
            (other, And) => (other, other),
            (other, Or | Implies) => (Self::Expression, other),
            _ => (Self::Expression, Self::Expression),
        }
    }

    pub fn assert_expression<'e>(self, expect_expression: bool, exp: &'e ast::Exp) -> Result<(), Error<'e>> {
        if matches!(self, TranslationMode::Expression) == expect_expression {
            Ok(())
        } else {
            Err(Error::type_error(exp))
        }
    }
}
