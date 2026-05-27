use std::path::PathBuf;

use silver_oxide::{parse::{self, ConstHeapKind}, translate::{exp::{Exp, ExpConds, ExpLine, ExpLineKind}, resource::{Resource, ResourceExp}, CompoundId, ExpLocal, Local, Ty, TyKind}, HashMap, TiVec, TyCtxt};

use crate::{error::Error, pure::{EggExpKind, PathCondition}, state::ValueState, translator::{resource::{EggResource, EggResourceExp}, VerificationState}};

pub struct Silicon<'a, 'tcx> {
    pub ctx: &'a VerificationState<'tcx>,
    pub value_state: ValueState<'tcx>,
    // pub stmt_state: StmtState<'a, 'e>,

    // pub log_dir: &'a PathBuf,
    // pub method: &'a silver_oxide::parse::Method,
}

/// This represents either locally declared variables and their current values,
/// or arguments to a function/predicate and their values. A none key means `result`.
pub type Bindings<'a> = HashMap<Option<&'a parse::Ident>, egg::Id>;

// #[derive(Debug, Clone)]
// pub struct StmtState<'a, 'e> {
//     pub pc: PathCondition,
//     pub bindings: Bindings<'e>,

//     pub decls: &'a VerificationState<'e>,
// }

// #[derive(Debug, Clone)]
// pub enum SiliconStatement<'a> {
//     Viper(&'a silver_oxide::parse::Statement),
//     ReplacePathCond(PathCondition),
// }

impl<'a, 'tcx> Silicon<'a, 'tcx> {
    pub fn new(ctx: &'a VerificationState<'tcx>) -> Self {
        let value_state = ValueState::new(ctx.tcx());
        // let pc = PathCondition::new(&value_state.egraph);
        // let stmt_state = StmtState {
        //     pc,
        //     bindings: Default::default(),
        //     decls,
        // };
        Silicon {
            ctx,
            value_state,
            // stmt_state,
        }
    }

    pub fn register_locals(&mut self, locals: &TiVec<Local, Ty<'tcx>>) {
        assert!(self.value_state.locals.is_empty());
        self.value_state.locals = locals.iter_enumerated()
            .map(|(local, ty)| {
                self.value_state.egraph.add(*ty, local)
            }).collect();
    }

    pub fn translate_exp_resource(&mut self, re: &ResourceExp<'tcx>, snap: Result<Option<egg::Id>, CompoundId>) -> EggResourceExp<'tcx> {
        let snap = snap.unwrap_or_else(|snap| {
            let ty = self.ctx.tcx().interner.mk_ty_from_kind(TyKind::Compound(snap));
            Some(self.value_state.egraph.next_heap_value_like(ty))
        });
        assert!(snap.is_some() || re.resources.is_empty());

        let resources = re.resources.iter().enumerate().map(|(i, res)| {
            let deref_ty = res.deref_ty();
            let res = self.translate_resource(&re.pure, res);

            // TODO: add support for exhale
            let value = self.value_state.egraph.add(deref_ty, EggExpKind::Field(snap.unwrap(), i));
            // TODO: make this nicer
            let is_field = !matches!(deref_ty.kind(), TyKind::Compound(_));
            let bound = is_field.then_some(self.value_state.egraph.write());
            self.value_state.inhale_resource(&res, Some(value), bound, self.ctx);
            res
        }).collect();
        let (pure, _exp_locals) = self.translate_exp(&re.pure);
        // TODO: add support for exhale
        self.value_state.assume(pure, "inhale");
        let egraph = self.value_state.egraph.egraph_for_log();
        EggResourceExp { locals: re.locals.clone(), resources, pure, egraph }
    }

    pub fn translate_resource(&mut self, exp: &Exp<'tcx>, res: &Resource<'tcx>) -> EggResource {
        self.translate_exp_bounded(exp, res.after_line);
        let cond = self.translate_cond(&res.cond);
        let loc = self.value_state.translate_exp_operand(res.loc);
        let perm = self.value_state.translate_exp_operand(res.perm);
        EggResource {
            cond,
            loc,
            perm
        }
    }

    pub fn translate_cond(&mut self, cond: &ExpConds<'tcx>) -> PathCondition {
        let cond = cond.map(|c| self.value_state.translate_exp_operand(c.cond));
        PathCondition::from(cond)
    }

    pub fn translate_exp(&mut self, exp: &Exp<'tcx>) -> (egg::Id, TiVec<ExpLocal, egg::Id>) {
        self.translate_exp_inner(exp, None);
        let result = self.value_state.translate_exp_operand(exp.result);
        (result, core::mem::take(&mut self.value_state.exp_locals))
    }

    fn translate_exp_bounded(&mut self, exp: &Exp<'tcx>, to_line: ExpLocal) {
        self.translate_exp_inner(exp, Some(to_line));
    }

    // Do not call directly
    fn translate_exp_inner(&mut self, exp: &Exp<'tcx>, to_line: Option<ExpLocal>) {
        let from = self.value_state.exp_locals.len();
        let to = to_line.map(usize::from).unwrap_or(exp.lines.len() - 1) + 1;
        let to = ExpLocal::from(to);
        for (id, line) in exp.lines[ExpLocal::from(from)..to].iter_enumerated() {
            let value = self.translate_exp_line(line);
            let i = self.value_state.exp_locals.push_and_get_key(value);
            assert_eq!(i, ExpLocal::from(from + usize::from(id)));
        }
    }

    // Do not call directly
    fn translate_exp_line(&mut self, line: &ExpLine<'tcx>) -> egg::Id {
        let cond = self.translate_cond(&line.cond);
        // TODO: check preconditions
        use ExpLineKind::*;
        let kind = match line.kind {
            Call(did, ref nds) => {
                let args = nds.iter().map(|&nd| self.value_state.translate_exp_operand(nd)).collect();
                EggExpKind::Call(did, args)
            }
            Heap(op, [heap, addr]) => {
                let heap = heap.as_const().expect("todo");
                assert_eq!(heap.as_heap().unwrap(), ConstHeapKind::SelfFraming);
                let addr = self.value_state.translate_exp_operand(addr);
                return self.value_state.translate_heap_op(op, addr, &cond, self.ctx);
            }
            HeapUpdate(..) => todo!(),
            Quantifier(..) => todo!(),
            UnOp(op, nd) =>
                EggExpKind::UnOp(op, self.value_state.translate_exp_operand(nd)),
            BinOp(op, [lhs, rhs]) => {
                let lhs = self.value_state.translate_exp_operand(lhs);
                let rhs = self.value_state.translate_exp_operand(rhs);
                EggExpKind::BinOp(op, [lhs, rhs])
            }
            Ternary([c, t, e]) => {
                let c = self.value_state.translate_exp_operand(c);
                let t = self.value_state.translate_exp_operand(t);
                let e = self.value_state.translate_exp_operand(e);
                EggExpKind::Ternary([c, t, e])
            }
        };
        self.value_state.egraph.add(line.ty, kind)
    }

    // pub fn inhale(&mut self, exp: &'e parse::Exp, reason: &str) -> Result<TranslationResult, Error<'a>> {
    //     let exp = self.translate_for_inhale(exp)?;
    //     self.assume_fact(exp.expression, reason);
    //     Ok(exp)
    // }
    // pub fn assume(&mut self, exp: &'e parse::Exp) -> Result<(), Error<'a>> {
    //     let exp = self.translate_for_fact(exp)?;
    //     self.assume_fact(exp.expression, "assume");
    //     Ok(())
    // }
    // fn assume_fact(&mut self, exp: egg::Id, reason: impl Into<egg::Symbol>) {
    //     self.stmt_state.pc.assume(&mut self.value_state.egraph, exp, reason);
    // }

    // pub fn exhale(&mut self, exp: &'e parse::Exp) -> Result<(), Error<'a>> {
    //     let e = self.translate_for_exhale(exp)
    //         .map_err(|err| { self.log_err(&err); err })?;
    //     self.assert_fact(e.expression, exp)
    // }
    // pub fn assert(&mut self, exp: &'e parse::Exp) -> Result<(), Error<'a>> {
    //     let e = self.translate_for_fact(exp)?;
    //     self.assert_fact(e.expression, exp)
    // }
    // fn assert_fact(&mut self, exp: egg::Id, e: &'e parse::Exp) -> Result<(), Error<'a>> {
    //     let decls = &self.stmt_state.decls;
    //     self.stmt_state.pc.assert(&mut self.value_state.egraph, exp, "assert/exhale_pure", decls).map_err(|assertion| {
    //         let label = Some(format!("{}", self.value_state.egraph.normalise(assertion)));
    //         self.log_pure("assert", label);
    //         Error::exhale(e, assertion)
    //     })
    // }

    // pub fn unfold(&mut self, exp: &'e parse::AccExp) -> Result<(), Error<'a>> {
    //     let body = self.translate_for_unfold(exp)?;
    //     self.assume_fact(body, "unfold");
    //     Ok(())
    // }
    // pub fn fold(&mut self, exp: &'e parse::AccExp) -> Result<(), Error<'a>> {
    //     let body = self.translate_for_fold(exp)?;
    //     self.assert_fact(body, &exp.acc.loc)
    // }

    // pub fn log_err(&mut self, _err: &Error<'e>) {
    //     self.log_pure("err", None);
    // }
}
