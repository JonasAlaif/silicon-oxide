use fxhash::FxHashMap;

use crate::{error::Error, exp::{BinOp, Exp}, heap::Heap, pure::EGraph};

#[derive(Debug, Clone)]
pub struct State<'a> {
    pub egraph: EGraph,
    pub bindings: FxHashMap<silver_oxide::ast::Ident, egg::Id>,
    pub heap: Heap,

    pub declarations: &'a FxHashMap<&'a str, &'a silver_oxide::ast::Declaration>,
}

impl<'a> State<'a> {
    pub fn new(declarations: &'a FxHashMap<&'a str, &'a silver_oxide::ast::Declaration>) -> Self {
        Self {
            egraph: EGraph::default(),
            bindings: FxHashMap::default(),
            heap: Heap::default(),
            declarations,
        }
    }

    pub fn new_binding(&mut self, binding: &silver_oxide::ast::Ident) {
        let id = self.egraph.next_symbolic_value(Some(binding.0.clone()));
        self.bindings.insert(binding.clone(), id);
    }

    pub fn inhale<'e>(&mut self, exp: &'e silver_oxide::ast::Exp, reason: impl Into<egg::Symbol> + Clone, pc: &[egg::Id]) -> Result<(), Error<'e>> {
        self.inhale_inner(exp, reason, TranslationMode::Mutating { inhale: true }, pc)
    }
    pub fn assume<'e>(&mut self, exp: &'e silver_oxide::ast::Exp, reason: impl Into<egg::Symbol>, pc: &[egg::Id]) -> Result<(), Error<'e>> {
        self.inhale_inner(exp, reason, TranslationMode::Fact, pc)
    }
    fn inhale_inner<'e>(&mut self, exp: &'e silver_oxide::ast::Exp, reason: impl Into<egg::Symbol>, mode: TranslationMode, pc: &[egg::Id]) -> Result<(), Error<'e>> {
        let id = self.translate(&exp, mode, pc)?;
        let neg_pc = self.egraph.negate_pc(pc);
        let id = self.egraph.add(Exp::BinOp(BinOp::Or, [neg_pc, id]));

        let true_ = self.egraph.true_();
        self.egraph.equate(id, true_, reason);
        self.egraph.rebuild();
        Ok(())
    }

    pub fn exhale<'e>(&mut self, exp: &'e silver_oxide::ast::Exp, pc: &[egg::Id]) -> Result<(), Error<'e>> {
        self.exhale_inner(exp, TranslationMode::Mutating { inhale: false }, pc)
    }
    pub fn assert<'e>(&mut self, exp: &'e silver_oxide::ast::Exp, pc: &[egg::Id]) -> Result<(), Error<'e>> {
        self.exhale_inner(exp, TranslationMode::Fact, pc)
    }
    fn exhale_inner<'e>(&mut self, exp: &'e silver_oxide::ast::Exp, mode: TranslationMode, pc: &[egg::Id]) -> Result<(), Error<'e>> {
        let id = self.translate(&exp, mode, pc)?;
        if self.egraph.is_true(id) {
            return Ok(())
        }
        let neg_pc = self.egraph.negate_pc(pc);
        let id = self.egraph.add(Exp::BinOp(BinOp::Or, [neg_pc, id]));

        self.egraph.saturate();
        self.egraph.egraph.dot().with_config_line("ranksep=2.5").to_pdf(format!("./exhale.pdf")).unwrap();
        if self.egraph.is_true(id) {
            Ok(())
        } else {
            Err(Error::exhale_unknown(exp))
        }
    }

    pub fn translate<'e>(&mut self, exp: &'e silver_oxide::ast::Exp, mode: TranslationMode, pc: &[egg::Id]) -> Result<egg::Id, Error<'e>> {
        self.translate_inner(exp, mode, &mut pc.to_vec())
    }

    pub fn translate_field_target<'e>(&mut self, r: &'e silver_oxide::ast::Exp, field: &'e silver_oxide::ast::Ident, pc: &[egg::Id]) -> Result<egg::Id, Error<'e>> {
        self.translate_field_target_inner(r, field, &mut pc.to_vec())
    }

    fn translate_inner<'e>(&mut self, exp: &'e silver_oxide::ast::Exp, mode: TranslationMode, pc: &mut Vec<egg::Id>) -> Result<egg::Id, Error<'e>> {
        let pc_len = pc.len();
        let res = self.translate_inner_pc(exp, mode, pc);
        debug_assert_eq!(pc.len(), pc_len);
        res
    }
    fn translate_inner_pc<'e>(&mut self, exp: &'e silver_oxide::ast::Exp, mode: TranslationMode, pc: &mut Vec<egg::Id>) -> Result<egg::Id, Error<'e>> {
        use silver_oxide::ast::Exp::*;
        use silver_oxide::ast::BinOp;
        // Hopefully this gets inlined
        let exp = match exp {
            Const(c) => Exp::Const(c.clone()),
            Ident(i) =>
                return self.bindings
                    .get(i)
                    .copied()
                    .ok_or(Error::undeclared_variable(i)),
            BinOp(BinOp::MagicWand, ..) => todo!(),
            BinOp(op, l, r) => {
                let (lhs, rhs) = mode.under_bin_op(*op);
                let lhs = self.translate_inner(l, lhs, pc)?;
                match op {
                    BinOp::And | BinOp::Implies => pc.push(lhs),
                    BinOp::Or => pc.push(self.egraph.add(Exp::Not(lhs))),
                    _ => (),
                }
                let rhs = self.translate_inner(r, rhs, pc)?;
                if matches!(op, BinOp::And | BinOp::Implies | BinOp::Or) {
                    pc.pop();
                }
                return Ok(self.egraph.add_binop(*op, lhs, rhs))
            }
            Ternary(c, t, f) => {
                let c = self.translate_inner(c, TranslationMode::Expression, pc)?;
                pc.push(c);
                let t = self.translate_inner(t, mode, pc)?;
                pc.pop();
                pc.push(self.egraph.add(Exp::Not(c)));
                let f = self.translate_inner(f, mode, pc)?;
                pc.pop();
                Exp::Ternary([c, t, f])
            }
            Field(r, field) => {
                let resource = self.translate_field_target_inner(r, field, pc)?;
                let value = self.heap
                    .get_symbolic_value(&mut self.egraph, resource, pc)
                    .map_err(|_| Error::no_field(&**r, field))?;
                return Ok(value);
            },
            Neg(exp) =>
                Exp::Neg(self.translate_inner(exp, TranslationMode::Expression, pc)?),
            Not(exp) =>
                Exp::Not(self.translate_inner(exp, TranslationMode::Expression, pc)?),
            FuncApp(name, args) => {
                let declaration = self.declarations.get(name.0.as_str()).ok_or_else(|| Error::undeclared_function(name))?;
                use silver_oxide::ast::Declaration;
                // TODO: domain functions
                assert!(matches!(declaration, Declaration::Function(_) | Declaration::Predicate(_)));

                let args = args.iter()
                    .map(|arg| self.translate_inner(arg, TranslationMode::Expression, pc))
                    .collect::<core::result::Result<_, _>>()?;
                Exp::FuncApp(name.clone(), args)
            }
            Acc(acc) =>
                return self.translate_acc_exp(acc, mode, pc),
            _ => todo!("{exp:?}"),
        };
        Ok(self.egraph.add(exp))
    }

    fn translate_field_target_inner<'e>(&mut self, r: &'e silver_oxide::ast::Exp, field: &'e silver_oxide::ast::Ident, pc: &mut Vec<egg::Id>) -> Result<egg::Id, Error<'e>> {
        let decl = self.declarations.get(field.0.as_str()).ok_or_else(|| Error::undeclared_field(field))?;
        assert!(matches!(decl, silver_oxide::ast::Declaration::Field(_)));

        let recv = self.translate_inner(r, TranslationMode::Expression, pc)?;
        Ok(self.egraph.add(Exp::FuncApp(field.clone(), vec![recv])))
    }

    fn translate_acc_exp<'e>(&mut self, acc: &'e silver_oxide::ast::AccExp, mode: TranslationMode, pc: &mut Vec<egg::Id>) -> Result<egg::Id, Error<'e>> {
        use silver_oxide::ast::AccExp::*;
        let (loc, perm, bound) = match acc {
            Acc(silver_oxide::ast::LocAccess { loc }, perm) => {
                mode.assert_expression(false)?;

                let perm = perm.as_ref()
                    .map(|perm| self.translate_inner(perm, TranslationMode::Expression, pc))
                    .transpose()?
                    .unwrap_or(self.egraph.write());
                let (exp, bound) = match loc {
                    exp@silver_oxide::ast::Exp::FuncApp(..) => (self.translate_inner(exp, TranslationMode::Expression, pc), false),
                    silver_oxide::ast::Exp::Field(r, field) => (self.translate_field_target_inner(r, field, pc), true),
                    _ => unreachable!(),
                };
                (exp?, perm, bound)
            }
            PredicateAccess(exp@silver_oxide::ast::Exp::FuncApp(name, ..)) => {
                let declaration = self.declarations.get(name.0.as_str()).ok_or_else(|| Error::undeclared_function(name))?;
                let is_predicate = matches!(declaration, silver_oxide::ast::Declaration::Predicate(..));
                if is_predicate {
                    mode.assert_expression(false)?;
                }

                let exp = self.translate_inner(exp, TranslationMode::Expression, pc);
                if is_predicate {
                    (exp?, self.egraph.write(), false)
                } else {
                    return exp;
                }
            }
            PredicateAccess(_) => unreachable!(),
        };
        // println!("loc: {loc:?}, perm {perm:?}, bound {bound:?}, mode {mode:?}");
        match mode {
            TranslationMode::Mutating { inhale } => {
                if inhale {
                    let bound = bound.then_some(self.egraph.write());
                    self.heap.add_chunk(&mut self.egraph, loc, perm, pc, bound);
                } else {
                    self.heap.remove_chunk(&mut self.egraph, loc, perm, pc)
                        .map_err(|_| Error::exhale_missing_permission(acc))?;
                }
                Ok(self.egraph.true_())
            }
            TranslationMode::Fact => {
                let curr_perm = self.heap.get_permission(&mut self.egraph, loc, pc)
                    .map_err(|_| Error::misc_error(0))?;
                Ok(self.egraph.add_binop(silver_oxide::ast::BinOp::Le, perm, curr_perm))
            }
            TranslationMode::Expression => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TranslationMode {
    /// Inhale/Exhale -> add/remove permissions when encountering `acc(x.f)`
    Mutating { inhale: bool },
    /// Assume/Assert -> assume/check permissions when encountering `acc(x.f)`
    Fact,
    /// For example, under negation -> error on encountering `acc(x.f)`
    Expression,
}

impl TranslationMode {
    pub fn under_bin_op(self, op: silver_oxide::ast::BinOp) -> (Self, Self) {
        use silver_oxide::ast::BinOp::*;
        match (self, op) {
            (Self::Expression, _) => (Self::Expression, Self::Expression),
            (other, And) => (other, other),
            (other, Or | Implies) => (Self::Expression, other),
            _ => (Self::Expression, Self::Expression),
        }
    }

    pub fn assert_expression<'e>(self, expect_expression: bool) -> Result<(), Error<'e>> {
        if matches!(self, TranslationMode::Expression) == expect_expression {
            Ok(())
        } else {
            Err(Error::type_error())
        }
    }
}
