use std::path::PathBuf;

use fxhash::FxHashMap;
use silver_oxide::ast;

use crate::{declarations::Declarations, error::Error, exp::PathCondition, state::ValueState, translate::TranslationMode};

#[derive(Debug, Clone)]
pub struct Silicon<'a> {
    pub value_state: ValueState,
    pub stmt_state: StmtState<'a>,

    pub log_dir: &'a PathBuf,
    // pub method: &'a silver_oxide::ast::Method,
}

/// This represents either locally declared variables and their current values,
/// or arguments to a function/predicate and their values. A none key means `result`.
pub type Bindings<'a> = FxHashMap<Option<&'a ast::Ident>, egg::Id>;

#[derive(Debug, Clone)]
pub struct StmtState<'a> {
    pub pc: PathCondition,
    pub bindings: Bindings<'a>,

    pub statements: Vec<SiliconStatement<'a>>,
    pub declarations: &'a Declarations<'a>,
}

#[derive(Debug, Clone)]
pub enum SiliconStatement<'a> {
    Viper(&'a silver_oxide::ast::Statement),
    ReplacePathCond(PathCondition),
}

impl<'a> Silicon<'a> {
    pub fn verify(
        method: &'a silver_oxide::ast::Method,
        declarations: &'a Declarations<'a>,
        log_dir: &'a PathBuf,
    ) -> Result<(), Error<'a>> {
        let Some(body) = &method.body else {
            return Ok(());
        };
        let value_state = ValueState::new();
        let pc = PathCondition::new(&value_state.egraph);
        let stmt_state = StmtState {
            pc,
            bindings: Default::default(),
            statements: body.statements.iter().rev().map(SiliconStatement::Viper).collect(),
            declarations,
        };
        let mut self_ = Silicon {
            value_state,
            stmt_state,
            log_dir,
        };
        for arg in method.signature.args.iter() {
            self_.new_arg(arg);
        }
        for ret in method.signature.ret.iter() {
            self_.new_arg(ret);
        }
        if let Some(pre) = &method.contract.precondition {
            self_.inhale(pre, "precondition").unwrap();
        }
        let mut id = 0;

        while let Some(stmt) = self_.stmt_state.statements.pop() {
            self_.value_state.egraph.saturate();
            self_.log_pure(&format!("_stmt{id}"), Some(format!("{stmt:?}")));
            println!("HEAP: {:#?}", self_.value_state.heap);
            println!("PATH CONDITION: {:?}", self_.stmt_state.pc);
            println!("STATEMENT {id}: {stmt:?}");
            self_.execute(stmt).unwrap();
            println!();
            id += 1;
        }

        self_.log_pure("post", None);
        println!("HEAP: {:#?}", self_.value_state.heap);
        if let Some(post) = &method.contract.postcondition {
            self_.exhale(post).unwrap();
        }
        Ok(())
    }

    fn new_arg(&mut self, arg: &'a silver_oxide::ast::ArgOrType) {
        let silver_oxide::ast::ArgOrType::Arg((ident, _)) = arg else {
            return;
        };
        let id = self.value_state.egraph.next_symbolic_value(Some(ident.0.clone()));
        self.stmt_state.bindings.insert(Some(ident), id);
    }

    fn execute(&mut self, stmt: SiliconStatement<'a>) -> Result<(), Error<'a>> {
        use silver_oxide::ast::Statement::*;
        let stmt = match stmt {
            SiliconStatement::Viper(stmt) => stmt,
            SiliconStatement::ReplacePathCond(r) => {
                self.stmt_state.pc = r;
                return Ok(());
            }
        };
        match stmt {
            Assign(targets, exp) => {
                let new_value = self.translate_exp(exp).unwrap();
                assert_eq!(targets.len(), 1);
                let target = &targets[0];
                match target {
                    silver_oxide::ast::Exp::Ident(ident) => {
                        self.stmt_state.bindings.insert(Some(ident), new_value);
                    }
                    silver_oxide::ast::Exp::Field(r, field) => {
                        let translator = self.stmt_state.translator(TranslationMode::Expression);
                        let resource = translator.translate_field_target(r, field, &mut self.value_state).unwrap();
                        self.value_state.heap
                            .update_symbolic_value(&mut self.value_state.egraph, resource, new_value, self.stmt_state.pc)
                            .map_err(|kind| Error::expression(target, kind)).unwrap();
                    }
                    _ => todo!("{target:?}"),
                }
            }
            If(cond, then, elseif, else_) => {
                let cond = self.translate_exp_pc(cond, self.stmt_state.pc).unwrap();
                let pc = self.stmt_state.pc.add(&mut self.value_state.egraph, cond);
                let mut curr_cond = self.stmt_state.pc.add_negate(&mut self.value_state.egraph, cond);

                let conditions: Vec<_> = elseif.iter()
                    .map(|(cond, block)| {
                        let cond = self.translate_exp_pc(cond, curr_cond).unwrap();
                        let pc = curr_cond.add(&mut self.value_state.egraph, cond);
                        curr_cond = curr_cond.add_negate(&mut self.value_state.egraph, cond);
                        (pc, block)
                    })
                    .collect();

                self.stmt_state.statements.push(SiliconStatement::ReplacePathCond(self.stmt_state.pc));
                if let Some(else_) = else_ {
                    self.stmt_state.statements.extend(else_.statements.iter().rev().map(SiliconStatement::Viper));
                    self.stmt_state.statements.push(SiliconStatement::ReplacePathCond(curr_cond));
                }
                for (cond, block) in conditions.into_iter().rev() {
                    self.stmt_state.statements.extend(block.statements.iter().rev().map(SiliconStatement::Viper));
                    self.stmt_state.statements.push(SiliconStatement::ReplacePathCond(cond));
                }
                self.stmt_state.statements.extend(then.statements.iter().rev().map(SiliconStatement::Viper));
                self.stmt_state.pc = pc;
            }
            Assume(exp) => {
                self.assume(exp).unwrap();
            }
            Inhale(exp) => {
                self.inhale(exp, "inhale").unwrap();
            }
            Assert(exp) => {
                self.assert(exp).unwrap();
            }
            Exhale(exp) => {
                self.exhale(exp).unwrap();

                // println!("{:#?}", self.value_state.heap);
                // // self.value_state.egraph.egraph.dot().to_pdf(format!("./post-exhale.pdf")).unwrap();
                // // println!("{:#?}", self.value_state.egraph.egraph);

                // e.unwrap();
            }
            Fold(exp) => {
                self.state.fold(exp, self.stmt_state.pc).unwrap();
            }
            Unfold(exp) => {
                self.state.unfold(exp, self.stmt_state.pc).unwrap();
            }
            _ => todo!("{stmt:?}"),
        }
        Ok(())
    }

    fn inhale(&mut self, exp: &'a ast::Exp, reason: &str) -> Result<(), Error<'a>> {
        let exp = self.translate_for_inhale(exp)?;
        self.assume_fact(exp.expression, reason);
        Ok(())
    }
    fn assume(&mut self, exp: &'a ast::Exp) -> Result<(), Error<'a>> {
        let exp = self.translate_for_fact(exp)?;
        self.assume_fact(exp.expression, "assume");
        Ok(())
    }
    fn assume_fact(&mut self, exp: egg::Id, reason: impl Into<egg::Symbol>) {
        self.stmt_state.pc.assume(&mut self.value_state.egraph, exp, reason);
    }

    fn exhale(&mut self, exp: &'a ast::Exp) -> Result<(), Error<'a>> {
        let e = self.translate_for_exhale(exp)?;
        self.assert_fact(e.expression, exp)
    }
    fn assert(&mut self, exp: &'a ast::Exp) -> Result<(), Error<'a>> {
        let e = self.translate_for_fact(exp)?;
        self.assert_fact(e.expression, exp)
    }
    fn assert_fact(&mut self, exp: egg::Id, e: &'a ast::Exp) -> Result<(), Error<'a>> {
        self.stmt_state.pc.assert(&mut self.value_state.egraph, exp).map_err(|assertion| {
            let label = Some(format!("{}", self.value_state.egraph.normalise(assertion)));
            self.log_pure("exhale", label);
            Error::exhale(e, assertion)
        })
    }
}
