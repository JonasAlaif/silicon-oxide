use std::path::PathBuf;

use crate::{declarations::Declarations1, error::Error, function::FunctionVerified, silicon::{Silicon, SiliconStatement}, translate::TranslationMode};

pub struct Method<'a, 'e> {
    silicon: Silicon<'a, 'e, FunctionVerified<'a>, ()>,
    statements: Vec<SiliconStatement<'a>>,
}

pub struct MethodVerified(());

impl<'a, 'e> Method<'a, 'e> {
    pub fn verify(
        method: &'e silver_oxide::ast::Method,
        declarations: &'a Declarations1<'e>,
        log_dir: &'a PathBuf,
    ) -> Result<MethodVerified, Error<'e>> {
        let Some(body) = &method.body else {
            return Ok(MethodVerified(()));
        };
        let mut silicon = Silicon::new(declarations, log_dir);
        for arg in method.signature.args.iter() {
            silicon.new_arg(arg);
        }
        for ret in method.signature.ret.iter() {
            silicon.new_arg(ret);
        }
        if let Some(pre) = &method.contract.precondition {
            silicon.inhale(pre, "precondition").unwrap();
        }
        let mut id = 0;

        let mut self_ = Self {
            silicon,
            statements: body.statements.iter().rev().map(SiliconStatement::Viper).collect(),
        };
        while let Some(stmt) = self_.statements.pop() {
            self_.silicon.value_state.egraph.saturate();
            self_.silicon.log_pure(&format!("_stmt{id}"), Some(format!("{stmt:?}")));
            println!("HEAP: {:#?}", self_.silicon.value_state.heap);
            println!("PATH CONDITION: {:?}", self_.silicon.stmt_state.pc);
            println!("STATEMENT {id}: {stmt:?}");
            self_.execute(stmt).unwrap();
            println!();
            id += 1;
        }

        self_.silicon.log_pure("post", None);
        println!("HEAP: {:#?}", self_.silicon.value_state.heap);
        if let Some(post) = &method.contract.postcondition {
            self_.silicon.exhale(post).unwrap();
        }
        Ok(MethodVerified(()))
    }

    fn execute(&mut self, stmt: SiliconStatement<'e>) -> Result<(), Error<'e>> {
        use silver_oxide::ast::Statement::*;
        let stmt = match stmt {
            SiliconStatement::Viper(stmt) => stmt,
            SiliconStatement::ReplacePathCond(r) => {
                self.silicon.stmt_state.pc = r;
                return Ok(());
            }
        };
        match stmt {
            Assign(targets, exp) => {
                let new_value = self.silicon.translate_exp(exp).unwrap();
                assert_eq!(targets.len(), 1);
                let target = &targets[0];
                match target {
                    silver_oxide::ast::Exp::Ident(ident) => {
                        self.silicon.stmt_state.bindings.insert(Some(ident), new_value);
                    }
                    silver_oxide::ast::Exp::Field(r, field) => {
                        let translator = self.silicon.stmt_state.translator(TranslationMode::Expression);
                        let (resource, _) = translator.translate_field_target(r, field, &mut self.silicon.value_state).unwrap();
                        self.silicon.value_state.heap
                            .update_symbolic_value(&mut self.silicon.value_state.egraph, resource, new_value, self.silicon.stmt_state.pc)
                            .map_err(|kind| Error::expression(target, kind)).unwrap();
                    }
                    _ => todo!("{target:?}"),
                }
            }
            If(cond, then, elseif, else_) => {
                let cond = self.silicon.translate_exp_pc(cond, self.silicon.stmt_state.pc).unwrap();
                let pc = self.silicon.stmt_state.pc.add(&mut self.silicon.value_state.egraph, cond);
                let mut curr_cond = self.silicon.stmt_state.pc.add_negate(&mut self.silicon.value_state.egraph, cond);

                let conditions: Vec<_> = elseif.iter()
                    .map(|(cond, block)| {
                        let cond = self.silicon.translate_exp_pc(cond, curr_cond).unwrap();
                        let pc = curr_cond.add(&mut self.silicon.value_state.egraph, cond);
                        curr_cond = curr_cond.add_negate(&mut self.silicon.value_state.egraph, cond);
                        (pc, block)
                    })
                    .collect();

                self.statements.push(SiliconStatement::ReplacePathCond(self.silicon.stmt_state.pc));
                if let Some(else_) = else_ {
                    self.statements.extend(else_.statements.iter().rev().map(SiliconStatement::Viper));
                    self.statements.push(SiliconStatement::ReplacePathCond(curr_cond));
                }
                for (cond, block) in conditions.into_iter().rev() {
                    self.statements.extend(block.statements.iter().rev().map(SiliconStatement::Viper));
                    self.statements.push(SiliconStatement::ReplacePathCond(cond));
                }
                self.statements.extend(then.statements.iter().rev().map(SiliconStatement::Viper));
                self.silicon.stmt_state.pc = pc;
            }
            Assume(exp) => {
                self.silicon.assume(exp).unwrap();
            }
            Inhale(exp) => {
                self.silicon.inhale(exp, "inhale").unwrap();
            }
            Assert(exp) => {
                self.silicon.assert(exp).unwrap();
            }
            Exhale(exp) => {
                self.silicon.exhale(exp).unwrap();

                // println!("{:#?}", self.silicon.value_state.heap);
                // // self.silicon.value_state.egraph.egraph.dot().to_pdf(format!("./post-exhale.pdf")).unwrap();
                // // println!("{:#?}", self.silicon.value_state.egraph.egraph);

                // e.unwrap();
            }
            Fold(exp) => {
                self.silicon.fold(exp).unwrap();
            }
            Unfold(exp) => {
                self.silicon.unfold(exp).unwrap();
            }
            _ => todo!("{stmt:?}"),
        }
        Ok(())
    }
}
