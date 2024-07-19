use fxhash::FxHashMap;

use crate::{error::Error, exp::Exp, state::{State, TranslationMode}};

#[derive(Debug, Clone)]
pub struct Silicon<'a> {
    pub state: State<'a>,

    pub pc: Vec<egg::Id>,

    // pub method: &'a silver_oxide::ast::Method,
    pub statements: Vec<SiliconStatement<'a>>,
}

#[derive(Debug, Clone)]
pub enum SiliconStatement<'a> {
    Viper(&'a silver_oxide::ast::Statement),
    ReplacePathCond(Option<egg::Id>),
}

impl<'a> Silicon<'a> {
    pub fn verify(
        method: &'a silver_oxide::ast::Method,
        declarations: &'a FxHashMap<&'a str, &'a silver_oxide::ast::Declaration>
    ) -> Result<(), Error<'a>> {
        let Some(body) = &method.body else {
            return Ok(());
        };

        let mut self_ = Silicon {
            state: State::new(declarations),
            pc: vec![],
            // method,
            statements: body.statements.iter().rev().map(SiliconStatement::Viper).collect(),
        };
        for arg in method.signature.args.iter() {
            self_.new_arg(arg);
        }
        for ret in method.signature.ret.iter() {
            self_.new_arg(ret);
        }
        for pre in method.contract.preconditions.iter() {
            let e = self_.state.inhale(pre, "precondition", &self_.pc);

            // println!("{:#?}", self.state.heap);
            // self_.state.egraph.egraph.dot().with_config_line("ranksep=2.5").to_pdf(format!("./post-inhale-pre.pdf")).unwrap();

            e.unwrap();
        }
        let mut id = 0;

        while let Some(stmt) = self_.statements.pop() {
            self_.state.egraph.saturate();
            self_.state.egraph.egraph.dot().with_config_line("ranksep=2.5").to_dot(format!("./test{id}.dot")).unwrap();
            println!("HEAP: {:#?}", self_.state.heap);
            println!("PATH CONDITION: {:?}", self_.pc);
            println!("STATEMENT: {stmt:?}");
            self_.execute(stmt).unwrap();
            println!();
            id += 1;
        }

        println!("HEAP: {:#?}", self_.state.heap);
        for post in method.contract.postconditions.iter() {
            self_.state.assert(post, &self_.pc).unwrap()
        }
        Ok(())
    }

    fn new_arg(&mut self, arg: &silver_oxide::ast::ArgOrType) {
        let silver_oxide::ast::ArgOrType::Arg((ident, _)) = arg else {
            return;
        };
        self.state.new_binding(ident);
    }

    fn execute(&mut self, stmt: SiliconStatement<'a>) -> Result<(), Error<'a>> {
        use silver_oxide::ast::Statement::*;
        let stmt = match stmt {
            SiliconStatement::Viper(stmt) => stmt,
            SiliconStatement::ReplacePathCond(r) => {
                self.pc.pop();
                if let Some(r) = r {
                    self.pc.push(r);
                }
                return Ok(());
            }
        };
        match stmt {
            Assign(targets, exp) => {
                let new_value = self.state.translate(exp, TranslationMode::Expression, &self.pc).unwrap();
                assert_eq!(targets.len(), 1);
                let target = &targets[0];
                match target {
                    silver_oxide::ast::Exp::Ident(ident) => {
                        self.state.bindings.insert(ident.clone(), new_value);
                    }
                    silver_oxide::ast::Exp::Field(r, field) => {
                        let resource = self.state.translate_field_target(r, field, &self.pc).unwrap();
                        self.state.heap
                            .update_symbolic_value(&mut self.state.egraph, resource, new_value, &self.pc)
                            .map_err(|_| Error::missing_write_permission(r, field)).unwrap();
                    }
                    _ => todo!("{target:?}"),
                }
            }
            If(cond, then, elseif, else_) => {
                let cond = self.state.translate(cond, TranslationMode::Expression, &self.pc).unwrap();
                let mut curr_cond = self.state.egraph.add(Exp::Not(cond));

                let conditions: Vec<_> = elseif.iter()
                    .map(|(cond, block)| {
                        let cond = self.state.translate(cond, TranslationMode::Expression, &self.pc).unwrap();
                        let pc = self.state.egraph.add(Exp::BinOp(silver_oxide::ast::BinOp::And, [curr_cond, cond]));
                        let neg_cond = self.state.egraph.add(Exp::Not(cond));
                        curr_cond = self.state.egraph.add(Exp::BinOp(silver_oxide::ast::BinOp::And, [curr_cond, neg_cond]));
                        (pc, block)
                    })
                    .collect();

                self.statements.push(SiliconStatement::ReplacePathCond(None));
                if let Some(else_) = else_ {
                    self.statements.extend(else_.statements.iter().rev().map(SiliconStatement::Viper));
                    self.statements.push(SiliconStatement::ReplacePathCond(Some(curr_cond)));
                }
                for (cond, block) in conditions.into_iter().rev() {
                    self.statements.extend(block.statements.iter().rev().map(SiliconStatement::Viper));
                    self.statements.push(SiliconStatement::ReplacePathCond(Some(cond)));
                }
                self.statements.extend(then.statements.iter().rev().map(SiliconStatement::Viper));
                self.pc.push(cond);

                // let cond = self.state.translate(cond, TranslationMode::Expression, &self.pc).unwrap();
                // let mut next = self.clone();
                // self.state.egraph.assume(cond, "path condition");
                // self.statements = self.statements.iter().copied().chain(then.statements.iter().rev()).collect();

                // let mut prev_cond = next.state.egraph.add(Exp::Not(cond));
                // let mut self_ = vec![next];
                // for (cond, then) in elseif {
                //     let curr = self_.last_mut().unwrap();
                //     let cond = curr.state.translate(cond, TranslationMode::Expression, &self.pc).unwrap();
                //     let mut next = curr.clone();

                //     let path_cond = curr.state.egraph.add(Exp::BinOp(silver_oxide::ast::BinOp::And, [prev_cond, cond]));
                //     curr.state.egraph.assume(path_cond, "path condition");
                //     curr.statements = curr.statements.iter().copied().chain(then.statements.iter().rev()).collect();

                //     let neg_cond = next.state.egraph.add(Exp::Not(cond));
                //     prev_cond = next.state.egraph.add(Exp::BinOp(silver_oxide::ast::BinOp::And, [prev_cond, neg_cond]));
                //     self_.push(next);
                // }

                // let curr = self_.last_mut().unwrap();
                // curr.state.egraph.assume(prev_cond, "path condition");
                // if let Some(else_) = else_ {
                //     curr.statements = curr.statements.iter().copied().chain(else_.statements.iter().rev()).collect();
                // }
                // return Ok(self_)
            }
            Assume(exp) => {
                self.state.assume(exp, "assume", &self.pc).unwrap();
            }
            Inhale(exp) => {
                self.state.inhale(exp, "inhale", &self.pc).unwrap();
            }
            Assert(exp) => {
                self.state.assert(exp, &self.pc).unwrap();
            }
            Exhale(exp) => {
                let e = self.state.exhale(exp, &self.pc);

                println!("{:#?}", self.state.heap);
                // self.state.egraph.egraph.dot().to_pdf(format!("./post-exhale.pdf")).unwrap();
                // println!("{:#?}", self.state.egraph.egraph);

                e.unwrap();
            }
            _ => todo!("{stmt:?}"),
        }
        Ok(())
    }
}
