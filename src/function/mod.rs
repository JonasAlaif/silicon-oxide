use std::{ops::Deref, path::PathBuf};

use crate::{declarations::Declarations0, error::Error, exp::Exp, pure::EGraph, silicon::{Bindings, Silicon}};

pub struct Function<'a, 'e> {
    pub silicon: Silicon<'a, 'e, (), ()>,
}

pub struct FunctionVerified<'e>(Option<FunctionVerifiedBody<'e>>);
impl<'e> Deref for FunctionVerified<'e> {
    type Target = Option<FunctionVerifiedBody<'e>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
pub struct FunctionVerifiedBody<'e> {
    pub bindings: Bindings<'e>,
    pub egraph: EGraph,
    pub call: egg::Id,
}

impl<'a, 'e> Function<'a, 'e> {
    pub fn verify(
        function: &'e silver_oxide::ast::Function,
        declarations: &'a Declarations0<'e>,
        log_dir: &'a PathBuf,
    ) -> Result<FunctionVerified<'e>, Error<'e>> {
        let Some(body) = &function.body else {
            return Ok(FunctionVerified(None));
        };
        let mut args = Vec::new();

        let mut silicon = Silicon::new(declarations, log_dir);
        for arg in function.signature.args.iter() {
            let arg = silicon.new_arg(arg);
            args.push(arg);
        }
        if let Some(pre) = &function.contract.precondition {
            let pre = silicon.inhale(pre, "precondition").unwrap();
            args.push(pre.snapshot);
        };
        let call = silicon.value_state.egraph.add(Exp::FuncApp(function.signature.name.clone(), args));

        let fn_name = function.signature.name.0.as_str();
        silicon.log_pure(&format!("{fn_name}_pre"), None);

        let body = silicon.translate_exp(body).map_err(|e| {
            silicon.log_pure(&format!("{fn_name}_error"), None);
            e
        }).unwrap();

        if let Some(post) = &function.contract.postcondition {
            silicon.stmt_state.bindings.insert(None, body);
            silicon.assert(post).unwrap();
            // TODO: is this necessary?
            // silicon.stmt_state.bindings.insert(None, call);
            // silicon.assume(post).unwrap();
            // silicon.stmt_state.bindings.remove(&None);
        }
        silicon.value_state.egraph.equate(call, body, "return");

        silicon.log_pure(&format!("{fn_name}_post"), None);

        let bindings = silicon.stmt_state.bindings;
        let egraph = silicon.value_state.egraph;
        Ok(FunctionVerified(Some(FunctionVerifiedBody { bindings, egraph, call })))
    }
}
