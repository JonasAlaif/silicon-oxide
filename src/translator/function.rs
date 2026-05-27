use std::{ops::Deref, path::PathBuf};

use crate::{error::Error, pure::EGraph, silicon::{Bindings, Silicon}};

use super::VerificationState;

// pub struct Function<'tcx> {
//     pub silicon: Silicon<'tcx>,
// }

#[derive(Debug)]
pub struct FunctionVerified<'e>(Option<FunctionVerifiedBody<'e>>);
impl<'e> Deref for FunctionVerified<'e> {
    type Target = Option<FunctionVerifiedBody<'e>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// impl<'tcx> Function<'tcx> {
//     pub fn verify(
//         function: &'e silver_oxide::parse::Function,
//         decls: &'a VerificationState<'e>,
//         log_dir: &'a PathBuf,
//     ) -> Result<FunctionVerified<'e>, Error<'e>> {
//         let Some(body) = &function.body else {
//             return Ok(FunctionVerified(None));
//         };
//         let fn_name = function.signature.name.0.0.as_str();
//         let mut args = Vec::new();

//         let mut silicon = Silicon::new(decls, log_dir);
//         for arg in function.signature.args.iter() {
//             let arg = silicon.new_arg(arg);
//             args.push(arg);
//         }
//         assert_eq!(silicon.value_state.egraph.next_symbolic_value as usize, args.len(), "{fn_name}");
//         let pre = &function.contract.precondition;
//         // TODO: we are assuming that the snapshot of the heap input is the
//         // first `next_symbolic_value` assigned. The current `inhale` impl does
//         // this but it may break!
//         let pre = silicon.inhale(pre, "precondition").map_err(|e| {
//             silicon.log_pure(&format!("{fn_name}_precondition"), None);
//             e
//         }).unwrap();
//         args.push(pre.snapshot);
//         let silver_oxide::parse::ArgOrType::Type(ret) = &function.signature.ret[0] else {
//             unreachable!()
//         };

//         silicon.log_pure(&format!("{fn_name}_pre"), None);

//         let body = silicon.translate_exp(&body.0).map_err(|e| {
//             silicon.log_pure(&format!("{fn_name}_error"), None);
//             e
//         }).unwrap();

//         silicon.log_pure(&format!("{fn_name}_body"), None);

//         let post = &function.contract.postcondition;
//         silicon.stmt_state.bindings.insert(None, body);
//         silicon.assert(post).unwrap();
//         // TODO: is this necessary?
//         // silicon.stmt_state.bindings.insert(None, call);
//         // silicon.assume(post).unwrap();
//         // silicon.stmt_state.bindings.remove(&None);
//         let ret = TyKind::from(ret);
//         let call = silicon.value_state.egraph.add(Exp::FuncApp(function.signature.name.0.clone(), args.clone(), ret));
//         silicon.value_state.egraph.equate(call, body, "return");

//         silicon.log_pure(&format!("{fn_name}_post"), None);

//         let bindings = silicon.stmt_state.bindings;
//         let egraph = silicon.value_state.egraph;
//         Ok(FunctionVerified(Some(FunctionVerifiedBody { bindings, egraph, body, args })))
//     }
// }

#[derive(Debug)]
pub struct FunctionVerifiedBody<'tcx> {
    pub bindings: Bindings<'tcx>,
    pub egraph: EGraph<'tcx>,
    pub body: egg::Id,
    pub args: Vec<egg::Id>,
}
