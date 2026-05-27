use std::{ops::Deref, path::PathBuf};

use crate::{error::Error, pure::EGraph, silicon::{Bindings, Silicon}, state::ValueState};

use super::VerificationState;

// pub struct Predicate<'tcx> {
//     pub silicon: Silicon<'tcx>,
// }

#[derive(Debug)]
pub struct PredicateVerified<'e>(Option<PredicateVerifiedBody<'e>>);
impl<'e> Deref for PredicateVerified<'e> {
    type Target = Option<PredicateVerifiedBody<'e>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug)]
pub struct PredicateVerifiedBody<'tcx> {
    pub bindings: Bindings<'tcx>,
    pub value_state: ValueState<'tcx>,
    // pub body: TranslationResult,
}

// impl<'tcx> Predicate<'tcx> {
    // pub fn verify(
    //     predicate: &'e silver_oxide::parse::Predicate,
    //     decls: &'a VerificationState<'e>,
    // ) -> Result<PredicateVerified<'e>, Error<'e>> {
    //     let Some(body) = &predicate.body else {
    //         return Ok(PredicateVerified(None));
    //     };
    //     let pred_name = predicate.signature.name.0.0.as_str();
    //     let mut args = Vec::new();

    //     let mut silicon = Silicon::new(decls, log_dir);
    //     for arg in predicate.signature.args.iter() {
    //         let arg = silicon.new_arg(arg);
    //         args.push(arg);
    //     }

    //     silicon.log_pure(&format!("{pred_name}_pre"), None);

    //     let body = silicon.translate_for_inhale(&body.0).map_err(|e| {
    //         silicon.log_pure(&format!("{pred_name}_error"), None);
    //         e
    //     }).unwrap();

    //     silicon.log_pure(&format!("{pred_name}_post"), None);

    //     let bindings = silicon.stmt_state.bindings;
    //     Ok(PredicateVerified(Some(PredicateVerifiedBody { bindings, value_state: silicon.value_state, body })))
    // }
// }
