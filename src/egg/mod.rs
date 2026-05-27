// pub mod r#impl;
// mod rewrite;

// pub use rewrite::*;

use std::sync::OnceLock;

use crate::pure::{Constants, EggAnalysis, EggExp};

pub fn rewrites<'tcx>(_constants: Constants) -> &'static [egg::Rewrite<EggExp, EggAnalysis>] {
    static RULES: OnceLock<Vec<egg::Rewrite<EggExp, EggAnalysis>>> = OnceLock::new();
    &**RULES.get_or_init(|| {
        // TOOD:
        [
            
        ].to_vec()
    })
}
