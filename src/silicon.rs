use std::path::PathBuf;

use fxhash::FxHashMap;
use silver_oxide::ast;

use crate::{declarations::Declarations, error::Error, exp::PathCondition, state::ValueState, translate::{TranslationMode, TranslationResult}};

#[derive(Debug, Clone)]
pub struct Silicon<'a, 'e, F, M> {
    pub value_state: ValueState,
    pub stmt_state: StmtState<'a, 'e, F, M>,

    pub log_dir: &'a PathBuf,
    // pub method: &'a silver_oxide::ast::Method,
}

/// This represents either locally declared variables and their current values,
/// or arguments to a function/predicate and their values. A none key means `result`.
pub type Bindings<'a> = FxHashMap<Option<&'a ast::Ident>, egg::Id>;

#[derive(Debug, Clone)]
pub struct StmtState<'a, 'e, F, M> {
    pub pc: PathCondition,
    pub bindings: Bindings<'e>,

    pub declarations: &'a Declarations<'e, F, M>,
}

#[derive(Debug, Clone)]
pub enum SiliconStatement<'a> {
    Viper(&'a silver_oxide::ast::Statement),
    ReplacePathCond(PathCondition),
}

impl<'a, 'e, F, M> Silicon<'a, 'e, F, M> {
    pub fn new(declarations: &'a Declarations<'e, F, M>, log_dir: &'a PathBuf) -> Self {
        let value_state = ValueState::new();
        let pc = PathCondition::new(&value_state.egraph);
        let stmt_state = StmtState {
            pc,
            bindings: Default::default(),
            declarations,
        };
        Silicon {
            value_state,
            stmt_state,
            log_dir,
        }
    }

    pub fn new_arg(&mut self, arg: &'e silver_oxide::ast::ArgOrType) -> egg::Id {
        let silver_oxide::ast::ArgOrType::Arg((ident, _)) = arg else {
            panic!("Expected named arg, got {:?}", arg);
        };
        let id = self.value_state.egraph.next_symbolic_value(Some(ident.0.clone()));
        self.stmt_state.bindings.insert(Some(ident), id);
        id
    }

    pub fn inhale(&mut self, exp: &'e ast::Exp, reason: &str) -> Result<TranslationResult, Error<'a>> {
        let exp = self.translate_for_inhale(exp)?;
        self.assume_fact(exp.expression, reason);
        Ok(exp)
    }
    pub fn assume(&mut self, exp: &'e ast::Exp) -> Result<(), Error<'a>> {
        let exp = self.translate_for_fact(exp)?;
        self.assume_fact(exp.expression, "assume");
        Ok(())
    }
    fn assume_fact(&mut self, exp: egg::Id, reason: impl Into<egg::Symbol>) {
        self.stmt_state.pc.assume(&mut self.value_state.egraph, exp, reason);
    }

    pub fn exhale(&mut self, exp: &'e ast::Exp) -> Result<(), Error<'a>> {
        let e = self.translate_for_exhale(exp)
            .map_err(|err| { self.log_err(&err); err })?;
        self.assert_fact(e.expression, exp)
    }
    pub fn assert(&mut self, exp: &'e ast::Exp) -> Result<(), Error<'a>> {
        let e = self.translate_for_fact(exp)?;
        self.assert_fact(e.expression, exp)
    }
    fn assert_fact(&mut self, exp: egg::Id, e: &'e ast::Exp) -> Result<(), Error<'a>> {
        self.stmt_state.pc.assert(&mut self.value_state.egraph, exp).map_err(|assertion| {
            let label = Some(format!("{}", self.value_state.egraph.normalise(assertion)));
            self.log_pure("assert", label);
            Error::exhale(e, assertion)
        })
    }

    pub fn unfold(&mut self, exp: &'e ast::Exp) -> Result<(), Error<'a>> {
        let body = self.translate_for_unfold(exp)?;
        self.assume_fact(body, "unfold");
        Ok(())
    }
    pub fn fold(&mut self, exp: &'e ast::Exp) -> Result<(), Error<'a>> {
        let body = self.translate_for_fold(exp)?;
        self.assert_fact(body, exp)
    }

    pub fn log_err(&mut self, _err: &Error<'e>) {
        self.log_pure("err", None);
    }
}
