use std::{fmt, ops::{Index, IndexMut}, path::PathBuf};

use crate::{HashMap, HashSet};
use petgraph::{graph::{DiGraph, IndexType}, visit::Walker, Graph};
use silver_oxide::parse;

use crate::{declarations::ExpId, function::{Function, FunctionVerified}, method::{Method, MethodVerified}, predicate::Predicate};

use super::{CallableVerified, VerificationState};

/// The unique id of any callable declaration.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct DefId(pub(super) u32);

impl fmt::Display for DefId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "d{}", self.0)
    }
}

unsafe impl IndexType for DefId {
    fn new(x: usize) -> Self {
        Self(x as u32)
    }
    fn index(&self) -> usize {
        self.0 as usize
    }
    fn max() -> Self {
        Self(u32::MAX)
    }
}

#[derive(Default, Debug)]
pub struct CallableData<'a> {
    pub(super) data: Vec<CallableDecl<'a>>,
    pub(super) name_to_def: FxHashMap<&'a parse::Ident, DefId>,
    callgraph: DiGraph<(), (), DefId>,
    pub rev_topo_order: Vec<DefId>,
}

impl<'a> CallableData<'a> {
    pub fn insert(&mut self, name: &'a parse::Ident, kind: CallableDeclKind<'a>) -> ExpId {
        let def_id = DefId(self.data.len() as u32);
        let old = self.name_to_def.insert(name, def_id);
        assert!(old.is_none(), "redeclaration of {name:?}");

        let decl = CallableDecl { kind, analysis: CallableAnalysis::new(def_id) };
        self.data.push(decl);
        ExpId(def_id, 0)
    }

    pub fn get_def_id(&self, name: &parse::Ident) -> Option<DefId> {
        self.name_to_def.get(name).copied()
    }

    pub fn get_callable(&self, name: &parse::Ident) -> Option<&CallableDecl<'a>> {
        self.get_def_id(name).map(|def_id| &self[def_id])
    }

    pub fn pred_exp_id(&self, name: &parse::Ident) -> ExpId {
        let Some(pred_exp_id) = self.maybe_pred_exp_id(name) else {
            panic!("Expected predicate, got {name:?}");
        };
        pred_exp_id
    }

    pub fn maybe_pred_exp_id(&self, name: &parse::Ident) -> Option<ExpId> {
        let def_id = *self.name_to_def.get(name).expect("undeclared function call");
        if matches!(self[def_id].kind, CallableDeclKind::Predicate(..)) {
            Some(ExpId(def_id, 0))
        } else {
            None
        }
    }

    fn def_ids(&self) -> impl Iterator<Item = DefId> {
        (0..self.data.len()).map(|i| DefId(i as u32))
    }
    pub fn rev_topo_iter(&self) -> impl Iterator<Item = (DefId, &CallableDecl<'a>)> {
        self.rev_topo_order.iter().map(|&def_id| (def_id, &self[def_id]))
    }

    pub(super) fn calculate_analysis(&mut self, callgraph: FxHashMap<DefId, Vec<(&'a parse::Ident, bool)>>) {
        for (def_id, calls) in callgraph {
            let self_ = self as *const Self;
            let caller = &mut self.data[def_id.0 as usize].analysis;
            let calls = calls
                .iter()
                .filter_map(|&(call, is_regular_call)| {
                    let def_id = self.name_to_def[call];
                    // SAFETY: `self_` aliases `caller`, but they access different.
                    let kind = &(unsafe { &*self_ }).data[def_id.0 as usize].kind;
                    let is_regular_defn = !matches!(kind, CallableDeclKind::Predicate(..));
                    (is_regular_call == is_regular_defn).then(|| def_id)
                });
            caller.calls.extend(calls);
            caller.trans_calls.extend(&caller.calls);
        }

        // transitive closure
        let mut fixpoint = false;
        while !fixpoint {
            fixpoint = true;
            for def_id in self.def_ids() {
                let caller = &self[def_id].analysis.trans_calls;

                let mut new = FxHashSet::<DefId>::default();
                for &callee_def_id in caller.iter() {
                    if callee_def_id == def_id {
                        continue;
                    }
                    new.extend(&self[callee_def_id].analysis.trans_calls);
                }
                let pre_size = caller.len();

                let caller = &mut self[def_id].analysis.trans_calls;
                caller.extend(new.into_iter());
                fixpoint &= caller.len() == pre_size;
            }
        }

        let mut walked = vec![false; self.data.len()];
        let mut stack = vec![];
        for def_id in self.def_ids() {
            let w = std::mem::replace(&mut walked[def_id.0 as usize], true);
            if w {
                continue;
            }
            stack.push(vec![def_id]);
            while let Some(mut path) = stack.pop() {
                let top = *path.last().unwrap();
                let mut done = true;
                for &callee in &self[top].analysis.calls {
                    if walked[callee.0 as usize] {
                        continue;
                    }
                    walked[callee.0 as usize] = true;
                    path.push(callee);
                    stack.push(path);
                    done = false;
                    break;
                }
                if done {
                    self.rev_topo_order.push(top);
                }
            }
        }

        for def_id in self.def_ids() {
            let ix = self.callgraph.add_node(());
            assert_eq!(ix, def_id.into());
        }
        for analysis in &self.data {
            for &callee in &analysis.analysis.calls {
                self.callgraph.add_edge(analysis.analysis.def_id.into(), callee.into(), ());
            }
        }
        let mut dfs = petgraph::visit::DfsPostOrder::empty(&self.callgraph);
        dfs.stack = self.def_ids().map(|def_id| def_id.into()).collect();
        self.rev_topo_order = dfs.iter(&self.callgraph).map(|ix| DefId::new(ix.index())).collect();

        // println!("rev_topo_order {:?}:\n {:?}\n {:?}", self.name_to_def, self.rev_topo_order,
        //     self.rev_topo_order.iter().map(|&def_id| &self[def_id].kind.signature().name).collect::<Vec<_>>());
    }
}

impl<'a> Index<DefId> for CallableData<'a> {
    type Output = CallableDecl<'a>;
    fn index(&self, index: DefId) -> &Self::Output {
        &self.data[index.0 as usize]
    }
}

impl<'a> IndexMut<DefId> for CallableData<'a> {
    fn index_mut(&mut self, index: DefId) -> &mut Self::Output {
        &mut self.data[index.0 as usize]
    }
}

#[derive(Debug, Clone)]
pub struct CallableDecl<'a> {
    pub kind: CallableDeclKind<'a>,
    pub analysis: CallableAnalysis,
}

impl<'a> CallableDecl<'a> {
    pub(super) fn verify(&self, state: &VerificationState<'a>, log_dir: &PathBuf) -> CallableVerified<'a> {
        println!("### Verifying `{}` ###", self.kind.signature().name.0);
        match self.kind {
            CallableDeclKind::Function(f) => {
                CallableVerified::Function(Function::verify(f, state, log_dir).unwrap())
            }
            CallableDeclKind::Method(m) => {
                CallableVerified::Method(Method::verify(m, state, log_dir).unwrap())
            }
            CallableDeclKind::Predicate(p) =>
                CallableVerified::Predicate(Predicate::verify(p, state, log_dir).unwrap()),
            _ => unimplemented!("{self:?}"),
        }
    }
    // pub(super) fn verify_function(&self, decls: &Declarations1<'a>, log_dir: &PathBuf) -> Option<FunctionVerified<'a>> {
    //     match self.kind {
    //         CallableDeclKind::Function(f) => {
    //             Some(Function::verify(f, decls, log_dir).unwrap())
    //         }
    //         _ => None,
    //     }
    // }

    // pub(super) fn verify_method(&self, decls: &Declarations1<'a>, log_dir: &PathBuf) -> Option<MethodVerified> {
    //     match self.kind {
    //         CallableDeclKind::Method(m) => {
    //             Some(Method::verify(m, decls, log_dir).unwrap())
    //         }
    //         _ => None,
    //     }
    // }
}

#[derive(Debug, Clone, Copy)]
pub enum CallableDeclKind<'a> {
    DomainFunction(&'a parse::DomainFunction),
    Function(&'a parse::Function),
    Predicate(&'a parse::Predicate),
    Method(&'a parse::Method),
}

impl<'a> CallableDeclKind<'a> {
    pub fn signature(&self) -> &'a parse::Signature {
        match self {
            CallableDeclKind::DomainFunction(f) => &f.signature,
            CallableDeclKind::Function(f) => &f.signature,
            CallableDeclKind::Predicate(p) => &p.signature,
            CallableDeclKind::Method(m) => &m.signature,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CallableAnalysis {
    pub def_id: DefId,
    pub calls: FxHashSet<DefId>,
    pub trans_calls: FxHashSet<DefId>,
}

impl CallableAnalysis {
    fn new(def_id: DefId) -> Self {
        Self { def_id, calls: Default::default(), trans_calls: Default::default() }
    }

    pub fn is_recursive(&self) -> bool {
        self.trans_calls.contains(&self.def_id)
    }
}
