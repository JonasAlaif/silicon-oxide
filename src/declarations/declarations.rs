use std::{fmt, ops::{Deref, Index}, path::PathBuf, ptr::NonNull};

use crate::{HashMap, HashSet};
use silver_oxide::parse::{self, AstWalkable, AstWalker};

use crate::{function::{Function, FunctionVerified}, method::{Method, MethodVerified}, predicate::PredicateVerified, pure::TyKind};

use super::{CallableData, CallableDecl, CallableDeclKind, DefId, ExpData, ExpId};

#[derive(Default, Debug)]
pub struct Declarations<'a> {
    pub callable: CallableData<'a>,

    pub field: FxHashMap<&'a parse::Ident, &'a parse::Type>,
    pub ty: FxHashMap<&'a parse::Ident, TypeDecl<'a>>,
    pub axiom: Vec<&'a parse::Axiom>,
    pub expd: ExpData<'a>,
}

#[derive(Debug)]
pub struct VerificationState<'a> {
    decls: Declarations<'a>,
    curr_def_id: DefId,
    analysis: Vec<Option<CallableVerified<'a>>>,
}
impl<'a> Deref for VerificationState<'a> {
    type Target = Declarations<'a>;
    fn deref(&self) -> &Self::Target {
        &self.decls
    }
}

#[derive(Debug)]
pub enum CallableVerified<'a> {
    Function(FunctionVerified<'a>),
    Method(MethodVerified),
    DomainFunction,
    Predicate(PredicateVerified<'a>),
}

impl<'a> VerificationState<'a> {
    fn new(program: &'a parse::Program) -> Self {
        let mut collector = AstCollector::default();
        collector.walk_program(program);
        let decls = collector.finalise();
        VerificationState {
            curr_def_id: DefId::default(),
            analysis: (0..decls.callable.data.len()).map(|_| None).collect(),
            decls,
        }
    }

    pub fn verify(program: &'a parse::Program, log_dir: &PathBuf) -> Self {
        let mut self_ = Self::new(program);
        for (did, v) in self_.decls.callable.rev_topo_iter() {
            self_.curr_def_id = did;
            let v = v.verify(&self_, log_dir);
            self_.analysis[did.0 as usize] = Some(v);
        }
        self_
    }

    // pub fn verify_functions(self, log_dir: &PathBuf) -> Declarations1<'a> {
    //     let mut self_ = Declarations1 {
    //         decls: self,
    //         curr_def_id: DefId::default(),
    //         fn_analysis: Default::default(),
    //     };

    //     for (did, v) in self_.decls.callable.rev_topo_iter() {
    //         self_.curr_def_id = did;
    //         let Some(f) = v.verify_function(&self_, log_dir) else {
    //             continue;
    //         };
    //         self_.fn_analysis.insert(did, f);
    //     }
    //     // let fn_analysis = self.callable.rev_topo_iter().flat_map(
    //     //     |(did, v)| v.verify_function(&self, log_dir).map(|v| (did, v))
    //     // ).collect();
    //     // // let callable = self.callable.iter().map(
    //     // //     |&v| (k, v.verify_functions(&self, log_dir))
    //     // // ).collect();
    //     // Declarations1 {
    //     //     decls: self,
    //     //     fn_analysis,
    //     // }
    //     self_
    // }

    pub fn get_fn_analysis(&self, callee: DefId) -> Option<&FunctionVerified<'a>> {
        self.get_analysis(callee).map(|v| match v {
            CallableVerified::Function(f) => f,
            _ => panic!("Expected function analysis, got {v:?}"),
        })
    }

    fn get_analysis(&self, callee: DefId) -> Option<&CallableVerified<'a>> {
        let trans_calls = &self.callable[callee].analysis.trans_calls;
        if trans_calls.contains(&self.curr_def_id) {
            None
        } else {
            Some(&self.analysis[callee.0 as usize].as_ref().expect("topo order not preserved"))
        }
    }
}

// impl<'a> Declarations1<'a> {
//     pub fn verify_methods(self, log_dir: &PathBuf) -> Declarations2<'a> {
//         let method_analysis = self.callable.rev_topo_iter().flat_map(
//             |(did, v)| v.verify_method(&self, log_dir).map(|v| (did, v))
//         ).collect();
//         // let mut callable: FxHashMap<_, _> = self.callable.iter().flat_map(
//         //     |(&k, v)| v.verify_methods(&self, log_dir).map(|v| (k, v))
//         // ).collect();
//         // callable.extend(self.callable.into_iter().filter_map(
//         //     |(k, v)| v.move_non_methods().map(|v| (k, v))
//         // ));
//         Declarations2 {
//             decls: self,
//             method_analysis,
//         }
//     }
// }

// Declarations

#[derive(Debug)]
pub enum TypeDecl<'a> {
    Domain(&'a parse::Domain),
    Adt(&'a parse::Adt),
}

#[derive(Debug, Default)]
struct AstCollector<'a> {
    decls: Declarations<'a>,
    next_exp_id: Option<ExpId>,
    call_graph: FxHashMap<DefId, Vec<(&'a parse::Ident, bool)>>,
}

impl<'a> AstCollector<'a> {
    fn finalise(mut self) -> Declarations<'a> {
        self.decls.expd.init_perm_kind(|_expd, exp| {
            match exp {
                parse::Exp::Acc(acc) => {
                    let kind = match &acc.acc.loc {
                        parse::Exp::Field(_, ident) =>
                            TyKind::from(self.decls.field[ident]),
                        parse::Exp::FuncApp(ident, _) =>
                            TyKind::Snapshot(self.decls.callable.pred_exp_id(ident)),
                        _ => unreachable!(),
                    };
                    Some((&acc.acc.loc, kind))
                }
                parse::Exp::FuncApp(ident, _) =>
                    Some((exp, TyKind::Snapshot(self.decls.callable.maybe_pred_exp_id(ident)?))),
                _ => None,
            }
        });

        self.decls.callable.calculate_analysis(self.call_graph);

        self.decls
    }

    fn insert_callable_decl(&mut self, name: &'a parse::Ident, kind: CallableDeclKind<'a>) {
        let eid = self.decls.callable.insert(name, kind);
        self.next_exp_id = Some(eid);
    }
}

impl<'a> AstWalker<'a> for AstCollector<'a> {
    fn walk_domain_function(&mut self, ast: &'a parse::DomainFunction) {
        let kind = CallableDeclKind::DomainFunction(ast);
        self.insert_callable_decl(&ast.signature.name, kind);
        ast.walk_children(self);
        self.next_exp_id = None;
    }
    fn walk_axiom(&mut self, ast: &'a parse::Axiom) {
        self.decls.axiom.push(ast);
        ast.walk_children(self);
    }
    fn walk_field(&mut self, ast: &'a parse::Field) {
        for (name, ty) in &ast.fields {
            self.decls.field.insert(name, ty);
        }
        ast.walk_children(self);
    }
    fn walk_function(&mut self, ast: &'a parse::Function) {
        let kind = CallableDeclKind::Function(ast);
        self.insert_callable_decl(&ast.signature.name, kind);
        ast.walk_children(self);
        self.next_exp_id = None;
    }
    fn walk_predicate(&mut self, ast: &'a parse::Predicate) {
        let kind = CallableDeclKind::Predicate(ast);
        self.insert_callable_decl(&ast.signature.name, kind);
        ast.walk_children(self);
        self.next_exp_id = None;
    }
    fn walk_method(&mut self, ast: &'a parse::Method) {
        let kind = CallableDeclKind::Method(ast);
        self.insert_callable_decl(&ast.signature.name, kind);
        ast.walk_children(self);
        self.next_exp_id = None;
    }
    fn walk_domain(&mut self, ast: &'a parse::Domain) {
        self.decls.ty.insert(&ast.name, TypeDecl::Domain(ast));
        ast.walk_children(self);
    }
    fn walk_adt(&mut self, ast: &'a parse::Adt) {
        self.decls.ty.insert(&ast.name, TypeDecl::Adt(ast));
        ast.walk_children(self);
    }

    fn walk_exp(&mut self, ast: &'a parse::Exp) {
        if let Some(next_exp_id) = &mut self.next_exp_id {
            self.decls.expd.insert(ast, *next_exp_id);
            next_exp_id.1 += 1;

            match ast {
                parse::Exp::FuncApp(id, _) => {
                    self.call_graph.entry(next_exp_id.0).or_default().push((id, true));
                }
                parse::Exp::Unfolding(e, _) => {
                    let parse::Exp::FuncApp(id, _) = &e.acc.loc else {
                        panic!("Expected predicate in unfolding, got {e:?}")
                    };
                    self.call_graph.entry(next_exp_id.0).or_default().push((id, false));
                }
                _ => {}
            }
        }
        // TODO: do we actually need to assign an id to all sub-expressions as
        // well?
        ast.walk_children(self);
    }
}
