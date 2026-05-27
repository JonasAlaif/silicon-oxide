use crate::{exp::{BinOp, Exp, UnOp}, pure::Ty, translator::VerificationState};

use super::{EGraph, TyG, TyKind};
use ast::Ast;
use silver_oxide::HashMap;
use z3::*;

use std::{collections::hash_map::Entry, fmt::format, ops::{Add, Deref, Mul}};

impl<'tcx> EGraph<'tcx> {
    pub fn z3_assert(&self, assertion: egg::Id, reason: &str, decls: &VerificationState<'_>) -> bool {
        println!("Z3 asserting {assertion:?} for \"{reason}\"");
        let mut cfg = Config::new();
        cfg.set_bool_param_value("trace", true);
        cfg.set_timeout_msec(100);
        let ctx = Context::new(&cfg);
        let mut translator = EGraphToZ3::new(&ctx);
        translator.intern(self, decls);

        let assertion = self.normalise(assertion);
        let sat = translator.check(assertion);
        println!("sat: {sat:?}");
        let res = matches!(sat, SatResult::Unsat);
        if !res {
            let model = translator.solver.get_model();
            println!("**solver**:\n{}\n**model**:\n{:?}", translator.solver, model);
        }
        res
    }
}

struct EGraphToZ3Data<'z> {
    ctx: &'z Context,
    solver: Solver<'z>,

    null: ast::Dynamic<'z>,
    ref_sort: Sort<'z>,
    pred_sort: Sort<'z>,
}

impl<'z> EGraphToZ3Data<'z> {
    fn new(ctx: &'z Context) -> Self {
        let ref_sort = Sort::uninterpreted(ctx, "Ref".into());
        Self {
            ctx,
            solver: Solver::new(ctx),
            null: FuncDecl::new(ctx, "null", &[], &ref_sort).apply(&[]),
            ref_sort,
            pred_sort: Sort::uninterpreted(ctx, "Pred".into()),
        }
    }
}

struct EGraphToZ3<'z> {
    data: EGraphToZ3Data<'z>,
    eclasses: HashMap<egg::Id, ast::Dynamic<'z>>,
    functions: HashMap<silver_oxide::parse::Ident, FuncDecl<'z>>,
    // snapshots_unbuilt: HashMap<ResId, DatatypeBuilder<'z>>,
    // snapshots: HashMap<ResId, DatatypeSort<'z>>,
}

impl<'z> Deref for EGraphToZ3<'z> {
    type Target = EGraphToZ3Data<'z>;
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<'z> EGraphToZ3<'z> {
    pub fn new(ctx: &'z Context) -> Self {
        Self {
            data: EGraphToZ3Data::new(ctx),
            eclasses: Default::default(),
            functions: Default::default(),
            // snapshots_unbuilt: Default::default(),
            // snapshots: Default::default(),
        }
    }

    pub fn check(&mut self, assertion: egg::Id) -> SatResult {
        let assertion = self.get_representative(&assertion);
        let assertion = assertion.as_bool().unwrap();
        self.solver.assert(&assertion.not());
        self.solver.check()
    }

    pub fn intern(&mut self, egraph: &EGraph, decls: &VerificationState<'_>) {
        for eclass in egraph.egraph.classes() {
            let TyG::Snapshot(eid) = &eclass.data else {
                continue;
            };
            todo!()
            // self.create_datatype_builder(*eid, decls);
        }

        todo!();
        // let (eids, datatypes): (Vec<_>, Vec<_>) = self.snapshots_unbuilt.drain().unzip();
        // let datatypes = datatype_builder::create_datatypes(datatypes);
        // self.snapshots.extend(eids.into_iter().zip(datatypes.into_iter()));

        for eclass in egraph.egraph.classes() {
            let id = eclass.id;
            let ty = eclass.data.kind();
            let sort = self.ty_kind_to_sort(ty);
            let value = FuncDecl::new(&self.ctx, id.to_string(), &[], &sort).apply(&[]);
            self.eclasses.insert(id, value);
        }

        // for eclass in egraph.egraph.classes() {
        //     self.create_snap_representative(egraph, eclass.id);
        // }

        for eclass in egraph.egraph.classes() {
            let id = eclass.id;
            let ty = &eclass.data;

            for enode in &eclass.nodes {
                // use num_traits::cast::ToPrimitive;
                let node = match enode {
                    Exp::Const(c) => todo!(),
                    // match c {
                    //     silver_oxide::parse::ConstKind::Bool(b) =>
                    //         ast::Dynamic::from(ast::Bool::from_bool(&self.ctx, *b)),
                    //     silver_oxide::parse::ConstKind::Int(big_int) =>
                    //         ast::Dynamic::from(ast::Int::from_i64(&self.ctx, big_int.to_i64().unwrap())),
                    //     silver_oxide::parse::ConstKind::Null =>
                    //         self.null.clone(),
                    //     silver_oxide::parse::ConstKind::None =>
                    //         ast::Dynamic::from(ast::Real::from_real(&self.ctx, 0, 1)),
                    //     silver_oxide::parse::ConstKind::Write =>
                    //         ast::Dynamic::from(ast::Real::from_real(&self.ctx, 1, 1)),
                    //     silver_oxide::parse::ConstKind::Epsilon => todo!(),
                    //     silver_oxide::parse::ConstKind::Wildcard => {
                    //         let c = ast::Real::fresh_const(self.ctx, "wildcard");
                    //         self.solver.assert(&c.gt(&ast::Real::from_real(self.ctx, 0, 1)));
                    //         ast::Dynamic::from(c)
                    //     }
                    // }
                    Exp::FuncApp(ident, domain, _) =>
                        self.apply_function(ident, domain, id),
                    Exp::PredicateApp(ident, domain) =>
                        self.apply_function(ident, domain, id),
                    Exp::SymbolicValue(_) =>
                        continue,
                    Exp::BinOp(bin_op, [l, r]) => {
                        let (l, r) = (self.get_representative(l), self.get_representative(r));
                        macro_rules! int_or_real {
                            ($s:tt) => {
                                if let Some(l) = l.as_int() {
                                    ast::Dynamic::from(l.$s(&r.as_int().expect("lhs is int, rhs is not")))
                                } else {
                                    ast::Dynamic::from(l.as_real().expect("lhs is neither int or real").$s(&r.as_real().expect("lhs is real, rhs is not")))
                                }
                            };
                        };
                        match bin_op {
                            BinOp::And => ast::Dynamic::from(l.as_bool().unwrap() & r.as_bool().unwrap()),
                            BinOp::Or => ast::Dynamic::from(l.as_bool().unwrap() | r.as_bool().unwrap()),
                            BinOp::Eq => ast::Dynamic::from(l._eq(r)),
                            BinOp::Lt => int_or_real!(lt),
                            BinOp::Plus => int_or_real!(add),
                            BinOp::Mult => int_or_real!(mul),
                            BinOp::Div => int_or_real!(div),
                            BinOp::Mod => ast::Dynamic::from(l.as_int().unwrap() % r.as_int().unwrap()),
                        }
                    }
                    Exp::Ternary([c, t, e]) => {
                        let cz = self.get_representative(c).as_bool().unwrap();
                        let (tz, ez) = (self.get_representative(t), self.get_representative(e));
                        // let (tz_uns, ez_uns) = (tz.get_sort() == self.snap_sort, ez.get_sort() == self.snap_sort);
                        // // println!("c: {cz:?}, t: {tz_uns}/{tz:?} ({:?}), e: {ez_uns}/{ez:?} ({:?})", tz.get_sort(), ez.get_sort());
                        // if tz_uns ^ ez_uns {
                        //     // Get the sort of the non-generic snapshot
                        //     let sort = if tz_uns { *e } else { *t };
                        //     let sort = self.eclass_to_snap[&sort];
                        //     let sort = &self.snapshots[sort].1.sort;
                        //     let conversion = FuncDecl::new(&self.ctx, "gen_to_spec", &[&self.snap_sort], sort);
                        //     if tz_uns {
                        //         let tz = conversion.apply(&[tz]);
                        //         cz.ite(&tz, ez)
                        //     } else {
                        //         let ez = conversion.apply(&[ez]);
                        //         cz.ite(tz, &ez)
                        //     }
                        // } else {
                            cz.ite(tz, ez)
                        // }
                    }
                    Exp::UnOp(un_op, e) => {
                        let e = self.get_representative(e);
                        match un_op {
                            UnOp::Not => ast::Dynamic::from(e.as_bool().unwrap().not()),
                            UnOp::Neg => if let Some(e) = e.as_int() {
                                ast::Dynamic::from(- e)
                            } else {
                                ast::Dynamic::from(- e.as_real().unwrap())
                            }
                            UnOp::IntToReal => {
                                ast::Dynamic::from(e.as_int().unwrap().to_real())
                            }
                        }
                    }
                    Exp::Snapshot(domain, eid) => {
                        let args = domain.iter().map(|d| self.get_representative(d) as &dyn ast::Ast).collect::<Vec<_>>();
                        // println!("snapshot: {:?}, args: {args:?} ({:?})",
                        //     self.snapshots[eid].variants[0].constructor,
                        //     args.iter().map(|a| a.get_sort()).collect::<Vec<_>>(),
                        // );
                        todo!()
                        // self.snapshots[eid].variants[0].constructor.apply(&args)

                        // let Ty::Snapshot(Some(tys)) = ty else {
                        //     unreachable!()
                        // };
                        // if tys.is_empty() {
                        //     self.empty_snap.clone()
                        // } else {
                        //     let args = domain.iter().map(|d| self.get_representative(d) as &dyn ast::Ast).collect::<Vec<_>>();
                        //     let variant = &self.snapshots[self.eclass_to_snap[&id]].1.variants[0];
                        //     // println!("variant: {variant:?}, to args: {args:?}/{:?}", args.iter().map(|a| a.kind()).collect::<Vec<_>>());
                        //     variant.constructor.apply(&args)
                        // }
                    }
                    Exp::Project(s, i, ty) => {
                        // let Some(snap_idx) = &self.eclass_to_snap.get(s) else {
                        //     continue;
                        // };
                        let Ty::Snapshot(eid) = &egraph.egraph[*s].data else {
                            unreachable!()
                        };
                        // println!("projecting {s:?} at {i}, expected {ty}\n{:?}", exps[*eid]);
                        todo!()
                        // let accessor = &self.snapshots[eid].variants[0].accessors[*i];
                        // accessor.apply(&[self.get_representative(s)])
                    }
                    _ => todo!(),
                };
                let rep = self.get_representative(&id);
                assert_eq!(rep.get_sort(), node.get_sort(), "expected {rep:?}, got {node:?}, doing {enode:?}");
                self.solver.assert(&rep._eq(&node));
            }
        }
    }

    fn ty_kind_to_sort(&mut self, ty: TyKind) -> Sort<'z> {
        todo!()
        // let eid = match self.data.ty_kind_to_sort_simple(ty) {
        //     Ok(sort) => return sort,
        //     Err(eid) => eid,
        // };
        // self.snapshots[&eid].sort.clone()
    }

    // fn create_datatype_builder(&mut self, eid: ResId, decls: &VerificationState<'_>) {
    //     let Entry::Vacant(entry) = self.snapshots_unbuilt.entry(eid) else {
    //         return;
    //     };

    //     let mut recursive = Vec::new();
    //     let ResId::Concrete(_) = eid else {
    //         todo!("Opaque snapshots in z3");
    //     };
    //     let compound = &decls.resolved[eid];
    //     let names = compound.iter().enumerate().map(|(i, _)| format!("_{i}")).collect::<Vec<_>>();
    //     let fields = compound.iter()
    //         .map(|pk| TyKind::from(&decls.resolved[*pk]))
    //         .zip(names.iter().map(String::as_str))
    //         .map(|(pk, name)| (name, match self.data.ty_kind_to_sort_simple(pk) {
    //             Ok(sort) => DatatypeAccessor::Sort(sort),
    //             Err(eid) => {
    //                 let name = Self::eid_to_name(eid).into();
    //                 recursive.push(eid);
    //                 DatatypeAccessor::Datatype(name)
    //             }
    //         })).collect::<Vec<_>>();
    //     let name = Self::eid_to_name(eid);
    //     let variant_name = format!("new_{name}");
    //     let db = DatatypeBuilder::new(self.data.ctx, name)
    //         .variant(&variant_name, fields)
    //         // Avoids infinitely recursive types (which can still be constructed
    //         // if the permission for the recursive predicate is under `false`).
    //         .variant("magic", Vec::new());
    //     entry.insert(db);
    //     for r in recursive {
    //         self.create_datatype_builder(r, decls);
    //     }
    // }

    // fn eid_to_name(eid: ResId) -> String {
    //     format!("snap_{eid}")
    // }

    fn get_representative(&self, id: &egg::Id) -> &ast::Dynamic<'z> {
        &self.eclasses[id]
    }

    // fn create_snap_representative(&mut self, egraph: &EGraph, id: egg::Id) -> Sort<'z> {
    //     if let Some(rep) = &self.eclasses[&id] {
    //         return rep.get_sort();
    //     }

    //     let eclass = &egraph.egraph[id];
    //     // println!("creating snap representative for {eclass:?}");
    //     assert!(matches!(eclass.data, Ty::Snapshot(Some(_))));
    //     let causal_exp = eclass.nodes.iter().find(|e| matches!(e, Exp::Snapshot(_) | Exp::Ternary(_))).unwrap();
    //     let sort = match causal_exp {
    //         Exp::Snapshot(args) => { 
    //             let sorts: Vec<_> = args.iter().map(|arg| self.create_snap_representative(egraph, *arg)).collect();
    //             match self.snapshots.iter().position(|(other, _)| &sorts == other) {
    //                 Some(idx) => {
    //                     self.eclass_to_snap.insert(id, idx);
    //                     self.snapshots[idx].1.sort.clone()
    //                 }
    //                 None => {
    //                     let names = sorts.iter().enumerate().map(|(i, _)| format!("_{i}")).collect::<Vec<_>>();
    //                     let fields = names.iter().map(String::as_str).zip(sorts.iter()).map(|(name, sort)| {
    //                         (name, DatatypeAccessor::Sort(sort.clone()))
    //                     }).collect();
    //                     let ds = DatatypeBuilder::new(&self.ctx, format!("snap_sort_{}", self.snapshots.len())).variant(
    //                         "new",
    //                         fields
    //                     ).finish();
    //                     let sort = ds.sort.clone();
    //                     self.eclass_to_snap.insert(id, self.snapshots.len());
    //                     self.snapshots.push((sorts, ds));
    //                     sort
    //                 }
    //             }
    //         }
    //         Exp::Ternary([_, t, e]) => {
    //             if *t != id {
    //                 self.create_snap_representative(egraph, *t);
    //             }
    //             if *e != id {
    //                 self.create_snap_representative(egraph, *e);
    //             }
    //             let v = *self.eclass_to_snap.get(t).or_else(|| self.eclass_to_snap.get(e)).unwrap();
    //             self.eclass_to_snap.insert(id, v);
    //             self.snapshots[v].1.sort.clone()
    //         }
    //         _ => panic!(),
    //     };
    //     let rep = FuncDecl::new(&self.ctx, id.to_string(), &[], &sort).apply(&[]);
    //     self.eclasses.insert(id, Some(rep));
    //     sort
    // }

    fn apply_function(&mut self, ident: &silver_oxide::parse::Ident, domain: &[egg::Id], range: egg::Id) -> ast::Dynamic<'z> {
        let args = domain.iter().map(|d| &self.eclasses[d] as &dyn ast::Ast).collect::<Vec<_>>();
        let f = self.functions.entry(ident.clone()).or_insert_with_key(|k| {
            let domain = domain.iter().map(|d| self.eclasses[d].get_sort()).collect::<Vec<_>>();
            let domain = domain.iter().collect::<Vec<_>>();
            let range = self.eclasses[&range].get_sort();
            FuncDecl::new(&self.data.ctx, k.0.clone(), &domain, &range)
        });
        // println!("\napplying {f:?} to {args:?}/{:?}\n{:#?}", args.iter().map(|a| a.get_sort()).collect::<Vec<_>>(), self.snapshots);
        f.apply(&args)
    }

    // fn mk_snapshot(&mut self, tys: &Vec<TyKind>) -> &DatatypeSort<'z> {
    //     self.snapshots.entry(tys.clone()).or_insert_with_key(|tys| {
    //         let name = tys.iter().map(|ty| ty.to_string()).collect::<String>();
    //         let names = tys.iter().enumerate().map(|(i, _)| format!("_{i}")).collect::<Vec<_>>();
    //         DatatypeBuilder::new(&self.ctx, name).variant(
    //             "new",
    //             tys.iter().zip(names.iter().map(String::as_str)).map(|(ty, name)| {
    //                 let sort = match ty {
    //                     TyG::Bool(_) => Sort::bool(&self.ctx),
    //                     TyG::Integer(_) => Sort::int(&self.ctx),
    //                     TyG::Real(_) => Sort::real(&self.ctx),
    //                     TyG::Ref(_) => self.ref_sort.clone(),
    //                     TyG::Snapshot(_) => self.snap_sort.clone(),
    //                     TyG::PredicateId => self.pred_sort.clone(),
    //                     TyG::TypeError => todo!(),
    //                 };
    //                 (name, DatatypeAccessor::Sort(sort))
    //             }).collect()
    //         ).finish()
    //     })
    // }
}

// impl<'z> EGraphToZ3Data<'z> {
//     fn ty_kind_to_sort_simple(&self, ty: TyKind) -> Result<Sort<'z>, ResId> {
//         match ty {
//             TyKind::Bool(..) => Ok(Sort::bool(&self.ctx)),
//             TyKind::Integer(..) => Ok(Sort::int(&self.ctx)),
//             TyKind::Real(..) => Ok(Sort::real(&self.ctx)),
//             TyKind::Ref(..) => Ok(self.ref_sort.clone()),
//             TyKind::Snapshot(eid) => Err(eid),
//             TyKind::PredicateId => Ok(self.pred_sort.clone()),
//             TyKind::TypeError => todo!(),
//         }
//     }
// }
