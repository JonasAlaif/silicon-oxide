use std::sync::OnceLock;

use egg::{Applier, EGraph, Id, Searcher, Subst};

use crate::{exp::Exp, meaning::{Constant, Meaning}};

static RULES: OnceLock<[egg::Rewrite<Exp, Meaning>; 35]> = OnceLock::new();
pub fn rules() -> &'static [egg::Rewrite<Exp, Meaning>] {
    RULES.get_or_init(|| {
        [
            egg::rewrite!("commute-add"; "(+ ?a ?b)" => "(+ ?b ?a)"),
            egg::rewrite!("commute-gt"; "(> ?a ?b)" => "(< ?b ?a)"),
            egg::rewrite!("commute-eq"; "(== ?a ?b)" => "(== ?b ?a)"),
            egg::rewrite!("commute-neq"; "(!= ?a ?b)" => "(!= ?b ?a)"),
            egg::rewrite!("commute-and"; "(&& ?a ?b)" => "(&& ?b ?a)"),
            egg::rewrite!("commute-or"; "(|| ?a ?b)" => "(|| ?b ?a)"),

            egg::rewrite!("expand-le"; "(<= ?a ?b)" => "(|| (== ?a ?b) (< ?a ?b))"),
            egg::rewrite!("expand-ge"; "(>= ?a ?b)" => "(|| (== ?a ?b) (> ?a ?b))"),

            egg::rewrite!("and-true"; "(&& ?a true)" => "?a"),
            egg::rewrite!("and-false"; "(&& ?a false)" => "false"),
            egg::rewrite!("or-true"; "(|| ?a true)" => "true"),
            egg::rewrite!("or-false"; "(|| ?a false)" => "?a"),
            egg::rewrite!("eq-reflexive"; "(== ?a ?a)" => "true"),
            egg::rewrite!("neq-rewrite"; "(!= ?a ?b)" => "(! (== ?a ?b))"),

            egg::rewrite!("not-true"; "(! true)" => "false"),
            egg::rewrite!("not-false"; "(! false)" => "true"),
            egg::rewrite!("not-not"; "(! (! ?a))" => "?a"),
            egg::rewrite!("excluded-middle"; "(|| ?a (! ?a))" => "true"),

            egg::rewrite!("sub-to-plus"; "(- ?a ?b)" => "(+ ?a (- ?b))"),
            egg::rewrite!("minus-self"; "(+ ?a (- ?a))" => "0"),

            egg::rewrite!("plus-zero"; "(+ ?a 0)" => "?a"),
            egg::rewrite!("minus-zero"; "(- 0)" => "0"),

            egg::rewrite!("lt-false"; "(< ?a ?a)" => "false"),

            egg::rewrite!("ternary-true"; "(? true ?a ?b)" => "?a"),
            egg::rewrite!("ternary-false"; "(? false ?a ?b)" => "?b"),
            egg::rewrite!("ternary-same"; "(? ?a ?b ?b)" => "?b"),

            egg::rewrite!("natural-to-rational"; "?a" => "(/ ?a 1)" if is_number("?a")),
            egg::rewrite!("divide-unit"; "(/ ?a 1)" => "?a"),
            egg::rewrite!("multiply-unit"; "(* ?a 1)" => "?a"),
            egg::rewrite!("multiply-zero"; "(* ?a 0)" => "0"),

            // TODO: these are bad (matching loop?)
            // egg::rewrite!("div-div-0"; "(/ ?a (/ ?b ?c))" => "(/ (* ?a ?c) ?b)"),
            // egg::rewrite!("div-div-1"; "(/ (/ ?a ?b) ?c)" => "(/ ?a (* ?b ?c))"),

            // TODO: necessary?
            // egg::rewrite!("div-mul-0"; "(/ (* ?a ?c) ?b)" => "(/ ?a (/ ?b ?c))"),
            // egg::rewrite!("div-mul-1"; "(/ ?a (* ?b ?c))" => "(/ (/ ?a ?c) ?b)"),

            egg::rewrite!("write"; "write" => "1"),
            egg::rewrite!("none"; "none" => "0"),
            egg::rewrite!("number"; "?a" => "(/ ?a 1)" if is_number("?a")),

            // egg::rewrite!("test"; "(- (/ ?a ?b) (/ ?c ?d))" => "(/ (- (* ?a ?d) (* ?c ?b)) (* ?b ?d))"),// { MySillyApplier("foo") }),

            egg::Rewrite::new("bool-search", BoolSearcher, BoolSearcher).unwrap(),
            egg::Rewrite::new("calculator", Calculator, Calculator).unwrap(),
        ]
    })
}

// #[derive(Debug)]
// struct MySillyApplier(&'static str);
// impl Applier<Exp, Meaning> for MySillyApplier {
//     fn apply_one(&self, egraph: &mut egg::EGraph<Exp, Meaning>, id: Id, subst: &Subst, past: Option<&egg::PatternAst<Exp>>, symbol: egg::Symbol) -> Vec<Id> {
//         panic!("id {id:?}, subst {subst:?}, past {past:?}, symbol {symbol:?}");
//     }
// }

// fn is_true(var: &'static str) -> impl Fn(&mut EGraph<Exp, Meaning>, Id, &Subst) -> bool {
//     let var = var.parse().unwrap();
//     let true_ = Exp::Const(silver_oxide::ast::Const::True);
//     move |egraph, _, subst| !egraph[subst[var]].nodes.contains(&true_)
// }

fn is_number(var: &'static str) -> impl Fn(&mut EGraph<Exp, Meaning>, Id, &Subst) -> bool {
    let var = var.parse().unwrap();
    move |egraph, _, subst| egraph[subst[var]].nodes.iter().any(|node| matches!(node, Exp::Const(silver_oxide::ast::Const::Int(_))))
}

pub struct BoolSearcher;
impl BoolSearcher {
    pub fn search(
        egraph: &EGraph<Exp, Meaning>,
        eclass: Id,
        mut limit: usize,
        f: impl Fn(&Exp, bool, Id, Id) -> Option<Subst>,
    ) -> Option<egg::SearchMatches<'static, Exp>> {
        let ec = &egraph[eclass];
        let is_true = match ec.data.as_ref()? {
            Constant::Bool(b) => *b,
            _ => return None,
        };

        let true_ = egraph
            .lookup(Exp::Const(silver_oxide::ast::Const::True))?;
        // let true_ = egraph.find(true_);
        let false_ = egraph
            .lookup(Exp::Const(silver_oxide::ast::Const::False))?;
        // let false_ = egraph.find(false_);

        let mut substs = Vec::new();
        for enode in ec.iter() {
            if limit == 0 {
                break;
            }
            if let Some(subst) = f(enode, is_true, true_, false_) {
                substs.push(subst);
                limit -= 1;
            }
        }
        (!substs.is_empty()).then(|| egg::SearchMatches {
            eclass,
            substs,
            ast: None,
        })
    }
}

impl Searcher<Exp, Meaning> for BoolSearcher {
    fn search_eclass_with_limit(
        &self,
        egraph: &EGraph<Exp, Meaning>,
        eclass: Id,
        limit: usize,
    ) -> Option<egg::SearchMatches<Exp>> {
        BoolSearcher::search(egraph, eclass, limit, |enode, is_true, true_, false_| {
            match enode {
                Exp::BinOp(silver_oxide::ast::BinOp::And, [a, b]) => {
                    is_true.then(|| {
                        let mut subst = Subst::with_capacity(3);
                        subst.insert("?0".parse().unwrap(), true_);
                        subst.insert("?1".parse().unwrap(), *a);
                        subst.insert("?2".parse().unwrap(), *b);
                        subst
                    })
                }
                Exp::BinOp(silver_oxide::ast::BinOp::Or, [a, b]) => {
                    (!is_true).then(|| {
                        let mut subst = Subst::with_capacity(3);
                        subst.insert("?0".parse().unwrap(), false_);
                        subst.insert("?1".parse().unwrap(), *a);
                        subst.insert("?2".parse().unwrap(), *b);
                        subst
                    })
                }
                Exp::Ternary([c, t, e]) => {
                    // TODO: is this necessary?
                    if let Some(Constant::Bool(t_val)) = &egraph[*t].data {
                        if *t_val != is_true {
                            let mut subst = egg::Subst::with_capacity(2);
                            subst.insert("?0".parse().unwrap(), false_);
                            subst.insert("?1".parse().unwrap(), *c);
                            return Some(subst);
                        }
                    }
                    if let Some(Constant::Bool(e_val)) = &egraph[*e].data {
                        if *e_val != is_true {
                            let mut subst = egg::Subst::with_capacity(2);
                            subst.insert("?0".parse().unwrap(), true_);
                            subst.insert("?1".parse().unwrap(), *c);
                            return Some(subst);
                        }
                    }
                    None
                }
                Exp::Not(e) => {
                    let negation = if is_true { false_ } else { true_ };
                    let mut subst = egg::Subst::with_capacity(2);
                    subst.insert("?0".parse().unwrap(), negation);
                    subst.insert("?1".parse().unwrap(), *e);
                    Some(subst)
                }
                Exp::BinOp(silver_oxide::ast::BinOp::Eq, [a, b]) => {
                    is_true.then(|| {
                        let mut subst = Subst::with_capacity(2);
                        subst.insert("?0".parse().unwrap(), *a);
                        subst.insert("?1".parse().unwrap(), *b);
                        subst
                    })
                }
                _ => None,
            }
        })
    }

    fn vars(&self) -> Vec<egg::Var> {
        vec![
            "?0".parse().unwrap(),
            "?1".parse().unwrap(),
            "?2".parse().unwrap(),
        ]
    }
}


impl Applier<Exp, Meaning> for BoolSearcher {
    fn apply_one(
        &self,
        egraph: &mut EGraph<Exp, Meaning>,
        _eclass: Id,
        subst: &egg::Subst,
        _searcher_ast: Option<&egg::PatternAst<Exp>>,
        rule_name: egg::Symbol,
    ) -> Vec<Id> {
        let mut changed = false;
        let bool = subst["?0".parse().unwrap()];
        let mut i = 1;
        let mut changed_vec = Vec::new();
        while let Some(&to) = subst.get(format!("?{i}").parse().unwrap()) {
            i += 1;
            let changed_here = egraph.union_trusted(bool, to, rule_name);
            if changed_here {
                changed = true;
                changed_vec.push(to);
            }
        }
        if changed {
            changed_vec.push(bool);
        }
        changed_vec
    }
}

pub struct Calculator;
impl Searcher<Exp, Meaning> for Calculator {
    fn search_eclass_with_limit(
        &self,
        egraph: &EGraph<Exp, Meaning>,
        eclass: Id,
        mut limit: usize,
    ) -> Option<egg::SearchMatches<Exp>> {
        let mut substs = Vec::new();
        for enode in egraph[eclass].iter() {
            if limit == 0 {
                break;
            }
            let Exp::BinOp(op, [a, b]) = enode else {
                continue;
            };
            let Some(Constant::Rational(a)) = &egraph[*a].data else {
                continue;
            };
            let Some(Constant::Rational(b)) = &egraph[*b].data else {
                continue;
            };
            let subst = match op {
                silver_oxide::ast::BinOp::Lt => {
                    let is_true = a < b;
                    let value = if is_true {
                        egraph.lookup(Exp::Const(silver_oxide::ast::Const::True))?
                    } else {
                        egraph.lookup(Exp::Const(silver_oxide::ast::Const::False))?
                    };
                    let mut subst = Subst::with_capacity(2);
                    subst.insert("?0".parse().unwrap(), value);
                    subst.insert("?1".parse().unwrap(), eclass);
                    subst
                }
                silver_oxide::ast::BinOp::Eq => {
                    let is_true = a == b;
                    let value = if is_true {
                        egraph.lookup(Exp::Const(silver_oxide::ast::Const::True))?
                    } else {
                        egraph.lookup(Exp::Const(silver_oxide::ast::Const::False))?
                    };
                    let mut subst = Subst::with_capacity(2);
                    subst.insert("?0".parse().unwrap(), value);
                    subst.insert("?1".parse().unwrap(), eclass);
                    subst
                }
                _ => continue,
            };
            substs.push(subst);
            limit -= 1;
        }
        (!substs.is_empty()).then(|| egg::SearchMatches {
            eclass,
            substs,
            ast: None,
        })
    }

    fn vars(&self) -> Vec<egg::Var> {
        vec![
            "?0".parse().unwrap(),
            "?1".parse().unwrap(),
        ]
    }
}


impl Applier<Exp, Meaning> for Calculator {
    fn apply_one(
        &self,
        egraph: &mut EGraph<Exp, Meaning>,
        _eclass: Id,
        subst: &egg::Subst,
        _searcher_ast: Option<&egg::PatternAst<Exp>>,
        rule_name: egg::Symbol,
    ) -> Vec<Id> {
        let a = subst["?0".parse().unwrap()];
        let b = subst["?1".parse().unwrap()];
        let changed = egraph.union_trusted(a, b, rule_name);
        if changed {
            vec![a, b]
        } else {
            vec![]
        }
    }
}
