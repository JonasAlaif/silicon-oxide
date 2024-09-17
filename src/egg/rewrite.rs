use std::sync::OnceLock;

use egg::{Applier, EGraph, Id, Searcher, Subst};

use crate::{exp::Exp, meaning::{Constant, Meaning}, pure::Constants};

static RULES: OnceLock<Vec<egg::Rewrite<Exp, Meaning>>> = OnceLock::new();
pub fn rules(constants: Constants) -> &'static [egg::Rewrite<Exp, Meaning>] {
    RULES.get_or_init(|| {
        // use egg::*;

        // let mut runner = egg::Runner::<SymbolLang, ()>::default();
        // let t = runner.egraph.add_expr(&"true".parse().unwrap());
        // let f = runner.egraph.add_expr(&"false".parse().unwrap());
        // let and = runner.egraph.add_expr(&"(eq x y)".parse().unwrap());
        // runner.egraph.union(t, and);

        // // runner.egraph.add_expr(&"(f (g c d) b)".parse().unwrap());
        // runner.egraph.rebuild();

        // // let mp: MultiPattern<SymbolLang> = "?v1 = (f ?x ?y), ?v2 = (g ?x ?y)".parse().unwrap();
        // // assert_eq!(mp.n_matches(&runner.egraph), 2);

        // let rewrite = egg::multi_rewrite!("and-true"; "?v1 = true, ?v1 = (eq ?a ?b)" => "?a = ?b");

        // let runner = runner.run(&[rewrite]);
        // runner.egraph.dot().to_pdf("test.pdf").unwrap();
        // panic!("{:#?}", runner.egraph);

        let bool_rewriter = BoolRewriter { constants };
        [
            egg::rewrite!("commute-and"; "(&& ?a ?b)" => "(&& ?b ?a)"),
            egg::rewrite!("commute-or"; "(|| ?a ?b)" => "(|| ?b ?a)"),
            egg::rewrite!("commute-eq"; "(== ?a ?b)" => "(== ?b ?a)"),
            egg::rewrite!("commute-plus"; "(+ ?a ?b)" => "(+ ?b ?a)"),
            egg::rewrite!("commute-mult"; "(* ?a ?b)" => "(* ?b ?a)"),

            egg::multi_rewrite!("and-is-true"; "?v1 = true, ?v1 = (&& ?a ?b)" => "?a = true, ?b = true"),
            egg::multi_rewrite!("or-is-false"; "?v1 = false, ?v1 = (|| ?a ?b)" => "?a = false, ?b = false"),
            egg::multi_rewrite!("eq-true"; "?v1 = true, ?v1 = (eq ?a ?b)" => "?a = ?b"),

            // egg::multi_rewrite!("snap-pair-eq"; "?v1 = ($ ?a ?b), ?v1 = ($ ?c ?d)" => "?a = ?c, ?b = ?d"),

            // egg::multi_rewrite!("rewrite-ternary"; "?v1 = (? ?a ?b ?c)" => "?v1 = (&& ?y ?x), ?v2 = (|| ?y ?x)" if definitely_unequal("?v1", "?c")),
            // egg::rewrite!("ternary"; "(?a (? ?b ?c ?d) (? ?b ?e ?f))" => "(? ?b (?a ?c ?e) (?a ?d ?f))"),

            egg::rewrite!("and-true"; "(&& ?a true)" => "?a"),
            egg::rewrite!("and-false"; "(&& ?a false)" => "false"),
            egg::rewrite!("and-same"; "(&& ?a ?a)" => "?a"),
            egg::rewrite!("or-true"; "(|| ?a true)" => "true"),
            egg::rewrite!("or-false"; "(|| ?a false)" => "?a"),
            egg::rewrite!("or-same"; "(|| ?a ?a)" => "?a"),
            egg::rewrite!("eq-reflexive"; "(== ?a ?a)" => "true"),
            egg::rewrite!("lt-false"; "(< ?a ?a)" => "false"),

            egg::rewrite!("demorgan-or"; "(! (|| ?a ?a))" => "(&& (! ?a) (! ?a))"),
            egg::rewrite!("demorgan-and"; "(! (&& ?a ?a))" => "(|| (! ?a) (! ?a))"),
            egg::rewrite!("or-distribute"; "(|| ?a (&& ?b ?c))" => "(&& (|| ?a ?b) (|| ?a ?c))"),

            // TODO: these two may not be necessary
            egg::rewrite!("not-true"; "(! true)" => "false"),
            egg::rewrite!("not-false"; "(! false)" => "true"),

            egg::rewrite!("not-not"; "(! (! ?a))" => "?a"),
            egg::rewrite!("not-lt"; "(! (< ?a ?b))" => "(|| (< ?b ?a) (== ?a ?b))"),
            egg::rewrite!("excluded-middle"; "(|| ?a (! ?a))" => "true"),

            egg::rewrite!("minus-self"; "(+ ?a (- ?a))" => "0"),
            egg::rewrite!("minus-minus"; "(- (- ?a))" => "?a"),
            egg::rewrite!("minus-plus"; "(- (+ ?a ?b))" => "(+ (- ?a) (- ?b))"),
            // TODO: minus-mult, minus-div?

            egg::rewrite!("plus-zero"; "(+ ?a 0)" => "?a"),
            egg::rewrite!("minus-zero"; "(- 0)" => "0"),

            // TERNARY

            egg::rewrite!("ternary-true"; "(? true ?a ?b)" => "?a"),
            egg::rewrite!("ternary-false"; "(? false ?a ?b)" => "?b"),
            egg::rewrite!("ternary-same"; "(? ?a ?b ?b)" => "?b"),
            egg::rewrite!("ternary-swap"; "(? ?a ?b ?c)" => "(? (! ?a) ?c ?b)"),
            egg::rewrite!("ternary-nested"; "(? ?a (? ?a ?b ?c) ?d)" => "(? ?a ?b ?d)"),

            egg::rewrite!("ternary-not"; "(! (? ?a ?b ?c))" => "(? ?a (! ?b) (! ?c))"),
            egg::rewrite!("ternary-neg"; "(- (? ?a ?b ?c))" => "(? ?a (- ?b) (- ?c))"),

            // TODO: figure out
            // egg::rewrite!("ternary-and"; "(&& (? ?a ?b ?c) ?d)" => "(? ?a (&& ?b ?d) (&& ?c ?d))"),
            // egg::rewrite!("ternary-or"; "(|| (? ?a ?b ?c) ?d)" => "(? ?a (|| ?b ?d) (|| ?c ?d))"),
            // egg::rewrite!("ternary-eq"; "(== (? ?a ?b ?c) ?d)" => "(? ?a (== ?b ?d) (== ?c ?d))"),
            // egg::rewrite!("ternary-lt-a"; "(< (? ?a ?b ?c) ?d)" => "(? ?a (< ?b ?d) (< ?c ?d))"),
            // egg::rewrite!("ternary-lt-b"; "(< ?a (? ?b ?c ?d))" => "(? ?b (< ?a ?c) (< ?a ?d))"),
            // egg::rewrite!("ternary-plus"; "(+ (? ?a ?b ?c) ?d)" => "(? ?a (+ ?b ?d) (+ ?c ?d))"),
            // egg::rewrite!("ternary-mult"; "(* (? ?a ?b ?c) ?d)" => "(? ?a (* ?b ?d) (* ?c ?d))"),
            // egg::rewrite!("ternary-div-a"; "(/ (? ?a ?b ?c) ?d)" => "(? ?a (/ ?b ?d) (/ ?c ?d))"),
            // egg::rewrite!("ternary-div-b"; "(/ ?a (? ?b ?c ?d))" => "(? ?b (/ ?a ?c) (/ ?a ?d))"),
            // egg::rewrite!("ternary-mod-a"; "(% (? ?a ?b ?c) ?d)" => "(? ?a (% ?b ?d) (% ?c ?d))"),
            // egg::rewrite!("ternary-mod-b"; "(% ?a (? ?b ?c ?d))" => "(? ?b (% ?a ?c) (% ?a ?d))"),

            // MULT/DIV

            egg::rewrite!("natural-to-rational"; "?a" => "(/ ?a 1)" if is_number("?a")),
            egg::rewrite!("divide-unit"; "(/ ?a 1)" => "?a"),
            egg::rewrite!("multiply-unit"; "(* ?a 1)" => "?a"),
            egg::rewrite!("multiply-zero"; "(* ?a 0)" => "0"),
            egg::rewrite!("zero-divide"; "(/ 0 ?a)" => "0"),
            egg::rewrite!("multiply-neg"; "(* ?a (- ?b))" => "(* (- ?a) ?b)"),
            egg::rewrite!("div-neg-a"; "(/ ?a (- ?b))" => "(/ (- ?a) ?b)"),
            egg::rewrite!("div-neg-b"; "(/ (- ?a) ?b)" => "(/ ?a (- ?b))"),

            // TODO: these are bad (matching loop?)
            // egg::rewrite!("div-div-0"; "(/ ?a (/ ?b ?c))" => "(/ (* ?a ?c) ?b)"),
            // egg::rewrite!("div-div-1"; "(/ (/ ?a ?b) ?c)" => "(/ ?a (* ?b ?c))"),

            // TODO: necessary?
            // egg::rewrite!("div-mul-0"; "(/ (* ?a ?c) ?b)" => "(/ ?a (/ ?b ?c))"),
            // egg::rewrite!("div-mul-1"; "(/ ?a (* ?b ?c))" => "(/ (/ ?a ?c) ?b)"),

            egg::rewrite!("write"; "write" => "1"),
            egg::rewrite!("none"; "none" => "0"),

            // egg::rewrite!("test"; "(- (/ ?a ?b) (/ ?c ?d))" => "(/ (- (* ?a ?d) (* ?c ?b)) (* ?b ?d))"),// { MySillyApplier("foo") }),

            egg::Rewrite::new("bool-search", bool_rewriter, EquateApplier).unwrap(),
            egg::Rewrite::new("snap-inj", SnapshotInjective, EquateApplier).unwrap(),
        ].to_vec()
    })
}

fn is_number(var: &'static str) -> impl Fn(&mut EGraph<Exp, Meaning>, Id, &Subst) -> bool {
    let var = var.parse().unwrap();
    move |egraph, _, subst| egraph[subst[var]].nodes.iter().any(|node| matches!(node, Exp::Const(silver_oxide::ast::Const::Int(_))))
}

// fn definitely_equal(v1: &'static str, v2: &'static str) -> impl Fn(&mut EGraph<Exp, Meaning>, Id, &Subst) -> bool {
//     compare(v1, v2, |r| r == Some(true))
// }
// fn definitely_unequal(v1: &'static str, v2: &'static str) -> impl Fn(&mut EGraph<Exp, Meaning>, Id, &Subst) -> bool {
//     compare(v1, v2, |r| r == Some(false))
// }

// fn compare<T>(v1: &'static str, v2: &'static str, f: impl Fn(Option<bool>) -> T) -> impl Fn(&mut EGraph<Exp, Meaning>, Id, &Subst) -> T {
//     let v1 = v1.parse().unwrap();
//     let v2 = v2.parse().unwrap();
//     move |egraph, _, subst|
//         f(egraph[subst[v1]].data.as_ref().and_then(|d| d.compare(egraph[subst[v2]].data.as_ref())))
// }

#[derive(Debug, Clone, Copy)]
pub struct BoolRewriter {
    constants: Constants
}

impl BoolRewriter {
    pub fn search(
        egraph: &EGraph<Exp, Meaning>,
        eclass: Id,
        mut limit: usize,
        f: impl Fn(&Exp, &Constant) -> Option<Subst>,
    ) -> Option<egg::SearchMatches<'static, Exp>> {
        let ec = &egraph[eclass];
        let meaning = ec.data.as_ref()?;

        let mut substs = Vec::new();
        for enode in ec.iter() {
            if limit == 0 {
                break;
            }
            if let Some(subst) = f(enode, meaning) {
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

impl Searcher<Exp, Meaning> for BoolRewriter {
    fn search_eclass_with_limit(
        &self,
        egraph: &EGraph<Exp, Meaning>,
        eclass: Id,
        limit: usize,
    ) -> Option<egg::SearchMatches<Exp>> {
        BoolRewriter::search(egraph, eclass, limit, |enode, meaning| {
            match enode {
                Exp::Ternary([c, t, e]) => {
                    if meaning.compare(egraph[*t].data.as_ref()).is_some_and(|eq| !eq) {
                        let mut subst = egg::Subst::with_capacity(2);
                        subst.insert("?0".parse().unwrap(), self.constants.false_);
                        subst.insert("?1".parse().unwrap(), *c);
                        Some(subst)
                    } else if meaning.compare(egraph[*e].data.as_ref()).is_some_and(|eq| !eq) {
                        let mut subst = egg::Subst::with_capacity(2);
                        subst.insert("?0".parse().unwrap(), self.constants.true_);
                        subst.insert("?1".parse().unwrap(), *c);
                        Some(subst)
                    } else {
                        None
                    }
                }
                _ => None,
            }
        })
    }

    fn vars(&self) -> Vec<egg::Var> {
        vec![
            "?0".parse().unwrap(),
            "?1".parse().unwrap(),
        ]
    }
}

struct SnapshotInjective;

impl Searcher<Exp, Meaning> for SnapshotInjective {
    fn search_eclass_with_limit(
        &self,
        egraph: &EGraph<Exp, Meaning>,
        eclass: Id,
        mut limit: usize,
    ) -> Option<egg::SearchMatches<Exp>> {
        let mut substs = Vec::new();
        let mut last_snapshot = None;
        for enode in &egraph[eclass].nodes {
            let Exp::Snapshot(snapshot) = enode else {
                continue;
            };
            let last = last_snapshot.replace(snapshot);
            let Some(last) = last else {
                continue;
            };
            if snapshot.len() != last.len() {
                eprintln!("SnapshotInjective: different lengths");
                continue;
            }
            if limit == 0 {
                break;
            }
            for (last, snap) in last.iter().zip(snapshot.iter()) {
                let mut subst = egg::Subst::with_capacity(2);
                subst.insert("?0".parse().unwrap(), *last);
                subst.insert("?1".parse().unwrap(), *snap);
                substs.push(subst);
            }
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

struct EquateApplier;

impl Applier<Exp, Meaning> for EquateApplier {
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
