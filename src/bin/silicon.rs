use std::{fs::read_to_string, io};

use fxhash::{FxHashMap, FxHashSet};
use silicon_oxide::silicon::Silicon;
use silver_oxide::{ast, peg};

fn main() -> io::Result<()> {
    for file in std::env::args().skip(1) {
        let Ok(contents) = read_to_string(file.clone()) else {
            continue;
        };
        // let _ = Silver::parse(Rule::sil_program, &contents);

        let peg_parse = peg::silver_parser::sil_program(&contents).unwrap();
        let declarations: FxHashMap<_, _> = peg_parse
            .iter()
            .flat_map(name)
            .collect();
        // println!("{peg_parse:#?}");

        for (_, decl) in &declarations {
            if let silver_oxide::ast::Declaration::Method(method) = decl {
                Silicon::verify(method, &declarations).unwrap();
            }
        }
    }

    Ok(())
}

pub fn name(decl: &ast::Declaration) -> impl Iterator<Item = (&'_ str, &'_ ast::Declaration)> + '_ {
    let decls = match decl {
        ast::Declaration::Import(_) => todo!(),
        ast::Declaration::Define(_) => todo!(),
        ast::Declaration::Domain(d) => [(d.name.0.as_str(), decl)].into_iter().chain(d.elements.iter().map(|e| {
            match e {
                ast::DomainElement::DomainFunction(f) =>
                    (f.signature.name.0.as_str(), decl),
                ast::DomainElement::Axiom(_) => todo!(),
            }
        })).collect(),
        ast::Declaration::Field(f) => f.fields.iter().map(|f| (f.0.0.as_str(), decl)).collect(),
        ast::Declaration::Function(f) => vec![(f.signature.name.0.as_str(), decl)],
        ast::Declaration::Predicate(p) => vec![(p.signature.name.0.as_str(), decl)],
        ast::Declaration::Method(m) => vec![(m.signature.name.0.as_str(), decl)],
        ast::Declaration::Adt(_) => todo!(),
    };
    decls.into_iter()
}
