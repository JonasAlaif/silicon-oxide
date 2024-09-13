use std::path::PathBuf;

use num_bigint::BigInt;

use crate::{declarations::Declarations, exp::{BinOp, Exp, PathCondition}, meaning::Constant, pure::EGraph, silicon::Silicon, state::ValueState};

impl<'e> Silicon<'e> {
    pub fn log_pure(&mut self, file_name: &str, label: Option<String>) {
        let mut path = self.log_dir.join(file_name);
        let heap = self.dot_subgraph();

        let mut egraph = self.value_state.egraph.egraph.clone();
        for class in egraph.classes_mut() {
            class.nodes.retain(|node| {
                node.log(&self.value_state.egraph)
            });
        }
        let mut dot = egraph.dot().with_config_line("ranksep=2.5").to_string();
        dot = dot.replacen("digraph egraph {\n", &format!("digraph egraph {{\n  {heap}"), 1);
        if let Some(label) = label {
            dot = dot.replacen("\n}", &format!("\n  label=\"{}\"\n}}", label.replace('"', "\\\"")), 1);
        }
        for class in egraph.classes() {
            let Some(data) = &class.data else {
                continue;
            };
            let subgraph = format!("subgraph cluster_{} {{\n", class.id);
            dot = dot.replacen(&subgraph, &format!("{subgraph}    label=\"{data}\";\n"), 1);
        }

        path.set_extension("dot");
        std::fs::write(&path, &dot).expect("Unable to write log file");

        use std::process::{Command, Stdio};
        use std::io::Write;
        path.set_extension("pdf");
        let mut child = Command::new("dot")
            .args(&["-Tpdf", "-o", path.as_os_str().to_str().unwrap()])
            .stdin(Stdio::piped())
            .stdout(Stdio::null())
            .spawn()
            .unwrap();
        let stdin = child.stdin.as_mut().expect("Failed to open stdin");
        write!(stdin, "{dot}").unwrap();
        assert_eq!(child.wait().unwrap().code(), Some(0));
    }

    fn dot_subgraph(&mut self) -> String {
        let mut nodes = Vec::<String>::new();
        let mut edges = Vec::<String>::new();

        for (var, value) in self.stmt_state.bindings.iter() {
            let var = var.unwrap();
            let value = self.value_state.egraph.normalise(*value);
            nodes.push(format!("{}[label = \"{} := #{value}\"]", var.0, var.0));
            edges.push(format!("{}:se -> {value}.0 [lhead = cluster_{value}, ]", var.0));
        }

        let condition = PathCondition::new(&self.value_state.egraph);
        let chunks: Vec<_> = self.value_state.heap.chunks()
            .map(|(id, chunk)|
                (id, chunk.get_chunk(&mut self.value_state.egraph, condition, None))
            )
            .collect();
        for (id, chunk) in chunks {
            let chunk = chunk.get_chunk_unsafe();
            if self.value_state.egraph.is_number(chunk.permission, BigInt::from(0u8)) {
                continue;
            }

            let id = self.value_state.egraph.normalise(id);
            let value = self.value_state.egraph.normalise(chunk.symbolic_value);
            let permission = Some(self.value_state.egraph.normalise(chunk.permission))
                .filter(|id| !self.value_state.egraph.is_number(*id, BigInt::from(1u8)));
            let p = permission.map(|id| format!("#{id}")).unwrap_or_default();
            nodes.push(format!("chunk_{id}[label = \"#{id} ↦{p} #{value}\"]"));
            edges.push(format!("chunk_{id}:sw -> {id}.0 [lhead = cluster_{id}, ]"));
            edges.push(format!("chunk_{id}:se -> {value}.0 [lhead = cluster_{value}, ]"));
            if let Some(permission) = permission {
                edges.push(format!("chunk_{id}:s -> {permission}.0 [lhead = cluster_{permission}, ]"));
            }
        }
        format!("subgraph cluster_heap {{\n    style=solid\n    {}\n  }}\n  {}\n", nodes.join("\n    "), edges.join("\n  "))
    }
}

impl Exp {
    pub fn log(&self, egraph: &EGraph) -> bool {
        match self {
            Exp::BinOp(BinOp::And | BinOp::Or | BinOp::Lt | BinOp::Eq, [a, b]) if egraph.ids_equal(*a, *b) => false,
            Exp::BinOp(BinOp::Lt | BinOp::Eq, [a, b]) if egraph.egraph[*a].data.is_some() && egraph.egraph[*b].data.is_some() => false,
            Exp::Ternary([c, _, _]) if egraph.egraph[*c].data.is_some() => false,
            Exp::BinOp(BinOp::Or, [a, b]) if egraph.is_true(*a) || egraph.is_true(*b) => false,
            Exp::BinOp(BinOp::And, [a, b]) if egraph.is_false(*a) || egraph.is_false(*b) => false,

            Exp::BinOp(BinOp::Mult | BinOp::Plus | BinOp::And | BinOp::Or | BinOp::Eq, [a, b]) if egraph.normalise(*a) < egraph.normalise(*b) => false,
            Exp::BinOp(BinOp::Mult | BinOp::Div, [_, b]) if egraph.is_number(*b, BigInt::from(1u8)) => false,
            Exp::BinOp(BinOp::Mult, [a, _]) if egraph.is_number(*a, BigInt::from(1u8)) => false,
            Exp::BinOp(BinOp::Plus, [a, b]) if egraph.is_number(*a, BigInt::from(0u8)) || egraph.is_number(*b, BigInt::from(0u8)) => false,
            _ => true,
        }
    }
}

impl EGraph {
    fn ids_equal(&self, a: egg::Id, b: egg::Id) -> bool {
        self.normalise(a) == self.normalise(b)
    }

    fn is_number(&self, id: egg::Id, number: BigInt) -> bool {
        self.egraph[id].data.as_ref().is_some_and(|data|
            matches!(data, Constant::Rational(r) if *r.numer() == number && r.is_integer())
        )
    }
}
