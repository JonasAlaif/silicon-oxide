use std::path::PathBuf;

use num_bigint::BigInt;

use crate::{exp::{BinOp, Exp, PathCondition, UnOp}, meaning::Constant, pure::EGraph, silicon::Silicon};

impl EGraph {
    pub fn dot(&self, mut path: PathBuf, header: Option<String>, footer: Option<String>) {
        let mut egraph = self.egraph.clone();
        for class in egraph.classes_mut() {
            class.nodes.retain(|node| {
                node.log(class.id, self)
            });
        }
        let mut dot = egraph.dot().with_config_line("ranksep=2.5").to_string();

        if let Some(header) = header {
            dot = dot.replacen("digraph egraph {\n", &format!("digraph egraph {{\n  {header}"), 1);
        }
        if let Some(footer) = footer {
            dot = dot.replacen("\n}", &format!("\n  {footer}\n}}"), 1);
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
}

impl<'a, 'e, F, M> Silicon<'a, 'e, F, M> {
    pub fn log_pure(&mut self, file_name: &str, label: Option<String>) {
        let path = self.log_dir.join(file_name);
        let heap = self.dot_subgraph();

        let label = label.map(|label| format!("label=\"{}\"", label.replace('"', "\\\"")));
        self.value_state.egraph.dot(path, Some(heap), label);
    }

    fn dot_subgraph(&mut self) -> String {
        let mut nodes = Vec::<String>::new();
        let mut edges = Vec::<String>::new();

        for (var, value) in self.stmt_state.bindings.iter() {
            let var = var.as_ref().map(|v| v.0.as_str()).unwrap_or("result");
            let value = self.value_state.egraph.normalise(*value);
            nodes.push(format!("{var}[label = \"{var} := #{value}\"]"));
            edges.push(format!("{var}:se -> {value}.0 [lhead = cluster_{value}, ]"));
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
    pub fn log(&self, id: egg::Id, egraph: &EGraph) -> bool {
        let is_bool_constant = |id: &egg::Id| egraph.egraph[*id].data.as_ref().is_some_and(|data|
            matches!(data, Constant::Bool(..))
        );
        let is_rational_constant = |id: &egg::Id| egraph.egraph[*id].data.as_ref().is_some_and(|data|
            matches!(data, Constant::Rational(..))
        );
        match self {
            Exp::BinOp(BinOp::And | BinOp::Or | BinOp::Lt | BinOp::Eq, [a, b]) if egraph.ids_equal(*a, *b) => false,

            Exp::BinOp(BinOp::And | BinOp::Or | BinOp::Eq, [a, b])
                if is_bool_constant(a) && is_bool_constant(b) => false,
            Exp::UnOp(UnOp::Not, a) if is_bool_constant(a) => false,
            Exp::Ternary([c, _, _]) if is_bool_constant(c) => false,

            Exp::BinOp(BinOp::Eq | BinOp::Lt | BinOp::Plus | BinOp::Mult | BinOp::Div | BinOp::Mod, [a, b])
                if is_rational_constant(a) && is_rational_constant(b) => false,
            Exp::UnOp(UnOp::Neg, a) if is_rational_constant(a) => false,

            Exp::BinOp(BinOp::Or, [a, b]) if egraph.is_true(*a) || egraph.is_true(*b) => false,
            Exp::BinOp(BinOp::And, [a, b]) if egraph.is_false(*a) || egraph.is_false(*b) => false,

            Exp::BinOp(BinOp::And, [a, b]) if egraph.ids_equal(id, *a) && egraph.is_true(*b) => false,
            Exp::BinOp(BinOp::Or, [a, b]) if egraph.ids_equal(id, *a) && egraph.is_false(*b) => false,

            Exp::BinOp(BinOp::Mult | BinOp::Plus | BinOp::And | BinOp::Or | BinOp::Eq, [a, b]) if egraph.normalise(*a) < egraph.normalise(*b) => false,
            Exp::BinOp(BinOp::Mult | BinOp::Div, [_, b]) if egraph.is_number(*b, BigInt::from(1u8)) => false,
            Exp::BinOp(BinOp::Mult, [a, _]) if egraph.is_number(*a, BigInt::from(1u8)) => false,
            Exp::BinOp(BinOp::Plus, [a, b]) if egraph.is_number(*a, BigInt::from(0u8)) || egraph.is_number(*b, BigInt::from(0u8)) => false,
            Exp::BinOp(BinOp::Div, [a, _]) if egraph.is_number(*a, BigInt::from(0u8)) => false,
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
