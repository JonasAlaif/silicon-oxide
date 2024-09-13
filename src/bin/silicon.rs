use std::{fs::read_to_string, io};

use silicon_oxide::{declarations::{CallableDecl, Declarations}, silicon::Silicon};
use silver_oxide::peg;

fn main() -> io::Result<()> {
    for file in std::env::args().skip(1) {
        let Ok(mut path) = std::path::Path::new(&file).canonicalize() else {
            continue;
        };
        let Ok(contents) = read_to_string(&path) else {
            continue;
        };
        path.set_extension("");
        std::fs::create_dir_all(&path).unwrap();

        let mut peg_parse = peg::silver_parser::sil_program(&contents).unwrap();
        silver_oxide::mac::Macro::inline_macros(&mut peg_parse);

        let declarations = Declarations::new(&peg_parse);
        // println!("{peg_parse:#?}");

        for (_, decl) in &declarations.callable {
            if let CallableDecl::Method(method) = decl {
                Silicon::verify(method, &declarations, &path).unwrap();
            }
        }
    }

    Ok(())
}
