use std::{fs::read_to_string, io};

use silicon_oxide::{declarations::{CallableDecl, Declarations}, method::Method};
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
        // println!("{peg_parse:#?}");

        let decls0 = Declarations::new(&peg_parse);
        let decls1 = decls0.verify_functions(&path);
        let decls2 = decls1.verify_methods(&path);
    }

    Ok(())
}
