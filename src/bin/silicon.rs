use std::{fs::read_to_string, io};

use silicon_oxide::translator::VerificationState;

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
        std::env::set_var("VIPER_LOG", path.to_str().unwrap());

        let silver = silver_oxide::full(&contents).unwrap();

        let _vs = VerificationState::verify(&silver);
    }

    Ok(())
}
