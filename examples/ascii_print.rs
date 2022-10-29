use std::{
    env::var,
    io::{BufRead, BufReader},
};

use knotty::AbbreviatedDiagram;

fn read_input(file: Option<String>) -> Result<String, String> {
    let lines = match file.as_deref() {
        None | Some("-") => std::io::stdin()
            .lock()
            .lines()
            .map(|line| format!("{}\n", line.unwrap()))
            .collect::<String>(),
        Some(file) => BufReader::new(std::fs::File::open(file).unwrap())
            .lines()
            .map(|line| format!("{}\n", line.unwrap()))
            .collect::<String>(),
    };

    Ok(lines)
}

fn main() -> Result<(), String> {
    let knot = read_input(std::env::args().nth(1))?.parse::<AbbreviatedDiagram>()?;

    let grid = var("KNOTTY_GRID").ok().as_deref() == Some("true");
    let compact = var("KNOTTY_COMPACT").ok().as_deref() == Some("true");

    #[rustfmt::skip]
    let func = match (compact, grid) {
        (true,  true)  => AbbreviatedDiagram::ascii_print_compact::<true>,
        (true,  false) => AbbreviatedDiagram::ascii_print_compact::<false>,

        (false, true)  => AbbreviatedDiagram::ascii_print::<true>,
        (false, false) => AbbreviatedDiagram::ascii_print::<false>,
    };

    let display = func(&knot);
    print!("{display}");

    Ok(())
}
