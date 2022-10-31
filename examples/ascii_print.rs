use std::{
    env::var,
    io::{BufRead, BufReader},
};

use knotty::{self, AbbreviatedDiagram};

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
    let mut knot = read_input(std::env::args().nth(1))?.parse::<AbbreviatedDiagram>()?;
    let moves = std::env::args()
        .nth(2)
        .map(|source| {
            read_input(Some(source))
                .and_then(|move_input| move_input.parse::<knotty::DiagramMoves>())
        })
        .transpose()?;

    if let Some(moves) = moves {
        knot.try_apply_all(moves)?;
    };

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
