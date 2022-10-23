use std::io::{BufRead, BufReader};

// TODO simplify input reading now that we've moved parsing to the lib
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
    let display = read_input(std::env::args().nth(1))?
        .parse::<knotty::AbbreviatedDiagram>()?
        .ascii_print_compact();

    print!("{display}");

    Ok(())
}
