// examples
//// Unknot:
////   _
////  / \
//// <   >
////  \_/
////
//let unknot = vec![(0, b'A'), (0, b'V')];
//println!("{}", knotty::ascii_print_compact(unknot));

//// Trefoil:
////     ,______________
////    /               \
////   <                 >
////    \_   ,_____   ,_/
////      \ /      \ /
////       \        \
////   ,__/ \_   ,_/ \____
////  /       \ /         \
//// <         /           >
////  \_______/ \_________/
////
//let trefoil = vec![
//    (0, b'A'),
//    (2, b'A'),
//    (1, b'/'),
//    (0, b'\\'),
//    (1, b'/'),
//    (2, b'V'),
//    (0, b'V'),
//];
//println!("{}", knotty::ascii_print_compact(trefoil));

//// donut:
//let donut = vec![(0, b'A'), (1, b'A'), (1, b'V'), (0, b'V')];
//println!("{}", knotty::ascii_print_compact(donut));

//// C:
//let donut = vec![(0, b'A'), (1, b'A'), (2, b'V'), (0, b'V')];
//println!("{}", knotty::ascii_print_compact(donut));

//// weird terrace thing:
//let terrace = vec![
//    (0, b'A'),
//    (2, b'A'),
//    (4, b'A'),
//    (6, b'A'),
//    (5, b'V'),
//    (3, b'V'),
//    // (2, b'/'),
//    (1, b'V'),
//    // (0, b'/'),
//    (1, b'A'),
//    // (2, b'/'),
//    (3, b'A'),
//    (5, b'A'),
//    (6, b'V'),
//    (4, b'V'),
//    (2, b'V'),
//    (0, b'V'),
//];
//println!("{}", knotty::ascii_print_compact(terrace));

//// basket:
//let basket = vec![
//    (0, b'A'),
//    (1, b'A'),
//    (1, b'A'),
//    (3, b'/'),
//    (2, b'/'),
//    (4, b'/'),
//    (3, b'/'),
//    (1, b'V'),
//    (1, b'V'),
//    (0, b'V'),
//];
//println!("{}", knotty::ascii_print_compact(basket));

//// ugly trefoil:
//let ugly_trefoil = vec![
//    (0, b'A'),
//    (0, b'A'),
//    (1, b'/'),
//    (0, b'\\'),
//    (1, b'/'),
//    (0, b'V'),
//    (0, b'V'),
//];
//println!("{}", knotty::ascii_print_compact(ugly_trefoil));

use std::io::{BufRead, BufReader};

fn read_input(file: Option<String>) -> Result<Vec<(usize, u8)>, String> {
    let lines = match file.as_deref() {
        None | Some("-") => std::io::stdin()
            .lock()
            .lines()
            .map(|line| line.unwrap())
            .collect::<Vec<String>>(),
        Some(file) => BufReader::new(std::fs::File::open(file).unwrap())
            .lines()
            .map(|line| line.unwrap())
            .collect::<Vec<String>>(),
    };

    Ok({
        lines
            .into_iter()
            .filter_map(|line| parse(line.trim_end()).transpose())
            .collect::<Result<_, _>>()?
    })
}

fn parse(line: &str) -> Result<Option<(usize, u8)>, String> {
    let element = match line.as_bytes().first().copied() {
        Some(b'\n' | b'#') | None => return Ok(None),
        Some(element @ (b'A' | b'V' | b'\\' | b'/')) => element,
        Some(other) => {
            return Err(format!(
                "invalid element. Expected one of 'A', 'V', '\\', '/', got {other:?}"
            ))
        }
    };

    let index = line
        .get(1..)
        .ok_or_else(|| format!("Could not extract trailing index from {line:?}"))?
        .parse()
        .map_err(|e| format!("invalid index: {e}"))?;

    Ok(Some((index, element)))
}

fn main() -> Result<(), String> {
    let display = knotty::ascii_print_compact(read_input(std::env::args().nth(1))?);

    print!("{display}");

    Ok(())
}
