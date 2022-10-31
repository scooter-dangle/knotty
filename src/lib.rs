macro_rules! try_opt {
    ($expr:expr) => {
        match $expr {
            Some(expr) => expr,
            None => return None,
        }
    };
}

#[cfg(test)]
use pretty_assertions::assert_eq;

use core::fmt;
use std::{collections::VecDeque, mem, str::FromStr};

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Horiz {
    #[default]
    Empty,
    Line,
    CrossDownOver,
    CrossDownUnder,
    CrossUpOver,
    CrossUpUnder,
    OpenedBelow,
    OpenedAbove,
    ClosedBelow,
    ClosedAbove,
    TransferUpStart,
    TransferUp,
    TransferUpFinish,
    TransferDownStart,
    TransferDown,
    TransferDownFinish,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct VerboseLine(Vec<Horiz>);

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct VerboseDiagram(Vec<VerboseLine>);

const fn display_lines(grid_borders: bool) -> usize {
    3 + if grid_borders { 1 } else { 0 }
}

const DISPLAY_WITH_BORDERS_LINES: usize = display_lines(true);
const DISPLAY_LINES: usize = display_lines(false);

impl Horiz {
    #[rustfmt::skip]
    pub const fn display(&self) -> [&'static str; DISPLAY_LINES] {
        use Horiz::*;

        match self {
            Empty => [
                r#"   "#,
                r#"   "#,
                r#"   "#,
            ],
            Line => [
                r#"___"#,
                r#"   "#,
                r#"   "#,
            ],
            CrossDownOver => [
                r#"   "#,
                r#"\ /"#,
                r#" \ "#,
            ],
            CrossDownUnder => [
                r#"   "#,
                r#"\ /"#,
                r#" / "#,
            ],
            CrossUpOver | CrossUpUnder => [
                r#"/ \"#,
                r#"   "#,
                r#"   "#,
            ],
            OpenedBelow => [
                r#"   "#,
                r#"  /"#,
                r#" ( "#,
            ],
            OpenedAbove => [
                r#"  \"#,
                r#"   "#,
                r#"   "#,
            ],
            ClosedBelow => [
                r#"   "#,
                r#"\  "#,
                r#" ) "#,
            ],
            ClosedAbove => [
                r#"/  "#,
                r#"   "#,
                r#"   "#,
            ],
            TransferUpStart => [
                r#"__/"#,
                r#"   "#,
                r#"   "#,
            ],
            TransferUp => [
                r#"  /"#,
                r#" / "#,
                r#"/  "#,
            ],
            TransferUpFinish => [
                r#"  _"#,
                r#" / "#,
                r#"/  "#,
            ],
            TransferDownStart => [
                r#"_  "#,
                r#" \ "#,
                r#"  \"#,
            ],
            TransferDown => [
                r#"\  "#,
                r#" \ "#,
                r#"  \"#,
            ],
            TransferDownFinish => [
                r#"\__"#,
                r#"   "#,
                r#"   "#,
            ],
        }
    }

    #[rustfmt::skip]
    pub const fn display_with_borders(&self) -> [&'static str; DISPLAY_WITH_BORDERS_LINES] {
        use Horiz::*;

        match self {
            Empty => [
                r#"+---"#,
                r#"|   "#,
                r#"|   "#,
                r#"|   "#,
            ],
            Line => [
                r#"+---"#,
                r#"|___"#,
                r#"|   "#,
                r#"|   "#,
            ],
            CrossDownOver => [
                r#"+---"#,
                r#"|   "#,
                r#"|\ /"#,
                r#"| \ "#,
            ],
            CrossDownUnder => [
                r#"+---"#,
                r#"|   "#,
                r#"|\ /"#,
                r#"| / "#,
            ],
            CrossUpOver | CrossUpUnder => [
                r#"+---"#,
                r#"|/ \"#,
                r#"|   "#,
                r#"|   "#,
            ],
            OpenedBelow => [
                r#"+---"#,
                r#"|   "#,
                r#"|  /"#,
                r#"| ( "#,
            ],
            OpenedAbove => [
                r#"+---"#,
                r#"|  \"#,
                r#"|   "#,
                r#"|   "#,
            ],
            ClosedBelow => [
                r#"+---"#,
                r#"|   "#,
                r#"|\  "#,
                r#"| ) "#,
            ],
            ClosedAbove => [
                r#"+---"#,
                r#"|/  "#,
                r#"|   "#,
                r#"|   "#,
            ],
            TransferUpStart => [
                r#"+---"#,
                r#"|__/"#,
                r#"|   "#,
                r#"|   "#,
            ],
            TransferUp => [
                r#"+---"#,
                r#"|  /"#,
                r#"| / "#,
                r#"|/  "#,
            ],
            TransferUpFinish => [
                r#"+---"#,
                r#"|  _"#,
                r#"| / "#,
                r#"|/  "#,
            ],
            TransferDownStart => [
                r#"+---"#,
                r#"|_  "#,
                r#"| \ "#,
                r#"|  \"#,
            ],
            TransferDown => [
                r#"+---"#,
                r#"|\  "#,
                r#"| \ "#,
                r#"|  \"#,
            ],
            TransferDownFinish => [
                r#"+---"#,
                r#"|\__"#,
                r#"|   "#,
                r#"|   "#,
            ],
        }
    }
}

impl VerboseLine {
    pub fn display<const GRID_BORDERS: bool>(&self) -> impl 'static + Iterator<Item = String> {
        let horiz_len: usize = if GRID_BORDERS {
            Horiz::Empty.display_with_borders()[0].len()
        } else {
            Horiz::Empty.display()[0].len()
        };

        let mut l0 = " ".repeat(self.0.len() * horiz_len) + "\n";
        let mut l1 = l0.clone();
        let mut l2 = l0.clone();
        let mut l3 = if GRID_BORDERS {
            l0.clone()
        } else {
            String::new()
        };

        for (idx, horiz) in self.0.iter().enumerate() {
            let [h0, h1, h2, h3] = if GRID_BORDERS {
                horiz.display_with_borders()
            } else {
                let [h0, h1, h2] = horiz.display();
                [h0, h1, h2, ""]
            };
            let range = (idx * horiz_len)..((idx + 1) * horiz_len);

            l0.replace_range(range.clone(), h0);
            l1.replace_range(range.clone(), h1);
            l2.replace_range(range.clone(), h2);
            if GRID_BORDERS {
                l3.replace_range(range, h3);
            }
        }

        [l0, l1, l2]
            .into_iter()
            .chain(std::iter::once(l3).filter(|_| GRID_BORDERS))
    }
}

impl Horiz {
    #[rustfmt::skip]
    pub const fn subsequent(&self) -> Self {
        use Horiz::*;

        match self {
            | Empty
            | ClosedBelow
            | ClosedAbove
            | TransferUpStart
            | TransferUp
            | TransferDownStart
            | TransferDown
            => Empty,

            | Line
            | CrossDownOver
            | CrossDownUnder
            | CrossUpOver
            | CrossUpUnder
            | OpenedBelow
            | OpenedAbove
            | TransferUpFinish
            | TransferDownFinish
            => Line,
        }
    }

    pub const fn is_empty(&self) -> bool {
        matches!(self, Horiz::Empty)
    }

    ///// # Panics
    /////
    ///// If element is not a raw diagram element.
    //fn num_segments(element: u8) -> u8 {
    //    match element {
    //        _ => panic!("Not a raw diagram element: {element:?}"),
    //    }
    //}
}

fn raw_lines_is_empty_above(lines: &[Vec<Horiz>], idx: usize) -> bool {
    lines.get(idx..).unwrap_or_default().iter().all(|line| {
        line.last()
            .cloned()
            .unwrap_or_default()
            .subsequent()
            .is_empty()
    })
}

fn raw_lines_continue(lines: &mut [Vec<Horiz>]) {
    lines
        .iter_mut()
        .for_each(|line| line.push(line.last().unwrap_or(&Horiz::Empty).subsequent()));
}

fn raw_lines_expand_above(lines: &mut [Vec<Horiz>], idx: usize) {
    let (lower, upper) = lines.split_at_mut(idx);
    for _ in 0..3 {
        raw_lines_continue(lower);
    }

    let mut indexes: VecDeque<_> = upper
        .iter_mut()
        .map(|line| {
            let is_empty = line
                .last()
                .cloned()
                .unwrap_or_default()
                .subsequent()
                .is_empty();

            line.push(if is_empty {
                Horiz::Empty
            } else {
                Horiz::TransferUpStart
            });

            is_empty
        })
        .collect();

    indexes.rotate_right(1);

    upper
        .iter_mut()
        .zip(indexes.iter())
        .for_each(|(line, is_empty)| {
            line.push(if *is_empty {
                Horiz::Empty
            } else {
                Horiz::TransferUp
            });
        });

    indexes.rotate_right(1);

    upper
        .iter_mut()
        .zip(indexes.iter())
        .for_each(|(line, is_empty)| {
            line.push(if *is_empty {
                Horiz::Empty
            } else {
                Horiz::TransferUpFinish
            });
        });
}

#[test]
fn snapshot_raw_lines_expand_contract() {
    use Horiz::*;

    let mut lines = vec![vec![Line], vec![Line], vec![Line], vec![Empty], vec![Empty]];
    let original_lines = lines.clone();

    raw_lines_expand_above(&mut lines, 1);
    insta::assert_debug_snapshot!(lines);

    raw_lines_contract_above(&mut lines, 1);
    insta::assert_debug_snapshot!(lines);

    raw_lines_continue(&mut lines);
    let final_column = lines
        .iter()
        .map(|line| vec![line.last().cloned().unwrap()])
        .collect::<Vec<_>>();
    assert_eq!(final_column, original_lines);
}

fn raw_lines_contract_above(lines: &mut [Vec<Horiz>], idx: usize) {
    let (lower, upper) = lines.split_at_mut(idx);
    for _ in 0..3 {
        raw_lines_continue(lower);
    }

    let mut indexes: VecDeque<_> = upper
        .iter_mut()
        .enumerate()
        .map(|(idx, line)| {
            let is_empty = (0..2).contains(&idx)
                || line
                    .last()
                    .cloned()
                    .unwrap_or_default()
                    .subsequent()
                    .is_empty();

            line.push(if is_empty {
                if idx == 0 {
                    Horiz::ClosedAbove
                } else if idx == 1 {
                    Horiz::ClosedBelow
                } else {
                    Horiz::Empty
                }
            } else {
                Horiz::TransferDownStart
            });

            is_empty
        })
        .collect();

    indexes.rotate_left(1);

    upper
        .iter_mut()
        .zip(indexes.iter())
        .for_each(|(line, is_empty)| {
            line.push(if *is_empty {
                Horiz::Empty
            } else {
                Horiz::TransferDown
            });
        });

    indexes.rotate_left(1);

    upper
        .iter_mut()
        .zip(indexes.iter())
        .for_each(|(line, is_empty)| {
            line.push(if *is_empty {
                Horiz::Empty
            } else {
                Horiz::TransferDownFinish
            });
        });
}

fn raw_lines_append(lines: &mut [Vec<Horiz>], element: u8, idx: usize) {
    match element {
        b'A' => {
            if raw_lines_is_empty_above(&*lines, idx) {
                raw_lines_continue(lines);
            } else {
                raw_lines_expand_above(lines, idx);
            }
            *lines[idx].last_mut().unwrap() = Horiz::OpenedAbove;
            *lines[idx + 1].last_mut().unwrap() = Horiz::OpenedBelow;
        }
        b'V' => {
            let is_empty_above = raw_lines_is_empty_above(&*lines, idx + 2);
            if is_empty_above {
                raw_lines_continue(lines);
                *lines[idx].last_mut().unwrap() = Horiz::ClosedAbove;
                *lines[idx + 1].last_mut().unwrap() = Horiz::ClosedBelow;
            } else {
                raw_lines_contract_above(lines, idx);
            }
        }
        b'/' => {
            raw_lines_continue(lines);

            *lines[idx].last_mut().unwrap() = Horiz::CrossUpUnder;
            *lines[idx + 1].last_mut().unwrap() = Horiz::CrossDownOver;
        }
        b'\\' => {
            raw_lines_continue(lines);

            *lines[idx].last_mut().unwrap() = Horiz::CrossUpOver;
            *lines[idx + 1].last_mut().unwrap() = Horiz::CrossDownUnder;
        }
        _ => unimplemented!(),
    }
}

#[test]
fn snapshot_raw_lines_append() {
    let mut lines = vec![vec![]; 4];

    raw_lines_append(&mut lines, b'A', 0);
    insta::assert_debug_snapshot!(lines);

    raw_lines_append(&mut lines, b'A', 1);
    insta::assert_debug_snapshot!(lines);

    raw_lines_append(&mut lines, b'V', 0);
    insta::assert_debug_snapshot!(lines);

    raw_lines_append(&mut lines, b'V', 0);
    insta::assert_debug_snapshot!(lines);
}

impl VerboseDiagram {
    pub fn display<'a, const GRID_BORDERS: bool>(&'a self) -> impl 'a + Iterator<Item = String> {
        let last_idx = self.0.len() - 1;

        self.0
            .iter()
            .rev()
            .enumerate()
            .flat_map(move |(idx, line)| {
                line.display::<GRID_BORDERS>()
                    .take(display_lines(GRID_BORDERS) - if idx == last_idx { 2 } else { 0 })
            })
    }

    pub fn from_abbreviated(knot: &AbbreviatedDiagram) -> Result<Self, String> {
        let height = knot.height();

        let mut lines: Vec<Vec<Horiz>> = vec![Vec::with_capacity(knot.len()); height];

        for AbbreviatedItem { element, index } in knot.0.iter() {
            raw_lines_append(&mut lines, *element, *index);
        }

        Ok(Self(lines.into_iter().map(VerboseLine).collect()))
    }
}

#[test]
fn snapshot_from_abbreviated() {
    let knot = AbbreviatedDiagram::new_from_tuples(vec![(b'A', 0), (b'/', 0), (b'V', 0)]).unwrap();

    let verbose = VerboseDiagram::from_abbreviated(&knot).unwrap();
    insta::assert_debug_snapshot!(verbose);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Lean {
    Forward,
    Backward,
}

impl FromStr for Lean {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        use Lean::*;

        match string {
            "forward" => Ok(Forward),
            "backward" => Ok(Backward),
            _ => Err(format!("Invalid lean: {string:?}")),
        }
    }
}

impl fmt::Display for Lean {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Lean::*;

        write!(
            formatter,
            "{}",
            match self {
                Forward => "forward",
                Backward => "backward",
            },
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OverUnder {
    Over,
    Under,
}

impl FromStr for OverUnder {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        use OverUnder::*;

        Ok(match string {
            "over" => Over,
            "under" => Under,
            _ => return Err(format!("Invalid over/under: {string:?}")),
        })
    }
}

impl fmt::Display for OverUnder {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        use OverUnder::*;

        write!(
            formatter,
            "{}",
            match self {
                Over => "over",
                Under => "under",
            },
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UpDown {
    Up,
    Down,
}

impl FromStr for UpDown {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        use UpDown::*;

        Ok(match string {
            "up" => Up,
            "down" => Down,
            _ => return Err(format!("Invalid up/down: {string:?}")),
        })
    }
}

impl fmt::Display for UpDown {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        use UpDown::*;

        write!(
            formatter,
            "{}",
            match self {
                Up => "up",
                Down => "down",
            },
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Move {
    Swap,
    WrapAround,
    Bulge {
        lean: Lean,
        vertical_index: usize,
    },
    RemoveBulge,
    Reid1a {
        over_under: OverUnder,
    },
    Reid1aReduce,
    Reid1b {
        up_down: UpDown,
        over_under: OverUnder,
        vertical_index: usize,
    },
    Reid1bReduce,
    Reid2 {
        over_under: OverUnder,
        vertical_index: usize,
    },
    Reid2Reduce,
    Reid3,
}

impl fmt::Display for Move {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Move::*;

        match self {
            Swap => write!(formatter, "swap"),
            WrapAround => write!(formatter, "wrap_around"),
            RemoveBulge => write!(formatter, "remove_bulge"),
            Reid1a { over_under } => write!(formatter, "reid_1a({over_under})"),
            Reid1aReduce => write!(formatter, "reid_1a_reduce"),
            Reid1b {
                up_down,
                over_under,
                vertical_index,
            } => write!(
                formatter,
                "reid_1b({up_down}, {over_under}, {vertical_index})"
            ),
            Reid1bReduce => write!(formatter, "reid_1b_reduce"),
            Reid2 {
                over_under,
                vertical_index,
            } => write!(formatter, "reid_2a({over_under}, {vertical_index})"),
            Reid2Reduce => write!(formatter, "reid_2_reduce"),
            Reid3 => write!(formatter, "reid_3"),
            Bulge {
                lean,
                vertical_index,
            } => write!(formatter, "bulge({lean}, {vertical_index})"),
        }
    }
}

fn parse_bulge_args(mut string: &str) -> Result<(Lean, usize), String> {
    if string.ends_with(')') {
        string = &string[..string.len() - 1];
    } else {
        return Err("missing closing parenthesis".into());
    }

    let args = string.split(',').map(str::trim).collect::<Vec<_>>();
    if args.len() != 2 {
        return Err(format!("expected two arguments, got {}", args.len()));
    }

    Ok((
        args[0].parse()?,
        args[1]
            .parse()
            .map_err(|err| format!("invalid vertical index {}: {err}", args[1]))?,
    ))
}

impl FromStr for Move {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        use Move::*;

        let mut open_par_splits = string.split('(');

        let moov = match open_par_splits
            .next()
            .filter(|split| !split.is_empty())
            .ok_or_else(|| "Move can't be empty string")?
        {
            "swap" => Swap,
            "wrap_around" => WrapAround,
            "remove_bulge" => RemoveBulge,
            "reid_1a_reduce" => Reid1aReduce,
            "reid_1b_reduce" => Reid1bReduce,
            "reid_2_reduce" => Reid2Reduce,
            "reid_3" => Reid3,
            "reid_1a" => Reid1a {
                over_under: open_par_splits
                    .next()
                    .map(|split| split.trim_end_matches(')'))
                    .ok_or_else(|| "reid_1a requires an argument")?
                    .parse()?,
            },
            "reid_1b" => {
                let (up_down, over_under, vertical_index) = parse_reid_1b_args(
                    open_par_splits
                        .next()
                        .ok_or_else(|| "reid_1b requires arguments")?,
                )?;

                Reid1b {
                    up_down,
                    over_under,
                    vertical_index,
                }
            }
            "reid_2" => {
                let (over_under, vertical_index) = parse_reid_2_args(
                    open_par_splits
                        .next()
                        .ok_or_else(|| "reid_2 requires arguments")?,
                )?;

                Reid2 {
                    over_under,
                    vertical_index,
                }
            }
            "bulge" => {
                let (lean, vertical_index) = parse_bulge_args(
                    open_par_splits
                        .next()
                        .ok_or_else(|| "missing bulge arguments")?,
                )?;

                Bulge {
                    lean,
                    vertical_index,
                }
            }
            other => return Err(format!("Invalid move kind: {other:?}")),
        };

        if open_par_splits.next().is_some() {
            return Err("unexpected opening parenthesis".into());
        }

        Ok(moov)
    }
}

fn parse_reid_1b_args(mut string: &str) -> Result<(UpDown, OverUnder, usize), String> {
    if string.ends_with(')') {
        string = &string[..string.len() - 1];
    } else {
        return Err("missing closing parenthesis".into());
    }

    let args = string.split(',').map(str::trim).collect::<Vec<_>>();
    if args.len() != 3 {
        return Err(format!("expected three arguments, got {}", args.len()));
    }

    Ok((
        args[0].parse()?,
        args[1].parse()?,
        args[2]
            .parse()
            .map_err(|err| format!("invalid vertical index {}: {err}", args[2]))?,
    ))
}

fn parse_reid_2_args(mut string: &str) -> Result<(OverUnder, usize), String> {
    if string.ends_with(')') {
        string = &string[..string.len() - 1];
    } else {
        return Err("missing closing parenthesis".into());
    }

    let args = string.split(',').map(str::trim).collect::<Vec<_>>();
    if args.len() != 2 {
        return Err(format!("expected two arguments, got {}", args.len()));
    }

    Ok((
        args[0].parse()?,
        args[1]
            .parse()
            .map_err(|err| format!("invalid vertical index {}: {err}", args[1]))?,
    ))
}

impl fmt::Display for DiagramMove {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{}@{}", self.r#move, self.idx)
    }
}

impl FromStr for DiagramMove {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let (moove, idx) = string.split_once('@').ok_or_else(|| "No @ symbol")?;

        let idx = idx
            .parse::<usize>()
            .map_err(|err| format!("Invalid index {idx:?}: {err}"))?;

        let moov = moove.parse()?;

        Ok(Self { idx, r#move: moov })
    }
}

pub struct DiagramMove {
    idx: usize,
    r#move: Move,
}

pub struct DiagramMoves(Vec<DiagramMove>);

impl IntoIterator for DiagramMoves {
    type Item = DiagramMove;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl FromStr for DiagramMoves {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        string
            .split('\n')
            .map(str::trim)
            .filter(|line| !line.starts_with('#') && !line.is_empty())
            .map(str::parse)
            .collect::<Result<Vec<_>, _>>()
            .map(Self)
    }
}

impl fmt::Display for DiagramMoves {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0
            .iter()
            .map(|moov| writeln!(formatter, "{moov}\n"))
            .collect()
    }
}

trait Item {
    fn is_crossing(&self) -> bool;
    fn is_opening(&self) -> bool;
    fn is_closing(&self) -> bool;
}

impl Item for u8 {
    fn is_crossing(&self) -> bool {
        matches!(self, b'/' | b'\\')
    }

    fn is_opening(&self) -> bool {
        matches!(self, b'A')
    }

    fn is_closing(&self) -> bool {
        matches!(self, b'V')
    }
}

impl Item for AbbreviatedItem {
    fn is_crossing(&self) -> bool {
        self.element.is_crossing()
    }

    fn is_opening(&self) -> bool {
        self.element.is_opening()
    }

    fn is_closing(&self) -> bool {
        self.element.is_closing()
    }
}

trait SmallDistance {
    fn small_distance_from(&self, other: &Self) -> u8;

    fn is_at_least_2_away_from(&self, other: &Self) -> bool {
        self.small_distance_from(other) >= 2
    }
}

impl SmallDistance for usize {
    fn small_distance_from(&self, other: &Self) -> u8 {
        (self.max(other) - self.min(other)).min(u8::MAX as usize) as u8
    }
}

impl SmallDistance for AbbreviatedItem {
    fn small_distance_from(&self, other: &Self) -> u8 {
        self.index.small_distance_from(&other.index)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;
    use Lean::*;

    macro_rules! assert_eq_after {
        ($operation:expr, $idx:expr, [$($diagram:expr),* $(,)?], [$($expected:expr),* $(,)?] $(,)?) => {
            let idx = $idx;

            let diagram: Vec<(u8, usize)> = vec![$($diagram,)*];
            let expected: Vec<(u8, usize)> = vec![$($expected,)*];
            let actual: Vec<(u8, usize)> = {
                let mut diagram = AbbreviatedDiagram::new_from_tuples(diagram.clone()).unwrap();
                ($operation)(&mut diagram, idx).unwrap();
                diagram.to_tuples()
            };

            assert_eq!(
                actual
                    .clone()
                    .into_iter()
                    .map(|(element, index)| {
                        let element = element as char;
                        format!("{element}{index}\n")
                    })
                    .collect::<String>(),
                expected
                    .clone()
                    .into_iter()
                    .map(|(element, index)| {
                        let element = element as char;
                        format!("{element}{index}\n")
                    })
                    .collect::<String>(),
                "{}@{idx}\
                \noriginal:\n{}\
                \nexpected:\n{}\
                \nactual:\n{}",
                stringify!($operation),
                ascii_print::<false>(diagram),
                ascii_print::<false>(expected),
                ascii_print::<false>(actual),
            );
        };
    }

    fn apply(
        manip: fn(&mut AbbreviatedDiagram, usize) -> Result<(), String>,
        idx: usize,
        diagram: Vec<(u8, usize)>,
    ) -> Result<Vec<(u8, usize)>, String> {
        let mut diagram = AbbreviatedDiagram::new_from_tuples(diagram)?;
        manip(&mut diagram, idx)?;
        Ok(diagram.to_tuples())
    }

    macro_rules! assert_eq_after_apply {
        ($operation:ident, $idx:expr, [$($diagram:expr),* $(,)?], [$($expected:expr),* $(,)?] $(,)?) => {
            let idx = $idx;

            let diagram = vec![$($diagram,)*];
            let expected = vec![$($expected,)*];
            let actual = apply(AbbreviatedDiagram::$operation, idx, diagram.clone()).unwrap();

            assert_eq!(
                actual
                    .clone()
                    .into_iter()
                    .map(|(element, index)| {
                        let element = element as char;
                        format!("{element}{index}\n")
                    })
                    .collect::<String>(),
                expected
                    .clone()
                    .into_iter()
                    .map(|(element, index)| {
                        let element = element as char;
                        format!("{element}{index}\n")
                    })
                    .collect::<String>(),
                "{}@{idx}\
                \noriginal:\n{}\
                \nexpected:\n{}\
                \nactual:\n{}",
                stringify!($operation),
                ascii_print::<false>(diagram),
                ascii_print::<false>(expected),
                ascii_print::<false>(actual),
            );
        };
    }

    #[test]
    fn test_try_bulge() {
        assert_eq_after!(
            |diagram: &mut AbbreviatedDiagram, idx| diagram.try_bulge(Backward, 0, idx),
            1,
            [(b'A', 0), (b'V', 0)],
            [(b'A', 0), (b'A', 1), (b'V', 0), (b'V', 0)],
        );

        assert_eq_after_apply!(
            try_remove_bulge,
            1,
            [(b'A', 0), (b'A', 1), (b'V', 0), (b'V', 0)],
            [(b'A', 0), (b'V', 0)],
        );

        assert_eq_after!(
            |diagram: &mut AbbreviatedDiagram, idx| diagram.try_bulge(Forward, 0, idx),
            1,
            [(b'A', 0), (b'V', 0)],
            [(b'A', 0), (b'A', 0), (b'V', 1), (b'V', 0)],
        );

        assert_eq_after_apply!(
            try_remove_bulge,
            1,
            [(b'A', 0), (b'A', 0), (b'V', 1), (b'V', 0)],
            [(b'A', 0), (b'V', 0)],
        );
    }

    #[test]
    fn test_try_swap() {
        let mut diagram = AbbreviatedDiagram::new_from_tuples(vec![(b'A', 0), (b'V', 0)]).unwrap();
        assert!(diagram.try_swap(0).is_err());
        assert!(diagram.try_swap(0).is_err());

        assert_eq_after_apply!(
            try_swap,
            0,
            [(b'A', 0), (b'A', 2), (b'V', 2), (b'V', 0)],
            [(b'A', 0), (b'A', 0), (b'V', 2), (b'V', 0)],
        );

        assert_eq_after_apply!(
            try_swap,
            1,
            [(b'A', 0), (b'/', 0), (b'A', 2), (b'V', 2), (b'V', 0)],
            [(b'A', 0), (b'A', 2), (b'/', 0), (b'V', 2), (b'V', 0)],
        );

        assert_eq_after_apply!(
            try_swap,
            2,
            [
                (b'A', 0),
                (b'A', 2),
                (b'/', 2),
                (b'/', 0),
                (b'V', 2),
                (b'V', 0),
            ],
            [
                (b'A', 0),
                (b'A', 2),
                (b'/', 0),
                (b'/', 2),
                (b'V', 2),
                (b'V', 0),
            ],
        );

        assert_eq_after_apply!(
            try_swap,
            0,
            [(b'A', 0), (b'A', 0), (b'V', 0), (b'V', 0)],
            [(b'A', 0), (b'A', 2), (b'V', 0), (b'V', 0)],
        );

        assert_eq_after_apply!(
            try_swap,
            0,
            [(b'A', 0), (b'A', 2), (b'V', 0), (b'V', 0)],
            [(b'A', 0), (b'A', 0), (b'V', 0), (b'V', 0)],
        );

        assert_eq_after_apply!(
            try_swap,
            2,
            [(b'A', 0), (b'A', 0), (b'V', 0), (b'V', 0)],
            [(b'A', 0), (b'A', 0), (b'V', 2), (b'V', 0)],
        );

        assert_eq_after_apply!(
            try_swap,
            2,
            [(b'A', 0), (b'A', 0), (b'V', 2), (b'V', 0)],
            [(b'A', 0), (b'A', 0), (b'V', 0), (b'V', 0)],
        );
    }

    #[test]
    fn test_try_wrap_around() {
        assert_eq_after_apply!(
            try_wrap_around,
            1,
            [(b'A', 0), (b'A', 0), (b'/', 1), (b'V', 2), (b'V', 0)],
            [(b'A', 0), (b'A', 1), (b'\\', 0), (b'V', 2), (b'V', 0)],
        );
    }
}

impl AbbreviatedDiagram {
    // TODO proper error
    pub fn try_apply(&mut self, diagram_move: DiagramMove) -> Result<(), String> {
        use Move::*;

        (match diagram_move.r#move {
            Swap => Self::try_swap,
            WrapAround => Self::try_wrap_around,
            RemoveBulge => Self::try_remove_bulge,
            Reid1aReduce => Self::try_reid_1_a_reduce,
            Reid1bReduce => Self::try_reid_1_b_reduce,
            Reid2Reduce => Self::try_reid_2_reduce,
            Reid3 => Self::try_reid_3,
            Reid1a { over_under } => return self.try_reid_1_a(over_under, diagram_move.idx),
            Reid1b {
                up_down,
                over_under,
                vertical_index,
            } => return self.try_reid_1_b(up_down, over_under, vertical_index, diagram_move.idx),
            Reid2 {
                over_under,
                vertical_index,
            } => return self.try_reid_2(over_under, vertical_index, diagram_move.idx),
            Bulge {
                lean,
                vertical_index,
            } => return self.try_bulge(lean, vertical_index, diagram_move.idx),
        })(self, diagram_move.idx)
    }

    pub fn try_apply_all(
        &mut self,
        diagram_moves: impl IntoIterator<Item = DiagramMove>,
    ) -> Result<(), String> {
        diagram_moves
            .into_iter()
            .map(|diagram_move| self.try_apply(diagram_move))
            .collect()
    }

    fn vertical_height_at_index(&self, idx: usize) -> i32 {
        self
            .0
            .iter()
            .map(|item| match item.element {
                b'A' => 2,
                b'V' => -2,
                b'/' | b'\\' => 0,
                _ => {
                    let other = item.element as char;
                    unreachable!("BUG: shouldn't be able to get here for valid diagram. Invalid element: {other:?}")
                },
            })
            .take(idx)
            .sum::<i32>()
    }

    fn try_bulge(&mut self, lean: Lean, vertical_index: usize, idx: usize) -> Result<(), String> {
        if idx > self.len() {
            return Err(format!(
                "index ({idx}) out of bounds: {idx} > {}",
                self.len()
            ));
        }

        let vertical_height_at_index = self.vertical_height_at_index(idx);

        if vertical_index as i32 >= vertical_height_at_index {
            return Err(format!(
                "bulge vertical index ({vertical_index}) equal to or beyond diagram height ({vertical_height_at_index}) at {idx}",
            ));
        }

        let (opening_idx, closing_idx) = match lean {
            Lean::Backward => (vertical_index + 1, vertical_index),
            Lean::Forward => (vertical_index, vertical_index + 1),
        };

        self.0.reserve_exact(2);
        self.0.insert(
            idx,
            AbbreviatedItem {
                element: b'V',
                index: closing_idx,
            },
        );
        self.0.insert(
            idx,
            AbbreviatedItem {
                element: b'A',
                index: opening_idx,
            },
        );

        Ok(())
    }

    fn try_apply_(
        &mut self,
        operation: fn(
            AbbreviatedItem,
            AbbreviatedItem,
        ) -> Result<(AbbreviatedItem, AbbreviatedItem), String>,
        idx: usize,
    ) -> Result<(), String> {
        let range = idx..idx.checked_add(2).ok_or("cannot add to max integer")?;

        let mut items = self
            .0
            .get_mut(range.clone())
            .ok_or_else(|| format!("{range:?} is outside the range of the diagram"))?
            .iter_mut();

        // Okay to unwrap because we know the range is exactly 2
        let item0 = items.next().unwrap();
        let item1 = items.next().unwrap();
        debug_assert!(items.next().is_none());

        let (new_item0, new_item1) = operation(*item0, *item1)?;
        *item0 = new_item0;
        *item1 = new_item1;

        Ok(())
    }

    fn try_swap(&mut self, idx: usize) -> Result<(), String> {
        self.try_apply_(AbbreviatedItem::try_swap, idx)
    }

    fn try_wrap_around(&mut self, idx: usize) -> Result<(), String> {
        self.try_apply_(AbbreviatedItem::try_wrap_around, idx)
    }

    fn try_remove_bulge(&mut self, idx: usize) -> Result<(), String> {
        let closing_idx = idx + 1;

        let closing = self.0.get(closing_idx).ok_or_else(|| {
            format!(
                "index ({closing_idx}) out of bounds: {closing_idx} > {}",
                self.len()
            )
        })?;

        // Can't fail if getting closing succeeded
        let opening = self.0[idx];

        opening.error_on_remove_bulge(*closing)?;

        self.0.remove(idx);
        self.0.remove(idx);

        Ok(())
    }

    pub fn try_reid_1_a_reduce(&mut self, idx: usize) -> Result<(), String> {
        let second_idx = idx + 1;

        let item1 = self.0.get(second_idx).ok_or_else(|| {
            format!(
                "index ({second_idx}) out of bounds: {idx} + 1 >= {}",
                self.len()
            )
        })?;
        // Can't fail if getting item1 succeeded
        let item0 = self.0[idx];

        let idx_to_remove =
            AbbreviatedItem::reid_1_a_reduce_index(item0, *item1, idx).ok_or_else(|| {
                format!("cannot apply Reidemeister 1A reduction to {item0} and {item1} at {idx}",)
            })?;

        self.0.remove(idx_to_remove);

        Ok(())
    }

    pub fn try_reid_1_a(&mut self, over_under: OverUnder, idx: usize) -> Result<(), String> {
        let open_close = self
            .0
            .get(idx)
            .ok_or_else(|| format!("index ({idx}) out of bounds: {idx} >= {}", self.len()))?
            .clone();

        let insertion_idx = match open_close.element {
            b'A' => idx + 1,
            b'V' => idx,
            b'/' | b'\\' => {
                return Err(format!(
                    "cannot apply Reidemeister 1A to {open_close} at {idx}. \
                    (Must be an opening or closing)",
                ))
            }
            _ => unreachable!(
                "BUG: shouldn't be able to get here for valid diagram. Invalid \
                element: {open_close:?}"
            ),
        };

        self.0.insert(
            insertion_idx,
            AbbreviatedItem {
                element: match over_under {
                    OverUnder::Over => b'\\',
                    OverUnder::Under => b'/',
                },
                index: open_close.index,
            },
        );

        Ok(())
    }

    pub fn try_reid_1_b_reduce(&mut self, idx: usize) -> Result<(), String> {
        let third_idx = idx + 2;

        let item2 = self.0.get(third_idx).ok_or_else(|| {
            format!(
                "index ({third_idx}) out of bounds: {idx} + 2 >= {}",
                self.len()
            )
        })?;
        // These two can't fail if getting item2 succeeded
        let item1 = self.0[idx + 1];
        let item0 = self.0[idx];

        if AbbreviatedItem::is_reid_1_b_reduce_eligible(item0, item1, *item2) {
            self.0.remove(idx);
            self.0.remove(idx);
            self.0.remove(idx);

            Ok(())
        } else {
            Err(format!(
                "cannot apply Reidemeister 1B reduction to {item0}, {item1}, and {item2} at {idx}",
            ))
        }
    }

    pub fn try_reid_2(
        &mut self,
        over_under: OverUnder,
        vertical_index: usize,
        idx: usize,
    ) -> Result<(), String> {
        if idx > self.len() {
            return Err(format!(
                "index ({idx}) out of bounds: {idx} > {}",
                self.len()
            ));
        }

        let vertical_height_at_index = self.vertical_height_at_index(idx);

        if vertical_index as i32 > vertical_height_at_index - 2 {
            return Err(format!(
                "Reidemeister 2 vertical index ({vertical_index}) exceeds \
                diagram height + 2 ({vertical_height_at_index}) at {idx}",
            ));
        }

        let (element0, element1) = match over_under {
            OverUnder::Over => (b'/', b'\\'),
            OverUnder::Under => (b'\\', b'/'),
        };

        self.0.reserve_exact(2);

        self.0.insert(
            idx,
            AbbreviatedItem {
                element: element0,
                index: vertical_index,
            },
        );
        self.0.insert(
            idx + 1,
            AbbreviatedItem {
                element: element1,
                index: vertical_index,
            },
        );

        Ok(())
    }

    pub fn try_reid_2_reduce(&mut self, idx: usize) -> Result<(), String> {
        let second_idx = idx + 1;

        let item1 = self.0.get(second_idx).ok_or_else(|| {
            format!(
                "index ({second_idx}) out of bounds: {idx} + 1 >= {}",
                self.len()
            )
        })?;
        // Can't fail if getting item1 succeeded
        let item0 = self.0[idx];

        if AbbreviatedItem::is_reid_2_reduce_eligible(item0, *item1) {
            self.0.remove(idx);
            self.0.remove(idx);

            Ok(())
        } else {
            Err(format!(
                "cannot apply Reidemeister 2 reduction to {item0} and {item1} at {idx}",
            ))
        }
    }

    pub fn try_reid_3(&mut self, idx: usize) -> Result<(), String> {
        let third_idx = idx + 2;

        let mut items = self
            .0
            .get_mut(idx..=third_idx)
            .ok_or_else(|| format!("{idx}..{third_idx} is outside the range of the diagram"))?
            .iter_mut();

        let item0 = items.next().unwrap();
        let item1 = items.next().unwrap();
        let item2 = items.next().unwrap();
        debug_assert!(items.next().is_none());

        let (new_item0, new_item1, new_item2) =
            AbbreviatedItem::try_reid_3(*item0, *item1, *item2)?;
        *item0 = new_item0;
        *item1 = new_item1;
        *item2 = new_item2;

        Ok(())
    }

    pub fn list_available(
        &self,
        // Lame. Can't use this with bulges
        operation: fn(AbbreviatedItem, AbbreviatedItem) -> bool,
    ) -> impl '_ + Iterator<Item = usize> {
        self.0
            .windows(2)
            .enumerate()
            .filter_map(move |(idx, items)| operation(items[0], items[1]).then(|| idx))
    }

    pub fn available_swaps(&self) -> impl '_ + Iterator<Item = usize> {
        self.list_available(AbbreviatedItem::can_swap)
    }

    pub fn available_wrap_arounds(&self) -> impl '_ + Iterator<Item = usize> {
        self.list_available(AbbreviatedItem::can_wrap_around)
    }

    pub fn available_remove_bulges(&self) -> impl '_ + Iterator<Item = usize> {
        // Maybe don't do this the wildly inefficient way, recalculating
        // the diagram height at each index.
        self.0
            .windows(2)
            .enumerate()
            .filter_map(|(idx, items)| items[0].is_bulge_with(items[1]).then(|| idx))
    }

    pub fn available_bulges(&self) -> impl '_ + Iterator<Item = (usize, (Lean, usize))> {
        let mut height = 0isize;

        self.0
            .iter()
            .map(move |item| {
                height += match item.element {
                    b'A' => 2,
                    b'V' => -2,
                    b'/' | b'\\' => 0,
                    _ => {
                        let other = item.element as char;
                        unreachable!(
                            "BUG: shouldn't be able to get here for valid \
                            diagram. Invalid element: {other:?}",
                        )
                    }
                };

                height
            })
            .enumerate()
            .flat_map(|(idx, height)| {
                (0..height).flat_map(move |vertical_index| {
                    let idx = idx + 1;

                    [
                        (idx, (Lean::Backward, vertical_index as usize)),
                        (idx, (Lean::Forward, vertical_index as usize)),
                    ]
                })
            })
    }

    pub fn try_reid_1_b(
        &mut self,
        up_down: UpDown,
        over_under: OverUnder,
        vertical_index: usize,
        idx: usize,
    ) -> Result<(), String> {
        if idx > self.len() {
            return Err(format!(
                "index ({idx}) out of bounds: {idx} > {}",
                self.len()
            ));
        }

        let vertical_height_at_index = self.vertical_height_at_index(idx);

        let crossing_vertical_height = match up_down {
            UpDown::Up => {
                if vertical_index == 0 {
                    return Err(format!(
                        "Reidemeister 1b vertical index ({vertical_index}) \
                        must be greater than 0 if direction is up (i.e., it's \
                        up from where? -1? that's not a valid index)",
                    ));
                }
                if vertical_index as i32 > vertical_height_at_index {
                    return Err(format!(
                        "Reidemeister 1b vertical index ({vertical_index}) exceeds \
                        diagram height ({vertical_height_at_index}) at {idx}",
                    ));
                }
                vertical_index - 1
            }
            UpDown::Down => {
                if vertical_index as i32 + 1 > vertical_height_at_index {
                    return Err(format!(
                        "Reidemeister 1b vertical index ({vertical_index}) exceeds \
                        diagram height - 1 ({vertical_height_at_index}) at {idx}",
                    ));
                }
                vertical_index + 1
            }
        };

        self.0.insert(
            idx,
            AbbreviatedItem {
                element: b'A',
                index: vertical_index,
            },
        );
        self.0.insert(
            idx + 1,
            AbbreviatedItem {
                element: match over_under {
                    OverUnder::Over => b'\\',
                    OverUnder::Under => b'/',
                },
                index: crossing_vertical_height,
            },
        );
        self.0.insert(
            idx + 2,
            AbbreviatedItem {
                element: b'V',
                index: vertical_index,
            },
        );

        Ok(())
    }
}

#[test]
fn test_available_bulges() {
    let diagram = AbbreviatedDiagram::from_str(
        "\
        A0\n\
        V0\n\
        ",
    )
    .unwrap();

    let available_bulges = diagram.available_bulges().collect::<Vec<_>>();

    assert_eq!(
        available_bulges,
        vec![
            (1, (Lean::Backward, 0)),
            (1, (Lean::Forward, 0)),
            (1, (Lean::Backward, 1)),
            (1, (Lean::Forward, 1)),
        ]
    );

    // ------------------------------------------

    let diagram = AbbreviatedDiagram::from_str(
        "\
        A0\n\
        A0\n\
        V0\n\
        V0\n\
        ",
    )
    .unwrap();

    let available_bulges = diagram.available_bulges().collect::<Vec<_>>();

    assert_eq!(
        available_bulges,
        vec![
            (1, (Lean::Backward, 0)),
            (1, (Lean::Forward, 0)),
            (1, (Lean::Backward, 1)),
            (1, (Lean::Forward, 1)),
            // ---
            (2, (Lean::Backward, 0)),
            (2, (Lean::Forward, 0)),
            (2, (Lean::Backward, 1)),
            (2, (Lean::Forward, 1)),
            (2, (Lean::Backward, 2)),
            (2, (Lean::Forward, 2)),
            (2, (Lean::Backward, 3)),
            (2, (Lean::Forward, 3)),
            // ---
            (3, (Lean::Backward, 0)),
            (3, (Lean::Forward, 0)),
            (3, (Lean::Backward, 1)),
            (3, (Lean::Forward, 1)),
        ]
    );
}

#[allow(unused)]
// Only works for strictly increasing sequences
fn non_adjacent(iter: impl Iterator<Item = usize>) -> impl Iterator<Item = usize> {
    let mut iter = iter.peekable();
    let mut last = iter.peek().copied();

    iter.filter(move |&num| {
        let non_adjacent = num.checked_sub(1) != last;

        if non_adjacent {
            last = Some(num);
        }

        non_adjacent
    })
}

#[test]
fn test_non_adjacent() {
    assert_eq!(
        non_adjacent(vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9].into_iter()).collect::<Vec<_>>(),
        vec![0, 2, 4, 6, 8]
    );

    assert_eq!(
        non_adjacent(vec![0, 2, 3, 5, 6, 7, 8, 9].into_iter()).collect::<Vec<_>>(),
        vec![0, 2, 5, 7, 9]
    );
}

impl AbbreviatedItem {
    fn can_wrap_around(self, other: Self) -> bool {
        self.try_wrap_around(other).is_ok()
    }

    fn try_wrap_around(mut self, ref mut other: Self) -> Result<(Self, Self), String> {
        let self_ = &mut self;

        let (crossing, open_close) = match (self_.element, other.element) {
            (b'/' | b'\\', b'V') => (&mut *self_, &mut *other),
            (b'A', b'/' | b'\\') => (&mut *other, &mut *self_),
            _ => {
                return Err(format!(
                    "can only wrap when a crossing follows an opening or precedes a closing"
                ))
            }
        };

        if crossing.small_distance_from(open_close) != 1 {
            return Err(format!(
                "can only wrap when the crossing vertical index is exactly one \
                more or one less than the opening or closing",
            ));
        }

        mem::swap(&mut crossing.index, &mut open_close.index);
        crossing.element = match crossing.element {
            b'/' => b'\\',
            b'\\' => b'/',
            // We checked above that crossing is, in fact, a crossing
            _ => unreachable!(),
        };

        Ok((*self_, *other))
    }

    fn can_swap(self, other: Self) -> bool {
        self.try_swap(other).is_ok()
    }

    fn try_swap(mut self, ref mut item1: Self) -> Result<(Self, Self), String> {
        let ref mut item0 = self;

        // Use enums instead of this giant if-else chain
        if item0.is_crossing() && item1.is_opening() {
            // (b'/' | b'\\', b'A') => {
            let crossing = &mut *item0;
            let opening = &mut *item1;

            if crossing.index < opening.index {
                if !crossing.is_at_least_2_away_from(&opening) {
                    return Err(format!(
                        "swapping crossing {crossing} and opening {opening} \
                        would require splitting the crossing"
                    ));
                }
            } else {
                crossing.index += 2;
            }
        } else if item0.is_opening() && item1.is_crossing() {
            // (b'A', b'/' | b'\\') => {
            let opening = &mut *item0;
            let crossing = &mut *item1;
            if !opening.is_at_least_2_away_from(&crossing) {
                return Err(format!(
                    "opening {opening} enables subsequent crossing {crossing} \
                    and so can't be moved to the right of it"
                ));
            }

            if opening.index < crossing.index {
                // Isn't possible for crossing.index to be <= 2 if it's
                // greater than opening.index and at least 2 away from
                // it
                crossing.index = crossing.index.checked_sub(2).unwrap();
            }
        } else if item0.is_crossing() && item1.is_closing() {
            // (b'/' | b'\\', b'V') => {
            let crossing = &mut *item0;
            let closing = &mut *item1;

            if !closing.is_at_least_2_away_from(&crossing) {
                return Err(format!(
                    "crossing {crossing} requires subsequent closing {closing} \
                    and so can't be moved to the right of it"
                ));
            }

            if closing.index < crossing.index {
                // Isn't possible for crossing.index to be <= 2 if it's
                // greater than closing.index and at least 2 away from
                // it
                crossing.index = crossing.index.checked_sub(2).unwrap();
            }
        } else if item0.is_closing() && item1.is_crossing() {
            // (b'V', b'/' | b'\\') => {
            let closing = &mut *item0;
            let crossing = &mut *item1;
            if crossing.index < closing.index {
                if !crossing.is_at_least_2_away_from(&closing) {
                    return Err(format!(
                        "swapping closing {closing} and crossing {crossing} \
                        would require splitting the crossing"
                    ));
                }
            } else {
                crossing.index += 2;
            }
        } else
        // Open + Open
        // ===========
        //
        //
        //
        //                      ___
        //                     /
        //                    /
        //                   /  ___
        //                  /  /
        //                 /  /
        //              __/  /  ___
        //             /    /  /
        //            /    /  /
        //           /  __/  /  __
        //          /  /    /  /
        //         /  (    /  /
        //       _/    \__/  /  ___
        //      /           /  /
        //     (           /  (
        //      \_________/    \___
        //
        //
        //
        //
        //
        //           ___               ___
        //          /                 /
        //         /                 (
        //        /  ___              \___
        //       /  /
        //      /  /         ⇒
        //   __/  /  ___            ______
        //  /    /  /              /
        // (    /  (              (
        //  \__/    \___           \______
        //
        //
        //
        //
        // This one can't be swapped
        //
        //           ___
        //          /
        //         /
        //        /  ___
        //       /  /
        //      /  (
        //   __/    \___
        //  /
        // (
        //  \___________
        //
        //
        //
        //
        //      ___                    ___
        //     /                      /
        //    (                      /
        //     \___                 /  ___
        //                         /  /
        //               ⇒        /  /
        //   ______            __/  /  ___
        //  /                 /    /  /
        // (                 (    /  (
        //  \______           \__/    \___
        //

        // TODO: test these!
        if item0.is_opening() && item1.is_opening() {
            // (b'A', b'A') => {
            let opening0 = &mut *item0;
            let opening1 = &mut *item1;

            if opening0.index >= opening1.index {
                // This comment is obsolete.
                //
                // // This one is weird. We could technically just
                // // increment opening1.index by 2 and skip the swap, but
                // // that might make it harder to unify with all the other
                // // swap cases.

                opening0.index += 2;
            } else {
                if opening0.is_at_least_2_away_from(&opening1) {
                    opening1.index = opening1.index.checked_sub(2).unwrap();
                } else {
                    return Err(format!(
                        "swapping adjacent openings {opening0} and {opening1} \
                        would require a reidemeister II move"
                    ));
                }
            }
        } else
        // Close + Close
        // =============
        //
        //
        // __
        //   \
        //    \
        // __  \
        //   \  \
        //    \  \
        // __  \  \_
        //   \  \   \
        //    )  \   )
        // __/    \_/
        //
        //
        //
        //
        //
        // __
        //   \
        //    \
        // __  \
        //   \  \
        //    )  \
        // __/    \_
        //          \
        //           )
        // _________/
        //
        //
        //
        //
        // ___
        //    \
        //     )
        // ___/
        //
        //
        // ______
        //       \
        //        )
        // ______/
        //
        if item0.is_closing() && item1.is_closing() {
            // (b'V', b'V') => {
            let closing0 = &mut *item0;
            let closing1 = &mut *item1;

            if closing0.index <= closing1.index {
                // This comment is obsolete.
                //
                // // This one is weird. We could technically just
                // // increment closing1.index by 2 and skip the swap, but
                // // that might make it harder to unify with all the other
                // // swap cases.

                closing1.index += 2;
            } else {
                if closing0.is_at_least_2_away_from(&closing1) {
                    closing0.index = closing0.index.checked_sub(2).unwrap();
                } else {
                    return Err(format!(
                        "swapping adjacent closings {closing0} and {closing1} \
                        would require a reidemeister II move"
                    ));
                }
            }
        } else
        // Close + Open
        // ============
        //
        //                         ___
        //                        /   \
        //                       /     )
        //                      /  ___/
        //                     /  /
        //                    /  /
        // ____     __      _/  /  ______
        //     \   /           /  /
        //      ) /           /  /
        // ____/ /  __      _/  /  ______
        //      /  /           /  /
        //     /  (     ⇒     /  (
        // ___/    \__      _/    \______
        //
        //
        // This one is very weird since there are two possible outcomes.
        // We'll default to the 2nd one, but we might need to revisit.
        // It could invalidate some unexamined assumptions.
        //
        //              ___
        //             /   \
        //            (     \
        //             \___  \
        //                 \  \
        //                  \  \
        // _    _     _____  \  \_
        //  \  /           \  \
        //   )(    ⇒        )  \
        // _/  \_     _____/    \_
        //
        //
        // OR
        //                   __
        //                  /  \
        //                 /    )
        //                /  __/
        //               /  /
        //              /  /
        // _    _     _/  /  ____
        //  \  /         /  /
        //   )(    ⇒    /  (
        // _/  \_     _/    \____
        //
        //
        // NOTE: missing diagram for where closing.index < opening.index
        if item0.is_closing() && item1.is_opening() {
            // (b'V', b'A') => {
            let closing = &mut *item0;
            let opening = &mut *item1;

            *(if closing.index >= opening.index {
                &mut closing.index
            } else {
                &mut opening.index
            }) += 2;
        } else
        // Open + Close
        // ============
        //
        //   ___
        //  /   \
        // (     \
        //  \___  \
        //      \  \
        //       \  \
        // _____  \  \_     _    _
        //      \  \         \  /
        //       )  \    ⇒    )(
        // _____/    \_     _/  \_
        //
        //        __
        //       /  \
        //      /    )
        //     /  __/
        //    /  /
        //   /  /
        // _/  /  ____     _    _
        //    /  /          \  /
        //   /  (       ⇒    )(
        // _/    \____     _/  \_
        //
        //
        // This one can't be swapped...it can only be removed
        // _____
        //      \
        //       )
        //   ___/
        //  /
        // (
        //  \_____
        //
        //
        // This one can't be swapped...it can only be removed
        //   _____
        //  /
        // (
        //  \___
        //      \
        //       )
        // _____/
        //
        //
        //
        // This one can't be simplified
        //   _
        //  / \
        // (   )
        //  \_/
        //
        //
        // This one is not possible if diagram is constructed correctly
        // _      _____
        //  \    /
        //   \  (
        // _  \  \_____
        //  \  \
        //   \  \
        //    \  \___
        //     \     \
        //      \     )
        //       \___/
        //
        // TODO: moar examples
        if item0.is_opening() && item1.is_closing() {
            // (b'A', b'V') => {
            let opening = &mut *item0;
            let closing = &mut *item1;

            if !opening.is_at_least_2_away_from(&closing) {
                return Err(match opening.small_distance_from(&closing) {
                    0 => format!(
                        "swapping adjacent opening {opening} and closing {closing} \
                        would mean removing an unknot from the diagram"
                    ),
                    1 => format!(
                        "adjacent opening {opening} and closing {closing} \
                        constitute a bulge and can't be swapped"
                    ),
                    _ => unreachable!(
                        "BUG: We just saw that opening {opening} is less than \
                        2 away from closing {closing}"
                    ),
                });
            }

            closing.index = closing.index.checked_sub(2).unwrap();
        } else
        // Not sure if this one was right. Maybe
        // I meant for the the closing to be a crossing?
        //
        // if item0.is_closing() && item1.is_opening() {
        //     let closing = &mut *item0;
        //     let opening = &mut *item1;
        //     if closing.index < opening.index {
        //         if !closing.is_at_least_2_away_from(&opening) {
        //             return Err(format!(
        //                 "swapping closing {closing} and opening {opening} \
        //                 would require splitting the crossing"
        //             ));
        //         }
        //     } else {
        //         closing.index += 2;
        //     }

        //     mem::swap(closing, opening);
        //     return Ok(());
        // }
        if item0.is_crossing() && item1.is_crossing() {
            // (b'/' | b'\\', b'/' | b'\\') => {
            let crossing0 = &mut *item0;
            let crossing1 = &mut *item1;

            if !crossing0.is_at_least_2_away_from(&crossing1) {
                return Err(format!(
                    "cannot swap {crossing0} and {crossing1} because they are too close",
                ));
            }
        } else
        // No more cases!
        {
            // _ => {
            unreachable!(
                "BUG: We should have covered all cases, but we didn't. \
                item0: {item0:?}, item1: {item1:?}",
            );
        }

        Ok((*item1, *item0))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AbbreviatedItem {
    element: u8,
    index: usize,
}

impl FromStr for AbbreviatedItem {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let element = match string.as_bytes().first().copied() {
            None => return Err("empty string".to_string()),
            Some(element) => element,
        };

        let index = string
            .get(1..)
            .ok_or_else(|| format!("Could not extract trailing index from {string:?}"))?
            .parse()
            .map_err(|e| format!("invalid index: {e}"))?;

        Self::new(element, index).map_err(|element| {
            let formatted_element = String::from_utf8(vec![element])
                .map(|element| format!("{element:?}"))
                .unwrap_or_else(|_| format!("0x{element:x}"));

            format!(
                "invalid element. Expected one of \
                'A', 'V', '\\', or '/'; got {formatted_element}"
            )
        })
    }
}

impl fmt::Display for AbbreviatedItem {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { element, index } = self;
        write!(formatter, "{element}{index}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AbbreviatedDiagram(Vec<AbbreviatedItem>);

impl FromStr for AbbreviatedDiagram {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        string
            .split('\n')
            .filter(|line| !line.starts_with('#') && !line.is_empty())
            .map(str::parse)
            .collect::<Result<Vec<_>, _>>()
            .map(Self)
    }
}

impl AbbreviatedItem {
    pub const fn new(element: u8, index: usize) -> Result<Self, u8> {
        if matches!(element, b'A' | b'V' | b'/' | b'\\') {
            Ok(Self { element, index })
        } else {
            Err(element)
        }
    }

    pub const fn affected_indices(&self) -> Option<(usize, usize)> {
        Some((self.index, try_opt!(self.index.checked_add(1))))
    }

    fn is_bulge_with(self, closing: Self) -> bool {
        self.error_on_remove_bulge(closing).is_ok()
    }

    fn error_on_remove_bulge(self, closing: Self) -> Result<(), String> {
        let opening = self;

        if !opening.is_opening() {
            return Err(format!(
                "expected opening, found {:?}",
                opening.element as char,
            ));
        }

        if !closing.is_closing() {
            return Err(format!(
                "expected closing, found {:?}",
                closing.element as char,
            ));
        }

        let distance = opening.small_distance_from(&closing);
        if distance != 1 {
            return Err(format!(
                "found opening followed by closing when looking for bulge, \
                but their vertical distance is {distance} instead of the mandatory 1",
            ));
        }

        Ok(())
    }

    fn is_reid_1_a_reduce_eligible(item0: Self, item1: Self) -> bool {
        ((item0.is_opening() && item1.is_crossing()) || (item0.is_crossing() && item1.is_closing()))
            && (item0.index == item1.index)
    }

    fn reid_1_a_reduce_index(item0: Self, item1: Self, idx: usize) -> Option<usize> {
        Self::is_reid_1_a_reduce_eligible(item0, item1)
            .then(|| idx + if item0.is_opening() { 1 } else { 0 })
    }

    fn is_reid_1_b_reduce_eligible(item0: Self, item1: Self, item2: Self) -> bool {
        item0.is_opening()
            && item1.is_crossing()
            && item2.is_closing()
            && (item0.index == item2.index)
            && item0.small_distance_from(&item1) == 1
    }

    fn is_reid_2_reduce_eligible(item0: Self, item1: Self) -> bool {
        item0.index == item1.index
            && matches!(
                (item0.element, item1.element),
                (b'/', b'\\') | (b'\\', b'/'),
            )
    }

    fn is_reid_3_eligible(item0: Self, item1: Self, item2: Self) -> bool {
        match (item0.element, item1.element, item2.element) {
            (b'/', b'\\', b'/') | (b'\\', b'/', b'\\') => false,
            (b'/' | b'\\', b'/' | b'\\', b'/' | b'\\') => {
                item0.index == item2.index && item0.small_distance_from(&item1) == 1
            }
            _ => false,
        }
    }

    fn try_reid_3(
        ref mut item0: Self,
        ref mut item1: Self,
        ref mut item2: Self,
    ) -> Result<(Self, Self, Self), String> {
        if Self::is_reid_3_eligible(*item0, *item1, *item2) {
            mem::swap(&mut item0.element, &mut item2.element);
            mem::swap(&mut item0.index, &mut item1.index);
            item2.index = item0.index;

            Ok((*item0, *item1, *item2))
        } else {
            Err(format!("cannot apply Reid 3 to {item0}, {item1}, {item2}",))
        }
    }
}

impl AbbreviatedDiagram {
    pub fn new_from_tuples(tuples: Vec<(u8, usize)>) -> Result<Self, String> {
        Ok({
            Self(
                tuples
                    .into_iter()
                    .enumerate()
                    .map(|(position, (element, index))| {
                        AbbreviatedItem::new(element, index).map_err(|element| {
                            let formatted_element = String::from_utf8(vec![element])
                                .map(|element| format!("{element:?}"))
                                .unwrap_or_else(|_| format!("0x{element:x}"));

                            format!(
                                "invalid element at position {position}. Expected one of \
                                'A', 'V', '\\', '/', got {formatted_element}"
                            )
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            )
        })
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    fn height(&self) -> usize {
        self.0
            .iter()
            .fold((0isize, 0usize), |(mut num_open, max_open), item| {
                num_open += match item.element {
                    b'A' => 1,
                    b'V' => -1,
                    _ => 0,
                };

                (num_open, max_open.max(num_open as usize))
            })
            .1
            * 2
    }

    pub fn ascii_print<const GRID_BORDERS: bool>(&self) -> String {
        VerboseDiagram::from_abbreviated(self)
            .unwrap()
            .display::<GRID_BORDERS>()
            .collect::<String>()
    }

    pub fn ascii_print_compact<const GRID_BORDERS: bool>(&self) -> String {
        let inner = VerboseDiagram::from_abbreviated(self)
            .unwrap()
            .display::<GRID_BORDERS>()
            .collect::<Vec<_>>();

        let string_len = inner.first().unwrap().len();

        let mut out = (0..inner.len())
            .map(|_| String::with_capacity(string_len))
            .collect::<Vec<_>>();

        for idx in 0..string_len {
            if inner
                .iter()
                .all(|line| matches!(&line[idx..idx + 1], " " | "_"))
            {
                continue;
            }

            out.iter_mut().zip(inner.iter()).for_each(|(out, inner)| {
                out.push_str(&inner[idx..idx + 1]);
            });
        }

        out.into_iter().collect()
    }

    pub fn to_tuples(&self) -> Vec<(u8, usize)> {
        self.0
            .iter()
            .map(|item| (item.element, item.index))
            .collect()
    }
}

pub fn ascii_print<const GRID_BORDERS: bool>(knot: Vec<(u8, usize)>) -> String {
    AbbreviatedDiagram::new_from_tuples(knot)
        .unwrap()
        .ascii_print::<GRID_BORDERS>()
}

pub fn ascii_print_compact<const GRID_BORDERS: bool>(knot: Vec<(u8, usize)>) -> String {
    AbbreviatedDiagram::new_from_tuples(knot)
        .unwrap()
        .ascii_print_compact::<GRID_BORDERS>()
}

#[test]
fn snapshot_ascii_print() {
    // Unknot:
    //
    //  /\
    // <  >
    //  \/
    //
    let unknot = vec![(b'A', 0), (b'V', 0)];
    insta::assert_snapshot!(ascii_print_compact::<false>(unknot));

    // Trefoil:
    //       _____________
    //      /             \
    //     <               >
    //      \__   _____   /
    //         \ /     \ /
    //          \       \
    //   ______/ \_   _/ \________
    //  /          \ /            \
    // <            /              >
    //  \__________/ \____________/
    //
    //
    let trefoil = vec![
        (b'A', 0),
        (b'A', 2),
        (b'/', 1),
        (b'\\', 0),
        (b'/', 1),
        (b'V', 2),
        (b'V', 0),
    ];
    insta::assert_snapshot!(ascii_print_compact::<false>(trefoil));

    // donut:
    let donut = vec![(b'A', 0), (b'A', 1), (b'V', 1), (b'V', 0)];
    insta::assert_snapshot!(ascii_print_compact::<false>(donut));

    // C:
    let c_thingy = vec![(b'A', 0), (b'A', 1), (b'V', 2), (b'V', 0)];
    insta::assert_snapshot!(ascii_print_compact::<false>(c_thingy));

    // weird terrace thing:
    let terrace = vec![
        (b'A', 0),
        (b'A', 2),
        (b'A', 4),
        (b'A', 6),
        (b'V', 5),
        (b'V', 3),
        (b'V', 1),
        (b'A', 1),
        (b'A', 3),
        (b'A', 5),
        (b'V', 6),
        (b'V', 4),
        (b'V', 2),
        (b'V', 0),
    ];
    insta::assert_snapshot!(ascii_print_compact::<false>(terrace));

    // basket:
    let basket = vec![
        (b'A', 0),
        (b'A', 1),
        (b'A', 1),
        (b'/', 3),
        (b'/', 2),
        (b'/', 4),
        (b'/', 3),
        (b'V', 1),
        (b'V', 1),
        (b'V', 0),
    ];
    insta::assert_snapshot!(ascii_print_compact::<false>(basket));

    // ugly trefoil:
    let ugly_trefoil = vec![
        (b'A', 0),
        (b'A', 0),
        (b'/', 1),
        (b'\\', 0),
        (b'/', 1),
        (b'V', 0),
        (b'V', 0),
    ];
    insta::assert_snapshot!(ascii_print_compact::<false>(ugly_trefoil));

    // weird_thing_that_broke_once:
    let weird_thing_that_broke_once = vec![
        (b'A', 0),
        (b'A', 2),
        (b'V', 0),
        (b'A', 2),
        (b'V', 2),
        (b'A', 0),
        (b'V', 1),
        (b'V', 0),
    ];
    insta::assert_snapshot!(ascii_print_compact::<false>(weird_thing_that_broke_once));
}
