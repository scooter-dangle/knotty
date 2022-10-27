#[cfg(test)]
use pretty_assertions::assert_eq;

use std::{collections::VecDeque, str::FromStr};

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
struct AbbreviatedItem {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AbbreviatedDiagram(Vec<AbbreviatedItem>);

impl FromStr for AbbreviatedDiagram {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        string
            .split('\n')
            .filter(|line| !line.starts_with('#') && !line.is_empty())
            .map(|line| line.parse())
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
