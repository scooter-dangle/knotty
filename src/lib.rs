// struct Diagram(

#[cfg(test)]
use pretty_assertions::assert_eq;

use std::collections::VecDeque;

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

impl Horiz {
    #[rustfmt::skip]
    pub const fn display(&self) -> [&'static str; 3] {
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
                r#" < "#,
            ],
            OpenedAbove => [
                r#"  \"#,
                r#"   "#,
                r#"   "#,
            ],
            ClosedBelow => [
                r#"   "#,
                r#"\  "#,
                r#" > "#,
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
}

const HORIZ_LEN: usize = Horiz::Empty.display()[0].len();

impl VerboseLine {
    pub fn display(&self) -> [String; 3] {
        let mut l0 = " ".repeat(self.0.len() * HORIZ_LEN) + "\n";
        let mut l1 = l0.clone();
        let mut l2 = l0.clone();

        for (idx, horiz) in self.0.iter().enumerate() {
            let [h0, h1, h2] = horiz.display();
            let range = (idx * HORIZ_LEN)..((idx + 1) * HORIZ_LEN);

            l0.replace_range(range.clone(), h0);
            l1.replace_range(range.clone(), h1);
            l2.replace_range(range, h2);
        }

        [l0, l1, l2]
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
            let is_empty = line.last().cloned().unwrap_or_default().is_empty();

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
            let is_empty =
                (0..2).contains(&idx) || line.last().cloned().unwrap_or_default().is_empty();

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

fn raw_lines_append(lines: &mut [Vec<Horiz>], idx: usize, element: u8) {
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

    raw_lines_append(&mut lines, 0, b'A');
    insta::assert_debug_snapshot!(lines);

    raw_lines_append(&mut lines, 1, b'A');
    insta::assert_debug_snapshot!(lines);

    raw_lines_append(&mut lines, 0, b'V');
    insta::assert_debug_snapshot!(lines);

    raw_lines_append(&mut lines, 0, b'V');
    insta::assert_debug_snapshot!(lines);
}

impl VerboseDiagram {
    pub fn display<'a>(&'a self) -> impl 'a + Iterator<Item = String> {
        let last_idx = self.0.len() - 1;

        self.0
            .iter()
            .rev()
            .enumerate()
            .flat_map(move |(idx, line)| {
                line.display()
                    .into_iter()
                    .take(if idx == last_idx { 1 } else { 3 })
            })
    }

    pub fn from_abbreviated(knot: &AbbreviatedDiagram) -> Result<Self, String> {
        let height = knot.height();

        let mut lines: Vec<Vec<Horiz>> = vec![Vec::with_capacity(knot.len()); height];

        for (idx, element) in knot.0.iter() {
            raw_lines_append(&mut lines, *idx, *element);
        }

        Ok(Self(lines.into_iter().map(VerboseLine).collect()))
    }
}

#[test]
fn snapshot_from_abbreviated() {
    let knot = AbbreviatedDiagram(vec![(0, b'A'), (0, b'/'), (0, b'V')]);

    let verbose = VerboseDiagram::from_abbreviated(&knot).unwrap();
    insta::assert_debug_snapshot!(verbose);
}

pub struct AbbreviatedDiagram(Vec<(usize, u8)>);

impl AbbreviatedDiagram {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    fn height(&self) -> usize {
        self.0
            .iter()
            .fold((0isize, 0usize), |(mut num_open, max_open), (_, horiz)| {
                num_open += match horiz {
                    b'A' => 1,
                    b'V' => -1,
                    _ => 0,
                };

                (num_open, max_open.max(num_open as usize))
            })
            .1
            * 2
    }
}

pub fn ascii_print(knot: Vec<(usize, u8)>) -> String {
    VerboseDiagram::from_abbreviated(&AbbreviatedDiagram(knot))
        .unwrap()
        .display()
        .collect::<String>()
}

pub fn ascii_print_compact(knot: Vec<(usize, u8)>) -> String {
    let inner = VerboseDiagram::from_abbreviated(&AbbreviatedDiagram(knot))
        .unwrap()
        .display()
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

#[test]
fn snapshot_ascii_print() {
    // Unknot:
    //
    //  /\
    // <  >
    //  \/
    //
    let unknot = vec![(0, b'A'), (0, b'V')];
    insta::assert_snapshot!(ascii_print_compact(unknot));

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
        (0, b'A'),
        (2, b'A'),
        (1, b'/'),
        (0, b'\\'),
        (1, b'/'),
        (2, b'V'),
        (0, b'V'),
    ];
    insta::assert_snapshot!(ascii_print_compact(trefoil));

    // donut:
    let donut = vec![(0, b'A'), (1, b'A'), (1, b'V'), (0, b'V')];
    insta::assert_snapshot!(ascii_print_compact(donut));

    // C:
    let donut = vec![(0, b'A'), (1, b'A'), (2, b'V'), (0, b'V')];
    insta::assert_snapshot!(ascii_print_compact(donut));

    // weird terrace thing:
    let terrace = vec![
        (0, b'A'),
        (2, b'A'),
        (4, b'A'),
        (6, b'A'),
        (5, b'V'),
        (3, b'V'),
        (1, b'V'),
        (1, b'A'),
        (3, b'A'),
        (5, b'A'),
        (6, b'V'),
        (4, b'V'),
        (2, b'V'),
        (0, b'V'),
    ];
    insta::assert_snapshot!(ascii_print_compact(terrace));

    // basket:
    let basket = vec![
        (0, b'A'),
        (1, b'A'),
        (1, b'A'),
        (3, b'/'),
        (2, b'/'),
        (4, b'/'),
        (3, b'/'),
        (1, b'V'),
        (1, b'V'),
        (0, b'V'),
    ];
    insta::assert_snapshot!(ascii_print_compact(basket));

    // ugly trefoil:
    let ugly_trefoil = vec![
        (0, b'A'),
        (0, b'A'),
        (1, b'/'),
        (0, b'\\'),
        (1, b'/'),
        (0, b'V'),
        (0, b'V'),
    ];
    insta::assert_snapshot!(ascii_print_compact(ugly_trefoil));
}
