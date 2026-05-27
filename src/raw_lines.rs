use std::collections::VecDeque;

use crate::render::Horiz;

pub(crate) fn is_empty_above(lines: &[Vec<Horiz>], idx: usize) -> bool {
    lines.get(idx..).unwrap_or_default().iter().all(|line| {
        line.last()
            .cloned()
            .unwrap_or_default()
            .subsequent()
            .is_empty()
    })
}

pub(crate) fn advance(lines: &mut [Vec<Horiz>]) {
    lines
        .iter_mut()
        .for_each(|line| line.push(line.last().unwrap_or(&Horiz::Empty).subsequent()));
}

pub(crate) fn expand_above(lines: &mut [Vec<Horiz>], idx: usize) {
    let (lower, upper) = lines.split_at_mut(idx);
    for _ in 0..3 {
        advance(lower);
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

pub(crate) fn contract_above(lines: &mut [Vec<Horiz>], idx: usize) {
    let (lower, upper) = lines.split_at_mut(idx);
    for _ in 0..3 {
        advance(lower);
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

pub(crate) fn append(lines: &mut [Vec<Horiz>], element: u8, idx: usize) {
    match element {
        b'(' => {
            if is_empty_above(&*lines, idx) {
                advance(lines);
            } else {
                expand_above(lines, idx);
            }
            *lines[idx].last_mut().unwrap() = Horiz::OpenedAbove;
            *lines[idx + 1].last_mut().unwrap() = Horiz::OpenedBelow;
        }
        b')' => {
            let is_empty_above = is_empty_above(&*lines, idx + 2);
            if is_empty_above {
                advance(lines);
                *lines[idx].last_mut().unwrap() = Horiz::ClosedAbove;
                *lines[idx + 1].last_mut().unwrap() = Horiz::ClosedBelow;
            } else {
                contract_above(lines, idx);
            }
        }
        b'\\' => {
            advance(lines);

            *lines[idx].last_mut().unwrap() = Horiz::CrossUpUnder;
            *lines[idx + 1].last_mut().unwrap() = Horiz::CrossDownOver;
        }
        b'/' => {
            advance(lines);

            *lines[idx].last_mut().unwrap() = Horiz::CrossUpOver;
            *lines[idx + 1].last_mut().unwrap() = Horiz::CrossDownUnder;
        }
        _ => unimplemented!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn snapshot_raw_lines_expand_contract() {
        use Horiz::*;

        let mut lines = vec![vec![Line], vec![Line], vec![Line], vec![Empty], vec![Empty]];
        let original_lines = lines.clone();

        expand_above(&mut lines, 1);
        insta::assert_debug_snapshot!(lines);

        contract_above(&mut lines, 1);
        insta::assert_debug_snapshot!(lines);

        advance(&mut lines);
        let final_column = lines
            .iter()
            .map(|line| vec![line.last().cloned().unwrap()])
            .collect::<Vec<_>>();
        assert_eq!(final_column, original_lines);
    }

    #[test]
    fn snapshot_raw_lines_append() {
        let mut lines = vec![vec![]; 4];

        append(&mut lines, b'(', 0);
        insta::assert_debug_snapshot!(lines);

        append(&mut lines, b'(', 1);
        insta::assert_debug_snapshot!(lines);

        append(&mut lines, b')', 0);
        insta::assert_debug_snapshot!(lines);

        append(&mut lines, b')', 0);
        insta::assert_debug_snapshot!(lines);
    }
}
