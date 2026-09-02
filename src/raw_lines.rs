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

/// Builds the opening-centered grid, in which a feature at abbreviated index
/// `idx` sits at row `idx` alone rather than straddling rows `idx` and
/// `idx + 1`. That leaves the cell above a feature empty, so `Horiz::subsequent`
/// can no longer say whether the next column carries a strand — the live levels
/// are tracked instead.
pub(crate) struct OpeningCentered {
    lines: Vec<Vec<Horiz>>,
    live: Vec<bool>,
}

impl OpeningCentered {
    pub(crate) fn new(height: usize, columns: usize) -> Self {
        Self {
            lines: vec![Vec::with_capacity(columns); height],
            live: vec![false; height],
        }
    }

    pub(crate) fn into_lines(self) -> Vec<Vec<Horiz>> {
        self.lines
    }

    fn column(&mut self, glyphs: &[(usize, Horiz)]) {
        for row in 0..self.live.len() {
            // A glyph joins the levels above and below it inside its own
            // cell, so the cell directly above it stays empty; a line there
            // would draw a strand a whole cell too long.
            let shadowed = glyphs.iter().any(|&(at, _)| at + 1 == row);

            self.lines[row].push(match glyphs.iter().find(|&&(at, _)| at == row) {
                Some(&(_, glyph)) => glyph,
                None if shadowed || !self.live[row] => Horiz::Empty,
                None => Horiz::Line,
            });
        }
    }

    fn raise_once(&mut self, from: usize) {
        let glyphs: Vec<_> = (from..self.live.len())
            .filter(|&row| self.live[row])
            .map(|row| (row, Horiz::TransferUp))
            .collect();

        self.column(&glyphs);

        for row in (from + 1..self.live.len()).rev() {
            self.live[row] = self.live[row - 1];
        }
        self.live[from] = false;
    }

    fn lower_once(&mut self, from: usize) {
        let glyphs: Vec<_> = (from..self.live.len())
            .filter(|&row| self.live[row])
            .map(|row| (row - 1, Horiz::TransferDown))
            .collect();

        self.column(&glyphs);

        for row in from - 1..self.live.len() - 1 {
            self.live[row] = self.live[row + 1];
        }
        *self.live.last_mut().unwrap() = false;
    }

    pub(crate) fn append(&mut self, element: u8, idx: usize) {
        match element {
            b'(' => {
                if self.live[idx..].iter().any(|live| *live) {
                    // One cell per level of climb, and the opening cannot
                    // share either column: its own shadow row is where the
                    // rising strand would land.
                    self.raise_once(idx);
                    self.raise_once(idx + 1);
                }

                self.column(&[(idx, Horiz::OpenedBelow)]);
                self.live[idx] = true;
                self.live[idx + 1] = true;
            }
            b')' => {
                let above = self.live[idx + 2..].iter().any(|live| *live);

                self.column(&[(idx, Horiz::ClosedBelow)]);
                self.live[idx] = false;
                self.live[idx + 1] = false;

                if above {
                    self.lower_once(idx + 2);
                    self.lower_once(idx + 1);
                }
            }
            b'\\' => self.column(&[(idx, Horiz::CrossDownOver)]),
            b'/' => self.column(&[(idx, Horiz::CrossDownUnder)]),
            _ => unimplemented!(),
        }
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
    fn snapshot_opening_centered_append() {
        let mut lines = OpeningCentered::new(4, 8);

        lines.append(b'(', 0);
        insta::assert_debug_snapshot!(lines.lines);

        lines.append(b'(', 1);
        insta::assert_debug_snapshot!(lines.lines);

        lines.append(b'\\', 1);
        insta::assert_debug_snapshot!(lines.lines);

        lines.append(b')', 1);
        insta::assert_debug_snapshot!(lines.lines);

        lines.append(b')', 0);
        insta::assert_debug_snapshot!(lines.lines);
    }

    /// The climb an opening has to make when something is already live above it,
    /// and the descent a closing gives back — one whole cell per level, which is
    /// what makes `expand_above`/`contract_above`'s part-way-through halves
    /// unnecessary here.
    #[test]
    fn snapshot_opening_centered_raise_and_lower() {
        let mut lines = OpeningCentered::new(5, 6);

        lines.append(b'(', 0);
        lines.append(b'(', 0);
        insta::assert_debug_snapshot!(lines.lines);

        lines.append(b')', 2);
        insta::assert_debug_snapshot!(lines.lines);
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
