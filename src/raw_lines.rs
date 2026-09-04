use crate::render::Horiz;

/// The grid under construction and which rows currently carry a strand. Every
/// placement drives the same emitter, so the two placement modes cannot drift
/// on how a placed diagram becomes cells.
pub(crate) struct Grid {
    lines: Vec<Vec<Horiz>>,
    live: Vec<bool>,
}

impl Grid {
    fn new(height: usize, columns: usize) -> Self {
        Self {
            lines: vec![Vec::with_capacity(columns); height],
            live: vec![false; height],
        }
    }

    fn height(&self) -> usize {
        self.live.len()
    }

    fn into_lines(self) -> Vec<Vec<Horiz>> {
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
}

/// Builds the opening-centered grid under index-aligned placement, in which a
/// feature at abbreviated index `idx` sits at row `idx` alone rather than
/// straddling rows `idx` and `idx + 1`. That leaves the cell above a feature
/// empty, so `Horiz::subsequent` can no longer say whether the next column
/// carries a strand — the live levels are tracked instead.
pub(crate) struct OpeningCentered {
    grid: Grid,
}

impl OpeningCentered {
    pub(crate) fn new(height: usize, columns: usize) -> Self {
        Self {
            grid: Grid::new(height, columns),
        }
    }

    pub(crate) fn into_lines(self) -> Vec<Vec<Horiz>> {
        self.grid.into_lines()
    }

    fn raise_once(&mut self, from: usize) {
        let glyphs: Vec<_> = (from..self.grid.height())
            .filter(|&row| self.grid.live[row])
            .map(|row| (row, Horiz::TransferUp))
            .collect();

        self.grid.column(&glyphs);

        for row in (from + 1..self.grid.height()).rev() {
            self.grid.live[row] = self.grid.live[row - 1];
        }
        self.grid.live[from] = false;
    }

    fn lower_once(&mut self, from: usize) {
        let glyphs: Vec<_> = (from..self.grid.height())
            .filter(|&row| self.grid.live[row])
            .map(|row| (row - 1, Horiz::TransferDown))
            .collect();

        self.grid.column(&glyphs);

        for row in from - 1..self.grid.height() - 1 {
            self.grid.live[row] = self.grid.live[row + 1];
        }
        *self.grid.live.last_mut().unwrap() = false;
    }

    pub(crate) fn append(&mut self, element: u8, idx: usize) {
        match element {
            b'(' => {
                if self.grid.live[idx..].iter().any(|live| *live) {
                    // One cell per level of climb, and the opening cannot
                    // share either column: its own shadow row is where the
                    // rising strand would land.
                    self.raise_once(idx);
                    self.raise_once(idx + 1);
                }

                self.grid.column(&[(idx, Horiz::OpenedBelow)]);
                self.grid.live[idx] = true;
                self.grid.live[idx + 1] = true;
            }
            b')' => {
                let above = self.grid.live[idx + 2..].iter().any(|live| *live);

                self.grid.column(&[(idx, Horiz::ClosedBelow)]);
                self.grid.live[idx] = false;
                self.grid.live[idx + 1] = false;

                if above {
                    self.lower_once(idx + 2);
                    self.lower_once(idx + 1);
                }
            }
            b'\\' => self.grid.column(&[(idx, Horiz::CrossDownOver)]),
            b'/' => self.grid.column(&[(idx, Horiz::CrossDownUnder)]),
            _ => unimplemented!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn snapshot_opening_centered_append() {
        let mut lines = OpeningCentered::new(4, 8);

        lines.append(b'(', 0);
        insta::assert_debug_snapshot!(lines.grid.lines);

        lines.append(b'(', 1);
        insta::assert_debug_snapshot!(lines.grid.lines);

        lines.append(b'\\', 1);
        insta::assert_debug_snapshot!(lines.grid.lines);

        lines.append(b')', 1);
        insta::assert_debug_snapshot!(lines.grid.lines);

        lines.append(b')', 0);
        insta::assert_debug_snapshot!(lines.grid.lines);
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
        insta::assert_debug_snapshot!(lines.grid.lines);

        lines.append(b')', 2);
        insta::assert_debug_snapshot!(lines.grid.lines);
    }

}
