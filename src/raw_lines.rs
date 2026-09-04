use crate::diagram::AbbreviatedItem;
use crate::render::Horiz;

/// Assigns every strand the row it holds for its whole flat run: one more than
/// the tallest strand ever below it, or zero when nothing ever is. Returns
/// `(lower, upper)` per opening, in opening order.
///
/// The gap between a pair's two strands falls out of this; it is never computed
/// directly. Deriving it directly is what produced two wrong rules — notably
/// that the gap is the count of strands opened between the pair, which agrees
/// with every supplied fixture and is still wrong.
pub(crate) fn strand_heights(items: &[AbbreviatedItem]) -> Vec<(usize, usize)> {
    // Strands are numbered as they open. `below[s]` collects the strands seen
    // immediately under `s`; only adjacent neighbours are needed, since the
    // live strands are totally ordered at any instant, so any "ever below"
    // relation is realized by a chain of adjacencies at that same instant.
    let mut stack: Vec<usize> = Vec::new();
    let mut below: Vec<Vec<usize>> = Vec::new();
    let mut openings: Vec<(usize, usize)> = Vec::new();

    for &AbbreviatedItem { element, index } in items {
        match element {
            b'(' => {
                let (lower, upper) = (below.len(), below.len() + 1);
                below.push(Vec::new());
                below.push(Vec::new());
                openings.push((lower, upper));
                stack.splice(index..index, [lower, upper]);
            }
            b')' => {
                stack.drain(index..index + 2);
            }
            // Crossings join two levels without reordering them.
            _ => continue,
        }

        for pair in stack.windows(2) {
            let (under, over) = (pair[0], pair[1]);
            if !below[over].contains(&under) {
                below[over].push(under);
            }
        }
    }

    let mut heights = vec![None; below.len()];
    openings
        .into_iter()
        .map(|(lower, upper)| {
            (
                height_of(lower, &below, &mut heights),
                height_of(upper, &below, &mut heights),
            )
        })
        .collect()
}

/// Longest path to `strand` through the "ever below" relation. The relation is
/// acyclic because co-live strands never change relative order, so a plain memo
/// suffices.
fn height_of(strand: usize, below: &[Vec<usize>], heights: &mut Vec<Option<usize>>) -> usize {
    if let Some(height) = heights[strand] {
        return height;
    }

    let height = below[strand]
        .clone()
        .into_iter()
        .map(|under| height_of(under, below, heights) + 1)
        .max()
        .unwrap_or(0);

    heights[strand] = Some(height);
    height
}

/// Rows the grid needs: one past the tallest strand. Exceeds the count of
/// simultaneously live strands whenever a pair diverges, because a divergent
/// pair holds its gap open for its whole life.
pub(crate) fn grid_height(heights: &[(usize, usize)]) -> usize {
    heights
        .iter()
        .map(|&(_, upper)| upper + 1)
        .max()
        .unwrap_or(0)
}

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

    mod height_fixtures {
        use super::*;
        use crate::diagram::AbbreviatedDiagram;
        use std::str::FromStr;

        fn heights(encoding: &str) -> Vec<(usize, usize)> {
            strand_heights(&AbbreviatedDiagram::from_str(encoding).unwrap().items)
        }

        /// Owner-supplied expected values, transcribed from
        /// `specs/007-strand-height-precalc/fixtures/`. Never derived from
        /// running this code.
        const FIXTURES: &[(&str, &str, &[(usize, usize)])] = &[
            (
                "rotated-5_1",
                "(0 (2 (4 (6 /1 /3 /5 )4 )2 \\0 \\2 )1 )0",
                &[(0, 1), (2, 3), (4, 5), (6, 7)],
            ),
            (
                "square-knot",
                "(0 (0 \\1 (1 /0 /2 )1 \\1 )0 )0",
                &[(4, 5), (0, 3), (1, 2)],
            ),
            (
                "non-adjacent-crossing",
                "(0 (0 /1 (2 )2 \\1 /2 \\1 )1 )0",
                &[(4, 5), (0, 1), (2, 3)],
            ),
            (
                "little-dumb-link",
                "(0 (0 )2 (2 (4 )2 (3 )3 )1 )0",
                &[(2, 3), (0, 1), (2, 3), (4, 7), (5, 6)],
            ),
            (
                "square-knot-links-encircled",
                "(0 (1 (3 (3 (7 (7 )7 \\4 (4 /3 /5 )4 \\4 )3 )3 )3 (1 )3 )1 )0",
                &[
                    (0, 15),
                    (3, 4),
                    (9, 10),
                    (5, 8),
                    (13, 14),
                    (11, 12),
                    (6, 7),
                    (1, 2),
                ],
            ),
        ];

        #[test]
        fn fixtures_match_supplied_heights() {
            for (name, encoding, expected) in FIXTURES {
                assert_eq!(&heights(encoding), expected, "fixture {name}");
            }
        }

        /// The five fixtures above cannot distinguish the correct rule from the
        /// plausible wrong one — that a pair's gap is the count of strands
        /// opened between it, which agrees with all 23 of their pairs. These
        /// three can.
        #[test]
        fn discriminating_cases() {
            // Sequential siblings never coexist, so the second reuses the
            // first's rows. The counting rule would demand a gap of 4.
            assert_eq!(
                heights("(0 (1 )1 (1 )1 )0"),
                [(0, 3), (1, 2), (1, 2)],
                "sequential siblings",
            );

            // A sibling stacked above a divergent pair cannot reuse the rows
            // that pair holds open.
            assert_eq!(
                heights("(0 (1 (2 )2 (3 )3 )1 )0"),
                [(0, 7), (1, 4), (2, 3), (5, 6)],
                "sibling stacked above a divergent pair",
            );

            // Two strands that never coexist and never relate are still pushed
            // apart, because a third lies between them in the order.
            assert_eq!(
                heights("(0 (0 )0 (2 )2 )0"),
                [(2, 3), (0, 1), (4, 5)],
                "transitive push",
            );
        }

        #[test]
        fn degenerate_diagrams() {
            assert_eq!(heights(""), []);
            assert_eq!(heights("(0 )0"), [(0, 1)]);
            assert_eq!(heights("(0 (1 (2 )2 )1 )0"), [(0, 5), (1, 4), (2, 3)]);
        }

        #[test]
        fn deterministic() {
            for (_, encoding, _) in FIXTURES {
                assert_eq!(heights(encoding), heights(encoding));
            }
        }

        #[test]
        fn grid_height_is_one_past_the_tallest_strand() {
            for (name, encoding, expected) in FIXTURES {
                let tallest = expected.iter().map(|&(_, upper)| upper).max();
                assert_eq!(
                    grid_height(&heights(encoding)),
                    tallest.map_or(0, |tallest| tallest + 1),
                    "fixture {name}",
                );
            }
        }
    }
}
